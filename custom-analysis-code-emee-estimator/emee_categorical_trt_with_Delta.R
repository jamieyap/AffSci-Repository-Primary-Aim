################################################################################
# Code below this line was received from Tianchen Qian
################################################################################

##########################################
## Tianchen Qian, Yihan Bao
## 2023.05.07


emee_categorical_trt_with_Delta <- function(
    dta,
    id_varname,
    decision_time_varname,
    treatment_varname,
    outcome_varname,
    control_varname,
    moderator_varname,
    rand_prob_varname,
    rand_prob_A0_varname,
    avail_varname = NULL,
    rand_prob_tilde_varname = NULL, # \tilde{p}_t(1|H_t) in WCLS (variable name in the data set)
    rand_prob_tilde = NULL, # \tilde{p}_t(1|H_t) in WCLS (numeric number or vector)
    estimator_initial_value = NULL,
    Delta = 1,
    num_trt = 2) {

    ### 1. preparation ###

    sample_size <- length(unique(dta[, id_varname]))
    total_person_decisionpoint <- nrow(dta)

    if (is.null(avail_varname)) {
        avail <- rep(1, total_person_decisionpoint)
    } else {
        avail <- dta[, avail_varname]
    }

    A <- dta[, treatment_varname]
    
    # checking for NA in treatment indicator
    if (any(is.na(A[avail == 1]))) {
        stop("Treatment indicator is NA where availability = 1.")
    }
    A[avail == 0] <- 0

    p_t <- dta[, rand_prob_varname] # prob_A
    cA <- A - p_t # centered A
    Y <- dta[, outcome_varname]

    Xdm <- as.matrix(cbind(rep(1, nrow(dta)), dta[, moderator_varname]))
    # X (moderator) design matrix, intercept added

    Zdm <- as.matrix(cbind(rep(1, nrow(dta)), dta[, control_varname]))
    # Z (control) design matrix, intercept added

    if (is.null(rand_prob_tilde_varname) & is.null(rand_prob_tilde)) {
        p_t_tilde_mat <- matrix(rep(1 / (num_trt + 1), (num_trt + 1) * nrow(dta)), ncol = (num_trt + 1))
    } else if (is.null(rand_prob_tilde_varname)) {
        if (length(rand_prob_tilde) == num_trt + 1) {
            # if rand_prob_tilde is the same for all timepoints
            p_t_tilde_mat <- matrix(NA, nrow = total_person_decisionpoint, ncol = (num_trt + 1))
            for (k in 1:total_person_decisionpoint) {
                p_t_tilde_mat[k, ] <- rand_prob_tilde
            }
        } else if (dim(rand_prob_tilde)[1] == total_person_decisionpoint &
            dim(rand_prob_tilde)[2] == num_trt + 1) {
            # if rand_prob_tilde is a matrix
            p_t_tilde_mat <- rand_prob_tilde
        } else {
            stop("rand_prob_tilde is of incorrect length.")
        }
    } else {
        p_t_tilde_mat <- dta[, rand_prob_tilde_varname]
    }
    # p_t_tilde_mat is a matrix where each row vector is
    # p_t_tilde(A = 0), ..., p_t_tilde(A = num_trt) at that given time point t

    cA_tilde <- matrix(NA, nrow = total_person_decisionpoint, ncol = num_trt)
    # a center matrix with dim total_decisionpoint * number of treatments (A = 1, A = 2...)

    for (k in 1:total_person_decisionpoint) {
        for (j in 1:(num_trt)) {
            cA_tilde[k, j] <- (A[k] == j) - p_t_tilde_mat[k, j + 1]
            # cA_tilde <- rbind(cA_tilde, sum(A[k] == j) - p_t_tilde_mat[k,j])
        }
    }

    # inverse probability term multiply from t+1 to t+Delta-1
    WCLS_weight_inverseProb <- c()
    if (Delta == 1) {
        WCLS_weight_inverseProb <- rep(1, total_person_decisionpoint)
    } else {
        for (i in 1:sample_size) {
            dta_perid <- dta[dta[, id_varname] == i, ]
            dta_timepoint <- length(dta_perid[, decision_time_varname])
            inverseProb_numerator <- c(dta_perid[, treatment_varname] == 0, rep(1, Delta - 1))
            # append with A_{T+1} ... A_{T+\delta-1} = 0

            inverseProb_denominator <- c(dta_perid[, rand_prob_A0_varname], rep(1, Delta - 1))
            # append with p_{T+1}(A=0) ... p_{T+\delta-1}(A=0) = 1

            for (j in 1:dta_timepoint) {
                WCLS_weight_inverseProb <- c(WCLS_weight_inverseProb, prod(inverseProb_numerator[(j + 1):(j + Delta - 1)] /
                    inverseProb_denominator[(j + 1):(j + Delta - 1)]))
            }
        }
    }

    # for each time point k, pick out p_t_tilde for the actual corresponding A(k)
    p_t_tilde <- rep(NA, total_person_decisionpoint)
    for (k in 1:total_person_decisionpoint) {
        p_t_tilde[k] <- p_t_tilde_mat[k, A[k] + 1]
    }

    # WCLS_weight_withDelta corresponds to J_t in paper
    WCLS_weight_withDelta <- p_t_tilde / p_t * WCLS_weight_inverseProb

    p <- length(moderator_varname) + 1 # dimension of beta
    q <- length(control_varname) + 1 # dimension of alpha

    Xnames <- c("Intercept", moderator_varname)
    Znames <- c("Intercept", control_varname)

    ### 2. estimation ###

    estimating_equation <- function(theta) {
        # length of alpha is q, and length of beta_k is p for all level of treatments

        alpha <- as.matrix(theta[1:q])
        beta <- matrix(theta[(q + 1):(q + num_trt * p)], ncol = num_trt, byrow = FALSE)
        # dimension of beta is p * num_trt

        exp_Zdm_alpha <- exp(Zdm %*% alpha)

        exp_AXdm_beta <- rep(NA, total_person_decisionpoint)
        for (t in 1:total_person_decisionpoint) {
            exp_AXdm_beta[t] <- exp(sum(ind(A[t], num_trt) * (Xdm %*% beta)[t, ]))
            # ind(A[t],num_trt) is a vector (1(A_t == k)) with length 1 * num_trt
            # Noted here Xdm %*% beta is a matrix with dim total_person_decisionpoint * num_trt
        }

        residual <- Y - exp_Zdm_alpha * exp_AXdm_beta # a vector of length total_person_decisionpoint * 1
        weight <- exp_AXdm_beta^(-1) # a vector of length total_person_decisionpoint * 1

        ef <- rep(NA, q + p * num_trt) # value of estimating function
        for (i in 1:q) {
            ef[i] <- sum(weight * residual * avail * WCLS_weight_withDelta * Zdm[, i])
            # Zdm is g(H_t), the control variables with interept included
        }

        for (j in 1:num_trt) {
            for (i in 1:p) {
                ef[q + (j - 1) * p + i] <- sum(weight * residual * avail * WCLS_weight_withDelta * cA_tilde[, j] * Xdm[, i])
                # Xdm is S_t, the moderator variables with intercept included, with dim total_person_decisionpoint * p
                # cA_tilde is a matrix with dim total_person_decisionpoint * num_trt
            }
        }
        ef <- ef / sample_size # mean over the number of individuals
        return(ef)
    }

    if (is.null(estimator_initial_value)) {
        estimator_initial_value <- rep(0, length = num_trt * p + q)
    }

    solution <- tryCatch(
        {
            multiroot(estimating_equation, estimator_initial_value)
        },
        error = function(cond) {
            message("\nCatched error in multiroot inside weighted_centered_least_square():")
            message(cond)
            return(list(
                root = rep(NaN, num_trt * p + q), msg = cond,
                f.root = rep(NaN, num_trt * p + q)
            ))
        }
    )

    estimator <- get_alpha_beta_from_multiroot_result(solution, p, q, num_trt) # in the order of alpha, beta_1, ... beta_{num_trt}
    alpha_hat <- estimator$alpha # a matrix of dim q * 1
    beta_hat <- estimator$beta # a matrix of dim p * num_trt

    ### 3. asymptotic variance ###

    ### 3.1 Compute M_n matrix (M_n is the empirical expectation of the derivative of the estimating function) ###

    Mn_summand <- array(NA, dim = c(total_person_decisionpoint, num_trt * p + q, num_trt * p + q))
    # Mn_summand is \frac{\partial D^{(t),T}}{\partial \theta^T} r^(t) + D^{(t),T} \frac{\partial r^(t)}{\partial \theta^T}
    # See note 2018.08.06 about small sample correction

    r_term_collected <- rep(NA, total_person_decisionpoint)
    D_term_collected <- matrix(NA, nrow = num_trt * p + q, ncol = total_person_decisionpoint)
    partialr_partialtheta_collected <- matrix(NA, nrow = total_person_decisionpoint, ncol = num_trt * p + q)

    for (it in 1:total_person_decisionpoint) {
        # this is to make R code consistent whether X_it, Z_it contains more entries or is just the intercept.
        if (p == 1) {
            Xbeta <- Xdm[it, ] * beta_hat # (1*1) * (1*num_trt)
        } else {
            Xbeta <- as.numeric(Xdm[it, ] %*% beta_hat) # (1*p) %*% (p*num_trt)
        }
        if (q == 1) {
            Zalpha <- Zdm[it, ] * alpha_hat # (1*1)
        } else {
            Zalpha <- as.numeric(Zdm[it, ] %*% alpha_hat) # (1*q) %*% (q*1)
        }

        pre_multiplier <- exp(-sum(ind(A[it], num_trt) * Xbeta)) * WCLS_weight_withDelta[it]

        # partialD_partialtheta = \frac{\partial D^{(t),T}}{\partial \theta^T}, matrix of dim (num_trt*p+q)*(num_trt*p+q)
        partialD_partialtheta <- matrix(NA, nrow = num_trt * p + q, ncol = num_trt * p + q)

        # partial derivatives of alpha corresponds "g(H_t)" should be 0
        partialD_partialtheta[1:q, 1:q] <- 0

        # partial derivatives of beta_1,..., beta_k corresponds to g(H_t) in column
        partialD_partialtheta[1:q, (q + 1):(q + num_trt * p)] <- -pre_multiplier *
            Zdm[it, ] %o% (rep(ind(A[it], num_trt), each = p) * rep(Xdm[it, ], num_trt))
        # Implementation logic: for every beta_j, the derivative is 1(A_t = j) * S_t^T.
        # Since the length of S_t is p, the indicator vector of A needs to be replicated by p
        # and S_t repeats for a number of k times, where k is the number of treatments.
        # rep(ind(A[it],num_trt),each = p), a vector with length of 1 * (num_trt * p)
        # rep(Xdm[it, ],num_trt), a vector with length of 1 * (p * num_trt)
        # Zdm[it, ] # a vector with length of 1 * q

        # partial derivatives of alpha corresponds "C_{A_t = 1,...}*(S_t)" in column should be 0
        partialD_partialtheta[(q + 1):(q + num_trt * p), 1:q] <- 0

        partialD_partialtheta[(q + 1):(q + num_trt * p), (q + 1):(q + num_trt * p)] <- (-pre_multiplier) *
            (rep(cA_tilde[it, ], each = p) * rep(Xdm[it, ], num_trt)) %o%
                (rep(ind(A[it], num_trt), each = p) * rep(Xdm[it, ], num_trt))
        # the same logic as above, we can see the term after %o% remains the same
        # rep(cA_tilde[it,],each = p), a vector with length of 1 * (num_trt * p)
        # rep(Xdm[it, ],num_trt), a vector with length of 1 * (p * num_trt)
        # Zdm[it, ] # a vector with length of 1 * q

        # r_term = r^(t) (scalar)
        r_term <- (Y[it] - exp(Zalpha + sum(ind(A[it], num_trt) * Xbeta))) * avail[it] # Xbeta: 1*num_trt
        r_term_collected[it] <- r_term

        # D_term = D^{(t),T} (dim = 1*(num_trt*p+q)) row vector
        D_term <- pre_multiplier * c(Zdm[it, ], cA_tilde[it, ] %x% Xdm[it, ])
        D_term_collected[, it] <- D_term

        # partialr_partialtheta = \frac{\partial r^(t)}{\partial \theta^T}
        partialr_partialtheta <- -exp(Zalpha + sum(ind(A[it], num_trt) * Xbeta)) *
            c(Zdm[it, ], rep(ind(A[it], num_trt), each = p) * rep(Xdm[it, ], num_trt)) * avail[it]
        partialr_partialtheta_collected[it, ] <- partialr_partialtheta

        Mn_summand[it, , ] <- partialD_partialtheta * r_term + D_term %o% partialr_partialtheta
        # partialr_partialtheta -> row vector, D_term -> column vector
        # Mn_summand[it, , ] has dim of (num_trt*p+q) * (num_trt*p+q)
    }
    Mn <- apply(Mn_summand, c(2, 3), sum) / sample_size
    Mn_inv <- solve(Mn)

    ### 3.2 Compute \Sigma_n matrix (\Sigma_n is the empirical variance of the estimating function) ###

    Sigman_summand <- array(NA, dim = c(sample_size, num_trt * p + q, num_trt * p + q))
    # Sigman_summand is  \sum_{t=1}^T ( D^{(t),T} r^(t) )^{\otimes 2}
    # See note 2018.08.06 about small sample correction

    person_first_index <- c(find_change_location(dta[, id_varname]), total_person_decisionpoint + 1)

    for (i in 1:sample_size) {
        D_term_i <- D_term_collected[, person_first_index[i]:(person_first_index[i + 1] - 1)]
        # (num_trt*p+q) * (number of obs/ person)
        r_term_i <- matrix(r_term_collected[person_first_index[i]:(person_first_index[i + 1] - 1)], ncol = 1)
        # (number of obs/ person)*1

        Sigman_summand[i, , ] <- D_term_i %*% r_term_i %*% t(r_term_i) %*% t(D_term_i)
    }
    Sigman <- apply(Sigman_summand, c(2, 3), sum) / sample_size

    varcov <- Mn_inv %*% Sigman %*% t(Mn_inv) / sample_size
    alpha_se <- sqrt(diag(varcov)[1:q])
    beta_se <- sqrt(diag(varcov)[(q + 1):(q + num_trt * p)])


    ### 4. small sample correction ###

    Sigman_tilde <- 0
    for (i in 1:sample_size) {
        D_term_i <- D_term_collected[, person_first_index[i]:(person_first_index[i + 1] - 1)]
        r_term_i <- matrix(r_term_collected[person_first_index[i]:(person_first_index[i + 1] - 1)], ncol = 1)
        partialr_partialtheta_i <- partialr_partialtheta_collected[person_first_index[i]:(person_first_index[i + 1] - 1), ]
        H_ii <- partialr_partialtheta_i %*% Mn_inv %*% D_term_i / sample_size
        Ii_minus_Hii_inv <- solve(diag(nrow(H_ii)) - H_ii)

        Sigman_tilde <- Sigman_tilde + D_term_i %*% Ii_minus_Hii_inv %*% r_term_i %*% t(r_term_i) %*% t(Ii_minus_Hii_inv) %*% t(D_term_i)
    }
    Sigman_tilde <- Sigman_tilde / sample_size

    varcov_adjusted <- Mn_inv %*% Sigman_tilde %*% t(Mn_inv) / sample_size
    alpha_se_adjusted <- sqrt(diag(varcov_adjusted)[1:q])
    beta_se_adjusted <- sqrt(diag(varcov_adjusted)[(q + 1):(q + num_trt * p)])


    ### 6. return the result with variable names ###

    names(alpha_hat) <- names(alpha_se) <- names(alpha_se_adjusted) <- Znames

    # column 1 should be intercept and moderator for beta 1, column 2 should be intercept and moderator for beta 2, ...
    beta_hat <- as.vector(beta_hat) # transform from a matrix with dim p * num_trt to vector
    names(beta_hat) <- names(beta_se) <- names(beta_se_adjusted) <- rep(Xnames, num_trt)

    return(list(
        beta_hat = beta_hat, alpha_hat = alpha_hat,
        beta_se = beta_se, alpha_se = alpha_se,
        beta_se_adjusted = beta_se_adjusted, alpha_se_adjusted = alpha_se_adjusted,
        varcov = varcov,
        varcov_adjusted = varcov_adjusted,
        dims = list(p = num_trt * p, q = q),
        f.root = solution$f.root
    ))
}
