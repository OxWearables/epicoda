nobs <- 100000
M <- matrix(c(1, 0.6, 0.15, -0.1,
            0.6, 1, 0.35, -0.2,
            0.15, .35, 1, -0.3,
            -0.1, -0.2, -0.3, 1), nrow = 4, ncol = 4)
L <- chol(M)
nvars <- dim(L)[1]
r <- t(L)


# Random variables that follow an M correlation matrix
r <- t(L) %*% matrix(rnorm(nvars*nobs), nrow=nvars, ncol=nobs)
r <- t(r)

rdata <- as.data.frame(r)
names(rdata) <- c('ilr_1_compA_vs_remaining', 'ilr_2_compB_vs_remaining', 'ilr_3_compC_vs_remaining', 'ilr_4_compD_vs_remaining')

simdata <- rdata
simdata$ilr_1_compA_vs_remaining <- -5.5 + 1.8*rdata$ilr_1_compA_vs_remaining
simdata$ilr_2_compB_vs_remaining <- -1.3 + 0.5*rdata$ilr_2_compB_vs_remaining
simdata$ilr_3_compC_vs_remaining <- -0.8 + 0.4*rdata$ilr_3_compC_vs_remaining
simdata$ilr_4_compD_vs_remaining <- 0.1 + 0.2*rdata$ilr_4_compD_vs_remaining

sim_ilr_data <- simdata
simdata <- ilr_trans_inv(simdata)
