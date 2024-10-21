
sim_bias_multi <- function(reps, p, n, SNR, b, corr) {
  
  
  Sigma <- matrix(corr, p, p)
  diag(Sigma) <- 1
  beta <- rep(b, p)
  names(beta) <- paste0("x", 1:p)
  b0 <- 1
  sigma_error <-  sqrt(as.numeric(crossprod(beta, Sigma %*% beta) / SNR))
  
  rsq <- NULL
  coefs <- tvals <- matrix(NA, nrow = reps, ncol = p)
  cover <- matrix(0, nrow = reps, ncol = p)
  colnames(coefs) <- paste0("x", 1:p)
  colnames(cover) <- paste0("x", 1:p)
  colnames(tvals) <- paste0("x", 1:p)
  
  for (i in seq(reps)) {
    
    X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
    y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
    Xy <- as.data.frame( cbind(X, y))
    colnames(Xy) <- c(paste0("x", 1:p), "y")
    fit <- lm(y ~., data = Xy)
    sel <- step(fit, k = 2, trace = FALSE)
    s <- summary(sel)
    tval <- s$coefficients[,3][-1]
    tvals[i, names(tval)] <-  tval
    coefs[i, names(tval)] <- coef(sel)[-1]
    rsq[i] <- s$r.squared
    cis <- confint(sel)[-1,]
    if (length(cis) < 3) {
      cover[i,names(tval)] <- ifelse(cis[1] < beta[names(tval)] & 
                                       cis[2] > beta[names(tval)], 1, 0)
    } else {
      cover[i,names(tval)] <- ifelse(cis[names(tval),1] < beta[names(tval)] & 
                                       cis[names(tval),2] > beta[names(tval)], 
                                     1, 0)
    }
    
  }
  
  res <- list(coefs = coefs, tvals = tvals, cover = cover, 
              bias = coefs - beta, mse = (coefs - beta)^2, rsq = rsq, 
              corr = corr, p = p)
  
  res
  
}

sim_summary <- function(l) {
  
  df <- tibble::tibble(
    
    cor = l$corr,
    npred = l$p,
    predictor = colnames(l$cover),
    coverage = colMeans(l$cover),
    estimate = colMeans(l$coefs, na.rm = TRUE),
    bias = colMeans((l$coefs - 1), na.rm = TRUE),
    mse = colMeans((l$coefs - 1)^2, na.rm = TRUE),
    rsq = mean(l$rsq)
    
  )
  df
  
}



future::plan(future::multisession)
sims <- furrr::future_map(seq(2, 10), 
                          ~furrr::future_pmap(
                            list(
                              reps = 10000, p = .x, n = 200, SNR = 0.5, 1, 
                              corr = seq(0.1, 0.9, by = 0.1)),
                            sim_bias_multi))

flattened_sims <- purrr::flatten(sims)
simdf <- purrr::map_dfr(flattened_sims, sim_summary)

simdf <- purrr::map_dfr(sims, ~purrr::map_dfr(.x, sim_summary))

saveRDS(simdf, "out/simdf.rds")

simdf |> 
  dplyr::mutate(dplyr::across(c("coverage", "bias", "mse", "rsq"), scale)) |> 
  tidyr::pivot_longer(cols = c(coverage, bias, mse, rsq), names_to = "measure", values_to = 'value') |> 
  dplyr::mutate(measure = dplyr::recode(measure, "coverage" = "Coverage", "bias" = "Bias", "mse" = "MSE", "rsq" = "R^2")) |>
  ggplot() +
  aes(x = cor, y = npred, fill = value) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1, option = "inferno", alpha = .9) +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10)) +
  labs(x = latex2exp::TeX("$\\rho$"), y = "Number of predictors", fill = "SD") +
  theme_minimal(12) +
  facet_wrap(~measure, scales = "free", 
             labeller = label_parsed)


