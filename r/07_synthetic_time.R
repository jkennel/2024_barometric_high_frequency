# Regular spacing (requires subsetting) -----------------------------------
reg_by_n <- function(x, n) {

  dat_sub <- x[seq(1, nrow(x), n)]

  ba_lags <- seq(0, (86400) / n, 1)
  formula <- as.formula(wl~.)

  rec <- recipe(formula, dat_sub) |>
    step_lead_lag(baro, lag = ba_lags) |>
    step_intercept() |>
    step_drop_columns(baro) |>
    prep() |>
    bake()

  list(
    fit = lm(wl~. -1, rec$result, x = FALSE, y = FALSE, tol = 1e-50),
    rec = rec
  )

}


# Irregular spacing (no subset) -------------------------------------------
irr_by_n <- function(x, n) {

  ba_lags <- log_lags_arma(n, 86400)
  formula <- as.formula(wl~.)

  rec <- recipe(formula, x) |>
    step_lead_lag(baro, lag = ba_lags) |>
    step_intercept() |>
    step_drop_columns(baro) |>
    prep() |>
    bake()

  list(
    fit = lm(wl~. -1, rec$result, x = FALSE, y = FALSE, tol = 1e-50),
    rec = rec
  )

}


# Distributed lag ---------------------------------------------------------
dist_by_n <- function(x, n) {

  ba_lags <- c(0:4, 5 + log_lags_arma(n - 5, 86400 - 5))
  formula <- as.formula(wl~.)

  rec <- recipe(formula, x) |>
    step_distributed_lag(baro, knots = ba_lags, intercept = TRUE) |>
    step_intercept() |>
    step_drop_columns(baro) |>
    prep() |>
    bake()

  list(
    fit = lm(wl~. -1, rec$result, x = FALSE, y = FALSE, tol = 1e-50),
    rec = rec
  )

}




# Calculate method responses, fit characteristics and timings,
#   and generate figures for table
#
# subset to 2.5 days
prep_result_data <- function(results, method = "_reg_", add) {

  for (i in 1:nrow(results)) {

    n <- results$n[i]
    nm_box <- paste0("box", method, n, '.pdf')
    nm_resp <- paste0("brf", method, n, '.pdf')

    fit <- results[i,]$result[[1]][['fit']]
    rec <- results[i,]$result[[1]][['rec']]

    res <- (as.numeric(fit$residuals))
    res <- boxplot(log10(na.omit(res + 1e-16)), plot = FALSE) # avoid values of zero

    plot_synthetic_box(res, nm_box)
    form <- formula(wl~.)

    rec <- rec |>
      hydrorecipes::step_ols(form) |>
      hydrorecipes::prep() |>
      hydrorecipes::bake()
    resp <- rec$get_response_data("dt")[variable == "cumulative"]

    if (method == "_reg_") {
      plot_synthetic_brf(resp, cumsum(add), nm_resp, n)
    } else {
      plot_synthetic_brf(resp, cumsum(add), nm_resp, 1.0)
    }

  }

  results$r2 <- sapply(results$result, function(x) {
    summary(x[['fit']])$r.squared
  })
  results$se <- sapply(results$result, function(x) {
    summary(x[['fit']])$sigma
  })
  results$df <- sapply(results$result, function(x) {
    summary(x[['fit']])$df[2]
  })
  results$n_terms <- sapply(results$result, function(x) {
    NROW(coefficients(x[['fit']])) - 1
  })
  results$box <- lapply(results$result, function(x) {
    boxplot(log10(na.omit(abs(x[['fit']]$residuals) + 1e-16)),
            plot = FALSE)
  })


  setDT(results)

  if (method == "_reg_") {
    results[, delta_t := paste0(n, 's')]
  } else {
    results[, delta_t := paste0(1, 's')]
  }

  results$result <- NULL

  results[, image_box := paste0("img/dist_lag/", paste0("box", method, n, '.pdf'))]
  results[, image_brf := paste0("img/dist_lag/", paste0("brf", method, n, '.pdf'))]

}


# Compare regular, irregular and distributed lag models
run_method_comparison <- function(ba, kernel) {

  # subset dataset to be reasonable size for standard method
  ba_rf <- na.omit(ba)[1:(86400 * 2.5 + 4)][, list(wl = wl_comb, baro)]
  cols <- c('wl', 'baro')

  for (j in cols) set(ba_rf, j = j, value = ba_rf[[j]] - mean(ba_rf[[j]]))


  results_reg <- bench::press(
    n = c(3600, 600, 15),
    {
      bench::mark(reg_by_n(ba_rf, n))
    }
  )

  results_irr <- bench::press(
    n = c(20, 100, 400),
    {
      bench::mark(irr_by_n(ba_rf, n))
    }
  )

  results_dist <- bench::press(
    n = c(10, 15, 20) + 5,
    {
      bench::mark(dist_by_n(ba_rf, n))
    }
  )


  results <- list(
    results_reg  = prep_result_data(results_reg, "_reg_", kernel),
    results_irr  = prep_result_data(results_irr, "_irr_", kernel),
    results_dist = prep_result_data(results_dist, "_dist_", kernel)
  )

  results
}

