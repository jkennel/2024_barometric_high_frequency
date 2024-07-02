# Compare spatial variability of barometric pressure (time)
baro_compare_time <- function(ba, ba_knots_baro) {

  form <- as.formula(.~rd_130_shallow)
  form_ols <- as.formula(rd_08 + rd_10 + rd_130_deep~.)

  # generate recipe
  rec <- recipe(form, ba) |>
    step_distributed_lag(rd_130_shallow,
                         knots = ba_knots_baro, intercept = TRUE) |>
    step_intercept() |>
    step_drop_columns(datetime) |>
    step_drop_columns(rd_130_shallow) |>
    step_ols(formula = form_ols) |>
    prep() |>
    bake()

  rec$get_response_data(type = "dt")


}


# transfer ----------------------------------------------------------------

quad_taper <- function(n, len = 1000L, scale = 1000L, initial = 5, max_taper = NULL) {
  nhalf <- (n %/% 2)
  loc   <- seq(0, 1, length.out = len)
  tap   <- rep(scale, nhalf)
  tap[1:len] <- as.integer( (1 - (1 - loc)^2) * (scale - initial) + initial)

  if (!is.null(max_taper)) {
    tap[max_taper:nhalf] <- 3
  }

  as.tapers(tap)
}


# Compare spatial variability of barometric pressure (frequency)
baro_compare_frequency <- function(ba) {

  tapers <- quad_taper(nrow(ba), len = 10000, scale = 5000, initial = 3)
  niter <- 0
  # transfer function approach

  tf <-  pspectrum(as.matrix(ba[, list(rd_130_deep, rd_130_shallow)]),
                   verbose = TRUE,
                   niter = niter,
                   ntap = tapers,
                   plot = FALSE)
  coh_130 <- data.table(freq = tf$freq * 86400, coh = tf$coh[,1])
  coh_130[, variable := 'RD-130 deep']

  tf <-  pspectrum(as.matrix(ba[, list(rd_10, rd_130_shallow)]),
                   verbose = TRUE,
                   niter = niter,
                   ntap = tapers,
                   plot = FALSE)
  coh_10 <- data.table(freq = tf$freq * 86400, coh = tf$coh[,1])
  coh_10[, variable := 'RD-10']

  tf <-  pspectrum(as.matrix(ba[, list(rd_08, rd_130_shallow)]),
                   verbose = TRUE,
                   niter = niter,
                   ntap = tapers,
                   plot = FALSE)
  coh_08 <- data.table(freq = tf$freq * 86400, coh = tf$coh[,1])
  coh_08[, variable := 'RD-08']



  co <- rbindlist(list(coh_130, coh_10, coh_08))


}


