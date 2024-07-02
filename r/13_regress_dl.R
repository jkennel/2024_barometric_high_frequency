# Regression with distributed lags
regress_dl_rec <- function(wl,
                           et,
                           start,
                           end,
                           ba_knots,
                           df) {

  # water levels and earthtides
  wl <- wl[between(datetime, start, end)]

  # generate formula
  nms <- colnames(wl)
  setnames(wl, "rd_130_baro", "baro")
  wl <- wl[et, on = "datetime"]
  nms <- nms[!nms %in% c("baro", "volume_strain", "datetime")]
  nms <- nms[!grepl("sin_", nms)]
  nms <- nms[!grepl("cos_", nms)]
  form <- as.formula(paste0(paste(nms, collapse = "+"), "~."))

  # generate recipe
  rec <- recipe(form, wl) |>
    step_distributed_lag(baro, knots = ba_knots) |>
    step_spline_b(datetime, df = df) |>
    step_intercept() |>
    step_drop_columns(datetime) |>
    step_drop_columns(baro) |>
    step_ols(formula = form) |>
    prep() |>
    bake()

  return(rec)

}

# Regression with distributed lags for monitoring frequency
regress_dl_resp_subs <- function(wl,
                           et,
                           start,
                           end,
                           ba_knots,
                           df) {

  # water levels and earthtides
  wl <- wl[between(datetime, start, end)]

  # generate formula
  nms <- colnames(wl)
  setnames(wl, "rd_130_baro", "baro")
  wl <- wl[et, on = "datetime"]
  nms <- nms[!nms %in% c("baro", "volume_strain", "datetime")]
  nms <- nms[!grepl("sin_", nms)]
  nms <- nms[!grepl("cos_", nms)]
  form <- as.formula(paste0(paste(nms, collapse = "+"), "~."))

  delta_t <- c(7200, 3600, 1800, 900, 300, 120, 60)

  setDT(wl)

  out <- list()
  for (i in seq_along(delta_t)) {

    wl_sub <- wl[as.numeric(datetime) %% delta_t[[i]] == 0]
    ba_lags <- 0:(86400 * 1.25 / delta_t[[i]])

    # generate recipe
    rec <- recipe(form, wl_sub) |>
      step_lead_lag(baro, lag = ba_lags) |>
      step_spline_b(datetime, df = df) |>
      step_intercept() |>
      step_drop_columns(datetime) |>
      step_drop_columns(baro) |>
      step_ols(formula = form) |>
      prep() |>
      bake()

    rec <- rec$get_response_data("dt")
    rec[, delta_t := delta_t[[i]]]

    out[[i]] <- copy(rec)

  }

  return(collapse::rowbind(out))

}


regress_dl_predict <- function(rec) {

  collapse::qDT(collapse::rowbind(rec$get_step_data("decomposition")))

}

regress_dl_response <- function(rec) {

  collapse::qDT(collapse::rowbind(rec$get_step_data("response_data")))

}

regress_dl_formula <- function(rec) {

  rec$get_step_data("formula")

}




