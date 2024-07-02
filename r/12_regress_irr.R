
# Regression with irregularly spaced lags
regress_irr_rec <- function(wl,
                            et,
                            start,
                            end,
                            ba_knots,
                            df) {

  # water levels and earthtides
  wl <- wl[between(datetime, start, end)]

  # generate formula
  # nms are the names of the outcome variables
  nms <- colnames(wl)
  setnames(wl, "rd_130_baro", "baro")
  wl <- wl[et, on = "datetime"]
  nms <- nms[!nms %in% c("baro", "volume_strain", "datetime")]
  nms <- nms[!grepl("sin_", nms)]
  nms <- nms[!grepl("cos_", nms)]
  form <- as.formula(paste0(paste(nms, collapse = "+"), "~."))

  # generate recipe
  rec <- recipe(form, wl) |>
    step_lead_lag(baro, lag = ba_knots) |>
    step_spline_b(datetime, df = df) |>
    step_intercept() |>
    step_drop_columns(datetime) |>
    step_drop_columns(baro) |>
    step_ols(formula = form) |>
    prep() |>
    bake()

  return(rec)

}

