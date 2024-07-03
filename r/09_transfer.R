# tapers for psd... more tapers are used for higher frequencies
# psd can optimize these but this is faster and suitable for this purpose
quad_taper <- function(n, len = 1000L, scale = 1000L, initial = 5, max_taper = NULL) {
  nhalf <- (n %/% 2)
  loc   <- seq(0, 1, length.out = len)
  tap   <- rep(scale, nhalf)
  tap[1:len] <- as.integer((1 - (1 - loc)^2) * (scale - initial) + initial)
  if (!is.null(max_taper)) {
     tap[max_taper:nhalf] <- 3
  }
  as.tapers(tap)
}


# transfer function with psd::pspectrum
transfer_function <- function(wl, et, nm) {

  wl <- copy(wl)[et, on = 'datetime']
  setnames(wl, "rd_130_baro", "baro")

  tapers <- quad_taper(nrow(wl), len = 100000, scale = 25000, initial = 4)

  pspectrum(collapse::qM(wl[, c("baro", "volume_strain", nm), with = FALSE]),
            verbose = TRUE,
            niter = 0L,
            ntap = tapers,
            plot = FALSE)

}

# transfer function with psd::pspectrum for different monitoring frequencies
transfer_function_subs <- function(wl, et, nm) {

  wl <- wl[et, on = 'datetime']
  setnames(wl, "rd_130_baro", "baro")


  ddt <- c(7200, 3600, 1800, 900, 300, 120, 60)

  tf <- list()
  for (i in seq_along(ddt)) {
    print(ddt[i])

    delta_t <- ddt[i]
    pred_spec_sub <- wl[as.numeric(datetime) %% delta_t == 0]
    n <- nrow(pred_spec_sub)
    tapers <- quad_taper(n, len = n %/% 2, scale = n %/% 8, initial = 6)
    tf[[i]] <-  pspectrum(collapse::qM(pred_spec_sub[, c("baro", "volume_strain", nm), with = FALSE]),
                     verbose = TRUE,
                     niter = 0,
                     ntap = tapers,
                     plot = FALSE)


  }
  names(tf) <- paste0("n_", ddt)
  tf

}

# Calculate the residuals and then estimate the periodgram for the residuals
residual_pgram <- function(wl, et, reg_pred) {
  # 2016-08-19 12:00:04
  # 2016-10-13 12:00:00
  wl <- wl[et, on = 'datetime']
  data.table::setnames(wl, "rd_130_baro", "baro")

  form <- as.formula("wl~.")

  pred <- wl[, list(rd_130,
                    baro,
                    volume_strain,
                    res = rd_130 -
                      reg_pred$rd_130_step_spline_b_datetime -
                      reg_pred$rd_130_step_distributed_lag_baro -
                      reg_pred$rd_130_step_add_vars -
                      reg_pred$rd_130_step_intercept,
                    spline_component = reg_pred$rd_130_step_spline_b_datetime,
                    baro_component = reg_pred$rd_130_step_distributed_lag_baro)]
  pred <- na.omit(pred)


  # generate recipe
  rec <- recipe(form, pred) |>
    step_fft_pgram(c(rd_130,
                     baro,
                     volume_strain,
                     res,
                     spline_component,
                     baro_component),
                   spans = 1, taper = 1e-15,
                   pad_fft = FALSE) |>
    prep() |>
    bake()


  pgram <- collapse::qDT(rec$get_step_data("fft_result")[[1]])
  n_padded <- nrow(pgram)
  df       <- 1 / n_padded

  frequency <- seq.int(from = 0, by = df,
                       length.out = n_padded) * 86400

  # M2 1.9322736
  head(frequency[frequency > 1.92])
  head(frequency[frequency > 1.99])


  # scale for amplitude
  pgram <- data.table(frequency = frequency,
                      wl  = sqrt(2 * Re(pgram[[1]]) / n_padded),
                      ba  = sqrt(2 * Re(pgram[[7]]) / n_padded),
                      et  = sqrt(2 * Re(pgram[[12]]) / n_padded),
                      res = sqrt(2 * Re(pgram[[16]]) / n_padded),
                      sp  = sqrt(2 * Re(pgram[[19]]) / n_padded),
                      ba_comp = sqrt(2 * Re(pgram[[21]]) / n_padded)
                      )
  pgram <- pgram[-1]


}
