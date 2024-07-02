# This script takes ~5 minutes to run on my laptop

# calculate_results -------------------------------------------------------
# Barometric response functions
# Frequency response functions
# Static responses
calculate_results <- function(x,
                              wl_name,
                              max_syn_lag = 86400) {

  # prep dataset
  dat <- copy(x)[, c("datetime", "baro", wl_name), with = FALSE]
  setnames(dat, wl_name, "wl_reg")

  # difference spacing for clark method
  n_vals <- c(3600, 86400)


  # static efficiency
  # acworth
  be_ac <- hydrorecipes:::be_transfer(collapse::qM(dat[, list(wl_reg, baro)]),
                                  spans = 5, TRUE, TRUE, 0.1,
                                  2.0, 86400)[1]

  # least squares
  be_ls <- hydrorecipes:::be_least_squares_cpp(dep = dat[["wl_reg"]],
                                           ind = dat[["baro"]],
                                           inverse = FALSE)

  # clark's
  be_cl <- sapply(n_vals, function(y) {
    hydrorecipes:::be_clark_cpp(dep = dat[["wl_reg"]],
                            ind = dat[["baro"]],
                            lag_space = y,
                            inverse = FALSE)
  })


  # distributed lag
  rec <- recipe(wl_reg~., dat) |>
    step_distributed_lag(baro, knots = log_lags_arma(20, max_syn_lag)) |>
    step_drop_columns(baro) |>
    step_intercept() |>
    step_drop_columns(datetime) |>
    step_ols(formula) |>
    prep() |>
    bake()

  brf_dist <- rec$get_response_data('dt')[variable == "cumulative"]


  # frequency response function
  frf <- hydrorecipes:::transfer_pgram(collapse::qM(dat[, list(wl_reg, baro)]),
                                   spans = 5,
                                   detrend = TRUE,
                                   demean = TRUE,
                                   taper = 0.1)



  brf_frf <- data.table(value = hydrorecipes:::brf_from_frf(frf[, 1]))

  # x is the time lag
  brf_frf[, x := 0:(nrow(brf_frf) - 1)]


  return(data.table(value = c(be_ac, be_ls, be_cl,
                              mean(Mod(brf_dist$value)),
                              mean(brf_frf$value)),
                    method = c("Acworth", "Linear Regression", "Clark (1 hour)",
                               "Clark (1 day)", "Distributed lag", "Transfer Function")
  ))


}

# remove mean -------------------------------------------------------------
# select values so that FFT frequencies are nice
be <- function(ba, kernels) {

  # prep kernels
  kern_conf  <- kernels$kern_conf
  kern_vad_a <- kernels$kern_vad_a
  kern_slug  <- kernels$kern_slug
  kern_comb  <- kernels$kern_comb

  ba_model <- na.omit(ba)[1:(86400*31 + 3)]

  cols <- c('wl_conf', 'wl_vad', 'wl_slug', 'wl_comb', 'baro')

  # remove the mean value for plotting
  for (j in cols) set(ba_model, j = j, value = ba_model[[j]] - mean(ba_model[[j]]))

  # Aquifer type compare ----------------------------------------------------
  conf_resp <- calculate_results(ba_model, 'wl_conf')
  slug_resp <- calculate_results(ba_model, 'wl_slug')
  vad_resp  <- calculate_results(ba_model, 'wl_vad')
  comb_resp <- calculate_results(ba_model, 'wl_comb')

  # adjust the kernel value at x = 0 for log plotting
  conf_resp <- list(resp = conf_resp,
                    kern = data.table(value = rep(kern_conf, max_syn_lag + 1),
                                      x = c(0.1, 1:(max_syn_lag))))
  slug_resp <- list(resp = slug_resp,
                    kern = data.table(value = kern_slug,
                                      x = c(0.1, 1:(max_syn_lag))))
  vad_resp  <- list(resp = vad_resp,
                    kern = data.table(value = kern_vad_a,
                                      x = c(0.1, 1:(max_syn_lag))))
  comb_resp <- list(resp = comb_resp,
                    kern = data.table(value = (kern_vad_a + kern_slug),
                                      x = c(0.1, 1:(max_syn_lag))))
  syn <- list(conf = conf_resp,
              slug = slug_resp,
              vad  = vad_resp,
              comb = comb_resp)

}


