
# Kernels to create synthetic data
synthetic_kernels <- function(max_syn_lag) {
  # confined kernel ---------------------------------------------------------
  kern_conf <- 0.38

  lags <- data.table(time = as.numeric(0:max_syn_lag))

  # vadose kernels ----------------------------------------------------------
  kern_vad_a <- recipe(time~., dat = unclass(lags)) |>
    step_vadose_weeks(time = time,
                      air_diffusivity = 0.1,
                      thickness = 40,
                      precision = 1e-16,
                      inverse = TRUE) |>
    step_drop_columns(time) |>
    plate() |>
    unlist(use.names = FALSE)


  # remove small values
  kern_vad_a[kern_vad_a < 8e-16] <- 0

  # slug kernel -------------------------------------------------------------
  kern_slug = recipe(time~., data = lags) |>
    step_slug_cbp(
      times = time,
      radius = 0.1,
      radius_casing = 0.1,
      radius_well = 0.1,
      specific_storage = 1e-5,
      hydraulic_conductivity = 5e-4,
      thickness = 1.0,
      head_0 = 1.0,
      n_terms = 8L
    ) |>
    step_drop_columns(time) |>
    plate("dt") |>
    unlist(use.names = FALSE)

  min_vadose <- min(kern_vad_a)
  max_slug <- max(kern_slug)

  # combined kernel ---------------------------------------------------------
  # time 0 cumulative kernels should be 0
  kern_comb <- c(min_vadose, diff(kern_vad_a)) + c(max_slug, diff(kern_slug))
  kern_slug <- (c(max_slug, diff(kern_slug)))
  kern_vad_a <- (c(min_vadose, diff(kern_vad_a)))

  list(kern_conf  = kern_conf,
       kern_slug  = kern_slug,
       kern_vad_a = kern_vad_a,
       kern_comb  = kern_comb)

}


# Generate synthetic water levels from barometric pressure and input kernels
synthetic_wl <- function(baro,
                         datetime,
                         sd_noise = 0,
                         linear_trend = 0,
                         intercept = 0,
                         seed = NULL,
                         kernel = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # generate synthetic barometric pressure from fft coefs
  noise_wl <- rnorm(length(baro), sd = sd_noise)


  wl <- hydrorecipes:::convolve_filter(x = baro,
                                       y = kernel,
                                       remove_partial = TRUE,
                                       reverse = TRUE)

  lin_trend <- linear_trend * as.numeric(datetime)

  wl <- wl + lin_trend + noise_wl + intercept

  return(wl)

}


# Generate synthetic water levels from barometric pressure and input kernels
synthetic_data_wide <- function(wl, kernels, start, end) {
  # Subset data -------------------------------------------------------------
  ba <- wl[datetime %between% c(start, end), list(datetime, baro = rd_130_baro)]

  # Remove mean -------------------------------------------------------------
  ba[, baro := baro - mean(baro)]

  # prep kernels
  kern_conf  <- kernels$kern_conf
  kern_vad_a <- kernels$kern_vad_a
  kern_slug  <- kernels$kern_slug
  kern_comb  <- kernels$kern_comb

  # Generate synthetic water levels -----------------------------------------
  ba[, wl_conf := synthetic_wl(baro, datetime, kernel = kern_conf)]
  ba[, wl_vad  := synthetic_wl(baro, datetime, kernel = kern_vad_a)]
  ba[, wl_slug := synthetic_wl(baro, datetime, kernel = kern_slug)]
  ba[, wl_comb := synthetic_wl(baro, datetime, kernel = kern_comb)]

  ba
}

# Wide to long format
synthetic_data_long <- function(ba) {

  tf_dt <- copy(ba)
  var_fact <- c("Confined", "Vadose", "Storage", "Combined")
  setnames(tf_dt, c("datetime", "baro", var_fact))
  tf_dt <- na.omit(melt(tf_dt, id.vars = c("datetime", "baro")))

}

# Calculate transfer function with pgram method
freq_response <- function(mat){
  p <- hydrorecipes:::transfer_pgram(mat, 3, FALSE, FALSE, 0.1)
  n <- length(p)
  p <- data.table(frequency = 1.0 / n * 86400 * 0:(n - 1), tf = as.vector(p))
  return(p)
}

# Apply transfer function calculation to each type
freq_response_by_group <- function(x){
  tf_syn <- x[, freq_response(cbind(value, baro)), by = .(variable)]

  return(tf_syn)
}

