# Estimate the static barometric efficiency using different methods
static_barometric_efficiency <- function(wl, et) {

  wl <- copy(wl)[et, on = "datetime", nomatch = 0]

  clark_hr <- hydrorecipes:::be_clark_cpp(dep = wl$rd_130,
                                          ind = wl$rd_130_baro,
                                          lag_space = 3600L,
                                          inverse = FALSE)

  clark_day <- hydrorecipes:::be_clark_cpp(dep = wl$rd_130,
                                           ind = wl$rd_130_baro,
                                           lag_space = 86400L,
                                           inverse = FALSE)
  formula <- as.formula(rd_130 ~.)
  frec = recipe(formula = formula,
                data = unclass(wl)) |>
    step_baro_harmonic(datetime,
                       rd_130,
                       rd_130_baro,
                       volume_strain,
                       inverse = FALSE) |>
    prep() |>
    bake()

  acworth <- frec$get_step_data("barometric_efficiency")[[1]]$rau

  wl <- hydrorecipes:::detrend_and_demean_list(wl, TRUE, TRUE)
  least_squares <- hydrorecipes:::be_least_squares_cpp(dep = wl$rd_130,
                                                       ind = wl$rd_130_baro,
                                                       inverse = FALSE)

  list(clark_hr = clark_hr,
       clark_day = clark_day,
       acworth = acworth,
       least_squares = least_squares)

}
