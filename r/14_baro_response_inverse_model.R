
# Fit model to response function
fitting_weeks_cbp <- function(par, crf) {

  thickness <- par[1]
  air_diffusivity <- 10^par[2]
  hydraulic_conductivity <- 10^par[3]
  scale_1 <- par[4]
  scale_2 <- par[5]
  formula <- as.formula(value~x)

  r <- 4.5 * 2.54 / 2 / 100
  rec <- recipe(formula = formula, data = crf) |>
    step_vadose_weeks(time = x,
                      air_diffusivity = air_diffusivity,
                      thickness = thickness,
                      precision = 1e-16,
                      inverse = TRUE) |>
    step_slug_cbp(time = x,
                  radius_casing = r,
                  radius = r,
                  radius_well = r,
                  specific_storage = 1e-5,
                  hydraulic_conductivity = hydraulic_conductivity,
                  n_terms = 8) |>
    step_drop_columns(x) |>
    prep() |>
    bake()

  rec <- collapse::qDT(rec$result)

  comb <- (rec[["slug_cbp"]] + rec[["vadose_weeks"]]) * scale_1 + scale_2

  res <- sum( (rec[["value"]] - comb)^2 )

  return(res / nrow(crf))
}

fit_weeks_cbp <- function(resp) {

  ind <- unique(ceiling(10^seq(log10(31), log10(100000 - 30), length.out = 80)))
  resp <- resp[ind]

  set.seed(5)
  par <- c(42, -1, -3, 0.7, 0.2)

  par <- nlm(fitting_weeks_cbp, par,
             crf = resp,
             steptol = 1e-8,
             ndigit = 10)$estimate


  thickness <- par[1]
  air_diffusivity <- 10^par[2]
  hydraulic_conductivity <- 10^par[3]
  scale_1 <- par[4]
  scale_2 <- par[5]
  formula <- as.formula(value~x)

  r <- 4.5 * 2.54 / 2 / 100
  rec <- recipe(formula = formula, data = resp) |>
    step_vadose_weeks(time = x,
                      air_diffusivity = air_diffusivity,
                      thickness = thickness,
                      precision = 1e-16,
                      inverse = TRUE) |>
    step_slug_cbp(time = x,
                  radius_casing = r,
                  radius = r,
                  radius_well = r,
                  specific_storage = 1e-5,
                  hydraulic_conductivity = hydraulic_conductivity,
                  n_terms = 8) |>
    step_drop_columns(x) |>
    prep() |>
    bake()

  rec <- collapse::qDT(rec$result)

  pred <- (rec[["slug_cbp"]] + rec[["vadose_weeks"]]) * scale_1 + scale_2

  results <- rbind(resp[, list(x, value, type = "Empirical")],
                   resp[, list(x, value = pred, type = "Modeled")])

  return(list(parameters = par,
              results = results))

}
