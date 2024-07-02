# Estimate Earth tides
add_earthtide <- function(utc,
                          do_predict,
                          wave_groups,
                          latitude,
                          longitude,
                          elevation,
                          cutoff,
                          catalog,
                          method) {


  et <- calc_earthtide(utc,
                       do_predict = do_predict,
                       latitude = latitude,
                       longitude = longitude,
                       elevation = elevation,
                       cutoff = cutoff,
                       catalog = catalog,
                       wave_groups = wave_groups,
                       method = method,
                       n_thread = 12)

  return(et)

}
