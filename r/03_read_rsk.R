
rsk_baro <- function(files, dbar_to_m = 1.0199773339984, m_to_cm = 100.0) {

  start <- as.POSIXct('2019-03-09 01:54:03', tz = 'UTC')
  end   <- as.POSIXct('2019-04-01 05:27:22', tz = 'UTC')

  dat <- list()

  for (i in seq_along(files)) {

    fn <- files[i]
    d <- Rsk$new(fn)$data
    d <- rsk::rename_data(d)
    d <- d[datetime %between% c(start, end)]
    d <- d[, list(datetime, pressure)]
    d[, pressure := pressure * dbar_to_m * m_to_cm]

    d <- d[as.numeric(datetime) %% 1 == 0]
    col_name <- gsub(".rsk", "", basename(fn))
    col_name <- gsub("_baro", "", col_name)

    d[, file_name := col_name]

    dat[[i]] <- d
  }

  dat <- rbindlist(dat)
  dcast(dat, datetime~file_name, value.var = "pressure")
}


rsk_wl <- function(files, dbar_to_m = 1.0199773339984, m_to_cm = 100.0) {

  start <- as.POSIXct('2016-08-18', tz = 'UTC')
  end   <- as.POSIXct('2016-10-13 12:00:00', tz = 'UTC')

  dat <- list()
  for (i in seq_along(files)) {

    fn <- files[i]
    d <- Rsk$new(fn)$data
    d <- rsk::rename_data(d)
    d <- d[as.numeric(datetime) %% 1 == 0]
    d <- d[, list(datetime, pressure)]
    d <- d[datetime %between% c(start, end)]
    d[, pressure := pressure * dbar_to_m * m_to_cm]

    col_name <- gsub("_wl.rsk", "", basename(fn))
    col_name <- gsub(".rsk", "", col_name)

    d[, file_name := col_name]

    dat[[i]] <- d

  }

  dat <- rbindlist(dat)
  dcast(dat, datetime~file_name, value.var = "pressure")

}



