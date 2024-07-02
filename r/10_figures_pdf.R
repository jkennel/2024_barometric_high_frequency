# Figure 2 -----------------------------------------------------
plot_figure_02 <- function(results_syn,
                           tf_syn,
                           tf_dt,
                           kernels) {

  txt_sz <- 1.7
  line_col <- "#606060"

  be_f <- c()
  for (i in seq_along(results_syn)) {
    be_f[i] <- Mod(results_syn[[i]]$resp[method == "Acworth"]$value)
  }
  add_f <- data.table(frequency = 2,
                      gain = be_f,
                      variable = factor(c("Confined", "Well Storage", "Vadose", "Combined"),
                                        levels = c("Confined", "Well Storage", "Vadose", "Combined")))


  be_t <- list()
  for (i in seq_along(results_syn)) {
    be_t[[i]] <- Mod(results_syn[[i]]$resp[method != "Acworth"]$value[1:3])
  }
  add_t <- data.table(value = unlist(be_t),
                      yadj = c(0.20, 0.10, 0.0, 0.20, 0.10, 0.0, -0.06, 0.0, 0.06, -0.06, 0.0, 0.06),
                      name = rep(c("Least Squares", "Clark (1 hour)", "Clark (1 day)"), 4),
                      variable = factor(rep(c("Confined", "Well Storage", "Vadose", "Combined"), each = 3),
                                        levels = c("Confined", "Well Storage", "Vadose", "Combined")))




  # transfer function
  tf_syn[, gain := Mod(tf)]
  tf_syn[, variable := factor(variable,
                              levels = c("Confined", "Storage", "Vadose", "Combined"),
                              labels = c("Confined", "Well Storage", "Vadose", "Combined"))]

  p0 <- ggplot(tf_syn[frequency < 10000], aes(x = frequency, y = gain))
  p0 <- p0 + annotation_logticks(sides = "tb", colour = "grey",
                                 short = unit(0.05, "cm"),
                                 mid = unit(0.1, "cm"),
                                 long = unit(0.15,"cm"))
  p0 <- p0 + geom_line(color = line_col)
  p0 <- p0 + geom_point(data = add_f, mapping = aes(x = frequency, y = gain),
                        size = 1, fill = viridis(3, alpha = 0.5)[1], color = viridis(3)[1])
  p0 <- p0 + geom_text(data = add_f, mapping = aes(x = frequency, y = gain,
                                                   label = "Acworth (2 cpd)"),
                       vjust = -0.8, hjust = -0.0, size = txt_sz, color = viridis(3)[1], fontface = "bold")
  p0 <- p0 + scale_x_log10(limits = c(0.1, 10000), expand = c(0,0), labels = comma)
  p0 <- p0 + scale_y_continuous(breaks = seq(0, 1, 0.5))
  p0 <- p0 + facet_wrap(variable~., ncol = 1)
  p0 <- p0 + xlab(frequency_xlab)
  p0 <- p0 + ylab(frequency_ylab)
  p0 <- p0 + ggtitle("Frequency Domain Response")
  p0 <- p0 + theme_joh



  kerns <- data.table(
    time_lag = 0:86400,
    conf = rep(kernels$kern_conf, 86401),
    slug = cumsum(kernels$kern_slug),
    vad  = cumsum(kernels$kern_vad_a),
    comb = cumsum(kernels$kern_comb)
  )
  kerns <- melt(kerns, id.vars = "time_lag")
  kerns[, variable := factor(variable,
                             levels = c("conf", "slug", "vad", "comb"),
                             labels = c("Confined", "Well Storage", "Vadose", "Combined"))]


  # time response function

  p1 <- ggplot(kerns, aes(x = time_lag, y = value))
  p1 <- p1 + annotation_logticks(sides = "tb", colour = "grey",
                                 short = unit(0.05, "cm"),
                                 mid = unit(0.1, "cm"),
                                 long = unit(0.15,"cm"))
  p1 <- p1 + geom_line(color = line_col)
  p1 <- p1 + geom_segment(data = add_t,
                          aes(x = 85000, xend = 86400,
                              y = value + yadj, yend = value),
                          color = viridis(3)[1],
                          linewidth = 0.2)
  p1 <- p1 + geom_text(data = add_t,
                       aes(x = 80000, y = value + yadj, label = name),
                       vjust = 0.5, hjust = 1,
                       size =  txt_sz,
                       color = viridis(3)[1],
                       fontface = "bold")
  p1 <- p1 + scale_x_log10(limits = c(1, 100000), expand = c(0,0), labels = comma)
  p1 <- p1 + scale_y_continuous(breaks = seq(0, 1, 0.5))
  p1 <- p1 + facet_wrap(variable~., ncol = 1)
  p1 <- p1 + xlab("Time Lag (seconds)")
  p1 <- p1 + ylab("Cumulative Loading Response")
  p1 <- p1 + ggtitle("Time Domain Response")
  p1 <- p1 + theme_joh


  tf_dt[, variable := factor(variable,
                             levels = c("Confined", "Storage", "Vadose", "Combined"),
                             labels = c("Confined", "Well Storage", "Vadose", "Combined"))]


  p2 <- ggplot(na.omit(tf_dt[as.numeric(datetime) %% 300 == 0]), aes(x = datetime, y = value))
  p2 <- p2 + geom_line(color = line_col)
  p2 <- p2 + facet_wrap(variable~., ncol = 1)
  p2 <- p2 + scale_x_datetime(expand = c(0,0), date_labels = "%b-%d")
  p2 <- p2 + scale_y_continuous(limits = c(-6, 6),expand = c(0,0), breaks = seq(-5, 5, 5))
  p2 <- p2 + xlab("")
  p2 <- p2 + ylab("Absolute Pressure (cm water)")
  p2 <- p2 + ggtitle("Synthetic Water Pressure")
  p2 <- p2 + theme_joh

  pdf("img/figure_02.pdf", height = default_height_half, width = default_width * 1.5)
  print(p2 + p1 + p0)
  dev.off()

  "img/figure_02.pdf"
}


# Figure 3 ----------------------------------------------------------------

plot_figure_03 <- function(crds_sub) {

  CairoPNG("img/figure_03.png", width = 5156 / 2, height = 3544 / 2)

  bg_col <- "#FFFFFF"
  # site   <- st_read("data/site.kml", quiet = TRUE)
  # site <- st_union(st_cast(site, "MULTIPOLYGON"))

  # imag   <- brick("data/aerial_mosaic.tif")
  imag_crop <- raster("data/635701909.tif")
  imag_crop <- raster::projectRaster(imag_crop, crs = "EPSG:4326", method = "bilinear")

  e <- extent(-118.6833, -118.6729, 34.23165, 34.2388)
  imag_crop <- raster::crop(imag_crop, e)


  crds_sub <- data.table(unique(crds_sub[, list(name, x = lon, y = lat)]))

  crds_sub <- st_as_sf(crds_sub, coords = c("x", "y"),
                       crs = 4326,
                       agr = "constant")

  crds_sub   <- st_transform_proj(crds_sub, crs = "EPSG:4326", use_gdal = FALSE) # Works!


  par(mar = c(0,0,0,0), bg = "#f0f6f9")
  image(imag_crop, col = grey(1:100/100), asp = 1, maxpixels = 5e7, axes = FALSE)


  cols <- c(viridis(6), "#151515")[c(1,1,3,3,7)]
  plot(crds_sub, add = TRUE, col = cols, pch = 21, bg = bg_col, cex = 7, lwd = 14)
  # 1:   C-03 -118.6823 34.23280          561.06        138.68
  # 2: RD-121 -118.6759 34.23299          597.50         76.20
  # 3: RD-130 -118.6738 34.23793          573.46         53.34
  # 4: RD-45B -118.6811 34.23284          560.85        179.83
  # 5:  RD-77 -118.6769 34.23397          584.77         51.82
  # xleft, ybottom, xright, ytop,


  buf <- 0.0007
  rect(-118.6823 - buf, 34.23280 - buf, -118.6811 + buf, 34.23284 + buf / 1.5, border = viridis(6)[1], lwd = 14)
  rect(-118.6769 - buf, 34.23299 - buf, -118.6759 + buf, 34.23397 + buf / 1.5, border = viridis(6)[3], lwd = 14)
  rect(-118.6738 - buf, 34.23793 - buf, -118.6738 + buf / 1.1, 34.23793 + buf / 4.7, border = "#151515", lwd = 14)

  txt <- c("Semi-confined", "Unconfined", "Barometric")
  sw   <- strwidth(txt)
  sh   <- strheight(txt)
  xt <- c(-118.6811, -118.6759, -118.6738 + sw[3] * 1.8) - sw*2
  yt <- c(34.23284 + buf, 34.23397 + buf, 34.23793 + buf / 1.8)
  frsz <- 0.00013
  rect(
    xt - sw/2 - frsz * 4,
    yt - sh/2 - frsz,
    xt + sw/2 + frsz * 4,
    yt + sh/2 + frsz,
    col = bg_col,
    border = c(viridis(6)[c(1,3)], "#151515"),
    lwd = 14
  )
  text(xt, yt, txt, cex = 3.8, font = 2, col = c(viridis(6)[c(1,3)], "#151515"))



  xt <- st_coordinates(crds_sub)[, 1]
  yt <- st_coordinates(crds_sub)[, 2] - 0.0004
  yt[4] <- yt[1]
  txt <- crds_sub$name
  sw   <- strwidth(txt)
  sh   <- strheight(txt)
  frsz <- 0.00013
  rect(
    xt - sw/2 - frsz*2.9,
    yt - sh/2 - frsz,
    xt + sw/2 + frsz*2.9,
    yt + sh/2 + frsz,
    col = bg_col,
    border = cols,
    lwd = 14
  )


  text(xt, yt, txt, cex = 3.8, font = 2, col = cols)

  sb <- 0.0010

  shift <- -0.0001

  # 200 m
  # 110
  rect(-118.6823-buf, 34.238 + shift, -118.6823-buf + sb, 34.238 + 0.0003 + shift, col = bg_col, border = "#303030", lwd = 14)
  #200 m
  rect(-118.6823-buf, 34.238 + 0.0003 + shift, -118.6823-buf + sb*0.914, 34.238 + 0.0006 + shift, col = bg_col, border = "#303030", lwd = 14)

  text(-118.6823-buf/1.2, 34.238+ 0.00015 + shift, "100 m", cex = 4, font = 2, col = "#303030", adj = c(0, 0.5))
  text(-118.6823-buf/1.2, 34.238 + 0.00045 + shift,  "300 ft", cex = 4, font = 2, col = "#303030", adj = c(0, 0.5))

  rect(-118.6823-buf + sb + 0.0002, 34.238 + shift, -118.6823-buf + sb + 0.0006, 34.238 + 0.0006 + shift, col = bg_col, border = "#303030", lwd = 14)
  north.arrow(-118.6823-buf + sb + 0.0004, 34.2384 + shift, 0.00017, col1 = "#303030",
              col2 = bg_col, border1 = "#303030", coltext = "#303030")

  box(lwd = 20, col = "#303030")
  box(lwd = 4,  col = "#EEEEEE")
  invisible(dev.off())

  "img/figure_03.png"
}


# Figure 4 ----------------------------------------------------------------

plot_figure_04 <- function(wl, et, tf_pgram, default_height_half, default_width) {

  setDT(et)
  dates <- data.table(datetime =
                        seq.POSIXt(min(wl$datetime), max(wl$datetime), by = "60 sec"))
  wl <- melt(wl[, list(datetime, baro = rd_130_baro, rd_130)], id.vars = "datetime")
  wl <- wl[, value := scale(value, scale = FALSE), by = variable]
  wl <- wl[dates, on = "datetime"]

  pgram <- copy(tf_pgram)
  pgram[, ratio := wl / ba]

  rat_f <- pgram[ba > 0.25 & frequency %between% c(0.3, 2.2)]

  pgram_long <- melt(pgram[between(frequency, 0.2, 2.2), list(frequency, ba, wl)],
                     id.vars = "frequency")

  p1 <- ggplot(wl, aes(x = datetime, y = value, color = variable))
  p1 <- p1 + geom_line()
  p1 <- p1 + ylab("Pressure\n(cm equivalent freshwater head)")
  p1 <- p1 + xlab("")
  p1 <- p1 + scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 5))
  p1 <- p1 + scale_x_datetime(expand = c(0, 0), date_labels = "%Y-%m-%d")
  p1 <- p1 + scale_color_manual(values = c(ba_col, wl_col))
  p1 <- p1 + ggtitle("Time Domain")
  p1 <- p1 + annotate(geom = "text", x = as.POSIXct("2016-08-27"), y = 9,
                      label = "Water", hjust = 0, size = 4.5, color = wl_col)
  p1 <- p1 + annotate(geom = "text", x = as.POSIXct("2016-08-27"), y = -5,
                      label = "Barometric", hjust = 0, size = 4.5, color = ba_col)
  p1 <- p1 + theme_joh


  p2 <- ggplot(pgram_long, aes(x = frequency, y = value, color = variable))
  p2 <- p2 + geom_line()
  p2 <- p2 + geom_rect(aes(xmin = 1.86, xmax = 1.98, ymin = 0.0, ymax = 0.045),
                       fill = "transparent", color = "#303030", linewidth = 0.1)
  p2 <- p2 + scale_x_continuous(expand = c(0,0))
  p2 <- p2 + ylab("Amplitude\n(cm equivalent freshwater head)")
  p2 <- p2 + xlab(frequency_xlab)
  p2 <- p2 + scale_color_manual(values = c(ba_col, wl_col))
  p2 <- p2 + ggtitle("Frequency Domain")
  p2 <- p2 + theme_joh


  pzoom <- ggplot(pgram_long[between(frequency, 1.86, 1.98)], aes(x = frequency, y = value, color = variable))
  pzoom <- pzoom + geom_line()
  pzoom <- pzoom + geom_point(size = 2)
  pzoom <- pzoom + geom_text(aes(x = 1.895981969, y = 0.012, label = "N[2]"), parse = TRUE, hjust = 1)
  pzoom <- pzoom + geom_text(aes(x = 1.932273616, y = 0.041, label = "M[2]"), parse = TRUE, hjust = 0.01)
  pzoom <- pzoom + xlab("")
  pzoom <- pzoom + ylab("")
  pzoom <- pzoom + scale_color_manual(values = c(ba_col, wl_col))
  pzoom <- pzoom + theme_joh
  pzoom <- pzoom + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank())



  p2 <- p2 + annotation_custom(ggplotGrob(pzoom), xmin = 1.06, xmax = 1.92,
                               ymin = 0.1, ymax = 0.47)
  p2 <- p2 + annotate(geom = "text", x = rat_f[1]$frequency, y = 0.47,
                      label = paste("Ratio\n", round(rat_f[1]$ratio, 2)), hjust = 0.52, size = 4, angle = 90)
  p2 <- p2 + annotate(geom = "text", x = rat_f[2]$frequency, y = 0.47,
                      label = paste("Ratio\n", round(rat_f[2]$ratio, 2)), hjust = 0.52, size = 4, angle = 90)
  p2 <- p2 + annotate(geom = "text", x = rat_f[3]$frequency, y = 0.47,
                      label = paste("Ratio\n", round(rat_f[3]$ratio, 2)), hjust = 0.52, size = 4, angle = 90)
  p2 <- p2 + annotate(geom = "text", x = 0.35, y = 0.28,
                      label = "Water", hjust = 0, size = 4.5, color = wl_col)
  p2 <- p2 + annotate(geom = "text", x = 0.35, y = 0.38,
                      label = "Barometric", hjust = 0, size = 4.5, color = ba_col)

  et <- et[dates, on = "datetime"]
  p6 <- ggplot(et, aes(x = datetime, y = volume_strain))
  p6 <- p6 + geom_line(color = "#404040")
  p6 <- p6 + ylab("Earth Tide\n(nanostrain)")
  p6 <- p6 + xlab("")
  p6 <- p6 + scale_y_continuous(limits = c(-50, 50))
  p6 <- p6 + scale_x_datetime(expand = c(0, 0), date_labels = "%Y-%m-%d")
  p6 <- p6 + annotate(geom = "text", x = as.POSIXct("2016-08-25"), y = -37.5,
                      label = "Volumetric Strain", hjust = 0.5, vjust = 0.5, size = 4.5, color = "#404040")
  p6 <- p6 + theme_joh

  p5 <- ggplot(pgram[frequency %between% c(0.2, 2.2), list(frequency, et)], aes(x = frequency, y = et))
  p5 <- p5 + geom_line(color = "#404040")
  p5 <- p5 + scale_x_continuous(expand = c(0,0))
  p5 <- p5 + ylab("Amplitude\n(nanostrain)")
  p5 <- p5 + xlab(frequency_xlab)
  p5 <- p5 + scale_color_manual(values = c(ba_col, wl_col))
  p5 <- p5 + annotate(geom = "text", x = 0.5, y = 1,
                      label = "Volumetric Strain", hjust = 0.5, vjust = 0.5, size = 4.5, color = "#404040")
  p5 <- p5 + theme_joh


  theme_add <- theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 12),
                     axis.title.y = element_text(size = 16),
                     axis.ticks.x = element_blank(),
                     plot.margin = unit(c(0.2,0.2,0,0.2), "cm"),
                     plot.title = element_text(size = 18))
  theme_add2 <- theme(plot.margin = unit(c(0,0.2,0.2,0.2), "cm"),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 16))

  p1 <- p1 + theme_add
  p2 <- p2 + theme_add
  p5 <- p5 + theme_add2
  p6 <- p6 + theme_add2
  pdf("img/figure_04.pdf", height = default_height_half * 1.5, width = default_width * 2.5)
  print(p1 + p2 + p6 + p5 +
          plot_layout(widths = c(1, 1), heights = unit(c(9.5, 4.5), c("cm", "cm"))))
  invisible(dev.off())

  "img/figure_04.pdf"
}

# Figure 5 ----------------------------------------------------------------

plot_figure_05 <- function(ba_compare_time, ba_compare_frequency, wl_130) {

  # co <- read_fst("data/resp_baro_tf.fst", as.data.table = TRUE)
  # wa <- readRDS("data/wl_baro_et_tf_rd130.rds")
  #
  # wa <- data.table(freq = wa$freq * 86400, coh = wa$coh[,1])
  # wa[, variable := "RD-130 water"]


  # resp <- read_fst("data/resp_baro_dl.fst", as.data.table = TRUE)
  wl_130 <- data.table(freq = wl_130$freq*86400,
                       coh = wl_130$coh[,1],
                       variable = "RD-130 water")
  co <- ba_compare_frequency[freq != 0]
  co <- rbind(co, wl_130)
  resp <- ba_compare_time[x != 0]
  co[, variable := factor(variable, levels = c("RD-130 deep", "RD-08", "RD-10", "RD-130 water") )]
  co <- co[freq < 30000]
  resp[, outcome := factor(outcome,
                           levels = c("rd_130_deep", "rd_08", "rd_10"),
                           labels = c("RD-130 deep", "RD-08", "RD-10") )]
  resp <- resp[variable == "cumulative"]
  cols <- viridis(3, end = 0.7)

  p1 <- ggplot(resp, aes(x = x, y = value, color = outcome))
  p1 <- p1 + annotation_logticks(sides = "tb", colour = "grey",
                                 short = unit(0.05, "cm"),
                                 mid = unit(0.1, "cm"),
                                 long = unit(0.15,"cm"))
  p1 <- p1 + geom_line(linewidth = 1.25)
  p1 <- p1 + ggtitle("Time Domain")
  p1 <- p1 + xlab(time_xlab)
  p1 <- p1 + ylab(time_ylab)
  p1 <- p1 + scale_x_log10(limits = c(1,3600), expand = c(0,0))
  p1 <- p1 + scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2))
  p1 <- p1 + scale_color_manual(values = cols)
  p1 <- p1 + annotate(geom = "text", x = 10, y = 1.02,
                      label = "RD-130 Deep", hjust = 0.5, size = 4, color = cols[1])
  p1 <- p1 + annotate(geom = "text", x = 40, y = 0.6,
                      label = "RD-08 (3000 m)", hjust = 1, size = 4, color = cols[2])
  p1 <- p1 + annotate(geom = "text", x = 90, y = 0.6,
                      label = "RD-10 (700 m)", hjust = 0, size = 4, color = cols[3])
  p1 <- p1 + theme_joh
  p1 <- p1 + theme(axis.title = element_text(size = 16),
                   axis.text = element_text(size = 10))

  p2 <- ggplot(co, aes(x = freq, y = coh, color = variable))
  p2 <- p2 + annotation_logticks(sides = "tb", colour = "grey",
                                 short = unit(0.05, "cm"),
                                 mid = unit(0.1, "cm"),
                                 long = unit(0.15,"cm"))
  p2 <- p2 + geom_line(linewidth = 1.25)
  p2 <- p2 + ggtitle("Frequency Domain")
  p2 <- p2 + xlab(frequency_xlab)
  p2 <- p2 + ylab("Coherence with RD-130 shallow")
  p2 <- p2 + scale_x_log10(limits = c(0.5, 30000), expand = c(0, 0))
  p2 <- p2 + scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2))
  p2 <- p2 + scale_color_manual(values = c(cols, "#00000080" ))
  p2 <- p2 + annotate(geom = "text", x = 1000, y = 1.02,
                      label = "RD-130 Deep", hjust = 0, size = 4, color = cols[1])
  p2 <- p2 + annotate(geom = "text", x = 11, y = 0.6,
                      label = "RD-08 (3000 m)", hjust = 0.5, size = 4, color = cols[2])
  p2 <- p2 + annotate(geom = "text", x = 150, y = 0.6,
                      label = "RD-10 (700 m)", hjust = 0, size = 4, color = cols[3])
  p2 <- p2 + annotate(geom = "text", x = 1200, y = 0.2,
                      label = "RD-130 Water", hjust = 0.5, size = 4, color = "#00000080")
  p2 <- p2 + theme_joh
  p2 <- p2 + theme(axis.title = element_text(size = 16),
                   axis.text = element_text(size = 10))


  pdf("img/figure_05.pdf", height = default_height_half * 1.05, width = default_width * 2.5)
  print(p1 + p2)
  dev.off()

  "img/figure_05.pdf"
}

# Figure 6 ----------------------------------------------------------------

plot_figure_06 <- function(resp_dl_subs, response_dl, tf_rd_130_subs, tf_rd_130, static) {


  static <- data.table(
    name = c("Clark (1 hr)", "Clark (1 d)", "Acworth", "Least Squares"),
    value = unlist(static))


  loc_y <- 0.95
  setDT(response_dl)
  response_dl[, delta_t := 1]
  resp_dl_subs <- rbind(resp_dl_subs, response_dl)
  resp_dl_subs <- resp_dl_subs[outcome == "rd_130"]
  resp_dl_subs <- resp_dl_subs[variable == "cumulative"]
  resp_dl_subs[, x := x * delta_t]
  resp_dl_subs <- resp_dl_subs[delta_t %in% c(1, 60, 900, 3600, 7200)]

  lab_dt <- resp_dl_subs[x != 0, .SD[1], by = delta_t]
  p <- ggplot(resp_dl_subs[x != 0],
              aes(x = x,
                  y = value,
                  color = factor(delta_t, levels = c(1, 60, 900, 3600, 7200))))
  p <- p + annotation_logticks(sides = "tb", colour = "grey",
                               short = unit(0.05, "cm"),
                               mid = unit(0.1, "cm"),
                               long = unit(0.15,"cm"))
  p <- p + geom_line(linewidth = 1)
  # p <- p + geom_segment(data = static, aes(x = 90000, xend = 100000, y = value, yend = value), color = "#303030")
  # # p <- p + geom_segment(data = static, aes(x = 1000, xend = 100000, y = value, yend = value), linetype = "dashed", color = "#303030")
  # p <- p + geom_text(data = static, aes(x = 88000, y = value, label = name), vjust = 0.5, hjust = 1, color = "#303030", size = 2.5)
  p <- p + geom_segment(data = static, aes(x = 1, xend = 100, y = value, yend = value), linetype = "dashed", color = "#303030")
  p <- p + geom_segment(data = static, aes(x = 1000, xend = 100000, y = value, yend = value), linetype = "dashed", color = "#303030")
  p <- p + geom_text(data = static, aes(x = 300, y = value + c(0, 0.01, 0, -0.01), label = name),
                     vjust = 0.5, hjust = 0.5, color = "#303030", size = 3)

  p <- p + geom_line(data = data.table(x = c(1, 10),
                                       y = c(loc_y, loc_y)), aes(x = x, y = y), linewidth = 7.0,
                     color = viridis(6, alpha = 0.2)[4])
  p <- p + geom_line(data = data.table(x = c(10, 300),
                                       y = c(loc_y, loc_y)), aes(x = x, y = y), linewidth = 7.0,
                     color = "#193f6e25")
  p <- p + geom_line(data = data.table(x = c(300, 100000),
                                       y = c(loc_y, loc_y)), aes(x = x, y = y), linewidth = 7.0,
                     color = viridis(1, alpha = 0.2))
  p <- p + geom_text(data = data.table(x = c(3.1, 60, 6500),
                                       y = rep(loc_y, 3),
                                       label = c("Low signal to noise", "Storage response", "Vadose response")),
                     aes(x = x, y = y, label = label, fontface = "bold"),
                     color = c(viridis(6)[4], "#193f6e", viridis(1)), size = 3.2)
  p <- p + geom_text(data = lab_dt, aes(x = x, y = value, label = delta_t), hjust = 1, vjust = -0.2, size = 4.5)
  p <- p + annotate(geom = "text", x = 17, y = 0.56,
                    label = "Distributed Lag", hjust = 1, size = 4.5, color = "#00000080")
  p <- p + scale_x_log10(expand = c(0,0),
                         limits = c(1, 100000),
                         breaks = 10^(0:4),
                         labels = c("1", "10", "100", "1,000", "10,000"))
  p <- p + scale_y_continuous(limits = c(0, 1), expand = c(0,0), breaks = seq(0, 1, 0.2))
  p <- p + scale_color_manual(values = c("#00000080", viridis(4, end = 0.8)))
  p <- p + xlab(time_xlab)
  p <- p + ylab(time_ylab)
  p <- p + ggtitle("Time Domain Response")
  p <- p + theme_joh
  p <- p + theme(text = element_text(size = 16))



  tf_rd_130_subs <- c(tf_rd_130_subs, list(n_1 = tf_rd_130))
  tf_rd_130_subs <- tf_rd_130_subs[c(1, 2, 4, 7, 8)]
  tf <- list()
  for (i in seq_along(tf_rd_130_subs)) {
    tmp <- tf_rd_130_subs[[i]]
    dt <- as.numeric(gsub("n_", "", names(tf_rd_130_subs)[i]))
    tmp <- data.table(freq = tmp$freq * 86400 /
                        dt, tf = Mod(tmp$transfer[, 1]))
    tmp[, f_name := dt]

    tf[[i]] <- tmp
  }
  tf <- rbindlist(tf)

  lab_fr <- tf[, tail(.SD, 1), by = f_name]
  lab_fr[f_name == 1, tf := 0.73]
  lab_fr[f_name == 1, freq := 1090]

  tff <- tf[freq != 0]
  mf <- min(tff$freq)
  p2 <- ggplot(tff, aes(x = freq, y = tf, color = factor(f_name, levels = c(1, 60, 900, 3600, 7200))))

  p2 <- p2 + geom_segment(data = static, aes(x = 0.02, xend = 8, y = value, yend = value), linetype = "dashed", color = "#303030")
  p2 <- p2 + geom_segment(data = static, aes(x = 120, xend = 43200, y = value, yend = value), linetype = "dashed", color = "#303030")
  p2 <- p2 + geom_text(data = static, aes(x = 30, y = value + c(0, 0.01, 0, -0.01), label = name), vjust = 0.5, hjust = 0.5, color = "#303030", size = 3)


  p2 <- p2 + geom_line(linewidth = 1)
  p2 <- p2 + geom_line(data = data.table(x = c(mf, 30), y = c(loc_y, loc_y)), aes(x = x, y = y), linewidth = 7.0,
                       color = viridis(1, alpha = 0.2))
  p2 <- p2 + geom_line(data = data.table(x = c(30, 1000), y = c(loc_y, loc_y)), aes(x = x, y = y), linewidth = 7.0,
                       color = "#193f6e25")
  p2 <- p2 + geom_line(data = data.table(x = c(1000, 43200), y = c(loc_y, loc_y)), aes(x = x, y = y), linewidth = 7.0,
                       color = viridis(6, alpha = 0.2)[4])
  p2 <- p2 + geom_text(data = data.table(x = c(0.5, 150, 8000),
                                         y = rep(loc_y, 3),
                                         label = c("Vadose response", "Storage response", "Low signal to noise")),
                       aes(x = x, y = y, label = label, fontface = "bold"),
                       color = c(viridis(1), "#193f6e", viridis(6)[4]), size = 3)
  p2 <- p2 + annotation_logticks(sides = "tb", colour = "grey",
                                 short = unit(0.05, "cm"),
                                 mid = unit(0.1, "cm"),
                                 long = unit(0.15,"cm"))
  p2 <- p2 + geom_text(data = lab_fr, aes(x = freq, y = tf + c(-0.03, -0.03, +0.05, +0.03, +0.02), label = f_name),
                       hjust = 0.5, vjust = 0.5, size = 4.5)
  p2 <- p2 + scale_x_log10(expand = c(0,0),
                           limits = c(mf, 43200),
                           breaks = 10^(-1:4),
                           labels = c("0.1", "1", "10", "100", "1,000", "10,000"))
  p2 <- p2 + scale_y_continuous(limits = c(0.0, 1.0), expand = c(0,0), breaks = seq(0, 1, 0.2))
  p2 <- p2 + scale_color_manual(values = c("#00000080", viridis(4, end = 0.8)))
  p2 <- p2 + ggtitle("Frequency response")
  p2 <- p2 + xlab("Frequency (cycles per day)")
  p2 <- p2 + ylab("Gain (Water & Barometric Pressure)")
  p2 <- p2 + theme_joh
  p2 <- p2 + theme(text = element_text(size = 16))


  pdf("img/figure_06.pdf",
      height = default_height_half * 1.05,
      width = default_width*2.5)
  print(p + p2)
  dev.off()

  "img/figure_06.pdf"
}


# Figure 7 ----------------------------------------------------------------

plot_figure_07 <- function(wl, response_dl, predict_dl, pgram, wave_groups_dl) {
  setDT(response_dl)

  response_dl <- response_dl[outcome == "rd_130"]
  et_amp <- response_dl[grepl("cos_", term)]
  et_amp <- et_amp[, term := gsub("cos_", "amplitude_", term)]
  et_amp <- et_amp[, value := sqrt(response_dl[grepl("cos_", term)][["value"]]^2 +
                                     response_dl[grepl("sin_", term)][["value"]]^2)]
  et_amp[, x := earthtide::get_main_frequency(wave_groups_dl$start, wave_groups_dl$end)]

  predict_dl <- predict_dl[, list(earthtide = rd_130_step_add_vars,
                                  baro = rd_130_step_distributed_lag_baro,
                                  background = rd_130_step_spline_b_datetime,
                                  intercept = rd_130_step_intercept)]

  predict_dl[is.na(baro), earthtide := NA_real_]
  predict_dl[is.na(baro), background := NA_real_]
  predict_dl[is.na(baro), intercept := NA_real_]

  wl <- wl[, list(datetime, rd_130, baro_measured = rd_130_baro)]

  wl <- cbind(wl, predict_dl)
  wl[, residual := rd_130 - baro - earthtide - background - intercept]

  pgram_sub <- pgram[between(frequency, 0.8, 2.2)]

  resp_in <- et_amp[x < 2.2]
  resp_in[, variable := "Earth Tide Component"]
  data.table::setnames(pgram_sub, c("Frequency",
                        "Water Level",
                        "Barometric",
                        "Earth Tide Component",
                        "Residual",
                        "Spline Component",
                        "Distributed Lag Component"))
  pgram_sub <- melt(pgram_sub, id.vars = "Frequency")
  pgram_sub[, variable := factor(variable, levels = c("Water Level",
                                                      "Barometric",
                                                      "Distributed Lag Component",
                                                      "Spline Component",
                                                      "Earth Tide Component",
                                                      "Residual"))]
  pgram_sub_a <- pgram_sub[variable %in% c("Frequency", "Barometric", "Water Level", "Distributed Lag Component", "Spline Component")]
  pgram_sub_b <- pgram_sub[variable %in% c("Frequency","Earth Tide Component","Residual")]
  amp_scale <-  max(resp_in[variable  == "Earth Tide Component"]$value)
  pgram_sub_b <- pgram_sub[variable %in% c("Frequency", "Residual")]
  pgram_sub_b[, variable := factor(variable, levels = c("Earth Tide Component", "Residual"))]


  ksm <- as.data.table(copy(earthtide:::ksm04))
  ksm[, amp := sqrt(C0^2  + S0^2)]
  ksm <- ksm[frequency_cpd > 0.7]
  setkey(ksm, amp)
  ksm <- tail(ksm, 8)
  ksm[, x := frequency / 15 ]
  ksm[, value := amp * (amp_scale / max(amp)) ]
  ksm[, variable :=  "Earth Tide Component" ]

  ksm[, id := rev(c("M[2]", "K[1]", "S[2]", "O[1]", "P[1]", "N[2]", "K[2]", "Q[1]"))]

  ksm <- ksm[-c(2,4)]


  setnames(wl, c("datetime", "Water Level", "Barometric", "Earth Tide Component", "Distributed Lag Component", "Spline Component", "Intercept", "Residual"))
  dat_sub <- wl[seq(1, nrow(wl), 120)]
  dat_sub <- melt(dat_sub, id.vars = "datetime")
  dat_sub <- dat_sub[, value := scale(value, scale = FALSE), by = variable]
  dat_sub[, variable := factor(variable, levels = c("Water Level",
                                                    "Barometric",
                                                    "Distributed Lag Component",
                                                    "Spline Component",
                                                    "Earth Tide Component",
                                                    "Residual"))]

  dat_sub_a <- dat_sub[variable %in% c("Barometric", "Water Level", "Distributed Lag Component", "Spline Component")]
  dat_sub_b <- dat_sub[variable %in% c("Earth Tide Component", "Residual")]

  p1a <- ggplot(dat_sub_a, aes(x = datetime, y = value))
  p1a <- p1a + geom_line(color = "#000000")
  p1a <- p1a + facet_wrap(variable~., ncol = 1)
  p1a <- p1a + ggtitle("Time Domain")
  p1a <- p1a + xlab("")
  p1a <- p1a + ylab("Pressure (cm water)")
  p1a <- p1a + scale_x_datetime(expand = c(0,0), date_labels = "%Y-%m-%d")
  p1a <- p1a + scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 10))
  p1a <- p1a + theme_joh
  p1a <- p1a + theme(axis.title.y = element_text(size = 16, hjust = 0.1),
                     axis.text.y = element_text(size = 10),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     plot.margin = unit(c(0.2, 0.2, 0, 0.2), "cm"),
                     axis.ticks.x = element_blank())

  p1b <- ggplot(dat_sub_b, aes(x = datetime, y = value))
  p1b <- p1b + geom_line(color = "#000000")
  p1b <- p1b + facet_wrap(variable~., ncol = 1)
  # p1b <- p1b + ggtitle("Time Domain")
  p1b <- p1b + xlab("")
  p1b <- p1b + ylab("")
  p1b <- p1b + scale_x_datetime(expand = c(0, 0), date_labels = "%Y-%m-%d")
  p1b <- p1b + scale_y_continuous(expand = c(0, 0), limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.2))
  p1b <- p1b + theme_joh
  p1b <- p1b + theme(axis.title = element_text(size = 16),
                     axis.text = element_text(size = 10),
                     plot.margin = unit(c(0.0, 0.2, 0.2, 0.2), "cm"))



  xlim <- range(pgram_sub_a$Frequency)
  p2a <- ggplot(pgram_sub_a, aes(x = Frequency, y = value))
  p2a <- p2a + geom_area(color = "#606060",
                         fill = "#DDDDDD")
  p2a <- p2a + facet_wrap(variable~., ncol = 1)
  p2a <- p2a + ggtitle("Frequency Domain")
  p2a <- p2a + xlab("Frequency (cycles per day)")
  p2a <- p2a + ylab("Amplitude (cm water)")
  p2a <- p2a + scale_x_continuous(expand = c(0,0), limits = xlim)
  p2a <- p2a + scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.25))
  p2a <- p2a + theme_joh
  p2a <- p2a + theme(axis.title.y = element_text(size = 16, hjust = 0.1),
                     axis.text.y = element_text(size = 10),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     plot.margin = unit(c(0.2,0.2,0,0.2), "cm"),
                     axis.ticks.x = element_blank())


  p2b <- ggplot(pgram_sub_b, aes(x = Frequency, y = value))
  p2b <- p2b + geom_area(color = "#606060",
                         fill = "#DDDDDD")
  p2b <- p2b + geom_segment(data = resp_in,
                            aes(x = x, xend = x, y = 0, yend = value),
                            linewidth = 1.4, colour = "#AAAAAA")
  p2b <- p2b + geom_point(data = ksm, aes(x = x, y = value), shape = "-",
                          color = viridis(1), size = 10)
  p2b <- p2b + geom_text(
    data    = ksm,
    mapping = aes(x = frequency_cpd, y = value, label = id),
    hjust   = 0.5,
    vjust   = -0.6,
    color   = "black",
    parse = TRUE,
    size = 3
  )
  p2b <- p2b + facet_wrap(variable~., ncol = 1)
  # p2b <- p2b + ggtitle("Frequency Domain")
  p2b <- p2b + xlab("Frequency (cycles per day)")
  p2b <- p2b + ylab("")
  p2b <- p2b + scale_x_continuous(expand = c(0,0), breaks = seq(1, 2, 0.5), limits = xlim)
  p2b <- p2b + scale_y_continuous(expand = c(0, 0), limits = c(0, 0.06), breaks = seq(0, 0.06, 0.03))
  p2b <- p2b + theme_joh
  p2b <- p2b + theme(axis.title = element_text(size = 16),
                     axis.text = element_text(size = 10),
                     plot.margin = unit(c(0, 0.2, 0.2, 0.2), "cm"))



  pdf("img/figure_07.pdf", height = default_height_half * 1.8, width = default_width * 2.5)
  print(p1a + p2a + p1b + p2b + plot_layout(widths = c(1, 1), heights = c(2, 1)))
  dev.off()

  "img/figure_07.pdf"

}


# Figure 8 ----------------------------------------------------------------

plot_figure_08 <- function(response_dl,
                           tf_c_3,
                           tf_rd_45b,
                           tf_rd_130,
                           tf_rd_121,
                           tf_rd_77) {

  cols <- viridis(4, end = 0.8)


  response_dl <- response_dl[outcome != "rd_130"]
  response_dl <- response_dl[variable == "cumulative"]
  response_dl <- response_dl[x != 0]

  response_dl[, outcome := factor(outcome, levels = c("c_3", "rd_45b", "rd_77", "rd_121"))]
  cols <- viridis(4, end = 0.8)
  p1 <- ggplot(response_dl, aes(x = x, y = value, color = outcome))
  p1 <- p1 + geom_line(linewidth = 1.5)
  p1 <- p1 + xlab(time_xlab)
  p1 <- p1 + ylab(time_ylab)
  p1 <- p1 + annotate(geom = "text", x = 1000, y = 0.09,
                      label = "C-03", hjust = 1, size = 4.5, color = cols[1])
  p1 <- p1 + annotate(geom = "text", x = 120, y = 0.4,
                      label = "RD-45B", hjust = 0.5, size = 4.5, color = cols[2])
  p1 <- p1 + annotate(geom = "text", x = 2700, y = 0.4,
                      label = "RD-77", hjust = 0, size = 4.5, color = cols[3])
  p1 <- p1 + annotate(geom = "text", x = 380, y = 0.82,
                      label = "RD-121", hjust = 1, size = 4.5, color = cols[4])
  p1 <- p1 + ggtitle("Time Domain Response")
  p1 <- p1 + scale_x_log10(expand = c(0,0),
                           limits = c(1, 129600),
                           breaks = 10^(0:5),
                           labels = c("1", "10", "100", "1,000", "10,000", "100,000"))
  p1 <- p1 + scale_y_continuous(limits = c(0, 1), expand = c(0,0))
  p1 <- p1 + scale_color_manual(values = cols)
  p1 <- p1 + annotation_logticks(sides = "tb", colour = "grey",
                                 short = unit(0.05, "cm"),
                                 mid = unit(0.1, "cm"),
                                 long = unit(0.15,"cm"))
  p1 <- p1 + theme_joh
  p1 <- p1 + theme(axis.title = element_text(size = 16),
                   axis.text = element_text(size = 10))


  c_3 <- data.table(freq = tf_c_3$freq * 86400, tf = Mod(tf_c_3$transfer[, 1]))[freq < 2000][, variable := "C-03"]
  rd_45b <- data.table(freq = tf_rd_45b$freq * 86400, tf = Mod(tf_rd_45b$transfer[, 1]))[freq < 2000][, variable := "RD-45B"]
  # rd_130 <- data.table(freq = tf_rd_130$freq * 86400, tf = Mod(tf_rd_130$transfer[, 1]))[freq < 2000][, variable := "RD-130"]
  rd_121 <- data.table(freq = tf_rd_121$freq * 86400, tf = Mod(tf_rd_121$transfer[, 1]))[freq < 2000][, variable := "RD-121"]
  rd_77 <- data.table(freq = tf_rd_77$freq * 86400, tf = Mod(tf_rd_77$transfer[, 1]))[freq < 2000][, variable := "RD-77"]

  tf <- rbindlist(list(c_3, rd_45b, rd_121, rd_77))
  tf <- tf[freq != 0]
  tf[, variable := factor(variable, levels = c("C-03", "RD-45B", "RD-77", "RD-121"))]

  p2 <- ggplot(tf, aes(x = freq, y = tf, color = variable))
  p2 <- p2 + geom_line(linewidth = 1.5)
  p2 <- p2 + xlab(frequency_xlab)
  p2 <- p2 + ylab(frequency_ylab)
  p2 <- p2 + annotate(geom = "text", x = 10, y = 0.05,
                      label = "C-03", hjust = 0, size = 4.5, color = cols[1])
  p2 <- p2 + annotate(geom = "text", x = 80, y = 0.4,
                      label = "RD-45B", hjust = 0.5, size = 4.5, color = cols[2])
  p2 <- p2 + annotate(geom = "text", x = 6, y = 0.4,
                      label = "RD-77", hjust = 1, size = 4.5, color = cols[3])
  p2 <- p2 + annotate(geom = "text", x = 300, y = 0.82,
                      label = "RD-121", hjust = 1, size = 4.5, color = cols[4])
  p2 <- p2 + ggtitle("Frequency Domain Response")
  p2 <- p2 + scale_x_log10(expand = c(0,0),
                           breaks = 10^(-1:3),
                           labels = c("0.1","1.0","10", "100", "1,000"))
  p2 <- p2 + scale_y_continuous(limits = c(0, 1), expand = c(0,0))
  p2 <- p2 + scale_color_manual(values = cols)
  p2 <- p2 + annotation_logticks(sides = "tb", colour = "grey",
                                 short = unit(0.05, "cm"),
                                 mid = unit(0.1, "cm"),
                                 long = unit(0.15,"cm"))
  p2 <- p2 + theme_joh
  p2 <- p2 + theme(axis.title = element_text(size = 16),
                   axis.text = element_text(size = 10))


  pdf("img/figure_08.pdf", height = default_height_half * 1.05, width = default_width * 2.5)
  print(p1 + p2)
  dev.off()

  "img/figure_08.pdf"

}

# Figure 9 ----------------------------------------------------------------

plot_figure_09 <- function(response_dl, predict_dl, wl) {


  # response_dl[, outcome := factor(outcome, levels = c("rd_130", "c_3", "rd_45b", "rd_77", "rd_121"))]

  et_amp <- response_dl[grepl("cos_", term)]
  et_amp <- et_amp[, term := gsub("cos_", "amplitude_", term)]
  et_amp <- et_amp[, value := sqrt(response_dl[grepl("cos_", term)][["value"]]^2 +
                                     response_dl[grepl("sin_", term)][["value"]]^2)]
  et_amp[, x := rep(earthtide::get_main_frequency(wave_groups_dl$start, wave_groups_dl$end), 5)]


  wl <- cbind(wl, predict_dl)

  wl[, res130 := rd_130 - rd_130_step_add_vars - rd_130_step_distributed_lag_baro - rd_130_step_spline_b_datetime - rd_130_step_intercept]
  wl[, res121 := rd_121 - rd_121_step_add_vars - rd_121_step_distributed_lag_baro - rd_121_step_spline_b_datetime - rd_121_step_intercept]
  wl[, res77  := rd_77 - rd_77_step_add_vars - rd_77_step_distributed_lag_baro - rd_77_step_spline_b_datetime - rd_77_step_intercept]
  wl[, res45b := rd_45b - rd_45b_step_add_vars - rd_45b_step_distributed_lag_baro - rd_45b_step_spline_b_datetime - rd_45b_step_intercept]
  wl[, resc03 := c_3 - c_3_step_add_vars - c_3_step_distributed_lag_baro - c_3_step_spline_b_datetime - c_3_step_intercept]

  wl_sub <- wl[seq(1, nrow(wl), 10), list(datetime, res130, res121, res77, res45b, resc03)]
  wl_sub <- melt(wl_sub, id.vars = "datetime")
  wl_sub[, value := value - mean(value, na.rm = TRUE), by = variable]

  cols <- viridis(4, end = 0.8)
  wl_sub[, variable := factor(variable,
                              levels = c("res130", "resc03", "res45b", "res77", "res121"),
                              labels = c("RD-130", "C-03", "RD-45B", "RD-77", "RD-121"))]

  dat_text <- wl_sub[, list(label = paste0("Mean Absolute Error: ", round(mean(abs(value)), 3)), " (cm water)"), variable]


  p1 <- ggplot(wl_sub, aes(x = datetime, y = value, color = variable))
  p1 <- p1 + geom_line(linewidth = 0.5)
  p1 <- p1 + geom_text(
    data    = dat_text,
    mapping = aes(x = max(wl_sub$datetime), y = 1, label = label),
    hjust   = 1.1,
    vjust   = 0.5,
    color   = "black"
  )
  p1 <- p1 + ylab("Residuals (cm water)")
  p1 <- p1 + xlab("")
  p1 <- p1 + facet_wrap(variable~., ncol = 1)
  p1 <- p1 + scale_y_continuous(limits = c(-1.2, 1.2), breaks = seq(-1, 1, 1))
  p1 <- p1 + scale_x_datetime(expand = c(0, 0), date_labels = "%Y-%m-%d")
  p1 <- p1 + scale_color_manual(values = c(rd130col, cols))
  p1 <- p1 + ggtitle("Residuals From Model Fit")
  p1 <- p1 + theme_joh
  p1 <- p1 + theme(axis.title = element_text(size = 16),
                   axis.text = element_text(size = 10))



  et_amp[, outcome := factor(outcome,
                             levels = c("rd_130", "c_3", "rd_45b", "rd_77", "rd_121"),
                             labels = c("RD-130", "C-03", "RD-45B", "RD-77", "RD-121"))]


  dat_text <- as.data.table(copy(earthtide:::ksm04))
  dat_text[, amp := sqrt(C0^2  + S0^2)]
  dat_text <- dat_text[frequency_cpd > 0.7]
  setkey(dat_text, amp)
  dat_text <- tail(dat_text, 8)
  dat_text[, z := frequency / 15 ]
  dat_text[, id := rev(c("M[2]", "K[1]", "S[2]", "O[1]", "P[1]", "N[2]", "K[2]", "Q[1]"))]
  dat_text[, variable := factor("rd130",
                                levels = c("rd130", "c03", "rd45b", "rd77", "rd121"),
                                labels = c("RD-130", "C-03", "RD-45B", "RD-77", "RD-121"))]
  dat_text <- dat_text[, list(z,x = z, variable, value = 0.1, id)]
  dat_text <- dat_text[!id %in% c("P[1]", "K[2]")]

  p2 <- ggplot(et_amp,
               aes(x = x, xend = x, y = 0, yend = value, color = outcome))
  p2 <- p2 + geom_segment(linewidth = 1.4)
  p2 <- p2 + facet_wrap(outcome~., ncol = 1)
  p2 <- p2 + scale_x_continuous(limits = c(0.85, 2.05),
                                expand = c(0,0),
                                breaks = seq(1, 2, 0.5))
  p2 <- p2 + scale_y_continuous(limits = c(0.0, 0.1),  breaks = seq(0, 0.1, 0.05))
  p2 <- p2 + scale_color_manual(values = c(rd130col, cols))
  p2 <- p2 + xlab(frequency_xlab)
  p2 <- p2 + ylab("Amplitude (cm water)")
  p2 <- p2 + ggtitle("Earth Tide Harmonics")
  p2 <- p2 + geom_text(
    data    = dat_text,
    mapping = aes(x = z, y = 0.1, label = id),
    hjust   = 0.5,
    vjust   = 1,
    color   = "black",
    parse = TRUE,
    size = 2.5
  )
  p2 <- p2 + theme_joh
  p2 <- p2 + theme(axis.title = element_text(size = 16),
                   axis.text = element_text(size = 10))



  resp_in <- response_dl[x != 0]
  resp_in <- resp_in[variable == "cumulative"]

  resp_in[, outcome := factor(outcome,
                              levels = c("rd_130", "c_3", "rd_45b", "rd_77", "rd_121"),
                              labels = c("RD-130", "C-03", "RD-45B", "RD-77", "RD-121"))]





  p3 <- ggplot(resp_in,
               aes(x = x, y = value, color = outcome))
  p3 <- p3 + geom_line(linewidth = 1)
  p3 <- p3 + facet_wrap(outcome~., ncol = 1)
  p3 <- p3 + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5))
  p3 <- p3 + scale_x_log10(limits = c(1, 1e5), expand = c(0,0))
  p3 <- p3 + scale_color_manual(values = c(rd130col, cols))
  p3 <- p3 + xlab(time_xlab)
  p3 <- p3 + ylab(time_ylab)
  p3 <- p3 + ggtitle("Response (Distributed Lag)")
  p3 <- p3 + theme_joh
  p3 <- p3 + theme(axis.title = element_text(size = 16),
                   axis.text = element_text(size = 10))


  CairoPDF("img/figure_09.pdf", height = default_height_half * 1.5, width = default_width * 2.5)
  print(p3 + p2 + p1  + plot_layout(widths = c(1, 1, 1.8)))
  dev.off()

  # decrease file size and load times
  file_pdf <- file.path(getwd(), "img/figure_09.pdf")
  dest_png <- file.path(getwd(), "img/figure_09.png")

  magick::image_write(
    magick::image_read(file_pdf, 300), dest_png, "png",
    density = 300
  )

  "img/figure_09.png"
}



# Figure 10 ----------------------------------------------------------------

plot_figure_10 <- function(opt_pars) {
  loc_y <- 0.05

  pars <- opt_pars[[1]]
  ad <- as.numeric(round(10^pars[2], 2))
  tm <- as.numeric(round(10^pars[3], 4))
  e <- new.env()
  assign("ad", value = ad, envir = e)
  assign("tm", value = round(tm * 10000, 0), envir = e)

  dat_text <- data.table(label = c(
    as.character(as.expression(paste0("Thickness:~~~~~~~~~~~~~~~~~~~~~~", round(pars[1], 0), "~m"))),
    as.character(as.expression(substitute(expr = paste("Air Diffusivity:   ", ad, " m"^"2","/s"), env = e))),
    as.character(as.expression(substitute(expr = paste("Transmissivity:    ", tm, " x 10"^"-4"," m"^"2","/s"), env = e))),
    as.character(expression("Storativity:       5 x 10"^"-6")),
    as.character(as.expression(paste0("Loading~Efficiency:~~", round(pars[5], 2))))
  ),
  x = rep(50, 5),
  y = seq(0.96, 0.75, length.out = 5),
  type = "Modeled")


  lab <- data.table(x = 4000 + c(-3000, 2000), y = 0.35,
                    label = c("Empirical", "Modeled"),
                    color = c(viridis(1), viridis(6)[4]), type = "Modeled")

  p <- ggplot(opt_pars$results, aes(x = x, y = value, group = type, color = type)) +
    geom_line(aes(linetype = type), linewidth = 1.5) +
    geom_text(
      data    = dat_text,
      mapping = aes(x = x, y = y, label = label),
      hjust   = 0.0,
      vjust   = 0.5,
      color   = "black",
      parse = TRUE,
      family = "mono"
    ) +
    annotation_logticks(sides = "tb", colour = "grey",
                        short = unit(0.05, "cm"),
                        mid = unit(0.1, "cm"),
                        long = unit(0.15,"cm")) +
    geom_line(data = data.table(x = c(30, 300),
                                y = c(loc_y, loc_y), type = "modeled"), aes(x = x, y = y), linewidth = 7.0,
              color = "#193f6e25") +
    geom_line(data = data.table(x = c(300, 30000),
                                y = c(loc_y, loc_y), type = "modeled"), aes(x = x, y = y), linewidth = 7.0,
              color = viridis(1, alpha = 0.2)) +
    geom_text(data = data.table(x = c(3.1, 60, 3000),
                                y = rep(loc_y, 3),
                                type = "Modeled",
                                label = c("Low signal to noise", "Storage response", "Vadose response")),
              aes(x = x, y = y, label = label, fontface = "bold"),
              color = c(viridis(6)[4], "#193f6e", viridis(1)), size = 3.2) +
    geom_text(data = lab, aes(x = x, y = y, label = label), color = lab$color, size = 5) +
    scale_color_manual(values = c(viridis(1), viridis(6)[4])) +
    scale_x_log10(limits = c(30, 1e5), expand = c(0,0),
                  breaks = 10^(1:5),
                  labels = c("10", "100", "1,000", "10,000", "100,000")) +
    ylab("Cumulative loading response") +
    xlab("Time lag (seconds)") +
    scale_y_continuous(expand = c(0.0, 0.0), limits = c(0.0, 1.0)) +
    theme_joh +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 10))

  pdf("img/figure_10.pdf", height = default_height_half * 1, width = default_width * 1.25)
  print(p)
  dev.off()

  "img/figure_10.pdf"
}



# site map ----------------------------------------------------------------

north.arrow = function(x, y, h, coltext = "black",
                       col1 = "#ffffff90", col2 = "#000000",
                       border1 = "#000000",
                       border2 = NA) {
  polygon(c(x, x + h/2, x, x - h/2), c(y - h, y - (1 + sqrt(3)/2) *
                                         h, y, y - (1 + sqrt(3)/2) * h),
          col = col1, border = border1, lwd = 2)
  polygon(c(x, x, x + h/2), c(y - h, y, y - (1 + sqrt(3)/2) * h),
          col = col2, border = NA, lwd = 2)
  text(x, y, "N", adj = c(0.5, 0), cex = 4, font = 2, col = coltext)
}


# Noise free table --------------------------------------------------------
plot_synthetic_box <- function(box_data, nm, xlim = c(0.88, 1.15), ylim =c(-7, -1)) {

  pdf(paste0("img/dist_lag/", nm), width = 2, height = 0.5)
  par(mar = c(0,0,0,0), mgp = c(0,0,0))
  print(bxp(box_data,
            horizontal = TRUE,
            outline = FALSE,
            ylim = ylim,
            xlim = xlim,
            axes = FALSE,
            ann = FALSE,
            boxfill = "#00000020",
            boxcol = "#505050",
            col = "#505050",
            whiskcol = "#505050",
            whisklty = 1,
            whisklwd = 2,
            staplecol = "#505050",
            staplelty = 1,
            staplelwd = 2,
            medlty = 1,
            medlwd = 2,
            medcol = "#505050",
            boxwex = 0.5,
            boxlwd = 2))
  invisible(dev.off())
}


plot_synthetic_brf <- function(brf, add, nm, n) {
  b <- copy(as.data.table(brf)[variable == "cumulative"])
  b[, x := as.numeric(x) * n]
  b[x == 0, x := 0.1]
  pdf(paste0("img/dist_lag/", nm), width = 2, height = 0.5)
  par(mgp = c(0, 0, 0), mar = c(0, 0, 0, 0))
  add <- data.table(value = add, x = c(0.1, 1:(length(add) - 1)))
  plot(value~x, add,
       log = "x",
       col = "#AAAAAA",
       type = "l",
       lwd = 4,
       ylim = c(0.0, 1.02),
       axes = FALSE, ann = FALSE)
  points(value~x, b,
         col = "#000000", type = "l", lwd = 2)
  invisible(dev.off())
}
