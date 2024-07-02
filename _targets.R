# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("data.table",
               "rsk",
               "psd") # Packages that your targets need for their tasks.

)

# source files ------------------------------------------------------------
tar_source(files = "r")

# run targets -------------------------------------------------------------

list(
# file names ------------------------------------------------------------

  # barometric pressure comparison input datasets
  tar_target(rd_08, "data/rd_08_baro.rsk", format = "file"),
  tar_target(rd_10, "data/rd_10_baro.rsk", format = "file"),
  tar_target(rd_130_d, "data/rd_130_baro_deep.rsk", format = "file"),
  tar_target(rd_130_s, "data/rd_130_baro_shallow.rsk", format = "file"),

  # distributed lag comparison input datasets
  tar_target(rd_130_baro, "data/rd_130_baro.rsk", format = "file"),
  tar_target(c_3, "data/c_3_wl.rsk", format = "file"),
  tar_target(rd_45b, "data/rd_45b_wl.rsk", format = "file"),
  tar_target(rd_77, "data/rd_77.rsk", format = "file"),
  tar_target(rd_121, "data/rd_121_wl.rsk", format = "file"),
  tar_target(rd_130, "data/rd_130_wl.rsk", format = "file"),

  # well details data
  tar_target(well_details_file, "data/well_details.csv", format = "file"),

# read & generate data --------------------------------------------------

  # read well data
  tar_target(
    name = well_details,
    command = fread(well_details_file)),
  # read and process transducer data
  tar_target(
    name = baro,
    command = rsk_baro(c(rd_08, rd_10, rd_130_d, rd_130_s))),
  # read and process transducer data
  tar_target(
    name = wl,
    command = rsk_wl(c(rd_130_baro, c_3, rd_45b, rd_77, rd_121, rd_130))),
  # read and process transducer data
  tar_target(
    name = ba_compare_time,
    command = baro_compare_time(baro[86400:(86400*8)], ba_knots_baro)),
  # generate synthetic Earth tides
  # all together
  tar_target(
    name = et_all,
    command = add_earthtide(wl$datetime,
                            do_predict = TRUE,
                            wave_groups,
                            latitude,
                            longitude,
                            elevation,
                            1e-10,
                            catalog,
                            method
    )),
  # by group
  tar_target(
    name = et_groups,
    command = add_earthtide(wl$datetime,
                            do_predict = FALSE,
                            wave_groups_dl,
                            latitude,
                            longitude,
                            elevation,
                            1e-10,
                            catalog,
                            method
    )),


# frequency domain barometric pressure comparison -------------------------
  tar_target(
    name = ba_compare_frequency,
    command = baro_compare_frequency(baro[86400:(86400*8)])),


# analysis of synthetic data ----------------------------------------------

  # create synthetic kernels
  tar_target(
    name = kernels,
    command = synthetic_kernels(max_syn_lag)),

  # synthetic datasets
  tar_target(
    name = ba_wl_syn,
    command = synthetic_data_wide(wl, kernels, start, end)),
  tar_target(
    name = tf_dt,
    command = synthetic_data_long(ba_wl_syn)),

  # calculate transfer functions
  tar_target(
    name = tf_syn,
    command = freq_response_by_group(tf_dt)),
  # calculate barometric efficiency
  tar_target(
    name = results_syn,
    command = be(ba_wl_syn, kernels)),
  # compare timings and accuracy
  tar_target(
    name = results_method,
    command = run_method_comparison(ba_wl_syn, kernels$kern_comb)),


# analysis of field data --------------------------------------------------
  # barometric efficiency
  tar_target(
    name = static,
    command = static_barometric_efficiency(wl, et_all)),
  # distributed lag results
  tar_target(
    name = rec_dl,
    command = regress_dl_rec(wl,
                             et_groups,
                             start,
                             end,
                             ba_knots,
                             df)),
  # distributed lag results with different monitoring intervals
  tar_target(
    name = resp_dl_subs,
    command = regress_dl_resp_subs(wl,
                                   et_groups,
                                   start,
                                   end,
                                   ba_knots,
                                   df)),
  # irregular spaced lag results
  tar_target(
    name = rec_irr,
    command = regress_irr_rec(wl,
                             et_groups,
                             start,
                             end,
                             ba_lags_irr,
                             df)),
  # determine the time domain impulse response
  tar_target(
    name = response_dl,
    command = regress_dl_response(rec_dl)),
  # determine the time domain contribution for each component
  tar_target(
    name = predict_dl,
    command = regress_dl_predict(rec_dl)),
  tar_target(
    name = formula_dl,
    command = regress_dl_formula(rec_dl)),
  # transfer functions for different wells
  tar_target(
    name = tf_c_3,
    command = transfer_function(wl, et_all, "c_3")),
  tar_target(
    name = tf_rd_45b,
    command = transfer_function(wl, et_all, "rd_45b")),
  tar_target(
    name = tf_rd_77,
    command = transfer_function(wl, et_all, "rd_77")),
  tar_target(
    name = tf_rd_121,
    command = transfer_function(wl, et_all, "rd_121")),
  tar_target(
    name = tf_rd_130,
    command = transfer_function(wl, et_all, "rd_130")),
  tar_target(
    name = tf_rd_130_subs,
    command = transfer_function_subs(wl, et_all, "rd_130")),
  tar_target(
    name = tf_pgram,
    command = residual_pgram(wl, et_all, predict_dl)),


# fit hydraulic model -----------------------------------------------------
  # model fit
  # thickness, air_diffusivity, transmissivity, scale_1, scale_2
  tar_target(
    name = opt_pars,
    command = fit_weeks_cbp(collapse::qDT(response_dl)[variable == "cumulative" & outcome == "rd_130"])),


# create figures ----------------------------------------------------------
  tar_target(
    name = figure_02,
    command = plot_figure_02(results_syn, tf_syn, tf_dt, kernels),
    format = "file"),
  tar_target(
    name = figure_03,
    command = plot_figure_03(well_details),
    format = "file"),
  tar_target(
    name = figure_04,
    command = plot_figure_04(wl, et_all, tf_pgram, default_height_half, default_width),
    format = "file"),
  tar_target(
    name = figure_05,
    command = plot_figure_05(ba_compare_time, ba_compare_frequency, tf_rd_130),
    format = "file"),
  tar_target(
    name = figure_06,
    command = plot_figure_06(resp_dl_subs, response_dl, tf_rd_130_subs, tf_rd_130, static),
    format = "file"),
  tar_target(
    name = figure_07,
    command = plot_figure_07(wl, response_dl, predict_dl, tf_pgram, wave_groups_dl),
    format = "file"),
  tar_target(
    name = figure_08,
    command = plot_figure_08(response_dl,
                             tf_c_3,
                             tf_rd_45b,
                             tf_rd_130,
                             tf_rd_121,
                             tf_rd_77),
    format = "file"),
  tar_target(
    name = figure_09,
    command = plot_figure_09(response_dl, predict_dl, wl),
    format = "file"),
  tar_target(
    name = figure_10,
    command = plot_figure_10(opt_pars),
    format = "file"),


# generate report ---------------------------------------------------------
  tar_render(report,
             "brf_methods_joh_kennel_2024.Rmd")


)
