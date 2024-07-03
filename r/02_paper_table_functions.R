# Code for table generation

tbl_well_details <- function(well_details) {

  well_details <- copy(well_details)
  well_details[, total_depth       := round(total_depth * 0.3048, 1)]
  well_details[, diameter_borehole := round(diameter_borehole / 12 * 0.3048, 2)]
  well_details[, interval_top      := round(interval_top * 0.3048, 1)]
  well_details[, interval_bot      := round(interval_bot * 0.3048, 1)]
  well_details[, top_of_casing_m   := round(top_of_casing_m, 1)]
  well_details[, saturated_interval  := round(saturated_interval, 0)]
  well_details[, depth_to_gw_m  := round(depth_to_gw_m, 0)]

  setkey(well_details, name)
  well_details <- well_details[, list(name,
                                      diameter_borehole,
                                      interval_top,
                                      interval_bot,
                                      top_of_casing_m,
                                      depth_to_gw_m,
                                      saturated_interval)]

  well_details <- rbind(data.table(name = '',
                                   diameter_borehole = '(m)',
                                   interval_top = '(mbgs)',
                                   interval_bot = '(mbgs)',
                                   top_of_casing_m = '(masl)',
                                   depth_to_gw_m = '(m)',
                                   saturated_interval = '(m)'
  ),
  well_details)
  setnames(well_details,
           c('Well',
             'Diameter',
             'Interval top',
             'Interval bottom',
             'Top of casing',
             'Depth to water',
             'Sat. interval'
           ))




  res <- xtable(well_details,
                label = 'tab:paper-well-details',
                caption = "Well construction details. Depth to water is the mean measurement for 2016-2019.")
  align(res) <- c('l', 'l','r','r','r','r','r','r')
  res

}



tbl_syn_method_comparison <- function(results_method) {


  options(xtable.comment = FALSE)
  large <- function(x){
    x[1] <- ' '
    paste0('{\\bfseries ', x, '}')
  }

  large2 <- function(x){
    paste0('{\\bfseries ', x, '}')
  }

  small <- function(x){
    paste0('{\\small{ ', x, '}}')
  }


  dis    <- results_method[["results_dist"]]
  irr    <- results_method[["results_irr"]]
  reg    <- results_method[["results_reg"]]



  dis[, type := 'Dist.']
  irr[, type := 'Irr.']
  reg[, type := 'Reg.']

  res <- rbindlist(list(reg, irr, dis), use.names = TRUE)
  res <- res[, list(type, delta_t, n_terms, df,
                    time = as.character(signif(median, 2)),
                    memory = format(mem_alloc, digits = 1),
                    se, image_box, image_brf)]

  res[, df := n_terms + df + 1]

  res[, image_box := glue::glue("\\includegraphics[height=14px]{{{image_box}}}")]

  res[, image_brf := glue::glue("\\includegraphics[height=14px]{{{image_brf}}}")]


  addtorow <- list()
  addtorow$pos <- list(0,0, 3,3, 6,6)
  addtorow$command <- c(
    '\\multicolumn{6}{l}{} & \\boldmath$10^{-6}$ to \\boldmath$10^{-1}$  & \\textbf{\\textcolor[RGB]{130,130,130}{True} | Fit} \\\\',
    '\\hline \\multicolumn{4}{l}{\\emph{Regular lags}} \\\\',
    '\\multicolumn{4}{l}{} \\\\',
    '\\hline \\multicolumn{4}{l}{\\emph{Irregular lags}} \\\\',
    '\\multicolumn{4}{l}{} \\\\',
    '\\hline \\multicolumn{4}{l}{\\emph{Distributed lags}}  \\\\ '
  )

  res[, type := NULL]
  res[, n_terms := as.character(n_terms)]
  res[, df := as.character(df)]
  res[, se := sanitize.numbers(format(se*10, digits = 2, scientific = TRUE),
                               type = "latex", math.style.exponents = TRUE)]

  setnames(res, c('\\boldmath$\\Delta t$', 'N terms', 'N obs.', 'Time', 'Memory', 'SE (cm)', '|Residuals|', 'CRF'))

  res <- xtable(res, caption = "\\label{tab:paper-syn-method-comparison} Efficiency and accuracy of the barometric response function methods. The distributed lag model tends to be more efficient in terms of computation time and memory usage. SE refers to the residual standard deviation between the predicted and known values, and CRF is the cumulative response function.")

  align(res) <- c('l','l','r','r','r','r', rep("c", 3))

  res

}



tbl_syn_static <- function() {
  syn_comp <- readRDS('data/out_synthetic_compare.rds')

  dt <- as.data.table(sapply(syn_comp, function(x){
    round(c(x$be_ac, x$be_ls, x$be_cl[x == 3600]$value, x$be_cl[x == 86400]$value), 2)
  }))

  setnames(dt, c('Confined (0.38)', 'Storage (0.0)', 'Vadose (1.0)', 'Combined (1.0)'))
  dt <- cbind(data.table(Method = c('Simplified Acworth', 'Least Squares', 'Clark (1 hr)', 'Clark (1 day)')), dt)

  res <- xtable(dt, caption = "\\label{tab:paper-syn-static-comparison} Estimates of loading efficiency using different static methods on noise free synthetic data.  The expected value is presented in the column headings in parentheses.  Clark's method was tested using two different sampling frequencies.")
  align(res) <- c('l', 'l', 'r', 'r', 'r', 'r')
  res
}

