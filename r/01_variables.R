library(hydrorecipes) # for calculations
library(data.table)   # for general table manipulations
library(prettymapr)
library(earthtide)    # for synthetic Earth tides
library(patchwork)    # for plot organization
library(pdftools)     # convert pdf to png
library(viridis)      # colors for plotting
library(ggplot2)      # for plotting
library(magick)       # for figure output
library(lwgeom)       # for spatial plotting
library(raster)       # for spatial plotting
library(xtable)       # for tables
library(rticles)      # for article text format
library(scales)       # for plotting scales
library(Cairo)        # for figure output
library(bench)        # for timings
library(psd)          # for frequency analysis
library(png)          # for output
library(sf)           # for spatial operations
# variables ---------------------------------------------------------------

dbar_to_kpa <- 10
dbar_to_m <- 1.0199773339984
m_to_cm <- 100
ft_to_m <- 0.3048

# data limits
start <- as.POSIXct('2016-08-18', tz = 'UTC')
end   <- as.POSIXct('2016-10-13 12:00:00', tz = 'UTC')

theme_joh <- theme_bw() +
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(margin = margin(0.05,0.05,0.05,0.05, "cm")),
        strip.background = element_rect(fill = "#EEEEEE"))

# synthetic earthtide
latitude     <- 34.23411                           # latitude
longitude    <- -118.678                           # longitude
elevation    <- 500                                # elevation
cutoff       <- 1e-7                               # cutoff
catalog      <- 'ksm04'                            # hartmann wenzel catalog
astro_update <- 300                                # how often to update astro parameters
method       <- 'volume_strain'                    # which potential to calculate

wave_groups_dl <- as.data.table(earthtide::eterna_wavegroups)
wave_groups_dl <- na.omit(wave_groups_dl[time == '1 month'])
wave_groups_dl <- wave_groups_dl[wave_groups_dl$start > 0.5,]
ngr <- nrow(wave_groups_dl)

wave_groups <- as.data.table(earthtide::eterna_wavegroups)
wave_groups <- na.omit(wave_groups[time == 'all'])

# distributed lag
df           <- 27                                   # spline degree of freedom
max_ba_lag   <- 86400 * 1.5                          # on day max lag time
ba_knots     <- c(0:4, 5 + hydrorecipes:::log_lags_arma(15, max_ba_lag)) # knots for regression
max_syn_lag  <- 86400
n_knots_dist <- length(ba_knots)

ba_knots_baro <-  c(0:4, 5 + hydrorecipes:::log_lags_arma(5, 86400)) # knots for regression

ba_lags_irr <- c(0:4, 5 + hydrorecipes:::log_lags_arma(30, max_ba_lag)) # knots for regression

# transfer function
niter        <- 2                                    # number if reidsid iterations for psd
taper        <- 0.000001


# figure layout -----------------------------------------------------------

wl_col  <- '#4682B4'
ba_col  <- '#46b478'
et_col  <- '#b47846'
res_col <- '#b44682'
rd130col <- '#00000080'

frequency_ylab <- 'Gain (Water & Barometric Pressure)'
frequency_xlab <- 'Frequency (Cycles Per Day)'
time_xlab <- 'Time Lag (seconds)'
time_ylab <- 'Cumulative Loading Response'

default_width <- 6.7
default_width_half <- default_width/2
default_height <- 8.0
default_height_half <- default_height/1.8
default_height_third <- default_height/2.8
default_height_quarter <- default_height/3.8


