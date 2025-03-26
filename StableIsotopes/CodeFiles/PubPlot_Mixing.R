# Pub Plot for the MixSIAR Output

# Ponds needed:
  # Swanny -- Leech 
  # West Cannery West -- Beetle, Dragonfly, Mite
  # Wigeon -- Caddisfly, Clams, Damselfly, Dragonfly, Fly, Mite

### Setting up the workspace ####
library(MixSIAR)
library(tidyr) # For pivoting later in new output_jags
library(ggplot2)
library(GGally)
library(grid)
library(stringr)

#Create new output function
#source("SIMMS_Workshop/extra_code/output_jags_new.R") # changed this to reflect the file it is in here
source("https://raw.githubusercontent.com/andrewcparnell/simms_course/master/extra_code/output_jags_new.R")


### Swanny ####

#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/SWN/")

# Leech

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Leech_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="Swanny_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Swanny_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

SWN_Leech_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Leech", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
SWN_Leech_Plot

SWN_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

SWN_Leech_Plot <- ggplot(SWN_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "SWN - Leech", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")


### West Cannery West ####

#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/WCW/")

# 1. Beetle

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Beetle_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="WestCanneryWest_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="WestCanneryWest_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WCW_Beetle_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Amphipod", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels, can be left as is or modified
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank()  # This removes facet labels on the side
  )
WCW_Beetle_Plot

WCW_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WCW_Beetle_Plot <- ggplot(WCW_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WCW - Beetle", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

# Dragonfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Dragonfly_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="WestCanneryWest_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="WestCanneryWest_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WCW_Dragonfly_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Dragonfly", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WCW_Dragonfly_Plot

WCW_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WCW_Dragonfly_Plot <- ggplot(WCW_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WCW - Dragonfly", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

# Fly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Fly_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="WestCanneryWest_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="WestCanneryWest_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WCW_Fly_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Fly", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WCW_Fly_Plot

WCW_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WCW_Fly_Plot <- ggplot(WCW_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WCW - Fly", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

# Mite

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Mite_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="WestCanneryWest_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="WestCanneryWest_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WCW_Mite_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Mite", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WCW_Mite_Plot

WCW_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WCW_Mite_Plot <- ggplot(WCW_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WCW - Mite", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

### Wigeon ####

#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/WGN/")

# Caddisfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Caddisfly_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="Wigeon_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Wigeon_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WGN_Caddisfly_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Caddisfly", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WGN_Caddisfly_Plot

WGN_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WGN_Caddisfly_Plot <- ggplot(WGN_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WGN - Caddisfly", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")

# Clams

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Clams_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="Wigeon_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Wigeon_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WGN_Clams_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Clams", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WGN_Clams_Plot

WGN_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WGN_Clams_Plot <- ggplot(WGN_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WGN - Clams", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")


# Damselfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Damselfly_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="Wigeon_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Wigeon_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WGN_Damselfly_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Damselfly", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WGN_Damselfly_Plot

WGN_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WGN_Damselfly_Plot <- ggplot(WGN_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WGN - Damselfly", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")


# Dragonfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Dragonfly_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="Wigeon_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Wigeon_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WGN_Dragonfly_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Dragonfly", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WGN_Dragonfly_Plot

WGN_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WGN_Dragonfly_Plot <- ggplot(WGN_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WGN - Dragonfly", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")


# Fly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Fly_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="Wigeon_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Wigeon_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WGN_Fly_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Fly", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WGN_Fly_Plot

WGN_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WGN_Fly_Plot <- ggplot(WGN_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WGN - Fly", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")


# Mite

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Mite_Consumer.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pond"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# source.filename <- system.file("extdata", "wolves_sources.csv", package = "MixSIAR")
# AA: same comment as above

# Load the source data
source <- load_source_data(filename="Wigeon_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Wigeon_Discrimination.csv", mix)

## ---- eval=FALSE--------------------------------------------------------------
#  # Make an isospace plot
#  plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

## -----------------------------------------------------------------------------
# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

## ---- eval=FALSE--------------------------------------------------------------
#  # default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#  plot_prior(alpha.prior=1,source)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

## ---- eval=FALSE--------------------------------------------------------------
#  run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
jags.1 <- run_model(run="test", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  jags.1 <- run_model(run="normal", mix, source, discr, model_filename)

## ---- eval=FALSE--------------------------------------------------------------
#  output_options <- list(summary_save = TRUE,
#                         summary_name = "summary_statistics",
#                         sup_post = FALSE,
#                         plot_post_save_pdf = TRUE,
#                         plot_post_name = "posterior_density",
#                         sup_pairs = FALSE,
#                         plot_pairs_save_pdf = TRUE,
#                         plot_pairs_name = "pairs_plot",
#                         sup_xy = TRUE,
#                         plot_xy_save_pdf = FALSE,
#                         plot_xy_name = "xy_plot",
#                         gelman = TRUE,
#                         heidel = FALSE,
#                         geweke = TRUE,
#                         diag_save = TRUE,
#                         diag_name = "diagnostics",
#                         indiv_effect = FALSE,
#                         plot_post_save_png = FALSE,
#                         plot_pairs_save_png = FALSE,
#                         plot_xy_save_png = FALSE,
#                         diag_save_ggmcmc = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  output_JAGS(jags.1, mix, source, output_options)
out_stuff <- output_JAGS(jags.1, mix = mix, source = source,
                         c('summary_diagnostics',
                           'summary_statistics',
                           'summary_quantiles',
                           'plot_global',
                           'plot_factors'))

WGN_Mite_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Mite", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WGN_Mite_Plot

WGN_Elodea <- out_stuff$plot_global$data[out_stuff$plot_global$data$Source == "Elodea", ]

WGN_Mite_Plot <- ggplot(WGN_Elodea, aes(x = Proportion, fill = Source)) +
  geom_density(alpha = 0.6) +
  labs(title = "WGN - Mite", x = "Proportion", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")



combined_plot <- grid.arrange(SWN_Leech_Plot, WCW_Beetle_Plot, WCW_Dragonfly_Plot, WCW_Mite_Plot, WGN_Caddisfly_Plot, 
                                  WGN_Clams_Plot, WGN_Damselfly_Plot, WGN_Dragonfly_Plot, WGN_Fly_Plot, WGN_Mite_Plot,
                                  ncol = 2)  # Only one column, so plots stack vertically

combined_plot
# Save the combined plot
ggsave("Combined_PosteriorPlot.png", plot = combined_plot, width = 12, height = 16, dpi = 300)
