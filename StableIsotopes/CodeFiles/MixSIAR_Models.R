# MixSIAR Analysis December 2024

#Setting up the workspace
library(MixSIAR)
library(tidyr) # For pivoting later in new output_jags
library(ggplot2)
library(GGally)
library(grid)
library(stringr)

#Create new output function
#source("SIMMS_Workshop/extra_code/output_jags_new.R") # changed this to reflect the file it is in here
source("https://raw.githubusercontent.com/andrewcparnell/simms_course/master/extra_code/output_jags_new.R")


#AA: Need to fit a different model for each lake because there are different sources/#sources per pond/lake
  
#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/EYS/")

# EYS ####
# 1. Beetles

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="EyakSouth_Beetle_Consumer.csv", 
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
source <- load_source_data(filename="EyakSouth_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="EyakSouth_Discrimination.csv", mix)

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

EYS_Beetle_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Beetle", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels, can be left as is or modified
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank()  # This removes facet labels on the side
  )

EYS_Beetle_Plot



# 2. Caddisfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="EyakSouth_Caddisfly_Consumer.csv", 
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
source <- load_source_data(filename="EyakSouth_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="EyakSouth_Discrimination.csv", mix)

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

EYS_Caddisfly_Plot <- out_stuff$plot_factors[[1]] + 
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
EYS_Caddisfly_Plot

# 3. Damselfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="EyakSouth_Damselfly_Consumer.csv", 
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
source <- load_source_data(filename="EyakSouth_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="EyakSouth_Discrimination.csv", mix)

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

EYS_Damselfly_Plot <- out_stuff$plot_factors[[1]] + 
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
EYS_Damselfly_Plot

# 4. Dragonfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="EyakSouth_Dragonfly_Consumer.csv", 
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
source <- load_source_data(filename="EyakSouth_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="EyakSouth_Discrimination.csv", mix)

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

EYS_Dragonfly_Plot <- out_stuff$plot_factors[[1]] + 
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
EYS_Dragonfly_Plot

# 5. Fly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="EyakSouth_Fly_Consumer.csv", 
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
source <- load_source_data(filename="EyakSouth_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="EyakSouth_Discrimination.csv", mix)

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

EYS_Fly_Plot <- out_stuff$plot_factors[[1]] + 
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
EYS_Fly_Plot

# 6. Mite

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="EyakSouth_Mite_Consumer.csv", 
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
source <- load_source_data(filename="EyakSouth_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="EyakSouth_Discrimination.csv", mix)

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

EYS_Mite_Plot <- out_stuff$plot_factors[[1]] + 
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
EYS_Mite_Plot

# 7. Water Boatman

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="EyakSouth_WaterBoatman_Consumer.csv", 
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
source <- load_source_data(filename="EyakSouth_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="EyakSouth_Discrimination.csv", mix)

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

EYS_WaterBoatman_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Water Boatman", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_text(
      angle = 45,          # Angles the X-axis tick labels at 45 degrees
      hjust = 1         # Centers the labels horizontally
    ),
    axis.text.y = element_text(),   # Controls the Y-axis tick labels
    axis.title.x = element_text(),  # Keeps the X-axis title
    axis.title.y = element_text(),  # Keeps the Y-axis label
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
EYS_WaterBoatman_Plot

#Creating one plot for all of EYS

library(gridExtra)

# Combine plots into a grid
EYS_PosteriorPlot <- grid.arrange(EYS_Beetle_Plot, EYS_Caddisfly_Plot, EYS_Damselfly_Plot, EYS_Dragonfly_Plot,
             EYS_Fly_Plot, EYS_Fly_Plot, EYS_Mite_Plot, EYS_WaterBoatman_Plot,
             ncol = 1)  # Only one column, so plots stack vertically
EYS_PosteriorPlot

ggsave("EYS_Posterior.png", plot = EYS_PosteriorPlot, width = 8.5, height = 11, dpi = 300)


# MixSIAR Analysis December 2024

#Setting up the workspace
library(MixSIAR)
library(tidyr) # For pivoting later in new output_jags
library(ggplot2)
library(GGally)
library(grid)
library(stringr)

#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/EYS/")

#Create new output function
#source("SIMMS_Workshop/extra_code/output_jags_new.R") # changed this to reflect the file it is in here
source("https://raw.githubusercontent.com/andrewcparnell/simms_course/master/extra_code/output_jags_new.R")

# PCH ####
#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/PCH/")


# 1. Amphipod

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="ThePinch_Amphipod_Consumer.csv", 
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
source <- load_source_data(filename="ThePinch_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="ThePinch_Discrimination.csv", mix)

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

PCH_Amphipod_Plot <- out_stuff$plot_factors[[1]] + 
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

PCH_Amphipod_Plot



# 2. Beetle

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="ThePinch_Beetle_Consumer.csv", 
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
source <- load_source_data(filename="ThePinch_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="ThePinch_Discrimination.csv", mix)

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

PCH_Beetle_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Beetle", width = 15))+
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
PCH_Beetle_Plot

# 3. Damselfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="ThePinch_Damselfly_Consumer.csv", 
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
source <- load_source_data(filename="ThePinch_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="ThePinch_Discrimination.csv", mix)

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

PCH_Damselfly_Plot <- out_stuff$plot_factors[[1]] + 
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
PCH_Damselfly_Plot

# 4. Dragonfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="ThePinch_Dragonfly_Consumer.csv", 
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
source <- load_source_data(filename="ThePinch_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="ThePinch_Discrimination.csv", mix)

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

PCH_Dragonfly_Plot <- out_stuff$plot_factors[[1]] + 
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
PCH_Dragonfly_Plot

# 5. Fly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="ThePinch_Fly_Consumer.csv", 
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
source <- load_source_data(filename="ThePinch_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="ThePinch_Discrimination.csv", mix)

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

PCH_Fly_Plot <- out_stuff$plot_factors[[1]] + 
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
PCH_Fly_Plot

# 6. Water Boatman

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="ThePinch_WaterBoatman_Consumer.csv", 
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
source <- load_source_data(filename="ThePinch_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="ThePinch_Discrimination.csv", mix)

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

PCH_WaterBoatman_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Water Boatman", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_text(
      angle = 45,          # Angles the X-axis tick labels at 45 degrees
      hjust = 1         # Centers the labels horizontally
    ),
    axis.text.y = element_text(),   # Controls the Y-axis tick labels
    axis.title.x = element_text(),  # Keeps the X-axis title
    axis.title.y = element_text(),  # Keeps the Y-axis label
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
PCH_WaterBoatman_Plot

#Creating one plot for all of PCH

library(gridExtra)

# Combine plots into a grid
PCH_PosteriorPlot <- grid.arrange(PCH_Amphipod_Plot, PCH_Beetle_Plot, PCH_Damselfly_Plot, PCH_Dragonfly_Plot,
                                  PCH_Fly_Plot,PCH_WaterBoatman_Plot,
                                  ncol = 1)  # Only one column, so plots stack vertically
PCH_PosteriorPlot

ggsave("PCH_Posterior.png", plot = PCH_PosteriorPlot, width = 8.5, height = 11, dpi = 300)

# STB ####
#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/STB/")

# 3. Damselfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Strawberry_Damselfly_Consumer.csv", 
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
source <- load_source_data(filename="Strawberry_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Strawberry_Discrimination.csv", mix)

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

STB_Damselfly_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Damselfly", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels, can be left as is or modified
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank()  # This removes facet labels on the side
  )
STB_Damselfly_Plot

# 4. Dragonfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Strawberry_Dragonfly_Consumer.csv", 
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
source <- load_source_data(filename="Strawberry_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Strawberry_Discrimination.csv", mix)

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

STB_Dragonfly_Plot <- out_stuff$plot_factors[[1]] + 
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
STB_Dragonfly_Plot

# 5. Mite

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Strawberry_Mite_Consumer.csv", 
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
source <- load_source_data(filename="Strawberry_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Strawberry_Discrimination.csv", mix)

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

STB_Mite_Plot <- out_stuff$plot_factors[[1]] + 
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
STB_Mite_Plot

# 6. Water Boatman

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Strawberry_WaterBoatman_Consumer.csv", 
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
source <- load_source_data(filename="Strawberry_Sources.csv",
                           source_factors="Pond", 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# AA: same comment as the above two sections

# Load the discrimination/TDF data
discr <- load_discr_data(filename="Strawberry_Discrimination.csv", mix)

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

STB_WaterBoatman_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Water Boatman", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_text(
      angle = 45,          # Angles the X-axis tick labels at 45 degrees
      hjust = 1         # Centers the labels horizontally
    ),
    axis.text.y = element_text(),   # Controls the Y-axis tick labels
    axis.title.x = element_text(),  # Keeps the X-axis title
    axis.title.y = element_text(),  # Keeps the Y-axis label
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
STB_WaterBoatman_Plot

#Creating one plot for all of STB

library(gridExtra)

# Combine plots into a grid
STB_PosteriorPlot <- grid.arrange(STB_Damselfly_Plot, STB_Dragonfly_Plot, STB_Mite_Plot, STB_WaterBoatman_Plot,
                                  ncol = 1)  # Only one column, so plots stack vertically
STB_PosteriorPlot

ggsave("STB_Posterior.png", plot = STB_PosteriorPlot, width = 8.5, height = 11, dpi = 300)

# SWN ####
#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/SWN/")

# 1. Amphipod

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Amphipod_Consumer.csv", 
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

SWN_Amphipod_Plot <- out_stuff$plot_factors[[1]] + 
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
SWN_Amphipod_Plot

# 2. Beetle

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Beetle_Consumer.csv", 
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

SWN_Beetle_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Beetle", width = 15))+
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
SWN_Beetle_Plot

# 3. Caddisfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Caddisfly_Consumer.csv", 
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

SWN_Caddisfly_Plot <- out_stuff$plot_factors[[1]] + 
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
SWN_Caddisfly_Plot


# 4. Clams

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Clams_Consumer.csv", 
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

SWN_Clams_Plot <- out_stuff$plot_factors[[1]] + 
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
SWN_Clams_Plot

# 5. Damselfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Damselfly_Consumer.csv", 
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

SWN_Damselfly_Plot <- out_stuff$plot_factors[[1]] + 
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
SWN_Damselfly_Plot

# 6. Dragonfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Dragonfly_Consumer.csv", 
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

SWN_Dragonfly_Plot <- out_stuff$plot_factors[[1]] + 
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
SWN_Dragonfly_Plot

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

# Mite

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Mite_Consumer.csv", 
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

SWN_Mite_Plot <- out_stuff$plot_factors[[1]] + 
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
SWN_Mite_Plot

# Snail

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Swanny_Snail_Consumer.csv", 
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

SWN_Snail_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Snail", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_text(
      angle = 45,          # Angles the X-axis tick labels at 45 degrees
      hjust = 1         # Centers the labels horizontally
    ),
    axis.text.y = element_text(),   # Controls the Y-axis tick labels
    axis.title.x = element_text(),  # Keeps the X-axis title
    axis.title.y = element_text(),  # Keeps the Y-axis label
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
SWN_Snail_Plot

#Creating one plot for all of SWN

library(gridExtra)

# Combine plots into a grid
SWN_PosteriorPlot <- grid.arrange(SWN_Amphipod_Plot, SWN_Beetle_Plot, SWN_Caddisfly_Plot, SWN_Clams_Plot,
                                  SWN_Damselfly_Plot, SWN_Dragonfly_Plot, SWN_Leech_Plot, SWN_Mite_Plot, SWN_Snail_Plot,
                                  ncol = 1)  # Only one column, so plots stack vertically
SWN_PosteriorPlot

ggsave("SWN_Posterior.png", plot = SWN_PosteriorPlot, width = 8.5, height = 11, dpi = 300)


# WCW ####
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

WCW_Amphipod_Plot <- out_stuff$plot_factors[[1]] + 
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
WCW_Amphipod_Plot

# Caddisfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Caddisfly_Consumer.csv", 
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

WCW_Caddisfly_Plot <- out_stuff$plot_factors[[1]] + 
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
WCW_Caddisfly_Plot

# CLams

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Clams_Consumer.csv", 
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

WCW_Clams_Plot <- out_stuff$plot_factors[[1]] + 
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
WCW_Clams_Plot

# Damselfly

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Damselfly_Consumer.csv", 
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

WCW_Damselfly_Plot <- out_stuff$plot_factors[[1]] + 
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
WCW_Damselfly_Plot

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
WCW_Fly_Plot


# Leech

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Leech_Consumer.csv", 
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

WCW_Leech_Plot <- out_stuff$plot_factors[[1]] + 
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
WCW_Leech_Plot

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

# Water Boatman

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="WestCanneryWest_Waterboatman_Consumer.csv", 
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

WCW_WaterBoatman_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Water Boatman", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_text(
      angle = 45,          # Angles the X-axis tick labels at 45 degrees
      hjust = 1         # Centers the labels horizontally
    ),
    axis.text.y = element_text(),   # Controls the Y-axis tick labels
    axis.title.x = element_text(),  # Keeps the X-axis title
    axis.title.y = element_text(),  # Keeps the Y-axis label
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WCW_WaterBoatman_Plot

#Creating one plot for all of WGN

library(gridExtra)

# Combine plots into a grid
WCW_PosteriorPlot <- grid.arrange(WCW_Amphipod_Plot, WCW_Caddisfly_Plot, WCW_Clams_Plot, WCW_Damselfly_Plot,
                                WCW_Dragonfly_Plot, WCW_Fly_Plot, WCW_Leech_Plot, WCW_Mite_Plot, WCW_WaterBoatman_Plot,
                                  ncol = 1)  # Only one column, so plots stack vertically
WCW_PosteriorPlot

ggsave("WCW_Posterior.png", plot = WCW_PosteriorPlot, width = 8.5, height = 11, dpi = 300)

# WGN ####
#Set working directory
setwd("/Users/amaryllisadey/Library/CloudStorage/Dropbox/SWEL/CopperRiverDelta/Elodea/SI_Ponds/DataAnalysis/ElodeaStableIsotopes/Analysis_Fall2024/DataFiles/MixSIAR/WGN/")

# Beetle

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Beetle_Consumer.csv", 
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

WGN_Beetle_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Beetle", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_text(),  # Controls the tick labels, can be left as is or modified
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank()  # This removes facet labels on the side
  )
WGN_Beetle_Plot

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

# Water Damselfly

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

# Leech

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Leech_Consumer.csv", 
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

WGN_Leech_Plot <- out_stuff$plot_factors[[1]] + 
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
WGN_Leech_Plot

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

# Snail

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Snail_Consumer.csv", 
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

WGN_Snail_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Snail", width = 15))+
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
WGN_Snail_Plot

# Water Boatman

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Waterboatman_Consumer.csv", 
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

WGN_WaterBoatman_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Water Boatman", width = 15))+
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
WGN_WaterBoatman_Plot

# Water Boatman

## -----------------------------------------------------------------------------
# Replace the system.file call with the path to your file
# mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
# AA: skip this section as it is not relavent when using your own data

# Load the mixture/consumer data
mix <- load_mix_data(filename="Wigeon_Waterboatman_Consumer.csv", 
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

WGN_WaterBoatman_Plot <- out_stuff$plot_factors[[1]] + 
  theme_classic() + 
  ylab(str_wrap("Denstiy - Water Boatman", width = 15))+
  theme(
    legend.position = "none", 
    axis.text.x = element_text(
      angle = 45,          # Angles the X-axis tick labels at 45 degrees
      hjust = 1         # Centers the labels horizontally
    ),
    axis.text.y = element_text(),   # Controls the Y-axis tick labels
    axis.title.x = element_text(),  # Keeps the X-axis title
    axis.title.y = element_text(),  # Keeps the Y-axis label
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text.y = element_blank(),  # Removes facet labels on the side
    strip.text.x = element_blank()   # Removes facet labels on top
  )
WGN_WaterBoatman_Plot

#Creating one plot for all of WGN

library(gridExtra)

# Combine plots into a grid
WGN_PosteriorPlot <- grid.arrange(WGN_Beetle_Plot, WGN_Caddisfly_Plot, WGN_Clams_Plot, WGN_Damselfly_Plot, WGN_Dragonfly_Plot, 
                                  WGN_Fly_Plot, WGN_Leech_Plot, WGN_Mite_Plot, WGN_Clams_Plot,
                                  ncol = 1)  # Only one column, so plots stack vertically
WGN_PosteriorPlot

ggsave("WGN_Posterior.png", plot = WGN_PosteriorPlot, width = 8.5, height = 11, dpi = 300)


