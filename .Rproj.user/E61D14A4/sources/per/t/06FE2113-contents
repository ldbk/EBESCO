
# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#

# SCRIPT TO RUN SPECIES DISTRIBUTION MODELS

# ------------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#


rm(list=ls())

# ------------------------------------------------------------------------------#
####  LOAD PACKAGES  #### 
# ------------------------------------------------------------------------------#

library(here)
source(here('04_MODEL/packages/packages.R'))


# ------------------------------------------------------------------------------#
####  DEFINE PARAMETERS  #### 
# ------------------------------------------------------------------------------#

## Define species
# ----------------#
sp_scientific = "Solea solea"
# sp_scientific = "Dicentrarchus labrax"

common_name = "sole"
# common_name = "seabass"

## Define domain
# ----------------#
# study_domain = "Eastern_English_Channel"
study_domain = "Entire_English_Channel"


# ------------------------------------------------------------------------------#
####  LOAD AND TRANSFORM DATA  #### 
# ------------------------------------------------------------------------------#

source(here('03_TRANSFORM_DATA/transform_data.R'))


# ------------------------------------------------------------------------------#
####  CREATE THE MESH  #### 
# ------------------------------------------------------------------------------#

# Cutoff defines the minimum allowed distance between mesh vertices 
cutoff = 10

source(here('04_MODEL/functions/create_mesh.R'))


# ------------------------------------------------------------------------------#
####  RUN MODEL  #### 
# ------------------------------------------------------------------------------#

# Select the model version 
# --------------------------#

# model_version <- "m1_presence_year"
# model_version <- "m2_density_year"
# model_version <- "m3_presence_year_depth"
model_version <- "m4_density_year_depth"

source(here(paste0("04_MODEL/models_versions/models.R")))


# ------------------------------------------------------------------------------#
####  CHECK OUTPUTS  #### 
# ------------------------------------------------------------------------------#

summary(model)

sanity(model)

# Fixed effects with confidence intervals
tidy(model)

# Random effects and variance parameters
tidy(model, "ran_pars")


# ------------------------------------------------------------------------------#
####  DESIGN THE PREDICTION GRID  #### 
# ------------------------------------------------------------------------------#

# grid resolution in km
res_km = 6

source(here('04_MODEL/functions/design_grid.R'))


# ------------------------------------------------------------------------------#
####  PREDICT ON THE GRID #### 
# ------------------------------------------------------------------------------#

source(here("04_MODEL/functions/predict_on_grid.R"))


# -------------------------------------------------------------------------------
####  CREATE NEW REPERTORY FOR SAVING OUTPUTS ####  
# -------------------------------------------------------------------------------

new_repertory_model <- here(paste0("05_OUTPUTS/", study_domain,"/", common_name,"/", model_version,"/", 
                                   format(Sys.time(), "%d-%b-%Y-%H.%M")))
dir.create(new_repertory_model)


rmarkdown::render(input = here("04_MODEL", "report", "report.Rmd"),
                  output_dir = new_repertory_model, 
                  output_file = paste0("report_", common_name, "_", model_version, ".html"))



# ------------------------------------------------------------------------------#
####  GET INDEX  (abundance, density) #### 
# ------------------------------------------------------------------------------#

# DONE ALSO IN REPORT
# pred_fit_index <- predict(model, newdata = grid_pred,
#                           return_tmb_object = TRUE)
# 
# index <- get_index(pred_fit_index, 
#                    bias_correct = TRUE, 
#                    level = 0.95, 
#                    area = (res_km*res_km),  # area of a projection grid cell
#                    silent = TRUE)
# 
# 
# ggplot(index, aes(x = factor(year), y = est)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
#   labs(x = "Year", y = "Density index", title = "Density index") +
#   theme_minimal()
