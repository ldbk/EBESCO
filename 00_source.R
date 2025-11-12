
##################################################################################
# SCRIPT TO RUN SPECIES DISTRIBUTION MODELS
##################################################################################

rm(list=ls())

# -------------------------------------------------------------------------------
# LOAD PACKAGES
# -------------------------------------------------------------------------------

library(here)
source(here('03_MODEL/packages/packages.R'))


# ------------------------------------------------------------------------------
# PARAMETERS
# ------------------------------------------------------------------------------

# Define species
sp_scientific = "Solea solea" 


# ------------------------------------------------------------------------------
# LOAD AND TRANSFORM DATA
# ------------------------------------------------------------------------------

source(here('02_TRANSFORM_DATA/transform_data_CGFS_catch_operation.R'))













source(here('03_MODEL/functions/create_domain.R'))



# ------------------------------------------------------------------------------
# PLOT DATA
# ------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# MODEL VERSION 
# -------------------------------------------------------------------------------


# -------------------------------------------------------------------------------
# CREATE NEW REPERTORY FOR SAVING INPUTS AND OUTPUTS
# -------------------------------------------------------------------------------

new_repertory_model <- here(paste0("04_outputs/",model_version,"/", format(Sys.time(), "%d-%b-%Y-%H.%M")))
dir.create(new_repertory_model)


# -------------------------------------------------------------------------------
# LOAD THE DATA AUTOMATICALLY BASED ON MODEL VERSION
# -------------------------------------------------------------------------------

source(here(paste0("02-TRANSFORM-DATA/transform-data_", model_version, ".R")))

# Save the inputs 
save(Data, file = paste0(new_repertory_model, "/", "inputs_data.RData"))


# -------------------------------------------------------------------------------
# PLOT INPUT DATA 
# -------------------------------------------------------------------------------

# Visualize and check input data : saves plots as a pdf in "new_repertory_model"
if (!(model_version %in% c("M0_Nourdem", "M0_length"))) {
  source(here("03_models/r_functions", "plot_input_data_age.R"))
}

# -------------------------------------------------------------------------------
# LOAD PARAMETERS
# -------------------------------------------------------------------------------

source(here('03_models/r_functions/pars_tmb.R'))

# Save the initial parameters 
save(pars, file = paste0(new_repertory_model, "/", "inputs_parameters.RData"))


# -------------------------------------------------------------------------------
# RUN MODEL
# -------------------------------------------------------------------------------

source(here('03_models/r_functions/run.R'))


# -------------------------------------------------------------------------------
# PLOT OUTPUTS 
# ------------------------------------------------------------------------------
if (!(model_version %in% c("M0_Nourdem", "M0_length"))) {
  source(here("03_models/r_functions", "plot_outputs_age.R"))
}