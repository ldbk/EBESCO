
# ------------------------------------------------------------------------------#
####  SAVE OUTPUTS ####  
# ------------------------------------------------------------------------------#

sp_name_safe <- gsub("[^A-Za-z0-9_]", "_", sp_scientific)

species_output_dir <- here("05_OUTPUTS", sp_name_safe)
if (!dir.exists(species_output_dir)) dir.create(species_output_dir, recursive = TRUE)
  
# Create new directory with date, hour
new_repertory_model <- here(paste0("05_OUTPUTS/", sp_name_safe, "/", 
                                     sp_name_safe, "_", format(Sys.time(), "%d-%b-%Y-%H.%M")))
dir.create(new_repertory_model)

saveRDS(sanity_by_region, file.path(new_repertory_model, paste0(sp_name_safe, "_", "sanity_by_region.rds")))
saveRDS(converged_models, file.path(new_repertory_model, paste0(sp_name_safe, "_", "converged_models.rds")))
saveRDS(params_all, file.path(new_repertory_model, paste0(sp_name_safe, "_", "parameter_estimates.rds")))
saveRDS(residuals_by_region, file.path(new_repertory_model, paste0(sp_name_safe, "_", "residuals_by_region.rds")))
saveRDS(simulation_errors_by_region, file.path(new_repertory_model, paste0(sp_name_safe, "_", "simulation_errors_by_region.rds")))
saveRDS(AIC_by_region, file.path(new_repertory_model, paste0(sp_name_safe, "_", "AIC_by_region.rds")))
saveRDS(randomCV_by_region, file.path(new_repertory_model, paste0(sp_name_safe, "_", "randomCV_by_region.rds")))
saveRDS(blockedCV_by_region, file.path(new_repertory_model, paste0(sp_name_safe, "_", "blockedCV_by_region.rds")))
saveRDS(converged_models_predictions, file.path(new_repertory_model, paste0(sp_name_safe, "_", "converged_models_predictions.rds")))
saveRDS(prediction_maps, file.path(new_repertory_model, paste0(sp_name_safe, "_", "prediction_maps.rds")))
saveRDS(sim_cv_all, file.path(new_repertory_model, paste0(sp_name_safe, "_", "coefficient_variation_predictions.rds")))
saveRDS(plots_cv_all, file.path(new_repertory_model, paste0(sp_name_safe, "_", "coefficient_variation_maps.rds")))




