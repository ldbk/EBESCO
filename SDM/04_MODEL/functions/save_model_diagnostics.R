

save_model_diagnostics <- function(sp_scientific,
                                   sanity_by_region,
                                   residuals_by_region,
                                   dharma_by_region,
                                   AIC_by_region,
                                   randomCV_by_region, 
                                   blockedCV_by_region) {
  
  base_dir = here("05_OUTPUTS", "model_diagnostics")
  
  # Clean species name for file paths (no spaces)
  sp_safe <- gsub("[^A-Za-z0-9_]", "_", sp_scientific)
  
  # Create species-specific directory
  sp_dir <- file.path(base_dir, paste0("model_diagnostics_", sp_safe))
  if (!dir.exists(sp_dir)) dir.create(sp_dir, recursive = TRUE)
  
  saveRDS(sanity_by_region, file.path(sp_dir, "sanity_by_region.rds"))
  saveRDS(residuals_by_region, file.path(sp_dir, "residuals_by_region.rds"))
  saveRDS(dharma_by_region, file.path(sp_dir, "dharma_by_region.rds"))
  saveRDS(AIC_by_region, file.path(sp_dir, "AIC_by_region.rds"))
  saveRDS(cross_validation_by_region, file.path(sp_dir, "randomCV_by_region.rds"))
  saveRDS(cross_validation_by_region, file.path(sp_dir, "blockedCV_by_region.rds"))
  
}


