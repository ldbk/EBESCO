

region_validity <- readRDS(here("01_DATA/species_region_validity.rds"))
species_list <- sort(unique(region_validity$scientificName))
species_list <- species_list[1:8]
source(here("04_MODEL/functions/compute_plot_Moran.R"))
source(here("04_MODEL/functions/compute_plot_residuals_withoutRF.R"))

out_path <- here("05_OUTPUTS/Moran_all_species2.pdf")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
pdf(out_path, onefile = TRUE, width = 11, height = 7)

for (sp in species_list) {
  
  sp_scientific <- sp
  validity_sp <- dplyr::filter(region_validity, scientificName == sp_scientific)
  
  West_English_Channel <- isTRUE(validity_sp$west[1])
  East_English_Channel <- isTRUE(validity_sp$east[1])
  
  source(here("03_TRANSFORM_DATA/transform_data.R"))
  
  if (West_English_Channel) {
    p_Moran_west <- tryCatch(compute_plot_Moran(data_CGFS_west, region = "west"),
                       error = function(e) { message("SKIP west ", sp_scientific, " : ", e$message); NULL })
    p_resids_west <- tryCatch(residuals_withoutRF(data_CGFS_west, region = "west"),
                             error = function(e) { message("SKIP west ", sp_scientific, " : ", e$message); NULL })
    west_plots <- Filter(Negate(is.null), list(p_Moran_west, p_resids_west))
    if (length(west_plots)) print(patchwork::wrap_plots(west_plots, ncol = 2, widths = c(1, 2)))
  }

  if (East_English_Channel) {
    p_Moran_east <- tryCatch(compute_plot_Moran(data_CGFS_east, region = "east"),
                             error = function(e) { message("SKIP east ", sp_scientific, " : ", e$message); NULL })
    p_resids_east <- tryCatch(residuals_withoutRF(data_CGFS_east, region = "east"),
                              error = function(e) { message("SKIP east ", sp_scientific, " : ", e$message); NULL })
    east_plots <- Filter(Negate(is.null), list(p_Moran_east, p_resids_east))
    if (length(east_plots)) print(patchwork::wrap_plots(east_plots, ncol = 2, widths = c(1, 2)))
  }
}

dev.off()
