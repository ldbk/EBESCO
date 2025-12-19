
# SAVE RESULTS

# ------------------------------------------------------------------------------ #
# Build covariate label for output filenames
# ------------------------------------------------------------------------------ #

covariates_label <- c(if (year_factor_FE) "year",
                      if (depth_FE) "depth",
                      if (gear_factor_FE) "gear",
                      if (substrate_factor_FE) "substrate")

covariates_label <- paste(covariates_label, collapse = "_")



# ------------------------------------------------------------------------------ #
# Create output directory
# ------------------------------------------------------------------------------ #

new_repertory_model <- here(paste0("05_OUTPUTS/", 
                                   study_domain,"/", 
                                   common_name,"/", 
                                   response,"_", 
                                   distribution_family,"_", 
                                   covariates_label))
dir.create(new_repertory_model)


# ------------------------------------------------------------------------------ #
# Generate html report
# ------------------------------------------------------------------------------ #

rmarkdown::render(input = here("04_MODEL", "report", "report.Rmd"),
                  output_dir = new_repertory_model, 
                  output_file = paste0("report_", 
                                       common_name, "_", 
                                       response, "_", 
                                       distribution_family, "_",
                                       covariates_label, "_",
                                       format(Sys.time(), "%d-%b-%Y-%H.%M"),
                                       ".html"))



# ------------------------------------------------------------------------------ #
# Combine and save results (for models comparison)
# ------------------------------------------------------------------------------ #

DHARMa_outputs <- DHARMa::testResiduals(simulationOutput, plot = FALSE)

results <- list(
  dharma = list(obj = DHARMa_outputs,
                p.value_uniformity = DHARMa_outputs$uniformity$p.value,
                p.value_dispersion = DHARMa_outputs$dispersion$p.value,
                p.value_outliers = DHARMa_outputs$outliers$p.value),
  model = list(obj = model,
               summary = summary(model),
               sanity = sanity(model),
               tidy_fixed = tidy(model),
               tidy_ran   = tidy(model, "ran_pars")),
  predictions = list(pred_fit = pred_fit)
)

saveRDS(results, 
        file = file.path(new_repertory_model, 
                         paste0("results_", 
                                common_name, "_", 
                                response, "_", 
                                distribution_family, "_",
                                covariates_label, "_",
                                format(Sys.time(), "%d-%b-%Y-%H.%M"),
                                ".rds")))
