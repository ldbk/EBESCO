


compute_residuals_Moran <- function(models_converged) {
  
  purrr::map_dfr(models_converged, function(current_converged_model) {

    fit <- current_converged_model$fit
    computation_time <- current_converged_model$elapsed_fit
    cutoff <- current_converged_model$cutoff
    n_vertices <- current_converged_model$n_vertices
    barrier <- current_converged_model$barrier
    boundary <- current_converged_model$boundary
    region <- current_converged_model$region
    
    data_mod <- fit$data

    set.seed(123)
    
    residuals <- tryCatch(residuals(fit, type = "mle-mvn"), error = function(e) NULL)
    
    df_res <- data_mod %>%
      dplyr::mutate(residual = as.numeric(residuals)) %>%
      dplyr::select(year, lat, lon, residual) 
    
    resultats_moran <- df_res %>%
      dplyr::group_by(year) %>%
      dplyr::filter(!all(residual == 0)) %>%
      dplyr::group_modify(~{
        dists <- as.matrix(dist(.x[, c("lat", "lon")]))
        inv_dists <- 1 / dists
        diag(inv_dists) <- 0
        inv_dists[is.infinite(inv_dists)] <- 0
        moran <- tryCatch(ape::Moran.I(.x$residual, inv_dists, scaled = TRUE),
                          error = function(e) NULL)
        data.frame(moran_observed = moran$observed,
                   moran_expected = moran$expected,
                   moran_sd = moran$sd,
                   moran_p.value = moran$p.value)
      }) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ymin = moran_expected - moran_sd,
                    ymax = moran_expected + moran_sd,
                    ymin95 = moran_expected - 1.96 * moran_sd,
                    ymax95 = moran_expected + 1.96 * moran_sd,
                    signif = dplyr::case_when(moran_p.value < 0.001 ~ "***",
                                              moran_p.value < 0.01 ~ "**",
                                              moran_p.value < 0.05 ~ "*",
                                              TRUE ~ "ns"),
                    cutoff = cutoff,
                    n_vertices = n_vertices,
                    barrier = barrier,
                    boundary = boundary,
                    region = region)
    
    return(resultats_moran)
  })
}
  
  
  