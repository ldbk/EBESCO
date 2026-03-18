


compute_plot_Moran <- function(data_CGFS, region) {
  
  resultats_moran <- data_CGFS %>%
    dplyr::group_by(year) %>%
    dplyr::filter(!all(densityKgKm2 == 0)) |>
    dplyr::group_modify(~{
      # compute euclidian distances between all points per year 
      dists <- as.matrix(dist(.x[, c("lat", "lon")]))
      inv_dists <- 1 / dists
      diag(inv_dists) <- 0
      moran <- ape::Moran.I(.x$densityKgKm2, inv_dists, scaled = TRUE)
      data.frame(observed = moran$observed,
                 expected = moran$expected,
                 sd = moran$sd,
                 p_value = moran$p.value)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ymin = expected - sd,
           ymax = expected + sd,
           ymin95 = expected - 1.96 * sd,
           ymax95 = expected + 1.96 * sd,
           signif = case_when(p_value < 0.001 ~ "***",
                              p_value < 0.01 ~ "**", 
                              p_value < 0.05 ~ "*",
                              TRUE ~ "ns"))
  
  
  # plot_Moran <- ggplot(resultats_moran, aes(x = year)) +
  #   geom_errorbar(aes(ymin = ymin95, ymax = ymax95), width = 0.2) +
  #   geom_point(aes(y = observed), size = 3) +
  #   geom_line(aes(y = observed, group = 1)) +
  #   geom_point(aes(y = expected), shape = 1, size = 3) +
  #   geom_text(aes(y = ymax, label = signif), vjust = -0.3, size = 5) +
  #   theme_minimal()+
  #   labs(  title = bquote(italic(.(sp_scientific)) ~ " - " ~ .(region)),
  #        subtitle = "*** p<0.001, ** p<0.01, *p<0.05, white point : expected value without spatial autocorrelation",
  #        y = "Moran's index")
  
  
  ggplot2::ggplot(resultats_moran, aes(x = year)) +
    ggplot2::geom_errorbar(aes(ymin = ymin95, ymax = ymax95), width = 0.2, color = "darkred") +
    ggplot2::geom_point(aes(y = expected), shape = 1, size = 3, color = "darkred") +
    ggplot2::geom_point(aes(y = observed), size = 3, color = "darkblue") +
    ggplot2::geom_text(aes(y = max(ymax95) + 0.05, label = signif), nudge_y = 0.02, size = 5) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = bquote(italic(.(sp_scientific)) ~ " - " ~ .(region)),
                  subtitle = "*** p<0.001, ** p<0.01, *p<0.05, \nwhite point : expected value without SA",
                  y = "Moran's index")
  
  
}


# if (isTRUE(West_English_Channel)) {
#   plot_Moran_west <- compute_plot_Moran(data_CGFS_west, region = "west")
# }
# 
# if (isTRUE(East_English_Channel)) {
#   plot_Moran_east <- compute_plot_Moran(data_CGFS_east, region = "east")
# }


