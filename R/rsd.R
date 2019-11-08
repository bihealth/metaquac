# Different plots to visualize %RSD
# Author: Mathias Kuhring


# Calculate %RSD for a vector
rsd <- function(x, na.rm = TRUE) {
  return(sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm) * 100)
}


# Calculate and point-plot %RSD of targets (typically concentration, height or
# area) per groups based on grouping1 (typically compounds) and optionally
# grouping2 (another variable of interest). Filter your data set to contain only
# replicates of the same samples with expected target values to be similar.
# Default are set for Biocrates/MetIDQ data.
plot_rsd_points <- function(data,
                            target = PKG_ENV$CONCENTRATION,
                            grouping1 = "Compound",
                            grouping2 = NULL,
                            title = NULL){

  # Group data
  data_grouped <- data %>% group_by(UQ(sym(grouping1)))
  if (!is.null(grouping2)){
    data_grouped <- data_grouped %>% group_by(UQ(sym(grouping2)), add = TRUE)
  }

  # Calculate %RSDs on grouped data
  rsd_tab <- data_grouped %>% summarise(`%RSD` = rsd(UQ(sym(target))))

  # Point plot
  g <- ggplot()

  if (is.null(grouping2)){
    g <- g +
      geom_point(data = rsd_tab,
                 mapping = aes_string(x = "%RSD",
                                      y = paste0("`", grouping1, "`")),
                 stat = "identity")
  } else {
    g <- g +
      geom_point(data = rsd_tab,
                 mapping = aes_string(x = "%RSD",
                                      y = paste0("`", grouping1, "`"),
                                      color = paste0("`", grouping2, "`"),
                                      shape = paste0("`", grouping2, "`")),
                 stat = "identity")
  }

  if (!is.null(title)){
    g <- g + labs(title = title)
  }

  g <- g +
    scale_x_continuous(breaks = c(pretty(rsd_tab$`%RSD`), RSD_LIMITS, 0.05, 0.10)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position = "bottom") +
    geom_rect(aes(
      xmin = RSD_LIMITS[1], xmax = RSD_LIMITS[2],
      ymin = -Inf, ymax = Inf),
      alpha = 0.1, fill = COLORS_TRAFFIC[1]) +
    geom_rect(aes(
      xmin = RSD_LIMITS[2], xmax = RSD_LIMITS[3],
      ymin = -Inf, ymax = Inf),
      alpha = 0.1, fill = COLORS_TRAFFIC[2]) +
    geom_rect(aes(
      xmin = RSD_LIMITS[3], xmax = Inf,
      ymin = -Inf, ymax = Inf),
      alpha = 0.1, fill = COLORS_TRAFFIC[3])

  print(g)
}

