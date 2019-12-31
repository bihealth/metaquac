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


# Plot compound %RSD of biological samples vs qc samples
plot_rsd_versus <- function(
  data,
  target = PKG_ENV$CONCENTRATION,
  qc_type = SAMPLE_TYPE_REFERENCE_QC,
  threshold_bs = 15,
  threshold_qc = 15,
  label = "Compound",
  shape = NULL
){
  assertthat::assert_that(target %in% colnames(data))
  assertthat::assert_that(qc_type %in% data$Sample.Type)

  # Goupe data
  if (is.null(shape)) {
    data_rsd <- data %>%
      group_by(Sample.Type, Compound, Class)
  } else {
    data_rsd <- data %>%
      group_by_at(vars(Sample.Type, Compound, Class, one_of(shape)))
  }

  data_rsd <- data_rsd %>%
    # Select sample by type
    filter(Sample.Type %in% (c(SAMPLE_TYPE_BIOLOGICAL, qc_type))) %>%
    # Calculate %RSDs on grouped data
    summarise(`%RSD` = rsd(UQ(sym(target)))) %>%
    # Transform
    spread(Sample.Type, `%RSD`)

  # Point plot
  g <- ggplot(
    data = data_rsd,
    mapping = aes_string(
      x = paste0("`", SAMPLE_TYPE_BIOLOGICAL, "`"),
      y = paste0("`", qc_type, "`"),
      color = "Class"
    )
  ) +
    geom_vline(xintercept = threshold_bs, color = "red", alpha = 0.5) +
    geom_hline(yintercept = threshold_qc, color = "red", alpha = 0.5) +
    scale_shape(solid = FALSE) +
    scale_x_continuous(
      name = paste0("Compound %RSD in biological samples"),
      breaks = c(pretty(data_rsd[[SAMPLE_TYPE_BIOLOGICAL]]), threshold_bs)) +
    scale_y_continuous(
      name = paste0("Compound %RSD in QC samples (", qc_type, ")"),
      breaks = c(pretty(data_rsd[[qc_type]]), threshold_qc)) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )

  if (is.null(shape)) {
    g <- g + geom_point(alpha = 0.75, size = 4)
  } else {
    g <- g + geom_point(mapping = aes_string(shape = paste0("`", shape, "`")),
                        alpha = 0.9, size = 4)
  }

  # Add label in low density areas
  if (!is.null(label)) {
    g <- g +
      ggpmisc::stat_dens2d_filter(
        mapping = aes_string(label = paste0("`", label, "`")),
        geom = ggrepel::geom_text_repel()$geom)
  }

  g
  return(g)
}
