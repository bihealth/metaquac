# PCA over compounds per sample
# Author: Mathias Kuhring


# Sample PCA based on compound values (typically concentration)
#' @import ggfortify
plot_pca <- function(data,
                     target = ENV$CONCENTRATION,
                     colour = "Sample.Type",
                     shape = NULL,
                     label = NULL,
                     label_number = 20,
                     loadings = FALSE){

  # Select relevant columns
  data_long <- data %>% select(one_of(
    c("Sample.Name", "Compound", target, colour, shape, label)))

  # Spread compounds
  compounds <- compounds <- unique(as.character(data_long$Compound))
  data_wide <- spread(data_long, key = Compound, value = UQ(sym(target)))

  # Create matrix for PCA
  date_wide_targets_only <- subset(data_wide, select = compounds)

  # Ensure numeric values
  date_wide_targets_only <- mutate_all(date_wide_targets_only, function(x) as.numeric(x))

  # Calculate PCA
  pca <- prcomp(date_wide_targets_only, scale. = TRUE)

  if (loadings){
    # Plot PCA with loadings
    g <- ggplot2::autoplot( # needs ggfortify autoplot.pca_common
      pca, data = data_wide, colour = colour, shape = shape,
      size = 4, alpha = 0.5, title = title,
      loadings = TRUE, loadings.colour = 'blue',
      loadings.label = TRUE, loadings.label.size = 5)
  } else {
    # Plot PCA without loadings
    g <- ggplot2::autoplot( # needs ggfortify autoplot.pca_common
      pca, data = data_wide, colour = colour, shape = shape,
      size = 4, alpha = 0.5, title = title)
  }

  # Add label in low density areas
  if (!is.null(label)){
    g <- g +
      ggpmisc::stat_dens2d_filter(
        mapping = aes_string(label = paste0("`", label, "`"),
                             color = paste0("`", colour, "`")),
        geom = ggrepel::geom_text_repel()$geom, keep.number = label_number)
  }

  # Add title
  g <- g +
    labs(title = paste("PCA on", target)) +
    theme(plot.title = element_text(hjust = 0.5))

  # Can't be used yet, since PCA is not calculated separately
  # if (!is.null(facet)){
  #   g <- g + facet_wrap(as.formula(paste("~", facet)))
  # }

  return(g)
}
