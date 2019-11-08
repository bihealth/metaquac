# Compound concentration profiles visualizations
# Author: Mathias Kuhring

# Concentration profiles via bars for all samples separately
plot_sample_profiles_single <- function(
  bcdata = biocrates,
  target = PKG_ENV$CONCENTRATION,
  sample_types = c(SAMPLE_TYPE_BIOLOGICAL),
  facet_cols = 2
){
  assert_that(all(c(target, "Sample.Type") %in% names(bcdata)))
  assert_that(all(sample_types %in% bcdata$Sample.Type))

  # Select samples
  if (!is.null(sample_types)) {
    bcdata <- bcdata %>%
      filter(Sample.Type %in% sample_types)
  }

  # Prep for log
  bcdata[target] <- bcdata[target] + 1

  # Plot
  g <- ggplot(data = bcdata,
              mapping = aes_string(x = "Compound",
                                   y = paste0("`", target, "`"))) +
    facet_wrap(Sample.Name ~ ., strip.position = "right",
               scales = "fixed", ncol = facet_cols) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom", legend.box = "vertical") +
    scale_y_log10() +
    ylab(label = paste(target, "(log10(+1))")) +
    geom_bar(stat = "identity", alpha = 0.75, position = "dodge2")

  return(g)
}


# Concentration profiles via points and lines with all samples in one plot
plot_sample_profiles_points <- function(
  bcdata = biocrates,
  target = PKG_ENV$CONCENTRATION,
  sample_types = c(SAMPLE_TYPE_BIOLOGICAL)
){
  assert_that(all(c(target, "Sample.Type") %in% names(bcdata)))
  assert_that(all(sample_types %in% bcdata$Sample.Type))

  # Select samples
  if (!is.null(sample_types)) {
    bcdata <- bcdata %>%
      filter(Sample.Type %in% sample_types)
  }

  # Prep for log
  bcdata[target] <- bcdata[target] + 1

  # Plot
  g <- ggplot(data = bcdata,
         mapping = aes_string(x = "Compound", color = "Sample.Name", group = "Sample.Name",
                              y = paste0("`", target, "`"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none", legend.box = "vertical") +
    scale_y_log10() +
    ylab(label = paste(target, "(log10(+1))")) +
    geom_point(alpha = 0.25, size = 4) +
    geom_line(alpha = 0.25)

  return(g)
}


# Concentration profiles via boxplots over all samples
plot_sample_profiles_box <- function(
  bcdata = biocrates,
  target = PKG_ENV$CONCENTRATION,
  sample_types = c(SAMPLE_TYPE_BIOLOGICAL),
  order_and_color = "Class"
){
  assert_that(all(c(target, "Sample.Type") %in% names(bcdata)))
  assert_that(all(sample_types %in% bcdata$Sample.Type))

  # Select samples
  if (!is.null(sample_types)) {
    bcdata <- bcdata %>%
      filter(Sample.Type %in% sample_types)
  }

  # Order compounds


  # Prep for log
  bcdata[target] <- bcdata[target] + 1

  # Plot
  g <- ggplot(data = bcdata,
         mapping = aes_string(x = "Compound", color = paste0("`", order_and_color, "`"),
                              y = paste0("`", target, "`"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom", legend.box = "vertical") +
    scale_y_log10() +
    ylab(label = paste(target, "(log10(+1))")) +
    geom_boxplot(alpha = 0.75)

  return(g)
}
