# Exclusion of compounds and samples
# Author: Mathias Kuhring


# Count NAs in a vector
na_count <- function(values){
  number_na <- sum(is.na(values))
  return(number_na)
}


# Calculate ratio of NAs in vector
na_ratio <- function(values){
  number_total <- length(values)
  number_na <- sum(is.na(values))
  ratio <- number_na / number_total
  return(ratio)
}


# Calculate percentage of NAs in vector
na_percent <- function(values){
  return(na_ratio(values) * 100)
}


# Create table with missing value counts and ratios
table_na <- function(data,
                     group = "Compound",
                     target = PKG_ENV$CONCENTRATION,
                     compare_key = "Sample.Type",
                     compare_values = c(SAMPLE_TYPE_BIOLOGICAL,
                                        SAMPLE_TYPE_REFERENCE_QC,
                                        SAMPLE_TYPE_POOLED_QC)){

  data_na <- data %>%
    filter(UQ(sym(compare_key)) %in% compare_values) %>%
    group_by(UQ(sym(group)), UQ(sym(compare_key))) %>%
    summarize(`# Total` = length(UQ(sym(target))),
              `# Missing Values` = na_count(UQ(sym(target))),
              `% Missing Values` = na_percent(UQ(sym(target))))

  return(data_na)
}


# Plot histogram of ratio of NAs
plot_na_histogram <- function(data,
                              group = "Compound",
                              target = PKG_ENV$CONCENTRATION,
                              compare_key = "Sample.Type",
                              compare_values = c(SAMPLE_TYPE_BIOLOGICAL,
                                                 SAMPLE_TYPE_REFERENCE_QC,
                                                 SAMPLE_TYPE_POOLED_QC),
                              max_ratio = NULL){

  data_na <-
    table_na(data = data, group = group, target = target,
             compare_key = compare_key, compare_values = compare_values)

  if (!is.null(compare_key)) {
    compare_key = paste0("`", compare_key, "`")
  }

  g <- ggplot() +
    geom_histogram(data = data_na,
                   mapping = aes_string(x = "`% Missing Values`",
                                        fill = compare_key),
                   bins = 33, position = "dodge", alpha = 0.9) +
    xlab(label = "% Missing Values") +
    ylab(label = "Frequency") +
    theme(legend.position = "bottom") +
    expand_limits(x = c(0, 100))

  if (!is.null(max_ratio)) {
    g <- g +
      geom_rect(aes(
        xmin = -Inf, xmax = max_ratio,
        ymin = -Inf, ymax = Inf),
        alpha = 0.1, fill = NA_RATIO_COLORS[1]) +
      geom_rect(aes(
        xmin = max_ratio, xmax = Inf,
        ymin = -Inf, ymax = Inf),
        alpha = 0.1, fill = NA_RATIO_COLORS[2]) +
      scale_x_continuous(breaks = c(pretty(data_na$`% Missing Values`), max_ratio),
                         limits = c(NA, NA))
  }

  return(list(data = data_na, plot = g))
}


# DEPRECATED: replaced by plot_compound_na_variable_bars for more than two variables
# Plot comparison of number of NAs for compounds over a set of samples
# with comparison based on variable with two factors/classes
plot_compound_na_scatter <- function(data,
                                     compare,
                                     target = PKG_ENV$CONCENTRATION,
                                     sample_type = SAMPLE_TYPE_BIOLOGICAL,
                                     color = NULL, # NULL means total NA ratio
                                     max_ratio = 0.2,
                                     label = TRUE,
                                     label_number = 20){

  data_sub <- data %>% filter(Sample.Type == sample_type)

  if (length(unique(data_sub[[compare]])) > 2){
    message(paste0("Warning: MV scatter plot doesn't support comparison via a variable ",
                   "with more then two factors:\n\t", compare, ": ",
                   paste(unique(data_sub[[compare]]), collapse = " ")))
    return()
  }

  class1 <- sort(unique(data_sub[[compare]]))[1]
  class2 <- sort(unique(data_sub[[compare]]))[2]

  if (is.null(color)){
    # Calculate NA ratios for compared variables
    data_na_compare <- data %>%
      filter(Sample.Type == SAMPLE_TYPE_BIOLOGICAL) %>%
      group_by(Compound, UQ(sym(compare))) %>%
      summarize(NA.Ratio.Class = na_ratio(UQ(sym(target))))

    # Spread by compare classes
    data_na_wide <-
      spread(data_na_compare, key = UQ(sym(compare)), value = NA.Ratio.Class) %>%
      filter(UQ(sym(class1)) > 0 | UQ(sym(class2)) > 0)

    # Calculate total NA ratios i.e. not in groups of compared variables
    data_na <- data %>%
      filter(Sample.Type == sample_type) %>%
      group_by(Compound) %>%
      summarize(NA.Ratio.Total = na_ratio(UQ(sym(target))))

    # Merge class NA ratios with total NA ratios
    data_na_wide <- merge(data_na_wide, data_na)

    #
    data_na_wide$NA.Ratio.Total <- data_na_wide$NA.Ratio.Total <= max_ratio
    color <- "NA.Ratio.Total"
    legend_name <- paste("Total NA ratio <=", max_ratio)
  } else {
    # Calculate NA ratios for compared variables
    data_na_compare <- data %>%
      filter(Sample.Type == sample_type) %>%
      group_by(Compound, UQ(sym(color)), UQ(sym(compare))) %>%
      summarize(NA.Ratio.Class = na_ratio(UQ(sym(target))))

    # Spread by compare classes
    data_na_wide <-
      spread(data_na_compare, key = UQ(sym(compare)), value = NA.Ratio.Class) %>%
      filter(UQ(sym(class1)) > 0 | UQ(sym(class2)) > 0)

    legend_name <- color
  }

  # Plot
  g <- ggplot(data = data_na_wide,
              mapping = aes_string(x = paste0("`", class1, "`"),
                                   y = paste0("`", class2, "`"),
                                   color = paste0("`", color, "`"))) +
    # labs(title = paste0("Ratio of missing values (NAs) per compound compared ",
    #                     "via variable \"", compare, "\""),
    #      subtitle = paste0("In samples of type \"", sample_type, "\" (",
    #                        "Compounds with total NA ratio of 0 not included)")) +
    geom_point(alpha = 0.5, size = 4) +
    xlab(label = paste("% Missing Values for", compare, class1, "samples")) +
    ylab(label = paste("% Missing Values for", compare, class2, "samples")) +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = legend_name)

  # Add label in low density areas
  if (label){
    g <- g +
      ggpmisc::stat_dens2d_filter(
        mapping = aes(label = Compound),
        geom = ggrepel::geom_text_repel()$geom, keep.number = label_number)
  }

  return(g)
}


# Plot number of missing values vs total intensity of each sample
plot_sample_na_intens_scatter <- function(data,
                                          label = NULL, label_number = 20,
                                          sample_types = NULL,
                                          color = "Sample.Type",
                                          shape = NULL,
                                          missing_values_in = PKG_ENV$CONCENTRATION,
                                          total_of = "Analyte Intensity [cps]"){

  # Filtering
  if (!is.null(sample_types)) {
    data <- data %>% filter(Sample.Type %in% sample_types)
  }

  # Grouping
  data_grouped <- data %>%
    group_by(Sample.Name, Sample.Type, UQ(sym(color)))
  if (!is.null(shape)){
    data_grouped <- data_grouped %>% group_by(UQ(sym(shape)), add = TRUE)
  }
  if (!is.null(label)){
    data_grouped <- data_grouped %>% group_by(UQ(sym(label)), add = TRUE)
  }

  # Calculate NA ratios for compared variables
  data_na_intens <-
    data_grouped %>%
    summarize(`# Missing Values` = na_count(UQ(sym(missing_values_in))),
              Total = sum(UQ(sym(total_of)), na.rm = TRUE))

  # Plot
  g <- ggplot(data = data_na_intens,
              mapping = aes_string(x = "Total",
                                   y = "`# Missing Values`",
                                   color = paste0("`", color, "`"))) +
    xlab(label = paste("Total of", total_of)) +
    ylab(label = "# Missing Values") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "vertical") +
    expand_limits(x = 0, y = 0)

  if (is.null(shape)){
    g <- g + geom_point(alpha = 0.5, size = 4)
  } else {
    g <- g + geom_point(mapping = aes_string(shape = paste0("`", shape, "`")),
                        alpha = 0.5, size = 4)
  }

  # Add label in low density areas
  if (!is.null(label)){
    g <- g +
      ggpmisc::stat_dens2d_filter(
        mapping = aes_string(label = paste0("`", label, "`")),
        geom = ggrepel::geom_text_repel()$geom, keep.number = label_number)
  }

  return(g)
}


# Remove compounds with too many missing values within at least one of the
# groups of samples of the specified types
remove_compounds_na <- function(data,
                                target = PKG_ENV$CONCENTRATION,
                                sample_types = c(SAMPLE_TYPE_REFERENCE_QC, SAMPLE_TYPE_BIOLOGICAL),
                                max_ratio = 0.2){

  # Identify compound with too many missing values
  to_remove <- data.frame()
  if (!is.null(max_ratio)) {
    assertthat::assert_that(0 <= max_ratio && max_ratio <= 1)
    to_remove <- data %>%
      filter(Sample.Type %in% sample_types) %>%
      group_by(Compound, Sample.Type) %>%
      summarize(`# Missing Values` = na_count(UQ(sym(target))),
                `% Missing Values` = na_percent(UQ(sym(target)))) %>%
      filter(`% Missing Values` >= max_ratio * 100)
  }

  # Remove invalid compounds
  data <- subset(data, subset = !Compound %in% unique(to_remove$Compound))

  return(list(data = data, removed = to_remove))
}

# Remove compounds with too many missing values only if ratios of each classes
# of indicated variable exceed the threshold. Only calculates ratios on samples
# of type SAMPLE_TYPE_BIOLOGICAL.
remove_compounds_na_class <- function(data,
                                      variable,
                                      target = PKG_ENV$CONCENTRATION,
                                      max_ratio = 0.2){

  # Identify compound with to many missing values
  to_remove <- data.frame()
  if (!is.null(max_ratio)) {
    assertthat::assert_that(0 <= max_ratio && max_ratio <= 1)
    to_remove <- data %>%
      filter(Sample.Type == SAMPLE_TYPE_BIOLOGICAL) %>%
      group_by(Compound, UQ(sym(variable))) %>%
      summarize(`# Total` = length(UQ(sym(target))),
                `# Missing Values` = na_count(UQ(sym(target))),
                `% Missing Values` = na_percent(UQ(sym(target)))) %>%
      group_by(Compound) %>%
      filter(all(`% Missing Values` >= max_ratio * 100))
  }

  # Remove invalid compounds
  data <- subset(data, subset = !Compound %in% unique(to_remove$Compound))

  return(list(data = data, removed = to_remove))
}


# Remove compounds with insufficient %RSD within at least one of the
# groups of samples of the specified types (replicates)
remove_compounds_rsd <- function(data,
                                 target = PKG_ENV$CONCENTRATION,
                                 sample_types = c(SAMPLE_TYPE_REFERENCE_QC, SAMPLE_TYPE_POOLED_QC),
                                 max_rsd = 15){

  # Identify compounds with insufficient %RSDs
  to_remove <- data.frame()
  if (!is.null(max_rsd)) {
    assertthat::assert_that(0 <= max_rsd && max_rsd <= 100)
    to_remove <- data %>%
      filter(Sample.Type %in% sample_types) %>%
      group_by(Compound, Sample.Type) %>%
      summarize(`%RSD` = rsd(UQ(sym(target)))) %>%
      filter(`%RSD` >= max_rsd)
  }

  # Remove invalid compounds
  data <- subset(data, subset = !Compound %in% unique(to_remove$Compound))

  return(list(data = data, removed = to_remove))
}


# Remove samples with to many missing values
remove_samples_na <- function(data,
                              target = PKG_ENV$CONCENTRATION,
                              max_ratio = 0.2,
                              max_mode = c("exclusive", "inclusive")[1]){

  assertthat::assert_that(max_mode %in% c("exclusive", "inclusive"))

  # Identify samples with to many missing values
  to_remove <- data.frame()
  if (!is.null(max_ratio)) {
    assertthat::assert_that(0 <= max_ratio && max_ratio <= 1)
    to_remove <- data %>%
      group_by(Sample.Name, Sample.Type) %>%
      summarize(`# Missing Values` = na_count(UQ(sym(target))),
                `% Missing Values` = na_percent(UQ(sym(target))))

    if (max_mode == "exclusive") {
      to_remove <- to_remove %>% filter(`% Missing Values` >= max_ratio * 100)
    } else {
      to_remove <- to_remove %>% filter(`% Missing Values` > max_ratio * 100)
    }
  }

  # Remove invalid compounds
  data <- subset(data, subset = !Sample.Name %in% unique(to_remove$Sample.Name))

  return(return(list(data = data, removed = to_remove)))
}
