# More QC plots for missing values
# Author: Mathias Kuhring


# Calculate number and percentages of NAs for compounds over a set of samples,
# but within different groups in a study/biological variable.
# Only reports compounds with more than 0 and less than 100% missing values.
calc_compound_na_variable <- function(
  data,
  compare,
  target = ENV$CONCENTRATION,
  sample_type = SAMPLE_TYPE_BIOLOGICAL,
  remove_fully_missing = TRUE,
  add_norm_perc_mvs = TRUE
){
  # Calculate NA stats for compared groups
  data_na <- data %>%
    filter(Sample.Type %in% sample_type)

  # Remove compounds with 100% missing values (over all compared groups)
  if (remove_fully_missing) {
    data_na <- data_na %>%
      group_by(Compound) %>%
      filter(na_percent(UQ(sym(target))) < 100)
  }

  data_na <- data_na %>%
    # Calculate NA count and percentages for compared groups
    group_by(Compound, Sample.Type, UQ(sym(compare))) %>%
    summarize(`Group Size` = n(),
              `# Missing Values` = na_count(UQ(sym(target))),
              `% Missing Values` = na_percent(UQ(sym(target)))) %>%
    # Remove compounds (per group) with no missing values
    filter(`# Missing Values` > 0) %>%
    ungroup()

  # Calculate normalized NA ratios per compound
  if (add_norm_perc_mvs) {
    data_na <- data_na %>%
      group_by(Compound) %>%
      mutate(`Normalized % Missing Values` =
               `% Missing Values` / sum(`% Missing Values`)) %>%
      ungroup()
  }

  return(data_na)
}

# Plot for comparison of number of NAs for compounds over a set of samples,
# but within different groups in a study/biological variable.
# Only shows compounds with more than 0 and less than 100% missing values.
plot_compound_na_variable_bars <- function(
  data,
  compare,
  target = ENV$CONCENTRATION,
  sample_type = SAMPLE_TYPE_BIOLOGICAL,
  remove_fully_missing = TRUE,
  add_norm_perc_mvs = TRUE,
  position = "stack"
){

  assertthat::assert_that(position %in% c("stack", "dodge"))

  data_na_compare <- calc_compound_na_variable(
    data = data,
    compare = compare,
    target = target,
    sample_type = sample_type,
    remove_fully_missing = remove_fully_missing,
    add_norm_perc_mvs = add_norm_perc_mvs
  )

  data_na_compare <- data_na_compare %>%
    tidyr::gather(
      key = "Name",
      value = "MVs",
      one_of("# Missing Values",
             "% Missing Values",
             "Normalized % Missing Values")
    )

  # Plot
  g <- ggplot(data = data_na_compare,
              mapping = aes_string(x = "Compound",
                                   y = "MVs",
                                   fill = paste0("`", compare, "`"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom", legend.box = "vertical") +
    ylab(label = NULL) +
    scale_fill_viridis_d() +
    facet_wrap(. ~ Name, nrow = 3, scales = "free_y")

  if (position == "dodge") {
    g <- g +  geom_bar(
      stat = "identity",
      alpha = 0.9,
      position = position_dodge(preserve = "single")
    )
  } else {
    g <- g +  geom_bar(stat = "identity", alpha = 0.9, position = "stack")
  }

  return(g)
}
