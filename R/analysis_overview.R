# Overview profile plots and tables
# Author: Mathias Kuhring


# Profile of study variables and group sizes based on UpSetR
plot_study_variables_profile <- function(data,
                                         variables,
                                         biological_sample_type = SAMPLE_TYPE_BIOLOGICAL){

  assert_that(tibble::is_tibble(data) || is.data.frame(data))
  assert_that(assertthat::has_name(data, COLUMN_SAMPLE_TYPE))
  assert_that(biological_sample_type %in% data[[COLUMN_SAMPLE_TYPE]])
  assert_that(assertthat::has_name(data, variables))

  # Reduce data
  test <- data %>%
    select(one_of(COLUMN_SAMPLE_NAME, COLUMN_SAMPLE_TYPE, variables)) %>%
    distinct() %>%
    filter(UQ(sym(COLUMN_SAMPLE_TYPE)) == biological_sample_type)

  set_count <- apply(X = test[variables], MARGIN = 2, FUN = function(x){length(unique(x))})
  set_colors <- viridis::viridis_pal()(length(set_count))
  set_colors <- rep(x = set_colors, times = set_count)

  # Clean and spread values
  for (variable in variables){
    test <- test %>%
      mutate(UQ(sym(variable)) := na_if(UQ(sym(variable)), "")) %>%
      mutate(value = 1,
             UQ(sym(variable)) := paste(variable, "=",
                                        UQ(sym(variable)))) %>%
      spread(UQ(sym(variable)), value, fill = 0)
  }

  UpSetR::upset(test, nsets = ncol(test)-2, nintersects = NA, sets = colnames(test)[3:ncol(test)],
        order.by = "freq", show.numbers = "yes", keep.order = TRUE, sets.bar.color = set_colors)
}


# Profile of statuses, either as frequency or percentage and optionally facetted by groups
plot_status_profile <- function(
  data,
  grouping = NULL,
  percentage = FALSE,
  facet_col = 4
){
  assert_that("MetIDQ_Status" %in% names(data))

  counts <- count_status(data = data, grouping = grouping)

  g <- ggplot(counts,
              mapping = aes(x = Status, fill = Status)) +
    coord_flip() +
    theme(legend.position = "none")

  if (percentage) {
    g <- g +
      geom_bar(aes(y = Percentage), stat = "identity", alpha = 0.75)
  } else {
    g <- g +
      geom_bar(aes(y = Frequency), stat = "identity", alpha = 0.75)
  }

  if (!is.null(grouping)) {
    assert_that(grouping %in% names(data))
    g <- g +
      facet_wrap(as.formula(paste(grouping, "~ .")), ncol = facet_col,
                 scales = "fixed", strip.position = "top")
  }

  return(g)
}


# Heatmap of statuses, either as frequency or percentage
plot_status_heatmap <- function(
  data,
  grouping,
  percentage = FALSE
){
  assert_that("MetIDQ_Status" %in% names(data))

  counts <- count_status(data = data, grouping = grouping)

  g <- ggplot(data = counts,
              mapping = aes_string(x = paste0("`", grouping, "`"), y = "Status")) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    scale_fill_viridis_c()

  if (percentage) {
    g <- g +
      geom_tile(aes(fill = Percentage))
  } else {
    g <- g +
      geom_tile(aes(fill = Frequency))
  }

  return(g)
}


# Calculate absolute and relative counts of statuses, optionally within groups
count_status <- function(data, grouping = NULL){
  assert_that("MetIDQ_Status" %in% names(data))

  status_counts <- data

  if (!is.null(grouping)) {
    assert_that(grouping %in% names(data))
    status_counts <- status_counts %>% group_by(UQ(sym(grouping)))
  }

  status_counts <- status_counts %>%
    rename(Status = MetIDQ_Status) %>%
    mutate(Group_Size = n()) %>%
    group_by(Status, add = TRUE) %>%
    summarize(Frequency = n())

  if (!is.null(grouping)) {
    status_counts <- status_counts %>% group_by(UQ(sym(grouping)))
  }
  status_counts <- status_counts %>%
    mutate(Percentage = Frequency / sum(Frequency) * 100,
           Total = sum(Frequency))

  return(status_counts)
}
