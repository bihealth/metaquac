# Plots for sample replicate visualization
# Author: Mathias Kuhring


# Point plots of compound measures over different groups or over an interval
plot_replicate_dispersion <- function(bcdata = biocrates,
                                      target = PKG_ENV$CONCENTRATION,
                                      sample_types = c(SAMPLE_TYPE_POOLED_QC,
                                                       SAMPLE_TYPE_REFERENCE_QC,
                                                       SAMPLE_TYPE_BIOLOGICAL),
                                      grouping = "Sample.Type",
                                      color = NULL,
                                      facet_cols = 3){

  # Select samples
  if (!is.null(sample_types)){
    bcdata <- bcdata %>%
      filter(Sample.Type %in% sample_types)
  }

  # Filter NAs in target
  bcdata <- bcdata %>%
    filter(!rlang::are_na(UQ(sym(target))))

  # Check if numeric
  converted <- type.convert(bcdata[[grouping]])
  is_number <- class(converted) %in% c("integer", "numeric")
  if (is_number){
    bcdata[[grouping]] <- converted
  }

  # Plot
  if (!is.null(color)){
    color = paste0("`", color, "`")
  }

  g <- ggplot(data = bcdata,
              mapping = aes_string(x = paste0("`", grouping, "`"),
                                   y = paste0("`", target, "`"),
                                   color = color)) +
    facet_wrap(Compound ~ ., scales = "free_y", ncol = facet_cols) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom", legend.box = "vertical")

  if (is_number){
    g <- g +
      geom_ribbon(
        mapping = aes_string(group = color),
        stat='smooth', method = "lm", se=TRUE, alpha=0.1, color = NA) +
      geom_point(alpha = 0.25, size = 3) +
      geom_line(stat="smooth", method = "lm", alpha = 0.75)
  } else {
    g <- g +
      geom_boxplot(alpha = 0.50, fill = "white") +
      geom_point(alpha = 0.25, size = 3,
                 position = position_jitterdodge(
                   jitter.height = 0, jitter.width = 0.15))
  }

  return(g)
}


# %RSD bar plots of actual biological or technical replicates
plot_bio_replicate_rsd <- function(bcdata = biocrates,
                                   rep_variables = c("Sample.Type"),
                                   target = PKG_ENV$CONCENTRATION,
                                   sample_types = c(SAMPLE_TYPE_POOLED_QC,
                                                    SAMPLE_TYPE_REFERENCE_QC,
                                                    SAMPLE_TYPE_BIOLOGICAL),
                                   bio_reps = "BR",
                                   tec_reps = "TR",
                                   tec_reps_use = c("include", "only",
                                                    "mean", "median")[1],
                                   rsd_threshold = 15,
                                   summarize = c("Compound", "Class", "Sample.Type")[1],
                                   summary_type = c("mean", "median")[2],
                                   facet_cols = 2){

  # Get RSDs
  rsddata <- table_bio_replicate_rsd(
    bcdata = bcdata,
    rep_variables = rep_variables,
    target = target,
    sample_types = sample_types,
    bio_reps = bio_reps,
    tec_reps = tec_reps,
    tec_reps_use = tec_reps_use,
    rsd_threshold = rsd_threshold,
    summarize = summarize,
    summary_type = summary_type
  )

  y_var <- "%RSD"
  if (summarize != "Compound"){
    y_var <- paste(stringr::str_to_title(summary_type), summarize, "%RSD")
  }

  # Plot
  g <- ggplot(data = rsddata,
              mapping = aes_string(x = paste0("`", summarize, "`"),
                                   y = paste0("`", y_var, "`"))) +
    facet_wrap(as.formula(paste(paste(rep_variables, collapse = "+"), "~ .")),
               labeller = label_wrap_gen(multi_line=TRUE), strip.position = "right",
               scales = "fixed", ncol = facet_cols) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom", legend.box = "vertical") +
    geom_bar(stat = "identity", alpha = 0.75, position = "dodge2") +
    geom_hline(yintercept = rsd_threshold, color = "red") +
    scale_y_continuous(breaks = c(pretty(rsddata[[y_var]]), rsd_threshold)) +
    ylab(label = y_var) + xlab(label = summarize)

  return(g)
}


# %RSD bar plots of actual biological or technical replicates
table_bio_replicate_rsd <- function(bcdata = biocrates,
                                    rep_variables = c("Sample.Type"),
                                    target = PKG_ENV$CONCENTRATION,
                                    sample_types = c(SAMPLE_TYPE_POOLED_QC,
                                                     SAMPLE_TYPE_REFERENCE_QC,
                                                     SAMPLE_TYPE_BIOLOGICAL),
                                    bio_reps = "BR",
                                    tec_reps = "TR",
                                    tec_reps_use = c("include", "only",
                                                     "mean", "median")[1],
                                    rsd_threshold = 15,
                                    summarize = c("Compound", "Class", "Sample.Type")[1],
                                    summary_type = c("mean", "median")[2]){

  assert_that(all(rep_variables %in% names(bcdata)))
  assert_that(target %in% names(bcdata))
  # disabled, since default doesn't apply to studies without pooled QC samples:
  # assert_that(all(sample_types %in% bcdata$Sample.Type))
  assert_that(tec_reps_use %in% c("include", "only", "mean", "median"))
  assert_that(summarize %in% c("Compound", "Class", "Sample.Type"))
  assert_that(summary_type %in% c("mean", "median"))

  rsddata <- bcdata

  # Select samples
  if (!is.null(sample_types)){
    rsddata <- rsddata %>%
      filter(Sample.Type %in% sample_types)
  }

  # Handle technical replicates
  if (tec_reps_use == "only"){
    # Only compare TRs, i.e. BR groups with more than one sample
    rsddata <- rsddata %>%
      group_by_at(vars(Compound, Class, one_of(rep_variables, bio_reps))) %>%
      filter(all(n() > 1))
  } else if (tec_reps_use == "mean"){
    # Summarize TRs as one BRs via mean
    rsddata <- rsddata %>%
      group_by_at(vars(Compound, Class, one_of(rep_variables, bio_reps))) %>%
      summarize(UQ(sym(target)) := mean(UQ(sym(target)), na.rm = TRUE))
  } else if (tec_reps_use == "median"){
    # Summarize TRs as one BRs via median
    rsddata <- rsddata %>%
      group_by_at(vars(Compound, Class, one_of(rep_variables, bio_reps))) %>%
      summarize(UQ(sym(target)) := median(UQ(sym(target)), na.rm = TRUE))
  } else {
    # Include, i.e. don't differentiate TRs from BRs
    rsddata <- rsddata %>%
      filter(!is.na(UQ(sym(target)))) %>%
      group_by_at(vars(Compound, Class, one_of(rep_variables)))
  }

  # Calculate %RSD
  rsddata <- rsddata %>%
    filter(!is.na(UQ(sym(target)))) %>%
    summarize(`%RSD` = rsd(UQ(sym(target))),
              `# Samples` = n())

  # Summarize by class (optional)
  y_var <- "%RSD"
  if (summarize != "Compound"){
    y_var <- paste(stringr::str_to_title(summary_type), summarize, "%RSD")
    rsddata <- rsddata %>%
      filter(!is.na(`%RSD`)) %>%
      group_by_at(vars(one_of(summarize, rep_variables, bio_reps)))
    if (summary_type == "mean"){
      rsddata <- rsddata %>%
        summarize(UQ(sym(y_var)) := mean(`%RSD`, na.rm = TRUE),
                  `# Compounds` = n(),
                  `Mean # Samples` =  mean(`# Samples`, na.rm = TRUE))
    } else { # median
      rsddata <- rsddata %>%
        summarize(UQ(sym(y_var)) := median(`%RSD`, na.rm = TRUE),
                  `# Compounds` = n(),
                  `Median # Samples` =  median(`# Samples`, na.rm = TRUE))
    }
  }

  return(rsddata)
}
