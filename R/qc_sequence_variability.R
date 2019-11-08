# Functions to access sequence behaviour


# Fit a linear model per batch on sequence position vs totals of target.
# Evaluate slope significance, i.e. a significant slope may indicate high fluctuations or batch
# drifts, in particular for target Concentration (which should already account for these effects
# due to internal standard normalization and external standard calibration).
sequence_batch_horizontality <- function(
  data = biocrates,
  target = PKG_ENV$CONCENTRATION,
  sample_type = SAMPLE_TYPE_REFERENCE_QC,
  result_message = TRUE,
  alpha = 0.01
)
{
  assert_that(all(c("Sequence.Position", "Batch", target, "Sample.Type") %in% names(data)))
  assert_that(sample_type %in% data[["Sample.Type"]])

  # Prepare data
  data_model <- data %>%
    filter(Sample.Type == sample_type) %>%
    group_by(Batch, Sequence.Position, Sample.Type) %>%
    summarize(Total = sum(UQ(sym(target)), na.rm = TRUE))

  # Assure batch order for modelling
  data_model$Batch <- factor(data_model$Batch, levels = sort(unique(data_model$Batch)))

  # Linear model for each batch
  model_results <- data_model %>%
    group_by(Batch) %>%
    summarize(
      `# Samples` = n(),
      `R_squared` = NA,
      Intercept = NA,
      `Intercept p-value` = NA,
      Slope = NA,
      `Slope p-value` = NA
    )

  for (i in seq_along(model_results$Batch)) {
    model = lm(Total ~ Sequence.Position, data_model %>% filter(Batch == model_results$Batch[i]))
    model_results$`R_squared`[i] = summary(model)[8]$r.squared
    model_results$Intercept[i] = summary(model)[4]$coefficients["(Intercept)", "Estimate"]
    model_results$`Intercept p-value`[i] = summary(model)[4]$coefficients["(Intercept)", "Pr(>|t|)"]
    model_results$Slope[i] = summary(model)[4]$coefficients["Sequence.Position", "Estimate"]
    model_results$`Slope p-value`[i] =
      summary(model)[4]$coefficients["Sequence.Position", "Pr(>|t|)"]
  }

  # Print a message according to slope significance
  if (result_message) {
    # Check if any batch features a significant slope in the acquisition sequence
    any_significance <- FALSE
    for (i in 1:nrow(model_results)) {
      if (is.nan(model_results$`Slope p-value`[i]) || is.na(model_results$`Slope p-value`[i])) {
        message(
          "Error: No p-value available for total ", target, " slope of samples of type ",
          sample_type, " in Batch **", model_results$Batch[i], "**! Too few samples available?"
        )
        any_significance <- any_significance || TRUE
      } else if (model_results$`Slope p-value`[i] < alpha) {
        message(
          "Warning: Batch **", model_results$Batch[i], "** features a significant ",
          target, " slope for samples of type ", sample_type, " (p-value < ", alpha, ")!"
        )
        any_significance <- any_significance || TRUE
      }
    }
    if (!any_significance) {
      message(paste0(
        "Success: No batch with undesirable ", target, " slope for samples of type ", sample_type, "."
      ))
    }
  }

  return(model_results)
}


# Fit a linear model on sequence position vs totals of target including all batches.
# Evaluate batch differences based on contrasts relatively to one randomly selected refence batch.
# Batches should feature insignificant differences in slopes and intercept, in particular for target
# Concentration (which should already account for differences due to internal standard normalization
# and external standard calibration). An additional Kruskal-Wallis Rank Sum Test is additionally
# used to verify batch conformity (no significant differences in target totals).
sequence_batch_conformity <- function(
  data = biocrates,
  target = PKG_ENV$CONCENTRATION,
  sample_type = SAMPLE_TYPE_REFERENCE_QC,
  result_message = TRUE,
  alpha = 0.01
)
{
  assert_that(all(c("Sequence.Position", "Batch", target, "Sample.Type") %in% names(data)))
  assert_that(sample_type %in% data[["Sample.Type"]])

  # Prepare data
  data_model <- data %>%
    filter(Sample.Type == sample_type) %>%
    group_by(Batch, Sequence.Position, Sample.Type) %>%
    summarize(Total = sum(UQ(sym(target)), na.rm = TRUE))

  # Random batch order for modelling, to prevent prefering first batch as reference
  batches <- sample(unique(data_model$Batch))
  data_model$Batch <- factor(data_model$Batch, levels = batches)

  # Kruskal-Wallis to check if batches differ significantly
  kruskal_result = kruskal.test(data_model$Total ~ data_model$Batch)
  kruskal_result

  # This model returns the delta intercepts and delta slope as well as the significance of the
  # deltas with respect to the first batch in the data.
  model <- lm(Total ~ Batch + Sequence.Position/Batch, data_model)
  summary(model)[8]$r.squared

  # Collect intercept and slope deltas and p-values
  model_results <- dplyr::tibble(
    Batch = batches,
    Type = c("Reference", rep("Delta", times = length(batches) - 1)),
    Intercept = summary(model)[4]$coefficients[1:length(batches), 'Estimate'],
    `Intercept p-value` = summary(model)[4]$coefficients[1:length(batches), 'Pr(>|t|)'],
    Slope = summary(model)[4]$coefficients[(1:length(batches)) + length(batches), 'Estimate'],
    `Slope p-value` = summary(model)[4]$coefficients[(1:length(batches)) + length(batches), 'Pr(>|t|)']
  )

  # Print a message according to significance in differences between batches
  # (including distribution / kruskal-wallis and model differences, i.e. intercepts and slopes).
  if (result_message) {
    any_significance <- FALSE
    if (kruskal_result$p.value < alpha) {
      message("Warning: Detected significant differences in batch distributions of total ", target,
              " for samples of type ", sample_type, " (Kruskal-Wallis p-value < ", alpha, ")!")
      any_significance <- any_significance || TRUE
    }
    if (any(model_results %>% filter(Type == "Delta") %>% pull(`Intercept p-value`) < alpha)) {
      message("Warning: Detected significant differences in batch intercepts of total ", target,
              " for samples of type ", sample_type, " (delta p-value < ", alpha, ")!")
      any_significance <- any_significance || TRUE
    }
    if (any(model_results %>% filter(Type == "Delta") %>% pull(`Slope p-value`)  < alpha)) {
      message("Warning: Detected significant differences in batch slopes of total ", target,
              " for samples of type ", sample_type, " (delta p-value < ", alpha, ")!")
      any_significance <- any_significance || TRUE
    }
    if (!any_significance) {
      message("Success: There are no significant differences in total ", target, " between batches",
              " for samples of type ", sample_type, ".")
    }
  }

  stats = list(
    kruskal_pvalue = kruskal_result$p.value,
    r_squared = summary(model)[8]$r.squared,
    coefficients = model_results
  )

  return(stats)
}


# Assisting plots for linear model analysis of batches
sequence_batch_assistant_plots <- function(
  data = biocrates,
  target = PKG_ENV$CONCENTRATION,
  sample_type = SAMPLE_TYPE_REFERENCE_QC,
  result_message = TRUE,
  alpha = 0.01
){
  assert_that(all(c("Sequence.Position", "Batch", target, "Sample.Type") %in% names(data)))
  assert_that(sample_type %in% data[["Sample.Type"]])

  # Prepare data
  data_model <- data %>%
    filter(Sample.Type == sample_type) %>%
    group_by(Batch, Sequence.Position, Sample.Type) %>%
    summarize(Total = sum(UQ(sym(target)), na.rm = TRUE))

  # Assure batch order for modelling
  data_model$Batch <- factor(data_model$Batch, levels = sort(unique(data_model$Batch)))

  # Create auxiliary sequence plot
  g_seq <- ggplot(data = data_model,
                  mapping = aes_string(x = "Sequence.Position", y = "Total",
                                       color = "Batch", linetype = "Batch", shape = "Batch")) +
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    geom_point(size = 3, alpha = 0.75, fill = NA) +
    geom_smooth(method = 'lm', alpha = 0.75, se = FALSE) +
    scale_shape(solid = FALSE) +
    scale_x_continuous(breaks = calc_sequence_x_breaks(data_model)) +
    labs(x = "Sequence Position", y = paste("Total", target)) +
    expand_limits(y = 0)

  # Create auxiliary distribution plot
  g_dist <- ggplot(data = data_model,
                   mapping = aes(x = Total, color = Batch, fill = Batch)) +
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    geom_density(alpha = 0.25) +
    labs(x = paste("Total", target), y = "Density")

  return(list(sequence = g_seq, distribution = g_dist))
}
