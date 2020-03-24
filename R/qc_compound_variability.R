# Function to access compound variability


# Evaluate technical vs biological compound variance with the use of linear modelling
# By courtesy of Yoann Gluagoen
compound_variability_scoring <- function(
  data,
  target = ENV$CONCENTRATION,
  technical = ENV$SAMPLE_TYPE_REFERENCE_QC,
  biological = SAMPLE_TYPE_BIOLOGICAL,
  result_message = TRUE
)
{
  # We look at the median of the sd respectively for the QC samples and biological samples
  # The median is only used to make sure that QC have lower sd in case the wilcoxon test on sd
  # between QC and biological samples return significant difference.
  # We could use other methods but this very simple approach is fine for a first verison.
  # If the variability in the QC samples turns out to be significantly higher than in biological samples,
  # this is a bad sign and a warning will appear on the report.

  # Calculate sd and mean per compound and sample type
  sd_mean_comp <- plot_compound_sd_vs_mean(
    data = data,
    target = target,
    sample_types = c(technical, biological),
    return_with_data = TRUE)

  # Remove data unsuitable for modelling and log
  sd_mean_comp$data <- sd_mean_comp$data %>%
    mutate(Group = Sample.Type,
           Mean = if_else(is.nan(Mean), NA_real_, Mean),
           Mean = if_else(Mean == Inf, NA_real_, Mean),
           Mean = if_else(Mean == 0, NA_real_, Mean),
           SD = if_else(is.nan(SD), NA_real_, SD),
           SD = if_else(SD == Inf, NA_real_, SD),
           SD = if_else(SD == 0, NA_real_, SD)) %>%
    tidyr::drop_na()

  # Assure sample type order for modelling, i.e. first technical then biological
  sd_mean_comp$data$Group <- factor(sd_mean_comp$data$Group,
                                    levels = c(technical, biological))

  # Significance only applies to pooled QC, as they share the same conc./intens. scale  with samples
  # One sided wilcoxon to check that QC SDs are lower than biological sample SDs
  # wilcox_result = wilcox.test(sd_mean_comp$data$SD ~ sd_mean_comp$data$Group,
  #                             alternative = "less")


  # Now we will check linearity and technical variation vs biological variation. The model will not
  # directly tell us about the linearity of the data but we assume that if r.squared > 0.8 we have a
  # good fit, any problem with linearity is easy to spot by the user using the plot.

  # This model returns the intercept for the QC samples which we won't use.
  # It then returns the delta intercept between Sample and QC and their respective slopes.
  # We will also need to check the Rsquared to make sure that we have an acceptable fit.
  model <- lm(log(SD) ~ Group + log(Mean)/Group, sd_mean_comp$data)

  # Delta intercept is Sample intercept - QC intercept
  # (Second estimate given by the model under variable name 'GroupeSample').
  delta_intercept = summary(model)[4]$coefficients['GroupSample','Estimate']
  delta_intercept_pvalue = summary(model)[4]$coefficients['GroupSample','Pr(>|t|)']

  # The model also return the estimate of the delta slope
  delta_slope = summary(model)[4]$coefficients['GroupSample:log(Mean)','Estimate']
  delta_slope_pvalue = summary(model)[4]$coefficients['GroupSample:log(Mean)','Pr(>|t|)']

  # And the estimate of the slope of the QC samples only - it is important for us to check that the
  # slope is positive.
  qc_slope = summary(model)[4]$coefficients['log(Mean)','Estimate']

  # Goodness of model fit
  r_squared = summary(model)[8]$r.squared


  # Now that we have all the data, we can cross the different variables to cover the most important
  # cases. As a first version, we will mostly check whether there is
  # 1) `k >= 7`: A clear biological variability on top of the technical one.
  # 2) `k = 2:6`: Inconclusive differences in biological and technical variability.
  # 3) `k = 0:1`: A strong technical variability which hides the biological one.
  # 4) `k < 0`: Something clearly wrong (when variability in QC decreases with increasing amount of metabolite).

  # The variables used for the automatic assessment are:
  # - wilcox_result$p.value (double)
  # - delta_intercept (double)
  # - delta_intercept_pvalue (double)
  # - delta_slope (double)
  # - delta_slope_pvalue (double)
  # - r.squared (double)

  # We compute simple scores using our variables
  # Final score will allow us to cover the main cases
  k = 0
  k = k + (r_squared > 0.8)
  k = k + 2*(delta_intercept > 0 & delta_intercept_pvalue < 0.05)
  k = k + 4*(delta_slope > 0) # & delta_slope_pvalue < 0.05) # Significants applies only for pooled QCs
  # k = k + 8*(wilcox_result$p.value < 0.05) # Significants applies only to pooled QCs
  k = k - 16*(qc_slope < 0)


  # Print a message according to score
  if (result_message) {
    if (k < 0) {
      message(
        "Error: A fundamental issue has been detected with the data. The variability of your QC
        samples is negatively correlated with the mean. You should not carry on with the
        analysis until you have identified the origin of this issue.")
    } else if (dplyr::between(k,0,1)) {
      message(
        "Warning: There is a high overlap between biological and technical variability. Technical
        variability is equal to or stronger than biological variability.")
    } else if (dplyr::between(k,2,6)) {
      message(
        "Warning: Technical and biological variability can be separated, however the relation
        between the both is not fully conclusive.")
    } else { # (k >= 7)
      message(
        "Success: There is a clear separation between technical and biological variability.")
    }
  }

  stats = list(
    r_squared = r_squared,
    qc_slope = qc_slope,
    delta_slope = delta_slope,
    delta_slope_pvalue = delta_slope_pvalue,
    delta_intercept = delta_intercept,
    delta_intercept_pvalue = delta_intercept_pvalue,
    # wilcox_pvalue = wilcox_result$p.value,
    k = k
  )

  return(stats)
}
