# Different normalization procedures for Biocrates data
# Author: Mathias Kuhring


# Normalize by mean/median
# Reproduce MetIDQ normalization Concentration with:
# target = ENV$CONCENTRATION
# sample_type = ENV$SAMPLE_TYPE_REFERENCE_QC
# average = "median"
# Note: Reproduction incomplete since expected QC concentration are missing!
normalize_by_average <- function(data,
                                 target = ENV$CONCENTRATION,
                                 sample_type = ENV$SAMPLE_TYPE_REFERENCE_QC,
                                 average = "median"){

  # Calcutare average target value for each compound
  if (average == "median"){
    norm_data <- data %>%
      group_by(Compound) %>%
      filter(Sample.Type == sample_type) %>%
      summarize(Average = median(UQ(sym(target))))
  } else if (average == "mean") {
    norm_data <- data %>%
      group_by(Compound) %>%
      filter(Sample.Type == sample_type) %>%
      summarize(Average = mean(UQ(sym(target))))
  } else {
    stop(paste0("average '", average,
                "' not supported (choose 'mean' or 'median')"))
  }

  compound_names <- norm_data$Compound
  norm_data <- norm_data$Average
  names(norm_data) <- compound_names

  # Normalize, i.e. compound target values / sample_type average
  data[target] <- data[target] / norm_data[data$Compound]

  return(data)
}


# Normalize with a linear fit over batch/sequence position
normalize_by_linear_fit <- function(data,
                                    target = ENV$CONCENTRATION,
                                    sample_type = ENV$SAMPLE_TYPE_REFERENCE_QC){

  compounds <- unique(as.character(data$Compound))
  data$Compound <- factor(data$Compound, levels = compounds)
  fits <- list()

  for (i in seq_along(compounds)){
    data_sub <- subset(data, subset =
                         Sample.Type == sample_type &
                         Compound == compounds[i])

    fit <- lm(data_sub[[target]] ~ data_sub$Sequence.Position)
    fits[[compounds[i]]] <- data.frame(Compound = compounds[i],
                          Intercept = fit$coefficients[1],
                          Slope = fit$coefficients[2],
                          row.names = NULL)
  }

  fits_all <- do.call(rbind, fits)
  g <- ggplot(data = subset(data, subset = Sample.Type == sample_type),
         mapping = aes_string(x = "Sequence.Position",
                              y = paste0("`", target, "`"))) +
    labs(title = paste("Linear fits on", sample_type)) +
    geom_point() +
    geom_abline(data = fits_all, aes(slope = Slope, intercept = Intercept)) +
    facet_wrap(~ Compound, scales = "free_y", ncol = 5)

  print(g)

  # Normalize with fit
  intercepts <- setNames(fits_all$Intercept, fits_all$Compound)
  slopes <- setNames(fits_all$Slope, fits_all$Compound)

  data[target] <-
    data[target] /
    (intercepts[data$Compound] +  slopes[data$Compound] + data[target])

  return(data)
}


# Probabilistic Quotient Normalization
#
# According to Dieterle et al. 2006, but normalizing over metabolite profiles instead of spectra
normalize_pqn <- function(data,
                          target_type = SAMPLE_TYPE_BIOLOGICAL,
                          reference_type = SAMPLE_TYPE_BIOLOGICAL,
                          target_values = ENV$CONCENTRATION,
                          pre_integral_norm=NULL){

  assert_that(
    all(c("Compound", "Sample.Name", "Sample.Type", target_values) %in% names(data)))
  assert_that(all(c(target_type, reference_type) %in% data$Sample.Type))

  # 1. Perform an integral normalization
  # (typically a constant integral of 100 is used).
  if (!is.null(pre_integral_norm)) {
    assert_that(class(pre_integral_norm) == "numeric")
    assert_that(pre_integral_norm > 0)

    data <- data %>%
      group_by(Sample.Name) %>%
      mutate(UQ(sym(target_values)) :=
               if_else(
                 Sample.Type == target_type,
                 UQ(sym(target_values)) / sum(UQ(sym(target_values)), na.rm = TRUE) * pre_integral_norm,
                 UQ(sym(target_values))
               )
      ) %>%
      ungroup()
  }

  # 2. Choose/calculate the reference spectrum
  # (the best approach is the calculation of the median spectrum of control samples).
  reference_sample <- data %>%
    filter(Sample.Type == reference_type) %>%
    group_by(Compound) %>%
    summarize(PQN_Reference := median(UQ(sym(target_values)), na.rm = TRUE)) %>%
    ungroup()

  # 3. Calculate the quotients of all variables of interest of the test spectrum
  # with those of the reference spectrum.
  data <- data  %>%
    left_join(reference_sample, by = "Compound") %>%
    mutate(PQN_Quotient = UQ(sym(target_values)) / PQN_Reference)

  # Plot quotients
  # ggplot(data = data %>% filter(Sample.Type == SAMPLE_TYPE_BIOLOGICAL), mapping = aes(x = PQN_Quotient)) +
  #   geom_histogram() +
  #   xlim(c(0,2)) +
  #   facet_wrap(. ~ Sample.Name)

  # 4. Calculate the median of these quotients.
  data <- data %>%
    group_by(Sample.Name) %>%
    mutate(PQN_MostProbableQuotient = median(PQN_Quotient, na.rm = TRUE)) %>%
    ungroup()

  # 5. Divide all variables of the test spectrum by this median.
  data <- data %>%
    mutate(UQ(sym(target_values)) :=
             if_else(
               Sample.Type == target_type,
               UQ(sym(target_values)) / PQN_MostProbableQuotient,
               UQ(sym(target_values))
             )
    )

  # Remove PQN variables
  data <- data %>%
    select(-PQN_Reference, -PQN_Quotient, -PQN_MostProbableQuotient)

  return(data)
}


#
sample_qqplot <- function(data,
                          target_type = SAMPLE_TYPE_BIOLOGICAL,
                          reference_type = SAMPLE_TYPE_BIOLOGICAL,
                          target_values = ENV$CONCENTRATION,
                          scale_log10 = TRUE){

  data_samples <- data %>%
    filter(Sample.Type == target_type) %>%
    select(Sample.Name, UQ(sym(target_values))) %>%
    arrange(Sample.Name, desc(is.na(UQ(sym(target_values)))), UQ(sym(target_values))) %>%
    group_by(Sample.Name) %>%
    mutate(Quantile = row_number()) %>%
    ungroup()

  reference_values <- paste("Reference", target_values)
  reference_sample <- data %>%
    filter(Sample.Type == reference_type) %>%
    group_by(Compound) %>%
    summarize(UQ(sym(reference_values)) := median(UQ(sym(target_values)), na.rm = TRUE)) %>%
    select(-Compound) %>%
    arrange(desc(is.na(UQ(sym(reference_values)))), UQ(sym(reference_values))) %>%
    mutate(Quantile = row_number())

  plot_data <-  data_samples %>%
    left_join(reference_sample, by = "Quantile")

  g <- ggplot(data = plot_data,
              mapping = aes(x = UQ(sym(reference_values)),
                            y = UQ(sym(target_values)),
                            color = Sample.Name)) +
    stat_smooth(geom = 'line', alpha = 0.25) +
    geom_point(size = 0.5, alpha = 0.5) +
    theme(legend.position = "none")

  if (scale_log10) {
    g <- g +
      xlab("Reference Quantiles (log10)") +
      ylab("Sample Quantiles (log10)") +
      scale_x_log10() +
      scale_y_log10()
  } else {
    g <- g +
      xlab("Reference Quantiles") +
      ylab("Sample Quantiles")
  }

  return(g)
}
