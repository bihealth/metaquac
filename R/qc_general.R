# Explorative plots for general sample reviewing
# Author: Mathias Kuhring


# Boxplot of compound concentrations per sample
plot_concentration_box <- function(data,
                                   target = ENV$CONCENTRATION,
                                   sample_id = "Sample.Name",
                                   log10 = TRUE,
                                   color = "Sample.Type",
                                   fill = NULL){

  data[[sample_id]] <- as.factor(data[[sample_id]])

  if (!is.null(color)){
    color = paste0("`", color, "`")
  }
  if (!is.null(fill)){
    fill = paste0("`", fill, "`")
  }

  g <- ggplot(data = data,
              mapping = aes_string(x = sample_id,
                                   y = paste0("`", target, "`"),
                                   color = color,
                                   fill = fill)) +
    labs(title = paste(target, "distribution")) +
    xlab(label = gsub(".", " ", sample_id, fixed = TRUE)) +
    scale_fill_brewer(palette="OrRd") +
    geom_boxplot() +
    coord_flip()

  if (log10){
    g <- g +
      scale_y_log10() +
      ylab(label = paste(target, "(log10)"))
  } else {
    g <- g +
      ylab(label = target)
  }

  return(g)
}


# Violin plots of compound concentrations per sample
plot_concentration_violin <- function(data,
                                      target = ENV$CONCENTRATION,
                                      sample_id = "Sample.Name",
                                      log10 = TRUE,
                                      violin_width = 0.9,
                                      color = "Sample.Type",
                                      fill = NULL){

  data[[sample_id]] <- as.factor(data[[sample_id]])

  if (!is.null(color)){
    color = paste0("`", color, "`")
  }
  if (!is.null(fill)){
    fill = paste0("`", fill, "`")
  }

  g <- ggplot(data = data,
              mapping = aes_string(x = sample_id,
                                   y = paste0("`", target, "`"),
                                   color = color,
                                   fill = fill)) +
    labs(title = paste(target, "distribution")) +
    xlab(label = gsub(".", " ", sample_id, fixed = TRUE)) +
    scale_fill_brewer(palette="OrRd") +
    geom_violin(width = violin_width) +
    coord_flip()

  if (log10){
    g <- g +
      scale_y_log10() +
      ylab(label = paste(target, "(log10)"))
  } else {
    g <- g +
      ylab(label = target)
  }

  return(g)
}


# Overlapping boxplots/violins over all compound measurements (concentration,
# area, intensity) within a sample, but grouped by sample type and batch.
# Each box/violin represents all compounds in one sample.
plot_sample_variability_overlapped <- function(
  data,
  targets = c(ENV$CONCENTRATION, ENV$AREA, ENV$INTENSITY),
  sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                   ENV$SAMPLE_TYPE_REFERENCE_QC,
                   SAMPLE_TYPE_POOLED_QC,
                   paste0("Standard L", 1:7)),
  plot_type = c("boxplot", "violin")[1],
  log10 = TRUE){

  data_plot <- data

  if (!is.null(sample_types)){
    data_plot <- data_plot %>%
      filter(Sample.Type %in% sample_types)
  }

  data_plot <- data_plot %>%
    gather(key = "Target", "Value", targets) %>%
    mutate(Target = factor(Target, levels = targets)) %>%
    drop_na(Value)

  g <- ggplot(data = data_plot,
              mapping = aes_string(x = "Batch",
                                   y = "Value",
                                   color = "Sample.Type",
                                   group = "Sample.Name")) +
    xlab(label = NULL) +
    coord_flip() +
    facet_grid(as.formula("Sample.Type ~ Target"), scales = "free_x") +
    theme(
      strip.text.y = element_blank(),
      strip.text.x = element_text(size = 6),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical"
      )

  if (length(unique(data_plot$Batch)) < 2) {
    g <- g +  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }

  assert_that(plot_type %in% c("boxplot", "violin"))
  if (plot_type == "boxplot") {
    g <- g + geom_boxplot(position = "identity", fill = NA)
  } else {
    g <- g + geom_violin(position = "identity", fill = NA)
  }

  if (log10) {
    g <- g +
      scale_y_log10() +
      ylab(label = "Log 10 Scale")
  } else {
    g <- g +
      ylab(label = "Unscaled")
  }

  return(g)
}


# Simple histogram matrix for several variables
plot_histogram_matrix <- function(data, variables,
                                  sample_type = SAMPLE_TYPE_BIOLOGICAL){
  data_var <- data %>%
    filter(Sample.Type == sample_type) %>%
    select("Sample.Name", variables) %>%
    distinct() %>%
    select(variables)

  data_var_long <- gather(data_var, Key, Value)

  g <- ggplot(data = data_var_long,
              mapping = aes(x = Value,
                            fill = Key)) +
    geom_histogram(stat = "count") +
    labs(title = "Histograms of study variables") +
    ylab(label = "Count") +
    xlab(label = NULL) +
    facet_wrap(~ Key, scales = "free")

  return(g)
}


# Pairwise sample scatter plot matrix
plot_sample_scatter_matrix <- function(data,
                                       target = ENV$CONCENTRATION,
                                       sample_types = NULL,
                                       max_cols = 9,
                                       cat_header = NULL){

  # Select samples
  if (!is.null(sample_types)) {
    data <- data %>% filter(Sample.Type %in% sample_types)
  }

  # Spread data to get Sample vs Compound matrix
  data_wide <- data %>%
    select(Sample.Name, Compound, UQ(sym(target))) %>%
    spread(key = Compound, value = UQ(sym(target)))

  count_data_matrix <- as.data.frame(data_wide)
  rownames(count_data_matrix) <- count_data_matrix$Sample.Name
  count_data_matrix$Sample.Name <- NULL
  count_data_matrix <- t(as.matrix(count_data_matrix))

  # Correlation panel based on code from Andranik Ivanov
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    r <- trunc(r*100)/100
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) cex.cor <- 0.1/strwidth(txt)
    text(0.5, 0.5, txt, cex = 2.5 * r)
  }

  # Text panel to rotate labels
  panel.text <- function(x = 0.5, y = 0.5, txt, cex, font){
    text(x, y, txt, cex = cex, font = font, srt = -45)
  }

  if (is.null(max_cols)){
    # Labels size
    label_max <- max(nchar(colnames(count_data_matrix)))
    label_cex <- (12 / label_max)

    # Scatter plot matrix based on code from Andranik Ivanov
    pairs(log10(count_data_matrix + 1),
          lower.panel = panel.smooth,
          upper.panel = panel.cor,
          text.panel = panel.text,
          pch = ".", cex = 4, col = "darkblue", gap = 0.2,
          cex.labels = label_cex,
          main=paste("Sample correlation based on", target, "(log10)"))
  } else {
    # Create separate plots if more than max_cols samples
    # With sliding windows of max size max_cols including one overlap
    sample_num <- ncol(count_data_matrix)
    window_num <- ceiling(sample_num / (max_cols - 1))
    window_size <- ceiling(sample_num / window_num) + 1

    # Iterate windows
    for (i in seq_len(window_num)){
      if (!is.null(cat_header)){
        cat(paste0(cat_header, i, "\n"))
      }

      start_idx <- ((i - 1) * (window_size - 1)) + 1
      stop_idx <- min(start_idx + window_size - 1, sample_num)

      # Labels size
      label_max <- max(nchar(colnames(count_data_matrix[, start_idx:stop_idx])))
      label_cex <- (12 / label_max) * (max_cols / (stop_idx - start_idx + 1))

      # Scatter plot matrix based on code from Andranik Ivanov
      pairs(log10(count_data_matrix[, start_idx:stop_idx] + 1),
            lower.panel = panel.smooth,
            upper.panel = panel.cor,
            text.panel = panel.text,
            pch = ".", cex = 4, col = "darkblue", gap = 0.2,
            cex.labels = label_cex,
            main=paste("Sample correlation based on", target, "(log10)"))
    }
  }
}


# Plot SD vs Mean of target for each compound, within different sample types
plot_compound_sd_vs_mean <- function(data,
                                     target = ENV$CONCENTRATION,
                                     sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                                      ENV$SAMPLE_TYPE_REFERENCE_QC,
                                                      SAMPLE_TYPE_POOLED_QC),
                                     study_class = NULL,
                                     shape = NULL,
                                     label = "Compound",
                                     label_number = 20,
                                     fit = c("lm", "loess")[1],
                                     return_with_data = FALSE){

  # Select and group samples
  if (!is.null(sample_types)) {
    data <- data %>%
      filter(Sample.Type %in% sample_types) %>%
      group_by(Sample.Type)
  }
  if (!is.null(shape)){
    data <- data %>% group_by(UQ(sym(shape)), add = TRUE)
  }

  # Indicate study classes
  if (!is.null(study_class)){
    sample_idx <- data$Sample.Type == SAMPLE_TYPE_BIOLOGICAL
    data$Sample.Type[sample_idx] <- paste(data$Sample.Type[sample_idx],
                                          data[[study_class]][sample_idx])
  }

  # Group by compound and calc SD and mean
  data_sum <- data %>%
    group_by(Compound, add = TRUE) %>%
    summarize(Mean = mean(UQ(sym(target)), na.rm = TRUE),
              SD = sd(UQ(sym(target)), na.rm = TRUE))


  g <- ggplot(data = data_sum,
              mapping = aes(x = Mean, y = SD,
                            color = Sample.Type)) +
    labs(title = "Compound SD and Mean within different Sample Types") +
    xlab(label = paste("Mean of", target, "(Log10)")) +
    ylab(label = paste("SD of", target, "(Log10)")) +
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_discrete(name = "Sample Type")

  if (is.null(shape)) {
    g <- g +
      geom_point(alpha = 0.25, size = 4) +
      geom_smooth(method = fit, alpha = 0.95)
  } else {
    g <- g +
      geom_point(mapping = aes_string(shape = paste0("`", shape, "`")),
                 alpha = 0.25, size = 4) +
      geom_smooth(method = fit, alpha = 0.95)
  }

  # Add label in low density areas
  if (!is.null(label)) {
    g <- g +
      ggpmisc::stat_dens2d_filter(
        mapping = aes_string(label = paste0("`", label, "`"),
                             color = "Sample.Type"),
        geom = ggrepel::geom_text_repel()$geom, keep.number = label_number)
  }

  if (return_with_data) {
    return(list("scatterplot" = g, "data" = data_sum))
  }
  else{
    return(g)
  }
}


# Plot %RSD vs Mean of target for each compound, within different sample types
plot_compound_rsd_vs_mean <- function(data,
                                      target = ENV$CONCENTRATION,
                                      sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                                       ENV$SAMPLE_TYPE_REFERENCE_QC,
                                                       SAMPLE_TYPE_POOLED_QC),
                                      study_class = NULL,
                                      shape = NULL,
                                      label = "Compound",
                                      label_number = 20,
                                      fit = c("lm", "loess")[1]){

  # Select and group samples
  if (!is.null(sample_types)) {
    data <- data %>%
      filter(Sample.Type %in% sample_types) %>%
      group_by(Sample.Type)
  }
  if (!is.null(shape)){
    data <- data %>% group_by(UQ(sym(shape)), add = TRUE)
  }

  # Indicate study classes
  if (!is.null(study_class)){
    sample_idx <- data$Sample.Type == SAMPLE_TYPE_BIOLOGICAL
    data$Sample.Type[sample_idx] <- paste(data$Sample.Type[sample_idx],
                                          data[[study_class]][sample_idx])
  }

  # Group by compound and calc SD and mean
  data_sum <- data %>%
    group_by(Compound, add = TRUE) %>%
    summarize(Mean = mean(UQ(sym(target)), na.rm = TRUE),
              `%RSD` = rsd(UQ(sym(target)), na.rm = TRUE))


  g <- ggplot(data = data_sum,
              mapping = aes(x = Mean, y = `%RSD`,
                            color = Sample.Type)) +
    labs(title = "Compound %RSD and Mean within different Sample Types") +
    xlab(label = paste("Mean of", target, "(Log10)")) +
    ylab(label = paste("%RSD of", target, "(Log10)")) +
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_discrete(name = "Sample Type") +
    geom_smooth(method = fit, alpha = 0.5)

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
        mapping = aes_string(label = paste0("`", label, "`"),
                             color = "Sample.Type"),
        geom = ggrepel::geom_text_repel()$geom, keep.number = label_number)
  }

  return(g)
}


# Scatter plots to compare different compound measeruments
plot_compound_scatter <- function(data,
                                  x = ENV$CONCENTRATION,
                                  y = "Analyte Intensity [cps]",
                                  y_istd = "Internal Std. Intensity [cps]",
                                  sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                                   ENV$SAMPLE_TYPE_REFERENCE_QC,
                                                   SAMPLE_TYPE_POOLED_QC,
                                                   paste0("Standard L", 1:7)),
                                  aspect = TRUE,
                                  log_zero_addend = 1,
                                  shape = NULL,
                                  ncol = 3){

  data <- data %>% filter(Sample.Type %in% sample_types)

  # Remove NA-only compounds
  # (shouldn't happen as long as standard or QC samples are available)
  data <- data %>%
    group_by(Compound) %>%
    mutate(All.Conc.NA = all(is.na(UQ(sym(x)))))

  na_comps <- data %>%
    filter(All.Conc.NA == TRUE) %>%
    summarize() %>%
    pull(Compound)

  # print(paste0("Skipped compounds with only NA: ",
  #              paste(na_comps, collapse = ", "
  #              )))

  data <- data %>% filter(All.Conc.NA == FALSE)

  # Normalize y
  data[[y]] <- data[[y]]/data[[y_istd]]

  # Prevent log of zero
  if (log_zero_addend){
    data[[x]] <- data[[x]] + log_zero_addend
    data[[y]] <- data[[y]] + log_zero_addend
  }

  # limits <- NULL
  # if (aspect){
  #   limits <- c(min(data_sub[[x]],data_sub[[y]], na.rm = TRUE),
  #               max(data_sub[[x]],data_sub[[y]], na.rm = TRUE))
  # }

  g <- ggplot(data = data,
              mapping = aes_string(x = paste0("`", x, "`"),
                                   y = paste0("`", y, "`"),
                                   color = "Sample.Type")) +
    xlab(label = paste(x, "(log10)")) +
    ylab(label = paste("Normalized ", y, "(log10)")) +
    theme(legend.position = "bottom",
          # axis.text.x = element_text(angle = 90, hjust = 1),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    scale_color_discrete(name = "Sample Type") +
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(Compound ~ ., scales = "free", ncol = ncol)
    # coord_fixed(xlim = limits, ylim = limits) +
    # geom_abline(slope = 1)

  if (is.null(shape)){
    g <- g + geom_point(alpha = 0.5, size = 2)
  } else {
    g <- g + geom_point(mapping = aes_string(shape = paste0("`", shape, "`")),
                        alpha = 0.5, size = 2)
  }

  return(g)
}
