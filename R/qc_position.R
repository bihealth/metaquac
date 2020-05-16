# Position specific QC
# Author: Mathias Kuhring


# Heatmap of well plate. Data need to be summarized per sample (target) and
# contain Well.Position and Sample.Type.
# See following functions for examples.
plot_well_plate_overview <- function(data,
                                     target,
                                     position = "Well.Position",
                                     color = "Sample.Type",
                                     size = target,
                                     fill = NULL,
                                     fill_color = c("white", "black"),
                                     orientation = c("horizontal", "vertical")[1],
                                     col_num = 12,
                                     row_num = 8,
                                     facet_var = "Batch",
                                     facet_col = 1,
                                     aspect = row_num/col_num,
                                     title = NULL){

  data_plot <- data

  assert_that("Well.Position" %in% names(data_plot))
  assert_that(orientation %in% c("horizontal", "vertical"))
  if (orientation == "horizontal"){
    data_plot <- data_plot %>%
      mutate(Well.X = ((UQ(sym(position))-1) %% col_num) + 1,
             Well.Y = floor((UQ(sym(position))-1) / col_num) + 1)
  } else { # orientation == "vertical"
    data_plot <- data_plot %>%
      mutate(Well.Y = ((UQ(sym(position))-1) %% row_num) + 1,
             Well.X = floor((UQ(sym(position))-1) / row_num) + 1)
  }

  g <- ggplot(data = data_plot) +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5),
          aspect.ratio = aspect) +
    scale_y_reverse(breaks = 1:row_num, label = LETTERS[1:row_num],
                    limits = c(row_num + 0.25,0.75)) +
    scale_x_continuous(breaks = 1:col_num, position = "top",
                       limits = c(0.75, col_num + 0.25)) +
    labs(x = NULL, y = NULL, title = title)

  if (is.null(fill)){
    g <- g +
      geom_point(alpha = 1,
                 mapping = aes_string(x = "Well.X",
                                      y = "Well.Y",
                                      color = paste0("`", color, "`"),
                                      size = paste0("`", size, "`")))
  } else {
    g <- g +
      geom_point(alpha = 1, shape = 21, stroke = 1.5,
                 mapping = aes_string(x = "Well.X",
                                      y = "Well.Y",
                                      color = paste0("`", color, "`"),
                                      fill = paste0("`", fill, "`"),
                                      size = paste0("`", size, "`"))) +
      scale_fill_continuous(low = fill_color[1], high = fill_color[2])
  }

  if (facet_col == 1){
    g <- g + facet_wrap(as.formula(paste(facet_var, "~ .")), ncol = facet_col,
               scales = "fixed", strip.position = "right")
  } else {
    g <- g + facet_wrap(as.formula(paste(facet_var, "~ .")), ncol = facet_col,
                        scales = "fixed", strip.position = "top")
  }

  return(g)
}


# Count missing values per sample and plot percentage on well plate heatmap.
plot_well_plate_overview_mv <- function(data,
                                        target = ENV$CONCENTRATION){

  data_plot <- data

  data_plot <- data_plot %>%
    group_by(Batch, Well.Position, Sample.Type) %>%
    summarize(`# Missing Values` = sum(is.na(UQ(sym(target)))),
              `% Missing Values` = sum(is.na(UQ(sym(target)))) / n() * 100,
              `# Total` = n())

  g <- plot_well_plate_overview(data_plot,
                                target = "% Missing Values",
                                title = "% Missing Values per Sample",
                                facet_col = 2)

  return(g)
}


# Plot total sum of target per sample as well plate heatmap.
plot_well_plate_overview_total <- function(data,
                                           target = ENV$CONCENTRATION){

  data_plot <- data

  data_plot <- data_plot %>%
    group_by(Batch, Well.Position, Sample.Type) %>%
    summarize(`Total` = sum(UQ(sym(target)), na.rm = TRUE))

  g <- plot_well_plate_overview(data_plot,
                                target = "Total",
                                title = paste("Total", target, "per Sample"),
                                facet_col = 2)

  return(g)
}


# Calculate sequence x breaks with steps of 10 but including min and max position
calc_sequence_x_breaks <- function(data){
  assert_that("Sequence.Position" %in% names(data))
  x_breaks <- seq(10, max(data$Sequence.Position), 10)
  x_breaks <- x_breaks[x_breaks > min(data$Sequence.Position)]
  x_breaks <- c(min(data$Sequence.Position), x_breaks, max(data$Sequence.Position))
  return(x_breaks)
}


# Point plot over sample sequence (aquisition order).Data need to be summarized
# per sample (summary_name) and contain Sequence.Position, Sample.Type, Batch as
# well as Target (optionally for facetting).
# See following functions for examples.
plot_sequence_overview <- function(data,
                                   summary_name,
                                   sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                                    ENV$SAMPLE_TYPE_REFERENCE_QC,
                                                    SAMPLE_TYPE_POOLED_QC),
                                   facet = "Target",
                                   ncol = 1){

  data_plot <- data

  if (!is.null(sample_types)){
    data_plot <- data_plot %>%
      filter(Sample.Type %in% sample_types)
  }

  assert_that("Sequence.Position" %in% names(data_plot))

  g <- ggplot(data = data_plot,
              mapping = aes_string(x = "Sequence.Position",
                                   y = paste0("`", summary_name, "`"),
                                   color = "Sample.Type",
                                   linetype = "Batch",
                                   shape = "Batch")) +
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    geom_point(size = 3, alpha = 0.75, fill = NA) +
    geom_smooth(method='lm', alpha = 0.75, se = FALSE) +
    scale_shape(solid = FALSE) +
    scale_x_continuous(breaks = calc_sequence_x_breaks(data_plot)) +
    scale_color_discrete(name = "Sample Type") +
    ylab(label = summary_name) +
    labs(x = "Sequence Position")

  if (!is.null(facet)){
    g <- g +
      facet_wrap(as.formula(paste(facet, "~ .")),
                 scales = "free_y", ncol = ncol)
  }

  return(g)
}


# Plot total sum of targets per sample as points over sequence.
plot_sequence_overview_total <- function(
  data,
  targets = c(ENV$CONCENTRATION, ENV$AREA, ENV$INTENSITY),
  sample_types = NULL,
  ncol = 1
){
  data_plot <- data

  data_plot <- data_plot %>%
    gather(key = "Target", "Value", targets) %>%
    mutate(Target = factor(Target, levels = targets)) %>%
    drop_na(Value)

  data_plot <- data_plot %>%
    group_by(Target, Batch, Sequence.Position, Sample.Type) %>%
    summarize(Total = sum(Value, na.rm = TRUE))

  g <- plot_sequence_overview(data_plot, summary_name = "Total",
                              sample_types = sample_types, facet = "Target")

  return(g)
}


# Plot % MVs of targets per sample as points over sequence.
plot_sequence_overview_mv <- function(data,
                                      targets = c(ENV$CONCENTRATION, ENV$AREA, ENV$INTENSITY),
                                      sample_types = NULL){

  data_plot <- data %>%
    gather(key = "Target", "Value", targets) %>%
    mutate(Target = factor(Target, levels = targets)) %>%
    group_by(Target, Batch, Sequence.Position, Sample.Type) %>%
    summarize(`% Missing Values` = sum(is.na(Value)) / n() * 100)

  g <- plot_sequence_overview(data_plot, summary_name = "% Missing Values",
                              sample_types = sample_types, facet = "Target")

  return(g)
}


# Position order boxplots/violins over all compound measurements (concentration,
# area, intensity) within a Reference and Pooled QC samples, grouped by sample
# type and batch. Each box/violin represents all compounds in one sample.
plot_sample_variability_sequential <- function(
  data,
  targets = c(ENV$CONCENTRATION, ENV$AREA, ENV$INTENSITY),
  sample_types = c(ENV$SAMPLE_TYPE_REFERENCE_QC,
                   SAMPLE_TYPE_POOLED_QC),
  plot_type = c("boxplot", "violing")[1],
  log10 = TRUE){

  data_plot <- data

  if (!is.null(sample_types)){
    data_plot <- data_plot %>%
      filter(Sample.Type %in% sample_types)
  }

  data_plot <- data_plot %>%
    gather(key = "Target", "Value", targets) %>%
    mutate(Target = factor(Target, levels = targets)) %>%
    mutate(Sequence.Position = as.factor(Sequence.Position)) %>%
    drop_na(Value)

  g <- ggplot(data = data_plot,
              mapping = aes_string(x = "Sequence.Position",
                                   y = "Value",
                                   color = "Sample.Type")) +
    coord_flip() +
    facet_grid(as.formula("Batch + Sample.Type ~ Target"), scales = "free") +
    theme(
      strip.text.x = element_text(size = 6),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical") +
    labs(x = "Sequence Position")

  assert_that(plot_type %in% c("boxplot", "violin"))
  if (plot_type == "boxplot"){
    g <- g + geom_boxplot(fill = NA)
  } else {
    g <- g + geom_violin(fill = NA)
  }

  if (log10){
    g <- g +
      scale_y_log10() +
      ylab(label = "Log 10 Scale")
  } else {
    g <- g +
      ylab(label = "Unscaled")
  }

  return(g)
}
