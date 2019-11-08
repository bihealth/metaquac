# Clustered heatmap from RNA-Seq pipeline applied on metabolite concentrations
# Author: Mathias Kuhring

# Original author of rseq functions: Kajetan Bentele
# Source: https://gitlab.bihealth.org/cubi/rseq


# Plot Heatmap
#
# Input matrix will be normalized (scaled and centered) by row. Then, values
# smaller than z_min are set to z_min, likewise values larger than z_max are
# set to z_max.
#
# @param mat a matrix of numbers
# @param z_min all values in scaled matrix smaller than z_min are set to z_min
# @param z_max all values in scaled matrix larger than z_max are set to z_max
# @param dist_fun a distance function used for hierarchical clustering
# @param title string, the title of the heatmap
# @return a list (see ?gplots::heatmap.2 for details)
plot_heatmap <- function(mat, title = "", z_min = -Inf, z_max = Inf,
                         dist_fun = NULL, colCol = NULL, lwid = NULL){
  assert_that(is.matrix(mat))
  assert_that(is.numeric(mat))
  scaled_mat <- t(scale(t(mat)))

  dual_scaled_mat <- pmin(pmax(scaled_mat, z_min), z_max)
  # pmin returns the minima of two vectors by position; the shorter vector gets recycled
  # pmax returns the maxima of two vectors by position; the shorter vector gets recycled
  if (is.null(dist_fun)) {
    cor_dist <- function(mat) {
      my_dist <- as.dist(1 - cor(t(mat), use = 'pairwise.complete.obs'))
      return(my_dist)
    }
    dist_fun <- cor_dist
  }else{
    assert_that(is.function(dist_fun))
  }

  # Remove blank rows, i.e. with only NAs
  dual_scaled_mat <- dual_scaled_mat[rowSums(is.na(dual_scaled_mat)) != ncol(dual_scaled_mat),]

  gplots::heatmap.2(
    dual_scaled_mat,
    trace = 'none',
    scale = 'none',
    distfun  = dist_fun,
    # Label margins. Adjust 2nd if row names are enabled.
    margins = c(8,1),
    srtCol = 45,
    na.color = 'grey',
    main = title,
    col = viridis::viridis_pal()(128),
    # Color column/sample labels
    colCol = colCol[colnames(dual_scaled_mat)],
    # Disable row names aka compound names
    labRow = NA,
    # Adjust layout ratios. Controlled from outside since based on sample
    # numbers and thus figure size in the R notebook.
    lwid = lwid
  )
}

# Plot Heatmap of Top Fluctuating Genes
#
# Takes a data frame with regularized log transformed counts, sorts genes by
# decreasing standard deviation and includes the top number of fluctuating
# features in a heatmap.
#
# @param num integer, number of top fluctuating features to be included in heatmap
# @param rld_df a data frame with variables feature_id, sample_name, reg_log_count
# @param ... parameters are passed to plot_heatmap
# @return a list (see ?gplots::heatmap.2 for details)
plot_heatmap_fluc_features <- function(num, rld_df, target, ...){
  assert_that(assertthat::is.count(num))
  assert_that(is.data.frame(rld_df))
  assert_that(assertthat::has_name(
    rld_df, c("feature_id", "sample_name", "reg_log_count")))

  rld_disp_wide <-
    rld_df %>%
    dplyr::select(feature_id, sample_name, reg_log_count) %>%
    dplyr::group_by(feature_id) %>%
    dplyr::mutate(sd_reg_log_count = sd(reg_log_count)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(sample_name, reg_log_count) %>%
    dplyr::arrange(desc(sd_reg_log_count))

  rld_mat <-
    rld_disp_wide %>%
    dplyr::slice(1:num) %>%
    dplyr::select(-sd_reg_log_count) %>%
    as.data.frame(stringsAsFactors = F) %>%
    tibble::column_to_rownames("feature_id")  %>%
    as.matrix()

  title = paste0(target, "\n(log10 > centered > scaled)")
  plot_heatmap(mat = rld_mat, title = title, ...)
}


# Plot heatmap of samples vs compounds with respect to the target variable
plot_sample_heatmap <- function(data,
                                target = PKG_ENV$CONCENTRATION,
                                sample_types = c(SAMPLE_TYPE_BIOLOGICAL,
                                                 SAMPLE_TYPE_REFERENCE_QC,
                                                 SAMPLE_TYPE_POOLED_QC),
                                sample_color_by = NULL,
                                dist_fun = NULL,
                                lwid = NULL){
  assert_that(is.data.frame(data))
  assert_that(target %in% colnames(data))
  assert_that(any(sample_types %in% data$Sample.Type))

  # Select and group samples
  if (!is.null(sample_types)) {
    data <- data %>%
      filter(Sample.Type %in% sample_types)
  }

  # Batch colors for sample labels (colums)+
  if (is.null(sample_color_by)){
    sample_colors <- NULL
  } else {
    assert_that(sample_color_by %in% colnames(data))
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length=n+1)
      hcl(h=hues, l=65, c=100)[1:n]
    }
    colors_names <- sort(unique(data[[sample_color_by]]))
    colors_values <- viridis::viridis_pal()(length(colors_names))
    names(colors_values) <- colors_names
    # print(colors_values)
    data_sub <- data %>% select(Sample.Name, sample_color_by) %>% unique
    sample_colors <- colors_values[as.character(data_sub[[sample_color_by]])]
    names(sample_colors) <- data_sub$Sample.Name
  }

  # Create data frame as expected by rseq heatmap function
  rld_df <- data.frame(feature_id = data$Compound,
                       sample_name = data$Sample.Name,
                       reg_log_count = log10(data[[target]] + 1))

  # Plot heatmap
  plot_heatmap_fluc_features(num = min(500,
                                       length(unique(rld_df$feature_id))),
                             rld_df = rld_df,
                             target = target,
                             colCol = sample_colors,
                             dist_fun = dist_fun,
                             lwid = lwid)

  # heatmap.2 plots directly and returns a list with components instead. Since
  # these components are currently not of interest, they are not returned.
}
