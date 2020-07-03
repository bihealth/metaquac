# Parser for Biocrates export
# Author: Mathias Kuhring


# A MetIDQ Excel export contains several sheets, the main sheet being
# "Data Export" and additional subset sheets for different compound classes.

# The main sheet basically contains several mainly wide tables of the form
# Sample vs Compound, with each table featuring a different value for the
# compound in the sample (e.g. on table for Concentration, one for Analyte
# Intensity, one for Internal Std. Intensity,...).

# Each table starts with a description line (see example below). Only the first
# table contains the header for all tables as well as two lines with additional
# info for the compounds (wide).

# This parser needs a csv/txt export of one sheet (e.g. "Data Export") and
# transforms it into one long table.


# Csv/txt export via Excel contain header like this:
# MetIDQ-6.0.0-DB104-Carbon-2743 Concentration [ng/ml] Target Normalization [Median QC Level 2] All Plate(s)
# Direct txt (tab-sep) from MetIDQ contains additional "#" upfront:
# #MetIDQ-6.0.0-DB104-Carbon-2743 Concentration [ng/ml] NoNormalization
TABLE_PREFIX <- "^#?MetIDQ-" # as regex
TABLE_TYPES <- c("Concentration \\[.*?\\]", # Unit may vary
                 "Analyte Intensity \\[cps\\]",
                 "Internal Std. Intensity \\[cps\\]",
                 "Accuracy \\[%\\]",
                 "Analyte Peak Area \\[area\\]",
                 "Internal Std. Peak Area \\[area\\]",
                 "Analyte Retention Time \\[min\\]",
                 "Internal Std. Retention Time \\[min\\]",
                 "Analyte Peak Width \\[min\\]",
                 "Internal Std. Peak Width \\[min\\]",
                 "Intensity Ratio",
                 "Area Ratio",
                 "Internal Std. Intensity-to-Internal Std. Peak Area Ratio")
TABLE_TYPES_REGEX_ALL <- paste(TABLE_TYPES, collapse = "|")


# Regex for removing Plate Bar Codes compound info header
# e.g. LOD (calc.) 1027199655/2 [µM], ULOQ 1027199655-2 [µM], etc.
PLATE_BAR_CODE_REGEX <- " \\d+[\\/-]\\d"


# Regex to identify status columns, to distinguish value and status columns
STATUS_PATTERN <- " Status$"


# Column typically available in a MetIDQ export file
TYPICAL_METIDQ_COLUMNS <- make.names(c(
  "Plate Bar Code",
  "Sample Bar Code",
  "Sample Type",
  "Sample Identification",
  "Submission Name",
  "Species",
  "Material",
  "OP",
  "Plate Note",
  "Well Position",
  "Sample Volume",
  "Run Number",
  "Injection Number",
  "Measurement Time"
))

# Columns required for the report
REQUIRED_METIDQ_COLUMNS <- make.names(c(
  "Sample Type",
  "Sample Identification",
  "Well Position",
  "Measurement Time"
))


# Escapes metacharacters for regex
# Source: https://stackoverflow.com/a/14838753/1444816
quotemeta <- function(string) {
  stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
}


# Import from tab delimited text file
import_metidq_tables <- function(filename, samples_expected = 96){

  assert_that(file.exists(filename))

  # Guess encoding, but limited to either ISO-8859-1/latin1 or UTF-8
  encoding <- readr::guess_encoding(file = filename)[[1,1]]
  if (startsWith(x = encoding, prefix = "ISO-8859")) {
    encoding <- "ISO-8859-1"
  } else if (startsWith(x = encoding, prefix = "UTF")) {
    encoding <- "UTF-8"
  } else {
    encoding <- "UTF-8"
  }
  cat(paste0("Guessed encoding ", encoding, ". "))

  # Read lines to enable own transformations
  lines <- readr::read_lines(
    file = filename,
    locale = readr::locale(encoding = encoding)
  )

  # Get header
  header <- stringr::str_split(lines[2], "\t")[[1]]

  # Get index of last column before the compounds
  # Is "Measurement Time" always the last?
  idx_last_non_compound <- which(header == "Measurement Time")

  # Catch duplicate column names and rename them (except for compound names)
  # (MetIDQ and study variables might collide, which would result in merge bugs later on)
  header <- handle_duplicate_headers(header, idx_last_non_compound + 1, filename)

  # Check if typcial columns are available
  check_required_columns(header, REQUIRED_METIDQ_COLUMNS, filename)

  # Get additional compound info
  # (all lines after the header starting "empty" ("\t"))
  lines_compound_info_end <-
    which(!startsWith(lines, "\t")[3:length(lines)])[1] - 1 + 2
  lines_compound_info <- lines[2:lines_compound_info_end]
  compound_info <- read.delim(
    text = lines_compound_info,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names=FALSE,
    encoding = "UTF-8"
  )
  col_names <- as.character(compound_info[["Measurement Time"]])
  idx <- which(names(compound_info) == "Measurement Time")
  compound_info[1:idx] <- NULL
  compound_info <- as.data.frame(t(compound_info), stringsAsFactors = FALSE)
  colnames(compound_info) <- col_names
  compound_info$Compound <- rownames(compound_info)
  rownames(compound_info) <- NULL

  colnames(compound_info) <- gsub(pattern = PLATE_BAR_CODE_REGEX,
                                  replacement = "", x = colnames(compound_info))

  # For now, only keep compound name and class. The rest is currently not used on the QC
  # Would need to figure out how to merge this information over several batches first, anyways.
  compound_info <- compound_info %>% select(Compound, Class)

  # Remove compounds with status suffix
  compound_info <- compound_info %>%
    filter(!grepl(pattern = STATUS_PATTERN, x = Compound))

  # Save compound info header names
  assign("COMPOUND_INFO_HEADER", colnames(compound_info), ENV)

  # Disabled, as LoD is currently not parsed (see above)
  # # Save LOD header name (starts with "LOD")
  # assign("LOD_HEADER",
  #        colnames(compound_info)[startsWith(colnames(compound_info), "LOD")], ENV)
  #
  # # Ensure numeric LOD (calc.)
  # x <- tryCatch({
  #   compound_info[[ENV$LOD_HEADER]] <- as.numeric(compound_info[[ENV$LOD_HEADER]])
  # }, warning=function(w) {
  #   message(paste0("Warning: Non-numeric LOD values in file ", filename,
  #                  ": ", conditionMessage(w)))
  # })

  # Remove non-table lines
  lines <- lines[-c(2:lines_compound_info_end)]


  # Get tables

  # Identify table blocks
  # i.e. starting at descrition line with prefix "MetIDQ-"
  # and ending 3 lines before the next block (two empty lines plus next descr.)
  idx_start <- grep(TABLE_PREFIX, lines)
  idx_end <- c(idx_start[-1]-3, length(lines))

  # Collection for "Invalid" values
  invalids <- vector(mode = "list")

  # Iterate table
  num_tables <- length(idx_start)
  tables <- vector(mode = "list", length = num_tables)
  tables_found_names <- vector(mode = "character", length = num_tables)
  tables_found_regex <- vector(mode = "character", length = num_tables)
  # unknown_tables <- 0
  for (i in 1:num_tables){
    # Parse table, but without the description (hence +1)
    table_wide <- read.delim(
      text = lines[(idx_start[i]+1):idx_end[i]],
      header = FALSE,
      stringsAsFactors = FALSE,
      colClasses = "character",
      encoding = "UTF-8"
    )
    colnames(table_wide) <- header

    # Identify table/value type from description
    table_description <- strsplit(lines[idx_start[i]], "\t")[[1]][1]
    # found = FALSE
    for (type in TABLE_TYPES){
      if (grepl(type, table_description, fixed = FALSE)){
        value_type <- stringr::str_extract(table_description, type)
        tables_found_names[i] <- value_type
        tables_found_regex[i] <- type
        found = TRUE
        # break
      }
    }
    # if (!found){
    #   unknown_tables <- unknown_tables + 1
    #   value_type <- paste0("UNKNOWN_DATA_", unknown_tables)
    # }

    # Long table
    table_long <- gather(table_wide, "Compound",
                         UQ(sym(value_type)), -(1:idx_last_non_compound))

    # # Collect "Invalid" values, set them to NA
    # idx_invalid <- grep(pattern = "Invalid", x = table_long[[value_type]],
    #                     fixed = TRUE)
    # if (length(idx_invalid) > 0){
    #   invalids <- rbind(invalids,
    #                     c(table_long[idx_invalid, c("Sample.Identification",
    #                                                 "Compound")],
    #                       Value.Type=value_type))
    #   table_long[[value_type]][idx_invalid] <- NA
    # }

    # # Ensure numeric values
    # if (!flags){
    #   tmp <- sub(pattern = "NA", replacement = NA, table_long[[value_type]])
    #   table_long[[value_type]] <- as.numeric(tmp)
    # }

    tables[[i]] <- table_long
  }

  # Merge the different tables
  single_table <- Reduce(function(x, y) merge(x, y, all=TRUE, sort=FALSE), tables)

  # Get number of records, i.e. samples * compounds (already excluding status entries)
  num_records <- nrow(single_table) / 2

  # Extract status values
  # Separate data and status rows
  single_table_data <- single_table %>%
    filter(!grepl(pattern = STATUS_PATTERN, x = Compound))
  single_table_status <- single_table %>%
    filter(grepl(pattern = STATUS_PATTERN, x = Compound))
  assertthat::assert_that(nrow(single_table_data) == num_records)
  assertthat::assert_that(nrow(single_table_status) == num_records)
  # Process statuses
  single_table_status <- single_table_status %>%
    select(Sample.Identification, Well.Position, Compound, matches(TABLE_TYPES[1])) %>%
    mutate(Compound = stringr::str_remove(Compound, STATUS_PATTERN)) %>%
    rename(MetIDQ_Status = matches(TABLE_TYPES[1]))
  # Merge data and status
  single_table <- single_table_data %>%
    left_join(single_table_status,
              by = c("Sample.Identification", "Well.Position", "Compound"))
  assertthat::assert_that(nrow(single_table) == num_records)

  # Check samples numbers --> should be moved to import info section
  first_compound <- unique(single_table$Compound)[1]
  samples <- single_table %>%
    filter(Compound == first_compound) %>%
    select(Sample.Identification, Well.Position, Compound) %>%
    mutate(Sample.Name = paste(Sample.Identification, Well.Position))
  # if (nrow(samples) > samples_expected){
  #   message(paste0(
  #     "Warning: More samples than expected (", samples_expected, ") in file ",
  #     filename, ": ", nrow(samples), "\n"))
  # }
  # if (nrow(samples) < samples_expected){
  #   message(paste0(
  #     "Warning: Fewer samples than expected (", samples_expected, ") in file ",
  #     filename, ": ", nrow(samples), "\n"))
  # }
  if (anyDuplicated(samples$Sample.Name)){
    message(paste0("Warning: Duplicated samples in file ", filename, ":\n"))
    print(unique(samples$Sample.Name[duplicated(samples$Sample.Name)]))
  }

  # Merge compound infos
  single_table <- merge(single_table, compound_info, by = "Compound", all = TRUE)
  assertthat::assert_that(nrow(single_table) == num_records)

  # # Cat invalid values, if any
  # if (length(invalids)){
  #   message("Warning: Invalid measurements (according to MetIDQ):\n")
  #   print(invalids)
  # }

  # Show missing tables
  if (num_tables < 8){
    message(paste0(
      "Warning: Typical MetIDQ tables not available in file ", filename, ":"
    ))
    print(gsub(
      pattern = "\\", replacement = "", fixed = TRUE,
      TABLE_TYPES[!TABLE_TYPES %in% tables_found_regex]
    ))
  }

  return(single_table)
}


# Preprocess MetIDQ data
preprocess_metidq_tables <- function(values, pool_indicator = NULL){
  # Add sample names for Blanks
  values <- values %>%
    mutate(Sample.Identification = if_else(Sample.Type == "Blank", "Blank", Sample.Identification))

  # Change sample type for pooled QC samples
  if (!is.null(pool_indicator)) {
    assert_that(pool_indicator %in% names(values))
    idx <- grepl(pattern = "pool", ignore.case = TRUE,
                 x = values[[pool_indicator]])
    if (any(idx)) {
      values[idx, ]$Sample.Type <- SAMPLE_TYPE_POOLED_QC
    }
  }

  # Calculate sequence position
  position <- 1:96
  names(position) <- as.vector(matrix(data = 1:96, nrow = 8, ncol = 12, byrow = TRUE))
  values$Sequence.Position <- position[as.character(values$Well.Position)]

  # Calculate well plate coordinates
  well_col <- ((as.integer(values$Well.Position)-1) %% 12) + 1
  well_row <- floor((as.integer(values$Well.Position)-1) / 12) + 1
  values$`Well Coordinates` <- paste0(LETTERS[well_row],
                                      sprintf("%02d", well_col))

  # Create unique sample name, i.e. Sample.Identifier + Sequence.Position
  values$Sample.Name <- paste0(values$Sample.Identification, "_",
                               values$Sequence.Position)

  # Update column types of relevant columns
  values <- values %>%
    mutate_at(vars(Well.Position, Sequence.Position), funs(as.integer)) %>%
    mutate_at(vars(Sample.Volume,
                   matches(TABLE_TYPES[1]), # i.e. "Concentration [.*?]"
                   one_of(
                     "Analyte Intensity [cps]",
                     "Internal Std. Intensity [cps]",
                     "Accuracy [%]",
                     "Analyte Peak Area [area]",
                     "Internal Std. Peak Area [area]",
                     "Analyte Retention Time [min]",
                     "Internal Std. Retention Time [min]"
                   )),
              funs(as.numeric))

  # Remove technical/method columns to ensure sample related columns are unique.
  # Otherwise, samples from e.g. LC1 and LC2 differ in later merge processes.
  suppressWarnings(
    values <- values %>%
      select(-one_of(c("Plate.Bar.Code",
                       "Sample.Bar.Code",
                       "Submission.Name",
                       # "Material",
                       "Plate.Production.No.",
                       "Plate.Note",
                       "Run.Number",
                       "Injection.Number",
                       "Measurement.Time",
                       # "Sample.Description",
                       "Collection.Date",
                       "Org..Info")),
             -starts_with("OP"))
  )

  return(values)
}


# Convert zero to NA
zero_to_na <- function(values){
  values[values == 0] <- NA
  return(values)
}


# Convert zeros to NA to have one common missing value type
unify_missing_values <- function(data){
  data <- data %>%
    mutate_at(
      vars(
        matches(TABLE_TYPES[1]),  # i.e. "Concentration [.*?]"
        one_of(
          "Analyte Intensity [cps]",
          "Internal Std. Intensity [cps]",
          "Analyte Peak Area [area]",
          "Internal Std. Peak Area [area]"
        )
      ),
      funs(zero_to_na))
  return(data)
}


# Import a paired Biocrates data set, i.e. either LC1+LC2 or FIA1+FIA2
import_biocrates_pair <- function(files, measurement_type = "LC"){

  assert_that(measurement_type %in% c("LC", "FIA"))
  samples_expected <- ifelse(measurement_type == "LC", 96, 89)

  # Import single file dataset (e.g. MxP® Quant 500 Kit)
  if (length(files) == 1){
    biocrates <- import_metidq_tables(filename = files[[1]], samples_expected)

  # Import double file dataset (e.g. AbsoluteIDQ® p400 HR Kit)
  } else if (length(files) == 2){
    # Import file 1 and 2
    data1 <- import_metidq_tables(files[[1]], samples_expected)
    data2 <- import_metidq_tables(files[[2]], samples_expected)

    # Check sample consensus
    samples1 <- unique(paste(data1$Sample.Identification, data1$Well.Position))
    samples2 <- unique(paste(data2$Sample.Identification, data2$Well.Position))
    if (!setequal(samples1,samples2)){
      message_text <- paste0(
        "Warning: Different samples in file pair:  \n'", files[[1]], "'  \n'", files[[2]], "'  \n",
        paste("In file 1 (Sample Identification + Well position):",
              paste(setdiff(samples1, samples2), collapse = ", "), "  \n"),
        paste("In file 2 (Sample Identification + Well position):",
              paste(setdiff(samples2, samples1), collapse = ", "), "  \n"))
      message(message_text)
      warning(message_text)
    }

    # Check compound difference
    # (i.e. file 1 and file 2 should contain different compounds)
    compounds1 <- unique(data1$Compound)
    compounds2 <- unique(data2$Compound)
    if (length(intersect(compounds1,compounds2)) > 0) {
      stop(paste0("Same compounds in files '",
                  file1, "' and '", file2, "':\n",
                  paste(intersect(compounds1,compounds2), collapse = ", "), "\n"))
    }

    # Check concentration unit consensus
    conc1 <- names(data1)[grepl(TABLE_TYPES[1], names(data1))]
    conc2 <- names(data2)[grepl(TABLE_TYPES[1], names(data2))]
    if (conc1 != conc2){
      stop(paste0("Inconsisten concentration units in files\n",
                  file1, "\t", conc1, "\n",
                  file2, "\t", conc2, "\n"))
    }

    # Bind data
    biocrates <- suppressWarnings(bind_rows(data1, data2))

  # Catch datasets with more files, as they are currently not expected
  } else {
    stop(paste0("Two many files (expected single or paires):\n", files))
  }

  return(biocrates)
}


# Import a paired data and paired status files
import_biocrates_unit <- function(data_files,
                                  zero2na = TRUE, measurement_type = "LC",
                                  pool_indicator = NULL){

  biocrates <- import_biocrates_pair(files = data_files,
                                     measurement_type = measurement_type)

  # Preprocessing
  biocrates <- preprocess_metidq_tables(biocrates, pool_indicator = pool_indicator)

  # Modifications
  if (zero2na){
    biocrates <- unify_missing_values(biocrates)
  }

  return(biocrates)
}


# Complete missing compound measurements
complete_missing_compounds <- function(data,
                                       expected_compounds = KNOWN_COMPOUNDS,
                                       method = "LC"){

  expected_compounds <- expected_compounds %>% filter(Method == method)

  # Separate sample info/meta data
  suppressWarnings(
    data_samples <-
      data %>%
      select(-one_of(c("Compound", ENV$COMPOUND_INFO_HEADER, "MetIDQ_Status")),
             -matches(TABLE_TYPES_REGEX_ALL)) %>%
      distinct()
  )

  # Check that each sample exists only once. Otherwise, technical columns might
  # be still present, leaving differences due the two runs (eg. LC1/LC2).
  dups <- data_samples %>% select(Sample.Name) %>% duplicated()
  if (any(dups)){
    dups <- data_samples$Sample.Name[dups]
    dups <- data_samples %>%
      filter(Sample.Name %in% dups) %>%
      arrange(Sample.Name) %>%
      group_by(Sample.Name) %>%
      select_if(~ length(unique(.)) > 1)
    message("Warning: Inconsistent sample information:")
    print(dups)
  }

  # Separate compound infos
  suppressWarnings(
    data_compounds <- data %>%
      select(one_of(c("Compound", "Batch", ENV$COMPOUND_INFO_HEADER))) %>%
      distinct()
  )

  # Separate measurements
  data_measurements <- data %>%
    select(one_of(c("Sample.Name", "Compound", "MetIDQ_Status")),
           matches(TABLE_TYPES_REGEX_ALL))

  # Calculate expected number of measurements
  num_samples <- length(unique(data_samples$Sample.Name))
  num_compounds <- nrow(expected_compounds)
  num_total <- num_samples * num_compounds

  # Define expected pairs of Sample and Compound
  data_expected <- expand.grid(Sample.Name = data_samples$Sample.Name,
                               Compound = expected_compounds$Compound,
                               stringsAsFactors = FALSE)
  assert_that(nrow(data_expected) == num_total)

  # Merge with measurements
  data_measurements_complete <- merge(data_measurements, data_expected, all = TRUE)
  assert_that(nrow(data_measurements_complete) == num_total)

  # Merge with sample info/meta data
  data_samples_complete <- merge(data_samples, data_measurements_complete,
                                 by = "Sample.Name", all = TRUE)
  assert_that(nrow(data_samples_complete) == num_total)

  # Merge with compound info
  data_complete <- merge(data_samples_complete, data_compounds,
                                  by = c("Compound", "Batch"), all = TRUE)
  assert_that(nrow(data_complete) == num_total)


  # Print infos
  data_diff <- setdiff(data_expected, data_measurements %>% select(Sample.Name, Compound))
  knitr::kable(data_diff, caption = "Supplemented Measurements")

  return(data_complete)
}
