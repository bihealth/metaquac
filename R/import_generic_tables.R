# Parser for generic data
# Author: Mathias Kuhring


# Generic data format (first draft)
#
# Tab-separated files with samples per row and sample meta data as well as
# features (compound, transition, ...) per column. The feature columns contain
# the measured values. Different types of values will be separated by file, e.g.
# one for concentrations, areas, intensities, statuses (?), ...
#
# Required sample meta data columns for MeTaQuaC:
# Sample Identification
# Sample Type
# Sequence Position (will enable Well Position later)
# Batch (will enable separate batch files later, similar to Biocrates)


# Columns required for the report
REQUIRED_GENERIC_COLUMNS <- make.names(c(
  "Sample Type",
  "Sample Identification"
  # "Sequence Position"
  # "Batch"
))


# Read and prepare a single generic data file
import_generic_table <- function(
  filename,
  value,
  index_first_compound,
  zero2na = TRUE
){
  # Read file
  generic_data <- readr::read_tsv(filename, col_types = readr::cols())

  # Check headers
  header <- colnames(generic_data)
  # Catch duplicate column names and rename them (except for compound names)
  header <- handle_duplicate_headers(header, index_first_compound, filename)
  # Check if typcial columns are available
  check_required_columns(header, REQUIRED_GENERIC_COLUMNS, filename)
  colnames(generic_data) <- header

  # Create long table
  # TODO: check that value name is not already in use by other columns
  header_meta_data <- colnames(generic_data)[1:(index_first_compound - 1)]
  generic_data <- generic_data %>%
    tidyr::gather(key = "Compound", value = UQ(sym(value)), -all_of(header_meta_data))

  # TODO
  # if (value != "Status" && zero2na) {
  #   generic_data[generic_data[,value] == 0,value] <- NA
  # }

  return(generic_data)
}


# Read and merge several generic data file with different value types
import_generic_table_set <- function(
  filenames,
  index_first_compound,
  zero2na = TRUE
) {
  # Check that value types are unique (i.e. only one file per type)
  if (any(duplicated(names(filenames)))) {
    stop(paste(
      "Can't merge data due to duplicate value types:",
      paste(names(filenames), collapse = ", ")
    ))
  }

  # TODO: Check that value types are expected (params$generic_data_types)

  # Read all files
  num_files <- length(filenames)
  tables <- vector(mode = "list", length = num_files)
  for (i in 1:num_files) {
    tables[[i]] <- import_generic_table(
      filename = filenames[i],
      index_first_compound = index_first_compound,
      value = names(filenames[i]),
      zero2na = zero2na
    )
  }

  # TODO: Check that non-compound columns have same content
  # TODO: Check that compound names in different files match

  # Check that all tables have the same dimension
  first_dim <- dim(tables[[1]])
  assert_that(all(sapply(tables, function(x){ all(first_dim == dim(c)) })))
  # Merge
  single_table <- Reduce(
    function(x, y) merge(x, y, all = TRUE, sort = FALSE), tables
  )

  # Check for sequence or well position
  if (!any(c(COLUMN_WELL_POSITION, COLUMN_SEQUENCE_POSITION) %in%
           names(single_table))){
    message(paste(
      "Error: Either column", COLUMN_SEQUENCE_POSITION, "or",
      COLUMN_WELL_POSITION, "required."
    ))
  }

  # Check that dimension still fits
  assert_that(all(first_dim[1] == dim(single_table)[1]))
  assert_that(all(first_dim[2] + num_files - 1 == dim(single_table)[2]))
  # TODO: complain if not, since meta columns don't match
  # or compounds don't match (name or number)

  # If no "Status" file has been import, set everything to "Valid"
  if (!"Status" %in% names(filenames)) {
    single_table$MetIDQ_Status <- "Valid"
    # TODO: warning
  } else {
    single_table <- single_table %>%
      rename(MetIDQ_Status = Status)
  }

  # If no compound "Class" has been imported, set everything to "Unknown"
  if (!"Class" %in% names(filenames)) {
    single_table$Class <- "Unknown"
    # TODO: warning
  }

  # Calculate sequence position, if only well position is given
  # TODO: move to restructuring section
  if (COLUMN_WELL_POSITION %in% names(single_table) &&
      (!COLUMN_SEQUENCE_POSITION %in% names(single_table))){
    # Calculate sequence position
    position <- 1:96
    names(position) <- as.vector(matrix(data = 1:96, nrow = 8, ncol = 12, byrow = TRUE))
    single_table$Sequence.Position <- position[as.character(single_table$Well.Position)]
  }

  # Start creating uniques sample name
  # TODO: move to restructuring section (for Biocrates data as well)
  # TODO: rename to MeTaQuaC.ID or similar
  single_table$Sample.Name <- paste0(
    single_table$Sample.Identification, "_", single_table$Sequence.Position
  )

  return(single_table)
}

