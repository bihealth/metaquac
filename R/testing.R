# Miscellaneous helper functions for testing
# Author: Mathias Kuhring


# Get paths of files included in the package
test_files <- function(files, path = "extdata"){
  return(system.file(path, files, package = "metaquac"))
}
