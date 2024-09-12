# Specify the file path
file_path <- "~/Stuff/github/SEFSC-SFD-SAF-Poor-Recruitment/data/SERFS_CTD_Data/"

for (i in 1:4) {
  # Construct the file name with the current index
  file_name <- paste0(file_path, "/qry_MARMAP_CTD_Effort", i, ".txt")
  # Read the file using read.table with '|' as the separator, skipping lines starting with '-'
  lines <- readLines(file_name)
  # find only lines with values not separator ---------
  datalines <- grep("^\\-+$", lines) + 1
  # dataux has values now
  dataaux <- lines[datalines]
  # will need to split everything by |
  split_lines <- strsplit(dataaux, "|", fixed = TRUE)
  # Remove leading and trailing whitespaces from each element
  split_lines <- lapply(split_lines, function(line) trimws(line))
  # Filter out empty lines
  split_lines <- split_lines[sapply(split_lines, function(line) length(line) > 1)]
  # Determine the maximum number of elements in a line
  max_elements <- max(sapply(split_lines, length))
  # Pad lines with fewer elements with NA
  split_lines <- lapply(split_lines, function(line) c(line, rep(NA, max_elements - length(line))))
  # Create a data frame
  data_frame <- as.data.frame(do.call(rbind, split_lines), stringsAsFactors = FALSE)
  # Set column names using the first row
  colnames(data_frame) <- data_frame[1, ]
  # Remove the first row (header)
  data_frame <- data_frame[-1, ]
  # Remove leading and trailing whitespaces in column names
  colnames(data_frame) <- trimws(colnames(data_frame))
  write.csv(data_frame, file = paste0("MARMAP_", i,".csv"), row.names = FALSE)
}

for (i in 1:2) {
  # Construct the file name with the current index
  file_name <- paste0(file_path, "/qry_SEFIS_CTD_Effort", i, ".txt")
  # Read the file using read.table with '|' as the separator, skipping lines starting with '-'
  lines <- readLines(file_name)
  # find only lines with values not separator ---------
  datalines <- grep("^\\-+$", lines) + 1
  # dataux has values now
  dataaux <- lines[datalines]
  # will need to split everything by |
  split_lines <- strsplit(dataaux, "|", fixed = TRUE)
  # Remove leading and trailing whitespaces from each element
  split_lines <- lapply(split_lines, function(line) trimws(line))
  # Filter out empty lines
  split_lines <- split_lines[sapply(split_lines, function(line) length(line) > 1)]
  # Determine the maximum number of elements in a line
  max_elements <- max(sapply(split_lines, length))
  # Pad lines with fewer elements with NA
  split_lines <- lapply(split_lines, function(line) c(line, rep(NA, max_elements - length(line))))
  # Create a data frame
  data_frame <- as.data.frame(do.call(rbind, split_lines), stringsAsFactors = FALSE)
  # Set column names using the first row
  colnames(data_frame) <- data_frame[1, ]
  # Remove the first row (header)
  data_frame <- data_frame[-1, ]
  # Remove leading and trailing whitespaces in column names
  colnames(data_frame) <- trimws(colnames(data_frame))
  write.csv(data_frame, file = paste0("SEFIS_", i,".csv"), row.names = FALSE)
}
