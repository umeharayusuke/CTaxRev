source_candidate <- tryCatch(sys.frame(1)$ofile, error = function(...) NULL)
source_file <- if (length(source_candidate) == 1L && nzchar(source_candidate)) {
  normalizePath(source_candidate, winslash = "/", mustWork = TRUE)
} else {
  NA_character_
}
if (is.na(source_file)) {
  file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_argument)) {
    source_file <- normalizePath(
      sub("^--file=", "", file_argument[[1]]),
      winslash = "/",
      mustWork = TRUE
    )
  } else {
    source_file <- normalizePath(
      file.path(getwd(), "Rprog", "All", "source.R"),
      winslash = "/",
      mustWork = TRUE
    )
  }
}

all_code_dir <- dirname(source_file)
source(file.path(all_code_dir, "common.R"), local = FALSE)
source(file.path(all_code_dir, "main_figures.R"), local = FALSE)
source(file.path(all_code_dir, "supplementary_figures.R"), local = FALSE)

project_root <- find_project_root(all_code_dir)
local_library <- file.path(project_root, "Rlib")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

check_required_packages()
output_dirs <- figure_output_dirs(project_root)

old_dir <- getwd()
on.exit(setwd(old_dir), add = TRUE)
setwd(project_root)

generate_main_figures(project_root, output_dirs)
generate_supplementary_figures(project_root, output_dirs)

message("All main and supplementary figures were created in: ", output_dirs$root)
