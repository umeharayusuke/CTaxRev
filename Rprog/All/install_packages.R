cran_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales",
  "patchwork",
  "readr",
  "readxl",
  "tidyverse",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "ggnewscale",
  "svglite"
)

missing_packages <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) install.packages(missing_packages)

if (!requireNamespace("gdxrrw", quietly = TRUE)) {
  message(
    "gdxrrw is also required. Install the version supplied with GAMS, then ",
    "set the GAMS_SYSDIR environment variable to the GAMS system directory."
  )
}
