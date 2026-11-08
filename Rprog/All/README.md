# Figure generation

All R code required to reproduce the paper figures is contained in this
directory. `main_figures.R` contains the complete data processing and plotting
code for Figs. 2-4. `supplementary_figures.R` contains the complete code for
Supplementary Figs. 1-13. The bundled source code does not depend on scripts
elsewhere under `Rprog/`.

Install the packages listed in `install_packages.R`. The GDX input files
require `gdxrrw` and a GAMS installation; set `GAMS_SYSDIR` to the GAMS system
directory when it is not detected automatically.

Run all main and supplementary figures from the project root:

```r
source("Rprog/All/source.R")
```

PNG files are written to `output/All/png/` and SVG files to
`output/All/svg/`.

Files to archive with the code are:

- `source.R`: single entry point for all figures
- `main_figures.R`: complete main-figure code
- `supplementary_figures.R`: complete supplementary-figure code
- `common.R`: shared execution and output functions
- `install_packages.R`: required R package installer
