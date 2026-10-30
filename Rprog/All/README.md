# Figure generation

Install the packages listed in `install_packages.R`. The GDX input files also
require `gdxrrw` and a GAMS installation; set `GAMS_SYSDIR` when GAMS is not on
the default path.

Run all main and supplementary figures from the project root:

```r
source("Rprog/All/source.R")
```

PNG files are written to `output/All/png/` and SVG files to
`output/All/svg/`.
