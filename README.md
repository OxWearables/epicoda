# epicoda

![R-CMD-check](https://github.com/activityMonitoring/epicoda/workflows/R-CMD-check/badge.svg) ![Windows/MacOS/Linux](https://github.com/activityMonitoring/epicoda/workflows/Windows/MacOS/Linux/badge.svg)
[![codecov](https://codecov.io/gh/activityMonitoring/epicoda/branch/master/graph/badge.svg?token=pUfd7QVqRe)](https://codecov.io/gh/activityMonitoring/epicoda)

Please note that: 
- This is the in-development version and major changes and corrections may be made 
- The package is shared as-is to enable scientific reproducibility and 'open science' 
- The package is not shared as an authoritative source
- We do not currently have capacity for extensive code review and testing
- We recommend sense-checking outputs and where possible verifying using alternative software
- We only routinely use a small proportion of the package's functionality, so do not assume `obvious' bugs will be spotted and fixed (this was the case in bug fix 9ca0185d)
- The package is likely to be breakable with non-standard cases (e.g. 'NULL' or 'NA' values)
- **Use of the package is strictly at your own risk**. 

Please share comments, suggestions and errors/bugs found, either directly on the GitHub page or by emailing rosemary.walmsley@gtc.ox.ac.uk.* 

We are actively seeking review of the code - if you are able to provide feedback, we would love to hear from you (either on GitHub or at rosemary.walmsley@gtc.ox.ac.uk). If you would like to co-develop the package, please also get in touch.

## What is `epicoda`? 
`epicoda` is an R package designed to support epidemiological analyses using compositional exposure variables. It provides wrappers for common epidemiological use cases. Simulated data (`simdata`) can be used to try out the functions, and a vignette illustrates the steps to carrying out an epidemiological analysis with a Compositional Data Analysis approach to the exposure. 

## Getting started
To install the `epicoda` package from GitHub:
```{r}
install.packages("devtools") # To install epicoda from GitHub, the devtools package is required.  
library(devtools)
devtools::install_github("activityMonitoring/epicoda",  build_opts = c("--no-resave-data"), build_vignettes = TRUE, build_manual = TRUE)
```
`epicoda` can now be loaded as a normal package in R using: 
```{r}
library(epicoda)
```
## How can `epicoda` be used? 
To see examples of what the package can do, see the vignette (long form documentation with code and text). This uses an example analysis to illustrate how the package can be used. To view it, run:  
```{r}
vignette("vignette-epicoda")
```
## Troubleshooting 
This is the in-development version - please get in touch with any feedback or problems on this page, or by emailing rosemary.walmsley@gtc.ox.ac.uk. 

Some issues we are aware of: 
- The package was written using base R syntax with data formatted as a data frame. Some functions can fail if data is formatted as a tibble (as per tidyverse and [some R books](https://r4ds.had.co.nz/) ) - and it may not be obvious from the error message that this is the cause. [A simple fix](https://r4ds.had.co.nz/tibbles.html#interacting-with-older-code) is to reformat the data as a data frame before analysis (`as.data.frame(my_data)`).
- There is one issue where a conflict between dependency packages can lead to plots not displaying axis labels. The current settings should avoid this, but if it does affect you, it would be really useful to know. 

## Citing this package
If you use this package, please cite:
```
[Walmsley2021] Walmsley R, Chan S, Smith-Byrne K, et al Reallocation of time between device-measured movement behaviours and risk of incident cardiovascular disease
 British Journal of Sports Medicine Published Online First: 06 September 2021. doi: 10.1136/bjsports-2021-104050
```
