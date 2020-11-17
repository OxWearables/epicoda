# epicoda

*This is the in-development version. Please share comments, suggestions and errors/bugs found, either directly on the github page or by emailing rosemary.walmsley@gtc.ox.ac.uk*. 

## What is `epicoda`? 
`epicoda` is an R package designed to support epidemiological analyses using compositional exposure variables. It provides wrappers for common epidemiological use cases. Simulated data (`simdata`) can be used to try out the functions, and a vignette (`vignette-epicoda.Rmd`) illustrates the steps to carrying out an epidemiological analysis with a Compositional Data Analysis approach to the exposure. 

## Getting started
To install and run functions from `epicoda`, download it as a subfolder of the R project/folder where you will use it, and run in your R script: 
```{r}
install.packages("devtools")
library(devtools)
devtools::load_all("epicoda")
```
## How can `epicoda` be used? 
To see examples of what the package can do, see the vignette (long form documentation with code and text). This uses an example analysis to illustrate how the package can be used. If you're using RStudio, to view it open 'epicoda/vignettes/vignette-epicoda.Rmd' and click `Knit`. 

## Citing this package
If you use this package, please cite:
```
[Walmsley2020] Walmsley R, Chan S, et al. (2020)
Reallocating time from machine-learned sleep, sedentary behaviour or light 
physical activity to moderate-to-vigorous physical activity is associated with 
lower cardiovascular disease risk (preprint https://doi.org/10.1101/2020.11.10.20227769)
```
