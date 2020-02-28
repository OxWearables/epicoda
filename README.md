# epicoda

*This is the in-development version. Please share comments, suggestions and errors/bugs found, either directly on the github page or by emailing rosemary.walmsley@gtc.ox.ac.uk*. 

This package is designed to support epidemiological analyses using compositional exposure variables. It provides wrappers for common epidemiological use cases. Simulated data, alongside a vignette, illustrates the steps to carrying out an epidemiological analysis with a Compositional Data Analysis approach to the exposure. 

To install and run functions from epicoda, download it as a subfolder of the R project/folder where you will use it, and run in your R script: 
```{r}
install.packages("devtools")
library(devtools)
devtools::load_all("epicoda")
```
The package contains simulated data (`simdata`), so all the functions can be tried without any additional data.  

To view long-form documentation: 
* An example Compositional Data Analysis: Go to 'epicoda/vignettes/vignette-epicoda.Rmd' and knit this file. 
* Documentation of the confidence intervals used: Go to 'epicoda/vignettes/derivation_of_CIs_used.Rmd' and knit this file. 
