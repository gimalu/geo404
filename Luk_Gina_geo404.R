## ----GlobalOptions, echo=FALSE------------------------------------------------
options(knitr.duplicate.label = 'allow')


## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=FALSE---------------------------------------------------------------
# This chunk automatically generates a file .R version of this script when running within knitr.
input  = knitr::current_input()  # filename of input document
output = paste(tools::file_path_sans_ext(input), 'R', sep = '.')
knitr::purl(input,output,documentation=1,quiet=T)


## ----cars---------------------------------------------------------------------
summary(cars)


## ----pressure, echo=FALSE-----------------------------------------------------
plot(pressure)

