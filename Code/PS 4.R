library(foreign)
library(ggplot2)
library(estimatr)
library(rdd)
library(rddensity)
library(rdrobust)

data <- read.dta("data_ps4.dta")

##3a) Plot the density of age at layoff

DCdensity(data$age, cutpoint = 40, bin = 1/3, plot = TRUE)
density <- rddensity(data$age, c = 40)
rdplotdensity(density, data$age)

##3b) Plot the log previous wage against the age at layoff with second order polinomia (should it look different before and after cutoff?) and vertical line at cutoff

rdplot(data$lwage0, data$age, nbins = 60, c = 40, p = 2, col.lines = "red", col.dots = "blue", title = "",
       x.label = "Age at layoff", y.label = "Wage before layoff", y.lim = NULL)

rdplot(data$nonemp, data$age, nbins = 60, c = 40, p = 2, col.lines = "red", col.dots = "blue", title = "",
       x.label = "Age at layoff", y.label = "Nonemployment duration", y.lim = NULL)

rdplot(data$jobfind, data$age, nbins = 60, c = 40, p = 2, col.lines = "red", col.dots = "blue", title = "",
       x.label = "Age at layoff", y.label = "Probability of finding a job within 39 weeks", y.lim = NULL)

rdplot(data$lwage1, data$age, nbins = 60, c = 40, p = 2, col.lines = "red", col.dots = "blue", title = "",
       x.label = "Age at layoff", y.label = "Wage after layoff", y.lim = NULL)
