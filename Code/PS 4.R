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
############################################################################################################################

# Q5 
data <- data %>% 
  mutate(D = case_when(age < 40  ~ 0,
                       age >= 40 ~ 1 ))

data_subset <- data %>% 
  filter(age>35 & age<45)

####5a) The reduced-form effect at the discontinuity in the full sample.

lm_1a <- lm_robust(nonemp ~ D, data = data)
lm_2a <- lm_robust(lwage1 ~ D, data = data)
lm_3a <- lm_robust(jobfind ~ D, data = data)

lm_1a %>% tidy %>% xtable()
lm_2a %>% tidy %>% xtable()
lm_3a %>% tidy %>% xtable()


#### 5b) subsetting data 

lm_1b <- lm_robust(nonemp ~ D, data = data_subset)
lm_2b <- lm_robust(lwage1 ~ D, data = data_subset)
lm_3b <- lm_robust(jobfind ~ D, data = data_subset)

lm_1b %>% tidy %>% xtable()
lm_2b %>% tidy %>% xtable()
lm_3b %>% tidy %>% xtable()


#5c) allow for different slop in running variable before and after cutoff using a 
#linear control for the running variable. We recenter the age variable 

lm_1c <- lm_robust(nonemp ~ D*age + lwage0, data = data)
lm_2c <- lm_robust(lwage1 ~ D*age + lwage0, data = data)
lm_3c <- lm_robust(jobfind ~ D*age + lwage0, data = data)


lm_1c %>% tidy %>% xtable()
lm_2c %>% tidy %>% xtable()
lm_3c %>% tidy %>% xtable()



#5d) allow for different slope in running variable before and after cutoff with fourth-order polynomial

lm_1d <- lm_robust(nonemp ~  D*age + D*I(age^2)+ D*I(age^3)+ D*I(age^4), data = data )
lm_2d <- lm_robust(lwage1 ~  D*age + D*I(age^2)+ D*I(age^3)+ D*I(age^4), data = data )
lm_3d <- lm_robust(jobfind ~ D*age + D*I(age^2)+ D*I(age^3)+ D*I(age^4), data = data ) 


lm_1d %>% tidy %>% xtable()
lm_2d %>% tidy %>% xtable()
lm_3d %>% tidy %>% xtable()

#5e) RDroboust


rdr1 <- rdrobust(y = data$nonemp,
                 x = data$age, c= 40)
rdr2 <- rdrobust(y = data$lwage1,
                 x = data$age, c= 40)
rdr3 <- rdrobust(y = data$jobfind,
                 x = data$age, c= 40)

rdr1 %>% tidy %>% xtable()


