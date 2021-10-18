# Clean and set working directory
rm(list=ls())
setwd('C:\Users\Jean Pierre\Documents\0-Local Workstation\0-RUG\0-courses\1a courses\Multivariate\Assignments\Assignment2\R_workspace\R_workspace')

set.seed(2021) # to reproduce results when using random numbers


#install.packages("probemod")
#install.packages("robmed")
library(robmed)
# install.packages("nlme")
# install.packages("haven")
library(nlme)
library(haven)
#install.packages("lavaan") 
#install.packages("psych")
#install.packages("ProjectTemplate")
library(lavaan) #SEM, GSEM, and MLSEM (HSEM), LGCSEM
library(moments) # to assess skewness and kurtosis
library(Hmisc) # to use the describe fx
library(pastecs) # to test for normality w/ shapiro-wilk
library(olsrr) # to calculate Mallow's Cp (not used after all)
library(car) # durbin-watson test for autocorrelation of errors + VIF
library(MASS)
##To replicate what the robmed package does
#install.packages("robustbase")
library(robustbase)


### Load the data base
db0 <- read_sav("C:\\Users\\Jean Pierre\\Documents\\0-Local Workstation\\0-RUG\\0-Courses\\1a courses\\Multivariate\\Assignments\\Assignment2\\R_workspace\\R_workspace\\assign2_ModMed&HLM_advertisingrecall(1).sav")



### preserve all output numbers as decimals
options(scipen = 10)


# Checking ranges and distribution of variables

stat.desc(db0 , norm = TRUE , p=0.95)

head(db0)

plot(db0$unaided, db0$aided)
abline(a=0,b=1) # unaided is always lower than aided
# Idea: the difference between unaided and aided is that the consumer was exposed 
# to a directed stimulus. Then, this can measure an additional effect
# which would be the marginal sensibility of recalling the brand once a small
# recall stimulus has been received (think of going to the supermarket to buy
# dish washing soap and being exposed to a below-the-line ad at the store, then
# this measurement could be a proxy to this effect)

db0$marginal_recall <- db0$aided - db0$unaided 

plot(db0$unaided, db0$marginal_recall)
cor(db0$unaided, db0$marginal_recall) # The more well known the brand, the less marginal recall it needs and has)

# Code to get the univariate descriptive statistics and Pearson correlations table (polychoric or spearman could also be used)
stat.desc(db0 , norm = TRUE , p=0.95)
cor(db0)

#Frequencies for categorical variables (nonsensical univariate statistics: year, day of week, product type)
table(db0$year)
table(db0$day)
table(db0$prodtype)
#Get dummy variables for all of them
db0$year.f      <- factor(db0$year)
db0$day.f       <- factor(db0$day)
db0$prodtype.f  <- factor(db0$prodtype)



# Get the ICC for dep vars: purchase intention and unaided recall
##  For purchase as dependent var
hlm0_purchase <- lme(purchase ~ 1 , random = ~ 1 | block, # pur. int. for product of commercial nested in blocks
                     data = db0, method = "ML")
mean(db0$purchase) # average is 27.54%
summary(hlm0_purchase) # corrected average is 27.55
VarCorr(hlm0_purchase)
hlm0_purchase.varests <- as.numeric(VarCorr(hlm0_purchase)[1:2])
hlm0_purchase.ICC <- hlm0_purchase.varests[1] / sum(hlm0_purchase.varests)
hlm0_purchase.ICC # ICC of 0.42 % Not enough!

##  For unaided recall as dependent var
hlm0_unaided  <- lme(unaided ~ 1 , random = ~ 1 | block, # unaid. recall for each commercial nested in blocks
                    data = db0, method = "ML")
mean(db0$unaided) # mean is 25.65%
summary(hlm0_unaided) # corrected mean is 25.95%
VarCorr(hlm0_unaided)
hlm0_unaided.varests <- as.numeric(VarCorr(hlm0_unaided)[1:2])
hlm0_unaided.ICC <- hlm0_unaided.varests[1] / sum(hlm0_unaided.varests)
hlm0_unaided.ICC




## Mixed model 1: purchase intention with random intercept @ block level

# Only controls
hlm1_purchase.controls <- lme(purchase ~ 1 + day.f + year.f + prodtype.f, 
                              random = ~ 1 | block,
                              data = db0, method = "ML")
summary(hlm1_purchase.controls)

# Full model
hlm1_purchase.full <- lme(purchase ~ 1 + day.f + year.f + prodtype.f +
                             duration + unaided + marginal_recall + primacy + recency, 
                             random = ~ 1 | block,
                             data = db0, method = "ML")
summary(hlm1_purchase.full)

### Calculating R2
## Between Blocks (LVL2)
#********************
# Controls model
VarCorr(hlm1_purchase.controls)
hlm1_purchase.controls.Var <- as.numeric(VarCorr(hlm1_purchase.controls)[1:2])
hlm1_purchase.controls.R2btwBlocks <- ((hlm0_purchase.varests[1] - hlm1_purchase.controls.Var[1]) / hlm0_purchase.varests[1] )
hlm1_purchase.controls.R2btwBlocks # 99.9\% R2 Berween blocks for the control
hlm1_purchase.controls.Var[1]
hlm0_purchase.varests[1]

#***************************
# Full model
VarCorr(hlm1_purchase.full)
hlm1_purchase.full.Var <- as.numeric(VarCorr(hlm1_purchase.full)[1:2])
hlm1_purchase.full.R2btwBlocks <- ((hlm0_purchase.varests[1] - hlm1_purchase.full.Var[1]) / hlm0_purchase.varests[1] )
hlm1_purchase.full.R2btwBlocks # 99.9\% R2 Berween blocks for the full model
hlm1_purchase.full.Var[1]
hlm0_purchase.varests[1]

## Between commercials (LVL1)
#*******************
# Controls model
hlm1_purchase.controls.R2btwComms <- ((hlm0_purchase.varests[2] - hlm1_purchase.controls.Var[2]) / hlm0_purchase.varests[2] )
hlm1_purchase.controls.R2btwComms # 0.9\% R2
hlm1_purchase.controls.Var[2]
hlm0_purchase.varests[2]


#***************************
# Full model
hlm1_purchase.full.R2btwComms <- ((hlm0_purchase.varests[2] - hlm1_purchase.full.Var[2]) / hlm0_purchase.varests[2] )
hlm1_purchase.full.R2btwComms # 12.7\% R2 Between blocks for the full model
hlm1_purchase.full.Var[2]
hlm0_purchase.varests[2]


#**********************
#Mediation Analysis
# (I)
# three step approach from Baron and Kenny (1986)
lmzx <- lm(unaided ~ duration, data = db0) # 1.- Regress Z on X
summary(lmzx) ## beta: 0.64 , sig.: <.001

lmyz <- lm(purchase ~ unaided, data = db0) # 2.- Regress Y on Z
summary(lmyz) ## beta: 0.37 , sig.: <.001

lmyx <- lm(purchase ~ duration, data = db0) # 3.- Regress Y on X
summary(lmyx) ## beta: 0.28 , sig.: <.001

# (II)
# Process (not by Hayes) mediation analysis
process_purchase <- test_mediation(db0 , x = "duration" , y = "purchase" , 
                                   m = "unaided" , covariates = c(10,11,14,16,17) ,
                                   test = "sobel")
 
summary(process_purchase) ## Not converging when including year effect dummies. 
# getting the residual variance of unaided recall in the mediation analysis with OLS
lm_med_rob <- lmrob(unaided ~ 1 + duration + primacy + recency + 
                                marginal_recall + day.f + prodtype.f  
                                , data = db0)
summary(lm_med_rob) # Same results
res_med_rob <- resid(lm_med_rob)
stat.desc(res_med_rob) ## variance of the residuals is 


# Degree of mediation with robmed
dm <- (0.37126 * 0.60624) / (0.07282 + (0.37126 * 0.60624))
dm ## 75.56%

# From the output in Stata (ML-GSEM)
dm_sem <- (0.6443917*0.3623856) / (0.0743871 + (0.6443917*0.3623856))
dm_sem ## 75.84








