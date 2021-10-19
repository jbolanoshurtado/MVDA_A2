#### Libraries #### 
# Set working directory
setwd("~/Documents/Schule & Universität/2020 - PhD/Courses/Multivariate Data Analysis/Assignments/2")

# install latest library version
#install.packages("car")
#install.packages("MASS")
#install.packages("psych")
#install.packages("pheatmap")
#install.packages("stargazer")
#install.packages("foreign")
#install.packages("haven")
#install.packages("nlme")
#install.packages("probemod")
#install.packages("sjPlot")
#install.packages("matrixStats")
#install.packages("robmed")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("tidyverse")

# load libraries
library(car)
library(MASS)
library(psych)
library(pheatmap)
library(stargazer)
library(foreign)
library(haven)
library(nlme)
library(probemod)
library(sjPlot)
library(matrixStats)
library(robmed)
library(dplyr, mask.ok = FALSE)
library(tidyr, mask.ok = FALSE)
library(tidyverse, mask.ok = FALSE)

#### Read file #### 

# read sav from file system
df <- foreign::read.spss('assign2_ModMed&HLM_advertisingrecall(1).sav', to.data.frame=TRUE) #labeled data
# df <- haven::read_spss('assign2_ModMed&HLM_advertisingrecall(1).sav') #key-based data

#### Data types and exploration #### 

df %>% describe()
df %>% summary()

# Check data types
df %>% sapply(typeof)

# Set data types
df$day <- factor(df$day)
df$year <- factor(df$year)
df$prodtype <- factor(df$prodtype)
df$duration.mc <- scale(df$duration, center = TRUE, scale = FALSE)
df$duration.mc <- as.double(df$duration.mc)

# Plots
df$purchase %>% hist()
df$unaided %>% hist() # left skewness
df$aided %>% hist() # right skewness
df$blocksiz %>% hist() # right skewness
df$duration %>% hist() # right skewness
df$duration.mc %>% hist() # right skewness

#### Data filtering and split #### 

# Filter variables of interest
df <- df %>% select(c(commerc, block, purchase, primacy, recency, duration, duration.mc))

# Test Validation Split
set.seed(1337)
groups <- df %>% select(block) %>% distinct(block) %>% rowwise() %>% mutate(group = sample(c("train", "validation"),1,replace = TRUE, prob = c(0.8, 0.2)))
train <- df %>% left_join(groups) %>% filter(group == "train")
validation  <- df %>% left_join(groups) %>% filter(group == "validation")

########### Moderation effect ###############

# Estimates Multi-level model, Intercept-only, without predictors
lme0 <- lme(purchase ~ 1, random = ~ 1 | block, data=df, method="ML")
lme0 %>% summary() 
lme0 %>% VarCorr()
varests <- as.numeric(VarCorr(lme0)[1:2])
varests[1]/sum(varests) # ICC

# Baseline model without moderation effect
lm0 <- lm(purchase ~ primacy + recency + duration, data=df)
lm0 %>% summary()

# Baseline model without moderation effect, duration mean-centered
lm0.mc <- lm(purchase ~ primacy + recency + duration.mc, data=df)
lm0.mc %>% summary()

# Model with moderation effect
lm1 <- lm(purchase ~ duration*primacy + duration*recency + primacy + recency + duration, data=df)
lm1 %>% summary()
lm1 %>% plot_model(typ="int")

# Model with moderation effect, duration mean-centered
lm1.mc <- lm(purchase ~ duration.mc*primacy + duration.mc*recency + primacy + recency + duration.mc, data=df)
lm1.mc %>% summary()
lm1.mc %>% plot_model(typ="int")

# Validation on train/validation split
lm1.train <- lm(purchase ~ duration*primacy + duration*recency + primacy + recency + duration, data=train)
validation$purchase.prediction <- predict(lm1.train, newdata=validation)
cor.test(validation$purchase, validation$purchase.prediction)
cor(validation$purchase, validation$purchase.prediction)**2
lm1.test <- lm(validation$purchase ~ validation$purchase.prediction)
summary(lm1.test) # significant p < 0.001
plot(lm1.test, which = 4) # Cook's distance significantly below 1

#### LaTeX output ####
sink('results.tex', append=FALSE)
cat(stargazer(lme0, lm0, lm1, lm1.test, type="latex",apply.se = TRUE, omit.stat=c("f", "ser", "sig"),intercept.top = T,ci = T, digits=2,model.names = T,single.row = T))
sink()
