#load libraries
library(geepack) ## for gee function
library(MuMIn) ## for dredge function
library(usdm) ## for vifcor function
library(QICpack) ## for qictab function
##################################################
## read all the data files
dt1 <- read.csv("hiv_law_formatted.csv", header=TRUE)
str(dt1)
dt1 <- dt1[,c("state", "year", "criminal_law", "criminal_law_detailed")]

dt2 <- read.csv("percent_diagnosis.csv", header=TRUE)
str(dt2)
dt2 <- dt2[,c("state", "year" , "plhiv", "percent_diagnosed_HIV")]

dt3 <- read.csv("education.csv", header=TRUE)
dt3$uneducated <- 100 - dt3$education_percentage
head(dt3)

dt4 <- read.csv("median_income.csv", header=TRUE)
head(dt4)

dt5 <- read.csv("poverty.csv", header=TRUE)
head(dt5)

dt6 <- read.csv("unemployment_rate.csv", header=TRUE)
head(dt6)

dt7 <- read.csv("urban_residence.csv", header=TRUE)
head(dt7)

dt8 <- read.csv("race_formatted.csv", header=TRUE)
head(dt8)

dt9 <- read.csv("population.csv", header=TRUE)
head(dt9)
##################################################
## merge the data files into a single dataframe
df <- merge(dt1, dt2,  by=c("state", "year"))
df <- merge(df, dt3,  by=c("state", "year"))
df <- merge(df, dt4,  by=c("state", "year"))
df <- merge(df, dt5,  by=c("state", "year"))
df <- merge(df, dt6,  by=c("state", "year"))
df <- merge(df, dt7,  by=c("state", "year"))
df <- merge(df, dt8,  by=c("state", "year"))
df <- merge(df, dt9,  by=c("state", "year"))
####################################################
df$year <- as.factor(df$year)
df <- na.omit(df) # remove rows with missing information
options(na.action = "na.fail")
##########################################################
## All predictors are centered and scaled to unit variances to assign same prior importance 
# to each predictor in the analysis
df$cpopulation <- scale(df$population, center=TRUE, scale=TRUE)
df$cuneducated <- scale(df$uneducated, center=TRUE, scale=TRUE)
df$cplhiv <- scale(df$plhiv, center=TRUE, scale=TRUE)
df$cmedian_income <- scale(df$median_income, center=TRUE, scale=TRUE)
df$cpoverty_percent<- scale(df$poverty_percent, center=TRUE, scale=TRUE)
df$cunemployment_rate <- scale(df$unemployment_rate, center=TRUE, scale=TRUE)
df$curban_residence_percentage <- scale(df$urban_residence_percentage, center=TRUE, scale=TRUE)
df$cpercent.Black <- scale(df$percentage.Black, center=TRUE, scale=TRUE)
df$cpercent.White <- scale(df$percentage.White, center=TRUE, scale=TRUE)
df$cpercent.Hispanic <- scale(df$percentage.Hispanic, center=TRUE, scale=TRUE)

##########################################################################
##calculate total number of HIV diagnoses
df$diagnosed_HIV <- round((df$percent_diagnosed_HIV *df$plhiv)/100)
##############################################################################
## check multi-collinearity between the predictors
df.pred <- df[,c("cpopulation","cuneducated", "cmedian_income", "cpoverty_percent", "cunemployment_rate",
                 "curban_residence_percentage", "cpercent.Black", "cpercent.White", "cpercent.Hispanic")]
vifcor(df.pred) ## VIF < 10 for all predictors. All predictors are, therefore, included in the full model
##################################################################################
## check two formulations for the full model. Model a includes the state population
## size as a predictor. Model b sets the state population size as an offset

full.model.a <- geeglm(diagnosed_HIV ~   criminal_law + cmedian_income + cunemployment_rate+ cpopulation+
                  cuneducated + curban_residence_percentage +  cpoverty_percent+
                  cpercent.Hispanic+ cpercent.Black + cpercent.White +cplhiv,  
                data=df, poisson(link=log),
                id=state, corstr = "ar1")

full.model.b <- geeglm(diagnosed_HIV ~  criminal_law + cmedian_income + cunemployment_rate+ cpopulation+
                         cuneducated +   curban_residence_percentage +  cpoverty_percent+
                         cpercent.Hispanic+ cpercent.Black + cpercent.White , 
                data=df, poisson(link=log), offset=(log(plhiv)),
                id=state, corstr = "ar1")

##################################################################################
###compare quasilikelihood information criterion (QIC)
model.set <- list(full.model.a, full.model.b)
mod.names <- c("modela", "modelb")
qictab(model.set, mod.names) # model b has the highest support

summary(full.model.b) # print summary of the full model
##################################################################################
## perform ANOVA analysis for the effect of HIV criminal law

full.model.b.reduced <- geeglm(diagnosed_HIV ~  cmedian_income + cunemployment_rate+ cpopulation+
                         cuneducated +   curban_residence_percentage +  cpoverty_percent+
                         cpercent.Hispanic+ cpercent.Black + cpercent.White , 
                       data=df, poisson(link=log), offset=(log(plhiv)),
                       id=state, corstr = "ar1")

anova(full.model.b, full.model.b.reduced)
################################################################################
################################################################################
# find the subset of predictors that best explain the response variable.  

dredge.full.model <- dredge(full.model.b, rank = "QIC")
subset(dredge.full.model, delta < 50)  
##########################################################################################
##best subset model is the one with lowest QIC

subset.model <- geeglm(diagnosed_HIV ~  criminal_law + cmedian_income + cunemployment_rate+ cpopulation+
                                        cuneducated +   cpoverty_percent+
                                        cpercent.Hispanic+ cpercent.Black + cpercent.White , 
                                      data=df, poisson(link=log), offset=(log(plhiv)),
                                      id=state, corstr = "ar1")

summary(subset.model) # print summary
####################################################################################
## anova analysis
subset.model.reduced <- geeglm(diagnosed_HIV ~  cmedian_income + cunemployment_rate+ cpopulation+
                         cuneducated +   cpoverty_percent+
                         cpercent.Hispanic+ cpercent.Black + cpercent.White , 
                       data=df, poisson(link=log), offset=(log(plhiv)),
                       id=state, corstr = "ar1")


anova(subset.model, subset.model.reduced)
#################################################################################
