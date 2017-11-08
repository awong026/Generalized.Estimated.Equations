#Assignment: GEEs
library(gee)
library(geepack)
library(doBy)
library(caret)
#Import data
library(readr)
EG_final <- read_csv("C:/Users/awong/Downloads/EG_final.csv")
View(EG_final)
eg <- EG_final
#################################################################

#Question 1: Binary response death as a function of dose, sex, and weight (wt).

################################################################
summary(eg)
str(eg)
str(eg$Dead) #Should be a factor but is int
str(eg$Dose) #should be a factor but is num
str(eg$Sex)  #Should be a factor but is chr
str(eg$Wt)  #Num
unique(eg$Dead)
unique(eg$Dose)
unique(eg$Sex)
unique(eg$Wt)

#Change variables to factors
eg$Dose <- factor(eg$Dose)
eg$Sex <- factor(eg$Sex)

#Check if they are now factors
is.factor(eg$Dose)
is.factor(eg$Sex)
eg$Sex

##Delete Rows with NA or N for Sex column
eg.fix <- eg[!is.na(eg$Sex),]
eg.fix <- eg.fix[which(eg.fix$Sex != "N"),]
eg.fix$Sex <- factor(eg.fix$Sex)
View(eg.fix)
levels(eg.fix$Sex)

#Change Dam ID to Dam_ID
names(eg.fix)[names(eg.fix) == 'Dam ID'] <- 'Dam_ID'
colnames(eg.fix)


#Create model, Count Data so binomial family, use Dam_ID as id, and corstr as "exchangeable" since is clustered data
fit.exch <- geeglm(Dead ~ Dose + Sex + Wt, family = binomial(link = "logit"), data = eg.fix, id = Dam_ID, corstr = "exchangeable")
summary(fit.exch)

##Sig are Dose .25, Dose .5, and Wt

#Check effect of Sex
fit2 <- geeglm(Dead ~ Dose + Wt, family = binomial(link = "logit"), data = eg.fix, id = Dam_ID, corstr = "exchangeable")

#Use anova to check effect of Sex
anova(fit.exch,fit2) #Since pvalue is .19 we confirm that Sex is not significant

#Model diagnotics 
est <- esticon(fit.exch, diag(6)) ##diag is the number of predictors in the model. 
est #factor 4 and 5 are the only ones where their CI includes zero. This confirms that Dose1 and SexM are not sig
summary(fit.exch) #To see which row goes with which factor


OR.CI <- exp(cbind(est$Estimate, est$Lower, est$Upper))
rownames(OR.CI) <- names(coef(fit.exch))
colnames(OR.CI) <- c("OR", "Lower", "upper")
OR.CI #ONly Dose1 and SexM contain 1, whcih means they don't need to be in model

#Create coonfustion matrix to assess fit of model
predict <- predict(fit.exch, type = "response")
table(eg.fix$Dead, predict>0.5) # Creates a confusion matrix which assess the fit of a model

###lOok into fitureing out % accuracy
(1003+1)/(1003 + 0 + 1 +1)

#Determine the odds ratio for male mice at average weight between dose level 0 
#and the highest dose
esticon(fit.exch,c(0,0,0,-1,0,0))


##########################################################################

#Question 2: Binary response malformation (Mal) as a function of dose, sex and weight.

###########################################################################

#Create model, Count Data so binomial family, use Dam_ID as id, and corstr as "exchangeable" since is clustered data
fit.q2 <- geeglm(Mal ~ Dose + Sex + Wt, family = binomial(link = "logit"), data = eg.fix, id = Dam_ID, corstr = "exchangeable")
summary(fit.q2)

#Dose.25, Dose .5, Dose 1, and Wt are sig

#Check effect of Sex
fit.q22 <- geeglm(Mal ~ Dose + Wt, family = binomial(link = "logit"), data = eg.fix, id = Dam_ID, corstr = "exchangeable")

#Use anova to check effect of Sex
anova(fit.q2,fit.q22) #Since pvalue is .59 we confirm that Sex is not significant


#Model diagnotics 
est <- esticon(fit.q2, diag(6)) ##diag is the number of predictors in the model. 
est #factor 5 is the only one where its CI includes zero. This confirms that SexM is not sig
summary(fit.q2) #To see which row goes with which factor


OR.CI <- exp(cbind(est$Estimate, est$Lower, est$Upper))
rownames(OR.CI) <- names(coef(fit.q2))
colnames(OR.CI) <- c("OR", "Lower", "upper")
OR.CI #Only SexM contain 1, whcih means it doesn't need to be in model

#Create coonfustion matrix to assess fit of model
predict <- predict(fit.q2, type = "response")
table(eg.fix$Mal, predict>0.5) # Creates a confusion matrix which assess the fit of a model
#Not as good
###lOok into fitureing out % accuracy
(628 + 146)/(628 + 88 + 143 + 146)


#False positives:
(88)/(628 + 88)

#Determine the odds ratio for male mice at average weight between dose level 0 
#and the highest dose
esticon(fit.q2,c(0,0,0,-1,0,0))

#######################################################

#Question 3: Continuous response weight as a function of malformation, 
#dose, sex and horn (position in the uterine horn: variablename left).

###########################################################

fit.n <- geeglm(Wt ~ factor(Mal) + Dose + Sex + factor(left),  data = eg.fix, id = Dam_ID, corstr = "exchangeable")
summary(fit.n)

#Check if response variable is normal dist. 
qqnorm(eg.fix$Wt) #Response is normal, the other variables are factors so can't tell if they are normal dist or not. 
#I think it's okay to use family = identity then for linear model. 

predict <- predict(fit.n, type = "response")
table(eg.fix$Wt, predict>0.5) 
anova(fit.n)
