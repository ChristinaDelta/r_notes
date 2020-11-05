# import packages:
library(dplyr)
library(mosaic)
library(ggplot2)
library(plotly)
library(psych)
library(lme4)
library(lmerTest)
library(ez)
library(car)

# load the discriminability data
data = read.csv("Desktop/models_for_regression/discriminability_behav/averaged_dt_rts.csv", 
                header = TRUE)

# factor animacy
data$animacy = factor(data$animacy,
                      levels = c("1", "2", "3"),
                      labels = c("1", "2", "3"))

table(data$animacy)
# factor pair condition
data$cond = factor(data$cond,
                   levels = c("1", "2"),
                   labels = c("1", "2"))

summary(data)
# calculate means etc..
# grand mean 
grand.mean = mean(data$rt)

# means of rt for the 2 levels of the factor pair 
pair.mean = with(data, tapply(rt, cond, mean))

# means of rt for the 23levels of the factor animacy
animacy.mean = with(data, tapply(rt, animacy, mean))

# means of rt for the 6 combinations of the 2 factors (2x3)
combs.mean = with(data, tapply(rt, list(cond,animacy), mean))

# difference between levels of pair condition
d.pair = diff(pair.mean)

# difference between successive levels of animacy condition
d.animacy = diff(animacy.mean)

# differences between levels of pair  at each level of animacy
d.pair.animacy = diff(combs.mean)

# ANLALISYS WITH A SINGLE FACTOR, TREATMENT MODEL:
# one-way anova with factor condition
data$e2 = (data$rt - grand.mean)^2  # SS TOTAL
data$pairmean = pair.mean[data$cond] # mean for pairs same and different
data$within = (data$rt - data$pairmean)^2 # SS WITHIN

data$between = (data$pairmean - grand.mean)^2 # SS BETWEEN

ss = apply(data[,c("e2", "within", "between")],2,sum) # sum of squares total, within, and between
df.total = nrow(data)-1 # total degrees of freedom
df.between = nlevels(data$cond) -1 # df between
df.within = df.total - df.between  # df within

f.stat = (ss["between"]/df.between) / (ss["within"]/df.within) # F stat = MS BETWEEN/ MS WITHIN
p.val = 1.0 - pf(f.stat, df.between, df.within) # p value

f.stat = unname(f.stat)

# let's run an anova directly:
a1 = aov(rt ~ cond, data)
summary(a1)

# though linear regression and Anova function:
model1 = lm(rt ~ cond, data)
summary(model1)
anova1 = Anova(model1, type = 3)
summary(anova1)

# so here, the effect of pair condition (the beta for condition2) is the differnce between same/different
coef(model1)["cond2"]
d.pair # diff between same and different pairs means

# the intercept is the mean for "same"
coef(model1)["(Intercept)"]
pair.mean[1] 

# so intercept + condition2 is the mean value for different objcts pairs
coef(model1)["(Intercept)"] + coef(model1)["condition2"]
pair.mean[2]

# the significance of this effect is the same as a t-test between the 2 levels:
sqrt(summary(a1)[[1]]["cond", "F value"])  # F is t squared
summary(a1)[[1]]["cond", "Pr(>F)"] # pvalue


coef(summary(model1))["cond2", "t value"]
coef(summary(model1))["cond2", "Pr(>|t|)"]

# test with a t test
t.test(data$rt[data$cond == "1"], data$rt[data$cond == "2"])

# THAT WAS THE TREATMENT CODING CONTRASTS
contr.treatment(2) # 0 = same objects, 1= different objects

# the model matrix that coresponds to that coding is:
model.matrix(~cond, expand.grid(cond=c("1", "2")))

# rows are factor leves and columns are coefficients
# the first row is 1 for the intercept and 0 for the mdoel variable cond (same)for the factor cond 
# (so this corresponds to the value of rt for cond = same)
# the 2nd row has intercept = 1 and model cond variable = 1 ( different). This corresponds to the sum of the value
# for same + the difference between same and diffrent ([same mean] + [difference of same & different means])

# ANALYSIS WITH TWO-LEVEL FACTOR - DEVIATION CODED
# let's run a linear model with different coding of the contrasts:
model2 = lm(rt ~ cond, data = data, contrasts = list(cond = contr.sum))
summary(model2)
anova(model2)

# the F statistic is very similar to the model1 the t-stats also make sense. Of course this is because we are doing the
# same thing here as above.. testing the effect of condition pair and distinguishing "same" and "different" pairs
# WHAT ABOUT THE COEFFICIENTS??

# The intercept here is the grand mean
coef(model2)["(Intercept)"] # or it should be (if the samples of the 2 levels were equal?)
grand.mean

# the effect of condition called "cond1" is the 1st contrast on condition.. it should be half of the difference 
# between same and different pairs
coef(model2)["cond1"]

# for comparison
-d.pair/2
pair.mean["1"] - grand.mean # this is not consistent 
grand.mean - pair.mean["2"] # also not consistent. 

# But generally same = intercept + cond1 different = intercept - cond1
coef(model2)["(Intercept)"] + coef(model2)["cond1"] # mean for same object pairs
coef(model2)["(Intercept)"] - coef(model2)["cond1"] # mean for different object pairs

# this is consistent... but where does it go wrong? The intercept is wrong here but the beta (for the cond1) is correct
# In deviation coding of contrasts the intercept corresponds to the grand mean.
# The intercept is what we get when all regressors are equal to zero. Here we have -1 for different and 1 for same 
# pairs, so normally it should be zero for their average which should be equally distant from both. So the intercept is
# actually implied by the contrast. As soon as we define the variable(s) coding the factor levels, the intercept is 
# whatever we get when all variables are equal to zero. 
contr.sum(2)

# here is the coresponding matrix for the deviation coding:
model.matrix(~cond, expand.grid(cond=c("1", "2")), contrasts = list(cond = contr.sum))

# the columns correspond the coeeficients and the lines to the two cases examined 
# 1. the first column is the intercept -- which should be equal to the mean between the two levels
# 2. the second column is the model effect variable, taling values -1,1 fro teh two levels:
# .. so for the "same" effect we get [grand.mean + 1] for the beta
# and for "different" we get [grand.mean - 1] for the beta

# what significance are we testing here?
# 1. for the grand mean (thesting if the mean of rt differs from 0)
# 2. for the ebta coef (testing if the half of the difference between "same" and "different" differes from 0)

# what if we wanted "different" as the reference and "same" as +1?
# we can make our own contrast:
model2a = lm(rt ~ cond, data = data, contrasts=list(cond=c(-1,1)))
summary(model2a) # same as model2 exept for the sign of cond1 effects

# compare the contrasts:
model2$contrasts
model2a$contrasts

# only the sign changes everything else is the same

#### ANALYSIS WITH 3-LEVEL FACTOR -- TREATMENT CODED ####
# Factor: Animacy, Levels: animate, both, inanimate
model3 = lm(rt ~ animacy, data = data)
anova(model3)

# compare with anova function of the car package
anova2 = Anova(model3, type = 3)
anova2
# and with the aov function:
summary(aov(rt ~ animacy, data = data))

# The F statistic remains the same across all models, but what about the coefficients?
summary(model3)

# the intercept is the mean rt at the reference level (here animate objects) by default
coef(model3)["(Intercept)"]
animacy.mean[1] 

# the sond coef is for  the both level, that is the difference between both and animate pairs (difference between
# level 2 and level 1 of the factor)
coef(model3)["animacy2"]
d.animacy[1] # for comparison

# the third coef is for the inanimate level, the difference between inanimate and animate pairs
coef(model3)["animacy3"]
animacy.mean[3] - animacy.mean[1] # for comparison

# so this model tests for significance:
# 1. the value at animate pairs
# the difference between the animate and both pairs
# the difference between the animate and inanimate pairs
# the difference between both and inanimates is not tested so we don't know if that differs from 0
# Why? 
# when we have 3 levels we can have only 2 independent comparisons. This is the default contrast coding:
contr.treatment(3)

# 2 dummy variables code the 3 level factor:
# 1. one variable codes the level "both" when it's tested (so this level is 1 whenever tested and 0 otherwise)
# 2. the other variable code the level "inanimates" (again, this level is set to 1 when tested and 0 otherwise)
# when both are set to 0 (1st row) then what's left is the "animate" level which is the intercept
# The corresponding model matrix:
model.matrix(~animacy, expand.grid(animacy = c("1", "2", "3")))
# here the 1st row is the intercept (all else is set to 0)
# the second row is the intercept + both
# the third is the intercept + inanimates

## 3-level factor, deviation coding:
model4 = lm(rt ~ animacy, data = data, contrasts = list(animacy=contr.sum))
summary(model4)
anova(model4)
anova3 = Anova(model4, type = 3)

# few things to notice when comparing these models to model3 is that the intercept is different, but the statistics 
# and coefficients don't change
# again we have 3 effects tested against 0:
# the intercept
coef(model4)["(Intercept)"] # it corresponds to the grand mean
grand.mean # slightly different

# what are the coeff 2 and 3?
coef(model4)["animacy1"]
coef(model4)["animacy2"]

# lets examine the contrasts:
contr.sum(3)

# here the 1st column shows that: 1=animate, 0=both and -1=inanimate
# the 2nd columns shows that: 0=animate, 1=both, -1=inanimate
# so..
# animate is coded like: animacy1 = 1 & animacy2 = 0
# both is coded like: animacy1 =0 & animacy2=1
# inanimate coded like: animacy1 = -1, animacy2= -1
# So two variable distinguish the three levels of the factor
# so with this combinations of contrasts:
# animacy1 contains the difference between animate and the intercept (grand mean)
# animacy2 contains the difference between both and intercept, so that animacy1+animacy2 refer to the difference
# between the intercept and inanimates (animacy3)

# let's test it:
animacy.mean[1] - grand.mean # should be equal to coef animacy1.. not quite..
animacy.mean[2] - grand.mean # not equal to coef animacy2
grand.mean - animacy.mean[3]

# look at the matrix of contrasts:
model.matrix(~animacy, expand.grid(animacy=c("1", "2", "3")), contrasts = list(animacy=contr.sum))

# two things to remember:
# 1. Intercept is the grand mean (in deviation coding) or the reference level (in treatment coding)
# 2. columns are model variables and rows are factor levels

### ANALYSIS WITH 2 FACTORS ####
# pair condition * animacy (2x3)

model5 = lm(rt ~ cond*animacy, data=data)
anova(model5)
anova4 = Anova(model5, type = 3, singular.ok = TRUE)

# the output of the 1st anova should be adentical to:
summary(aov(rt ~ cond*animacy, data = data))

# The F statistic on the last line here refers to the variance reduction by all model terms together, 
# that is, all sums of squares from the ANOVA table. Add all the SS and df to verify:
SSB=sum(summary(aov(rt~cond*animacy,data))[[1]][1:3,"Sum Sq"])/sum(summary(aov(rt~cond*animacy,data))[[1]][1:3,"Df"])
SSW=summary(aov(rt~cond*animacy,data))[[1]]["Residuals","Sum Sq"] / 
  summary(aov(rt~cond*animacy,data))[[1]]["Residuals","Df"]

SSB/SSW

summary(model5)$fstatistic["value"] # for comparison

# intercept here is the value at the reference levels of the 2 factors (cond=same, animacy=animate)
coef(model5)["(Intercept)"]

combs.mean[1,1] # works

# the first beta "cond2" is the difference between same and diff (a cond contrast) for animacy=animate (reference)
coef(model5)["cond2"]

# check it:
combs.mean[2,1] - combs.mean[1,1]

# second beta (animacy2) is the diff between both and animates ( that doesn't exist) for cond=same
coef(model5)["animacy2"]
combs.mean[1,2] - combs.mean[1,1] # THIS IS NOT CORRECT

# the 3rd beta (animacy3) is the difference between inanimate and both for cond=same
coef(model5)["animacy3"]
combs.mean[1,3] - combs.mean[1,1] # CORRECT

# the 4th beta (cond2:animacy2) is the difference bewteen both and animates for cond = different over the diff
# between animates-both for cond=same
coef(model5)["cond2:animacy2"]
(combs.mean[2,2] - combs.mean[2,1]) - (combs.mean[1,2] - combs.mean[1,1]) # not applicable.. this is because of the
# unequal/unbalanced observations between conditions

# the 5th beta is the difference bewteen inanimate and animate for cond= different, over the diff between 
# animate-inanimate levels for cond=same 
(combs.mean[2,3] - combs.mean[2,1]) - (combs.mean[1,3] - combs.mean[1,1]) # consistent

# in summary:
# 1. the pair condition effect is whatever difference caused by same/different not accounted for by the grand mean
# 2. the animacy condition effect is whatever difference caused by animate/both/inanimate not 
# accounted for by the grand mean
# 3. the connd:animacy effects are whatever differences caused by combinations of the two factors not accounted for 
# by the individual effects of cond and animacy

# let's take a look at the interaction:
model5a = lm(rt ~ cond + animacy, data)
anova(model5, model5a)

# let's take a look at the contrasts of the interaction
f = expand.grid(cond=c("1","2"),animacy=c("1","2","3")) ### #THIS IS IMPORTANT

model.matrix(~cond*animacy,f)
# row 1 = intercept
# row 2 = intercept + diff between intercept and different = different at animacy=animates
# row 3 = intercept + diff between intercept and both = both at cond = same
# and so on..
# in the above contrasts coding what we are looking for is:
# if both is different from animate when cond=same
# if there is difference between both and animate when cond=same and cond=different. But we don't know if there
# are differences animates and both for cond=different or if there is a difference between animates and both 
# in general

# LET'S TRY DEVIATION CODING + TREATMENT CODING (MIXING THE CONTRASTS CODINGS)
# here one factor will be treatment coded and the other deviation coded:
model6 = lm(rt ~ cond*animacy, data, contrasts=list(cond=contr.sum))
summary(model6)
anova(model6)
anova5 = Anova(model6, type = 3, singular.ok = TRUE)
anova5

# because we have cont.sum for the pair condition factor, it contributes to the intercept at the mean
# contr.treatment is used for animacy condition and thus this factor contrubutes to the intercept at its reference
# level

# the first coef (intercept) is the value for same on average (at the mean of same & different)
coef(model6)["(Intercept)"]
mean(combs.mean[,1]) # that is correct

# the 2nd coef is half the difference between same and different at animacy=animates
coef(model6)["cond1"]
(combs.mean[1,1] - combs.mean[2,1])/2

# it's half because the distance in beta units between same and different is the distance between -1 and 1 (2)
# the 3rd coef is the difference between animates and both on average (at the mean of same & different )
coef(model6)["animacy2"]
mean(combs.mean[,2]) - mean(combs.mean[,1]) # not applicable/ incomplete cases

# the 4th coef is the difference between animates and inanimates on average (at the mean of same and different)
coef(model6)["animacy3"]
mean(combs.mean[, 3]) - mean(combs.mean[, 1]) # this is consistent

# the 5th coef is the difference bewteen [half the diff between same and different at animacy=both] and [half the 
# diff between same and different at animacy = animates]
coef(model6)["cond1:animacy2"]
(combs.mean[1,2]-combs.mean[2,2])/2-(combs.mean[1,1]-combs.mean[2,1])/2 # NA -- unbalanced sample sizes for "both"

# the 6th coef is the difference between [half the difference same and diff at animacy=inanimates] and [half the
# difference between same and diff at animacy=animates]
coef(model6)["cond1:animacy3"]
(combs.mean[1,3]-combs.mean[2,3])/2-(combs.mean[1,1]-combs.mean[2,1])/2 # this is consistent 

# Take alook at the model matrix:
cbind(f, model.matrix(~cond*animacy,f, contrasts=list(cond=contr.sum))) # THIS IS IMPORTANT
# rows are factor level combinations, columns are model terms 

# two factors, both deviation coded
model7 = lm(rt ~ cond*animacy, data, contrasts = list(cond=contr.sum, animacy=contr.sum))
summary(model7)
anova(model7)
anova5 = Anova(model7, type = 3, singular.ok = TRUE)

# lets take a look at the coefficients
# The first coeff (intercept) is the grand mean 
coef(model7)["(Intercept)"] # not exactly.. (slightly different)
grand.mean

# the second coef (cond1) is half the difference between same and diff, on average
coef(model7)["cond1"]
-diff(apply(combs.mean,1,mean, na.rm=TRUE))/2 # not applicable

# the 3rd coef animacy1 is the difference between animates and grand mean
coef(model7)["animacy1"]
mean(combs.mean[,1]) - grand.mean # not consistent 

# the 4th coeff animacy2 is the difference between both and the grand mean (averaging over same, diff)
coef(model7)["animacy2"]
mean(combs.mean[,2], na.rm=TRUE) - grand.mean # NA -- because of unbalanced observations?

# the 5th coeff cond1:animacy1 is the difference between [the difference same and diff at animacy=animates] and [the
# mean diff same & diff over all levels of animacy
coef(model7)["cond1:animacy1"]
-(diff(combs.mean[,1])/2 - diff(apply(combs.mean,1,mean, na.rm=TRUE))/2)

# the 6th coeff cond1:animacy2 is the difference between [difference of same and different at animacy=both] and 
# [mean difference same and diff over all levels of animacy]
coef(model7)["cond1:animacy2"]
-(diff(combs.mean[,2])/2 - diff(apply(combs.mean,1,mean, na.rm=TRUE))/2) # NOT APLICABLE

# lets look at the matrix model:
cbind(f,model.matrix(~cond*animacy,f, contrasts=list(cond=contr.sum, animacy=contr.sum)))

# let's make a custom contrast. Let's say we want the beta for the cond contrast to be the distance between
# same and diff rather than the distance between each and the grand mean:
model8 = lm(rt ~ cond*animacy, data, contrasts=list(cond=c(-0.5,0.5), animacy=contr.sum))
summary(model8)
anova(model8)
anova6 = Anova(model8, type = 3, singular.ok = TRUE)


# here beta for cond effect is the difference between same and diff 
coef(model8)["cond1"]
diff(apply(combs.mean,1, mean, na.rm=TRUE))

# this is the main effect of cond, the effect of same vs diff averaging over levels of animacy, so the test for the 
#cond1 coeff is identical to the cond1 effcet in the anova output
coef(summary(model8))["cond1", "t value"]^2
summary(aov(rt ~ cond*animacy, data))[[1]]["cond", "F value"]

# what happens if we exclude the both level of animacy and we end up with 2  two level factors?
model9 = lm(rt ~cond*animacy, data, subset = (animacy!= "2"), contrasts = list(cond=c(-0.5,0.5), animacy=c(-0.5,0.5)))
summary(model9)
anova(model9)
anova7 = Anova(model9, type = 3)
anova7

# here we excluded level both from factor animacy. By doing so we don't get a main effect of animacy, because 
# we are excluding data relevant for evaluating animacy. 
# let's use aov function:
# testing the significance of cond1 is the same as testing the significance of the entire factor cond
a2 = summary(aov(rt~cond*animacy, data, subset=(animacy!="2")))
coef(summary(model9))["cond1","t value"]^2
a2[[1]]["cond", "F value"]

coef(summary(model9))["cond1", "Pr(>|t|)"]
a2[[1]]["cond", "Pr(>F)"]

# and testing the sig of animacy1 is the same as testing the sign of the entire factor animacy
coef(summary(model9))["animacy1", "t value"]^2
a2[[1]]["animacy", "F value"] # not consistent

# the intercept is equal to the grand mean 
coef(model9)["(Intercept)"]
mean(combs.mean[,-2]) # excluding "both

# the reg coef for the effect of cond is the same as the mean diff for same different pairs (averaged over the 2
# levels of animacy, since level both is excluded)
coef(model9)["cond1"]
diff(apply(combs.mean[,-2], 1, mean))

# similarly, the reg coef for animacy is equal to the mean difference between animate and inanimate (averaged over
# the 2 levels of pair cond)
coef(model9)["animacy1"]
diff(apply(combs.mean[,-2], 2, mean))

# if we had used contr.sum instead of c(-0.5,0.5) for the contrasts then the coef would be equal to half the distance
# between the 2 levels
# let's see the interaction effect
coef(model9)["cond1:animacy1"]
unname(diff(combs.mean[,3]- combs.mean[,1]))
# this is [the difference between same and different at inanimate] minus [difference between same different at animate]
# if the differences are the same it means that difference in rts or acc between same and different pairs do not 
# depend on whether the pairs are animate or inanimate, the effect of pair cond does not depend on aniamcy, but since
# there is an interaction we know that pair cond depends on animacy









