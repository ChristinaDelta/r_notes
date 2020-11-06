# The script runs:
# 1. t tests (parametric and non-parametric) on the behavioural 
# discriminability data to compare mean rts for same-objects and different-objects
# reaction times. 
# 2. Permutation test for hypothesis testing by permuting:
# a) rts, or
# b) labels (same, different)

# import packages:
library(dplyr)
library(mosaic)
library(ggplot2)


# load the behavioural data:
data = read.csv("Desktop/models_for_regression/discriminability_behav/dt_alldata.csv", 
                header = FALSE)

# add headers
colnames(data) = c("subNo", "pair", "rt", "response", "paircond", "pair_animacy")

View(data) # take a look at the dataframe

# remove incorect responses
data[data == 0] = NA
data

data = data[complete.cases(data),]

# check the names etc:
names(data)
levels(data$paircond)
# how many observations in each condition?
table(data$paircond)

# take a look at boxplots of reaction times by the 2 conditions:
boxplot(data$rt ~ data$paircond, las=1, ylab="rts (s)",
        xlab = "condition", main= "rts by condition")

# calculate the difference in means 
mean(data$rt[data$paircond=="different"])
mean(data$rt[data$paircond=="same"])
# calculate abs difference in means 
test.stat1 = abs(mean(data$rt[data$paircond=="different"]) -
                   mean(data$rt[data$paircond=="same"]))

# calculate the difference in median
median(data$rt[data$paircond=="different"])
median(data$rt[data$paircond=="same"])
# calculate abs difference in means 
test.stat2 = abs(median(data$rt[data$paircond=="different"]) -
                   median(data$rt[data$paircond=="same"]))

# permutation test. First permute the rts

# for reproducability of the results
set.seed(1979)
# the number of observations to sample
n = length(data$paircond)
# number of permutation samples 
p = 1000
# the variable we will resample from
variable = data$rt

# create a matrix to store the permutation data
permsamples = matrix(0, nrow = n, ncol = p)

# get the pernutation samples using a loop
for (i in 1:p) {
  permsamples[,i] = sample(variable, size = n, replace = FALSE)
  
}

# take a quick look at the first  5 columns of permsamples
permsamples[,1:5]

# run through a loop to calculate t-stats for each sample
# initialize vectors to store all the test-stats
perm.test.stat1 = perm.test.stat2 = rep(0,p)

# loop through and calculate t-stats for means
for (i in 1:p) {
  perm.test.stat1[i] = abs(mean(permsamples[data$paircond=="different", i]) -
                             mean(permsamples[data$paircond=="same", i]))
  
  # calculate t-stats for medians
  perm.test.stat2[i] = abs(median(permsamples[data$paircond=="different", i]) -
                             median(permsamples[data$paircond=="same", i]))
  
}

# now look at the first 15 permutated test stats for mean(1) and median(2)
round(perm.test.stat1[1:15], 1)
round(perm.test.stat2[1:15], 1)

# calculate the permutation p-value 
(perm.test.stat1 >= test.stat1)[1:15]
# ask for the mean of the 15 samples
mean((perm.test.stat1 >= test.stat1)[1:15])

# calculate p-value for all the samples
mean(perm.test.stat1 >= test.stat1)

# calculate p-value for all the test.stat2 samples (abs diff in medians)
mean(perm.test.stat2 >= test.stat2)

# some more stuff
# calculate the mean for each condition
with(data, tapply(rt, paircond, mean))
# calculate the difference in means for each condition
abs(diff(with(data, tapply(rt, paircond, mean))))

# calculate the median for each condition
with(data, tapply(rt, paircond, median))
# calculate the difference in meadians for each condition
abs(diff(with(data, tapply(rt, paircond, median))))

# plot a histogram with the permuted t-stats
ggplot(data = perm.test.stat1) + 
  geom_histogram(mapping =aes(x = same)) +
  xlab("mean difference")

## let's take a look at the 3 typical hyp tests we could 
# consider (each of which comes with their own limitations...)
# tests Ho: means are equal
t.test(data$rt ~ data$paircond, paired = TRUE, var.eq = FALSE)

# look at the Wilcoxon aka Mann-Whitney U 
# tests Ho: medians are equal
wilcox.test(data$rt ~ data$paircond, paired = TRUE)

# look at the Kolmogorov-Smirnov 2-sample test
# tests Ho: same and different distributions are same
ks.test(data$rt[data$paircond=="different"], data$rt[data$paircond=="same"],
        paired = TRUE)

# -----------------------------------------------------------------------------

# Run a permutation test, only this time shuffle the labels aka conditions and not 
# rts

set.seed(1236)
n = length(data$paircond)
p = 1000
variable = data$cond

# create a matrix to store the permutations
permsamples_cond = matrix(0, nrow = n, ncol = p)

# get the permutation sample using a loop
for (i in 1:p) {
  permsamples_cond[,i] = sample(variable, size = n, replace = FALSE)
  
}
dim(permsamples_cond)
# take a look at the first 5 columns
permsamples_cond[,1:5] # 1= same, 2=different

# loop through and calculate t-stats for all samples
# create a vector to store the t stats
perm.test.stat2a = perm.test.stat2b = rep(0,p)

for (i in 1:p) {
  # calculate the permutation test.stat2a and save it
  perm.test.stat2a[i] = abs(mean(data$rt[permsamples_cond[,i]=="1"]) -
                              mean(data$rt[permsamples_cond[,i]=="2"]))
  
  # calculate the permutation test.stat2b and save it
  perm.test.stat2b[i] = abs(median(data$rt[permsamples_cond[,i]=="1"]) -
                              median(data$rt[permsamples_cond[,i]=="2"]))
}

# take a look at the first rows. we have the 1st 15 calculated permuted 
# test-statistics. Our question here is:
# " what is the probability of getting the observed test-stat (0.10) or more
# extreme if the null is true"?
round(perm.test.stat2a[1:15], 1)
round(perm.test.stat2b[1:15], 1)

# calculate the permutation p-value 
(perm.test.stat2a >= test.stat1)[1:15]
# ask for the mean of the 15 samples
mean((perm.test.stat2a >= test.stat1)[1:15])

# calculate p-value for all the samples
mean(perm.test.stat2a >= test.stat1)

# calculate p-value for all the test.stat2 samples (abs diff in medians)
mean(perm.test.stat2b >= test.stat2)


# plot (I want to change this)
plot(density(perm.test.stat2a), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Permutation Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(30,0.100, "p-value", col="blue", cex=1)
