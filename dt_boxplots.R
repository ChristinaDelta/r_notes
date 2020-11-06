# visualize the discriminability data (behavioural)

library(ggplot2)
library(plotly)
library(gridExtra)

### LOAD THE AVERAGED --ACROSS TRIALS AND SUBJECTS DATA #####

# load the averaged rts data
data = read.csv("Desktop/models_for_regression/discriminability_behav/averaged_dt_rts.csv", 
                header = TRUE)

# load the subset that dosn't include the "both" data
animacy.subset = read.csv("Desktop/models_for_regression/discriminability_behav/averaged_dt_subset.csv", 
                header = TRUE)

category.subset = read.csv("Desktop/models_for_regression/discriminability_behav/dt_averaged_subset.csv", 
                                         header = TRUE)

### LOAD THE AVERAGED --ACROSS TRIALS DATA #####
data2 = read.csv("Desktop/models_for_regression/discriminability_behav/final_df_discriminability.csv", 
                 header = TRUE)

animacy2.subset = read.csv("Desktop/models_for_regression/discriminability_behav/final_dataframe_animacy_subset.csv", 
                           header = TRUE)

category2.subset =read.csv("Desktop/models_for_regression/discriminability_behav/final_dataframe_category_subset.csv", 
                            header = TRUE)

# convert numbers into strings (for categorical variables) for each dataset
# start with the 1st dataset
# factor the categorical variables:
data$cond2 = factor(data$cond,
                    levels = c("1", "2"),
                    labels = c("same", "different"))

data$anim = factor(data$animacy,
                   levels = c("1", "2", "3"),
                   labels = c("animate", "both", "inanimate"))

data$categ = factor(data$category,
                    levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", 
                              "12", "13", "14", "15"),
                    labels = c("bodies", "body-face", "body-animal", "body-scene", "body-object", 
                              "faces", "face-animal", "face-scene", "face-object", "animals", "animal-scene", 
                              "animal-object", "scenes", "scene-object", "objects"))


# factor pair condition
data2$cond = factor(data2$condition,
                    levels = c("1", "2"),
                    labels = c("same", "different"))

data2$anim = factor(data2$animacy,
                    levels = c("1", "2", "3"),
                    labels = c("animate", "both", "inanimate"))

data2$categ = factor(data2$category,
                     levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", 
                                "12", "13", "14", "15"),
                     labels = c("bodies", "body-face", "body-animal", "body-scene", "body-object", 
                                "faces", "face-animal", "face-scene", "face-object", "animals", "animal-scene", 
                                "animal-object", "scenes", "scene-object", "objects"))

# dataset 2 - animacy subset
# factor pair condition
animacy.subset$cond = factor(animacy.subset$condition,
                             levels = c("1", "2"),
                             labels = c("same", "different"))

animacy.subset$anim = factor(animacy.subset$animacy,
                             levels = c("1", "3"),
                             labels = c("animate", "inanimate"))


# dataset 3 -- category
category.subset$categ = factor(category.subset$category,
                               levels = c("1", "6", "10", "13", "15"),
                               labels = c("bodies", "faces", "animals", "scenes", "objects"))

# factor pair condition
category.subset$cond = factor(category.subset$cond,
                             levels = c("1", "2"),
                             labels = c("same", "different"))

# dataset 3 -- category
category2.subset$categ = factor(category2.subset$category,
                                levels = c("1", "6", "10", "13", "15"),
                                labels = c("bodies", "faces", "animals", "scenes", "objects"))

# factor pair condition
category2.subset$cond = factor(category2.subset$condition,
                               levels = c("1", "2"),
                               labels = c("same", "different"))



# split the data using RTs. First consider condition "object pair" which has 2 levels:
# 1. same-object pairs
# 2. different-object pairs

same.objects = data$rt[data$cond=="1"]
different.objects = data$rt[data$cond=="2"]

# split the data for condition "animacy" which is has 3 levels: 
animate.pair = data$rt[data$animacy == "1"]
both.pair = data$rt[data$animacy == "2"]
inanimate.pair = data$rt[data$animacy == "3"]

# split the data for both conditions:
# same-objects animates, same-objects inanimates
# different-objects animates, different-objects inanimates
sameobjects.animates = data$rt[data$cond=="1" & data$animacy=="1"]
sameobjects.inanimates = data$rt[data$cond=="1" & data$animacy=="3"]

diffobjects.animates = data$rt[data$cond=="2" & data$animacy=="1"]
diffobjects.inanimates = data$rt[data$cond=="2" & data$animacy=="3"]
# just to have it:
diffobjects.both = data$rt[data$cond=="2" & data$animacy=="2"]

# Split the data using accuracy level (performance)
same.objects.acc = data$accuracy[data$cond=="1"]
different.objects.acc = data$accuracy[data$cond=="2"]

# split the data for condition "animacy" which is has 3 levels: 
animate.pair.acc = data$accuracy[data$animacy == "1"]
both.pair.acc = data$accuracy[data$animacy == "2"]
inanimate.pair.acc = data$accuracy[data$animacy == "3"]

# split the data for both conditions:
# same-objects animates, same-objects inanimates
# different-objects animates, different-objects inanimates
sameobjects.animates.acc = data$accuracy[data$cond=="1" & data$animacy=="1"]
sameobjects.inanimates.acc = data$accuracy[data$cond=="1" & data$animacy=="3"]

diffobjects.animates.acc = data$accuracy[data$cond=="2" & data$animacy=="1"]
diffobjects.inanimates.acc = data$accuracy[data$cond=="2" & data$animacy=="3"]
# just to have it:
diffobjects.both.acc = data$accuracy[data$cond=="2" & data$animacy=="2"]


# -------------------------------------------------------

# create a function that computes stats:
data_summary = function(x) {
  m = mean(x)
  ymin = m-sd(x)
  ymax = m+sd(x)
  return(c(y=m, ymin=ymin, ymax=ymax))
}
# 1. make violin plots ob the same and different object pairs and RTs

# boxplot for pair condition
fig1 = ggplot(data = data, aes(x = cond2, y = rt, fill = cond2)) +
  geom_violin(trim = FALSE)

fig1

# make classic theme (without that weird grey background) and add mean and sd (min,max)
fig1 + theme_classic() + stat_summary(fun.data=data_summary) + 
  scale_x_discrete(limits=c("same", "different"))

# ...or add individual points instead
fig1 + theme_classic() + geom_jitter(shape=16, position = position_jitter(0.2)) 


# 2. make violin plots for animacy and RTs

# create the base
fig2 = ggplot(data = data, aes(x = anim, y = rt, fill = anim)) +
  geom_violin(trim = FALSE)

fig2

# make classic theme (without that weird grey background) and add mean and sd (min,max)
fig2 + theme_classic() + stat_summary(fun.data=data_summary)

# ...or add individual points instead
fig2 + theme_classic() + geom_jitter(shape=16, position = position_jitter(0.2)) 

# 3. make grouped violin plots for same-different pairs grouped by animacy
# NOTE: Normally we would expect that this plot would have 2 groups of 3 violins (animate, inanimate, both
# for same objects and animate, inanimate both for different objects), but same objects have only animate -
# inanimate pairs...
fig3 = ggplot(data = animacy.subset, aes(x = anim, y = rt, fill = cond)) +
  geom_violin(trim = FALSE)

fig3

fig3 + theme_classic() + stat_summary(fun.data=data_summary) 


# category violin plots 
fig4 = ggplot(data = data, aes(x = categ, y = rt, fill = categ)) +
  geom_violin(trim = FALSE)

fig4

fig4 + theme_classic() + stat_summary(fun.data=data_summary) 

# category violin plots with same-different objects
fig5 = ggplot(data = category.subset, aes(x = categ, y = rt, fill = cond2)) +
  geom_violin(trim = FALSE)

fig5

fig5 + theme_classic() + stat_summary(fun.data=data_summary) 


# ACCURACY VIOLIN PLOTS 

# accuracy violin plot for same-different pairs
fig5 = ggplot(data = data, aes(x = cond2, y = accuracy, fill = cond2)) +
  geom_violin(trim = FALSE)

fig5

fig5 + theme_classic() + stat_summary(fun.data=data_summary) + 
  scale_x_discrete(limits=c("same", "different"))

# accuracy violin plot for same-different pairs (2)
fig5a = ggplot(data = data, aes(x = cond2, y = accuracy, fill = cond2)) +
  geom_violin(trim = FALSE)

fig5a

fig5a + theme_classic() + stat_summary(fun.data=data_summary) + 
  scale_x_discrete(limits=c("same", "different"))


# accuracy violin plots for sanimacy pairs
fig6 = ggplot(data = data2, aes(x = anim, y = acc, fill = anim)) +
  geom_violin(trim = FALSE)

fig6

fig6 + theme_classic() + stat_summary(fun.data=data_summary) 

# accuracy for animacy based on pair condition (same-different)
# accuracy violin plots for sanimacy pairs
fig7 = ggplot(data = animacy2.subset, aes(x = anim, y = acc, fill = cond)) +
  geom_violin(trim = FALSE)

fig7 + theme_classic() + stat_summary(fun.data=data_summary) 

# accuracy for category
fig8 = ggplot(data = data, aes(x = categ, y = accuracy, fill = categ)) +
  geom_violin(trim = FALSE)

fig8 + theme_classic() + stat_summary(fun.data=data_summary) 


# ---------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------- #

# BOX PLOTS

# boxplot for pair condition
fig9 = ggplot(data = data, aes(x = cond2, y = rt, fill = cond2)) +
  geom_boxplot()

fig9

# make classic theme (without that weird grey background) and add mean and sd (min,max)
fig9 + theme_classic() + stat_summary(fun.data=data_summary) + 
  scale_x_discrete(limits=c("same", "different"))


# boxplot for animacy
fig10 = ggplot(data = data, aes(x = anim, y = rt, fill = anim)) +
  geom_boxplot()

fig10

# make classic theme (without that weird grey background) and add mean and sd (min,max)
fig10 + theme_classic() + stat_summary(fun.data=data_summary)

# boxplot for animacy - subset (animates-inanimates only)
fig10a = ggplot(data = animacy.subset, aes(x = anim, y = rt, fill = anim)) +
  geom_boxplot()

fig10a

# make classic theme (without that weird grey background) and add mean and sd (min,max)
fig10a + theme_classic() + stat_summary(fun.data=data_summary)


# make grouped box plots for same-different pairs grouped by animacy
# NOTE: Normally we would expect that this plot would have 2 groups of 3plots (animate, inanimate, both
# for same objects and animate, inanimate both for different objects), but same objects have only animate -
# inanimate pairs...
fig11 = ggplot(data = animacy.subset, aes(x = anim, y = rt, fill = cond)) +
  geom_boxplot()

fig11

fig11 + theme_classic() + stat_summary(fun.data=data_summary) 

# category box plots 
fig12 = ggplot(data = data, aes(x = categ, y = rt, fill = categ)) +
  geom_boxplot()

fig12

fig12 + theme_classic() + stat_summary(fun.data=data_summary) 

# category and pair conditions box plots
fig12a = ggplot(data = category.subset, aes(x = categ, y = rt, fill = cond)) +
  geom_boxplot()

fig12a

fig12a + theme_classic() + stat_summary(fun.data=data_summary) 

# category and pair boxplots using the 2nd subset
fig12b = ggplot(data = category2.subset, aes(x = categ, y = rt, fill = cond)) +
  geom_boxplot()

fig12b

fig12b + theme_classic() + stat_summary(fun.data=data_summary) 

# ACCURACY BOX PLOTS 
# accuracy boxplot for same-different pairs
fig13 = ggplot(data = data, aes(x = cond2, y = accuracy, fill = cond2)) +
  geom_boxplot()

fig13 + theme_classic() + stat_summary(fun.data=data_summary) + 
  scale_x_discrete(limits=c("same", "different"))

# accuracy boxplots for animacy pairs
fig14 = ggplot(data = data, aes(x = anim, y = accuracy, fill = anim)) +
  geom_boxplot()

fig14

fig14 + theme_classic() + stat_summary(fun.data=data_summary) 

# boxplot for animacy - subset (animates-inanimates only)
fig14a = ggplot(data = animacy.subset, aes(x = anim, y = accuracy, fill = anim)) +
  geom_boxplot()

fig14a

# make classic theme (without that weird grey background) and add mean and sd (min,max)
fig14a + theme_classic() + stat_summary(fun.data=data_summary)


# accuracy for animacy based on pair condition (same-different)
# accuracy violin plots for sanimacy pairs
fig15 = ggplot(data = animacy.subset, aes(x = anim, y = accuracy, fill = cond)) +
  geom_boxplot()

fig15 + theme_classic() + stat_summary(fun.data=data_summary) 

# accuracy for category
fig16 = ggplot(data = data, aes(x = categ, y = accuracy, fill = categ)) +
  geom_boxplot()

fig16 + theme_classic() + stat_summary(fun.data=data_summary) 

# accuracy for category and pair
fig16a = ggplot(data = category.subset, aes(x = categ, y = accuracy, fill = cond)) +
  geom_boxplot()

fig16a + theme_classic() + stat_summary(fun.data=data_summary) 

# accuracy for category and pair (subset2)
fig16b = ggplot(data = category2.subset, aes(x = categ, y = acc, fill = cond)) +
  geom_boxplot()

fig16b

fig16b + theme_classic() + stat_summary(fun.data=data_summary) 

# accuracy for category only
fig17 = ggplot(data = category.subset, aes(x = categ, y = rt, fill = categ)) +
  geom_boxplot()

fig17 + theme_classic() + stat_summary(fun.data=data_summary) 

# accuracy for category only
fig17a = ggplot(data = category.subset, aes(x = categ, y = accuracy, fill = categ)) +
  geom_boxplot()

fig17a

fig17a + theme_classic() + stat_summary(fun.data=data_summary) 


