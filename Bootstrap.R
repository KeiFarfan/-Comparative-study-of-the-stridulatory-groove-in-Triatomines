### Comparative study of the stridulatory groove in species of Triatoma genus (Hemiptera: Reduviidae) from Mexico ###

### Nancy Rivas 		ORCID 0000-0001-6408-3130
### Alberto Antonio-Campos 		ORCID 0000-0002-2224-559X
### Keity J. Farfán-Pira		ORCID   0000-0002-5010-4241
### Ricardo Alejandre-Aguilar	ORCID 0000-0003-4930-2440

### BOOTSTRAP HYPOTHESIS TEST (CODE FOR 1 STRIAE) ###

# load packages
library(tidyverse)

# establish working directory
setwd("~/MEGA/Beto_data_Triatomines")


# load in the Triatomine striae data (save it in "d")
read.table(file="striae_distance.csv", header=T, sep=",")

d <- read.table(file="striae_distance.csv", header=T, sep=",")

# let's add the data into the "data view"
View(d)

# check the names, etc
names(d)
class(d$specie)
class(d$sex)

#as specie and sex are character, transform to factor
d$specie <- as.factor(d$specie)
d$sex <- as.factor(d$sex)

# Now with factors, we can see the levels of specie and sex
levels(d$specie)
levels(d$sex)

# how many observations in each specie/sex?
table(d$specie)
table(d$sex)

# let's look at a boxplot of striae by those species

# add color to each specie (change if you want)
colors <- c("T_barberi" = "#ac92eb",
            "T_dimidiata"= "#4fc1e8", 
            "T_lecticularia" = "#a0d568", 
            "T_mexicana" = "#ffce54",
            "T_protracta_nahuatlae" = "#ed5564", 
            "T_protracta_protracta" = "#a8d0db",
            "T_rubida" = "#8c8787") 

# here is necessary have the column specie as character, to apply the colors
d$specie <- as.character(d$specie)

# create the boxplot with all the necessary aesthetics (E1)

p <-ggplot(d, aes(x = specie, y = dist_e1, colour = specie)) +
  geom_boxplot() +
  ylim(0, 10)+
  facet_grid(. ~ sex) +  # Add facet with variable "sex"
  scale_color_manual(values = colors)+
  theme(strip.text.x = element_text(size=32))+
  labs(x="Specie", y="Distance stridulatory groove E1 (um)")+
  ggtitle("Striae 1 distance")+
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 22, angle = 0, face = "bold"),
        axis.title.y = element_text(size = 22, angle = 90, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 25, face = "bold"),
        panel.background = element_rect(color="black", fill = "white"),
        panel.grid.major = NULL,
        panel.grid.minor = NULL,
        legend.text = element_text(size = 18, face = "bold"),
        legend.margin = margin(r = 0.5, l = 0.5, unit = "cm"),
        legend.key.size = unit(2, "cm"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.background = element_rect(fill = "white"),
        legend.position = "bottom", legend.direction = "horizontal")
p
png("E1.png", width = 35, height = 20, units = "cm", res = 600)
print(p)
dev.off()

# calculate the difference in sample means for each specie (CODE FOR 1 SPECIES)
# T. barberi
t_barberi <- d %>% 
  filter(specie == "T_barberi")

# for striae 1
mean(t_barberi$dist_e1[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e1[t_barberi$sex=="male"])  #mean for males

# for striae 2
mean(t_barberi$dist_e2[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e2[t_barberi$sex=="male"])  #mean for males
# for striae 3
mean(t_barberi$dist_e3[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e3[t_barberi$sex=="male"])  #mean for males
# for striae 4
mean(t_barberi$dist_e4[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e4[t_barberi$sex=="male"])  #mean for males
# for striae 5
mean(t_barberi$dist_e5[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e5[t_barberi$sex=="male"])  #mean for males
# for striae 6
mean(t_barberi$dist_e6[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e6[t_barberi$sex=="male"])  #mean for males
# for striae 7
mean(t_barberi$dist_e7[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e7[t_barberi$sex=="male"])  #mean for males
# for striae 8
mean(t_barberi$dist_e8[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e8[t_barberi$sex=="male"])  #mean for males
# for striae 9
mean(t_barberi$dist_e9[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e9[t_barberi$sex=="male"])  #mean for males
# for striae 10
mean(t_barberi$dist_e10[t_barberi$sex=="female"]) #mean for females
mean(t_barberi$dist_e10[t_barberi$sex=="male"])  #mean for males

# lets calculate the absolute diff in means (CODE FOR 1 SPECIES)

#T_barberi

#striae1
test.stat1tbe1 <- abs(mean(t_barberi$dist_e1[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e1[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe1 

#striae2
test.stat1tbe2 <- abs(mean(t_barberi$dist_e2[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e2[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe2

#striae3
test.stat1tbe3 <- abs(mean(t_barberi$dist_e3[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e3[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe3

#striae4
test.stat1tbe4 <- abs(mean(t_barberi$dist_e4[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e4[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe4

#striae5
test.stat1tbe5 <- abs(mean(t_barberi$dist_e5[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e5[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe5

#striae6
test.stat1tbe6 <- abs(mean(t_barberi$dist_e6[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e6[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe6

#striae7
test.stat1tbe7 <- abs(mean(t_barberi$dist_e7[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e7[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe7

#striae8
test.stat1tbe8 <- abs(mean(t_barberi$dist_e8[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e8[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe8

#striae9
test.stat1tbe9 <- abs(mean(t_barberi$dist_e9[t_barberi$sex=="female"]) - 
                        mean(t_barberi$dist_e9[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe9

#striae10
test.stat1tbe10 <- abs(mean(t_barberi$dist_e10[t_barberi$sex=="female"]) - 
                         mean(t_barberi$dist_e10[t_barberi$sex=="male"]))  #diff in means
test.stat1tbe10

# and, the same for the medians (CODE FOR 1 SPECIES)

#T_barberi
# for striae 1
median(t_barberi$dist_e1[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e1[t_barberi$sex=="male"])  #median for males
# for striae 2
median(t_barberi$dist_e2[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e2[t_barberi$sex=="male"])  #median for males
# for striae 3
median(t_barberi$dist_e3[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e3[t_barberi$sex=="male"])  #median for males
# for striae 4
median(t_barberi$dist_e4[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e4[t_barberi$sex=="male"])  #median for males
# for striae 5
median(t_barberi$dist_e5[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e5[t_barberi$sex=="male"])  #median for males
# for striae 6
median(t_barberi$dist_e6[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e6[t_barberi$sex=="male"])  #median for males
# for striae 7
median(t_barberi$dist_e7[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e7[t_barberi$sex=="male"])  #median for males
# for striae 8
median(t_barberi$dist_e8[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e8[t_barberi$sex=="male"])  #median for males
# for striae 9
median(t_barberi$dist_e9[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e9[t_barberi$sex=="male"])  #median for males
# for striae 10
median(t_barberi$dist_e10[t_barberi$sex=="female"]) #median for females
median(t_barberi$dist_e10[t_barberi$sex=="male"])  #median for males

# lets calculate the absolute diff in medians (CODE FOR 1 SPECIES)

#T_barberi
# striae 1
test.stat2tbe1 <- abs(median(t_barberi$dist_e1[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e1[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe1

#striae 2
test.stat2tbe2 <- abs(median(t_barberi$dist_e2[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e2[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe2

#striae 3
test.stat2tbe3 <- abs(median(t_barberi$dist_e3[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e3[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe3

#striae 4
test.stat2tbe4 <- abs(median(t_barberi$dist_e4[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e4[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe4

#striae 5
test.stat2tbe5 <- abs(median(t_barberi$dist_e5[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e5[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe5

#striae 6
test.stat2tbe6 <- abs(median(t_barberi$dist_e6[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e6[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe6

#striae 7
test.stat2tbe7 <- abs(median(t_barberi$dist_e7[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e7[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe7

#striae 8
test.stat2tbe8 <- abs(median(t_barberi$dist_e8[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e8[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe8

#striae 9
test.stat2tbe9 <- abs(median(t_barberi$dist_e9[t_barberi$sex=="female"]) - 
                        median(t_barberi$dist_e9[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe9

#striae 10
test.stat2tbe10 <- abs(median(t_barberi$dist_e10[t_barberi$sex=="female"]) - 
                         median(t_barberi$dist_e10[t_barberi$sex=="male"]))  #diff in medians
test.stat2tbe10

## let's take a look at the 3 "Classic" hyp tests we could 

# let's look at the Independent 2-sample t-test

##t_barberi
t.test(t_barberi$dist_e1~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e2~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e3~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e4~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e5~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e6~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e7~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e8~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e9~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal
t.test(t_barberi$dist_e10~t_barberi$sex, paired=F, var.eq=F)  # tests Ho: means are equal


# let's look at the Wilcoxon aka Mann-Whitney U (differences between medians)

## T_barberi
wilcox.test(t_barberi$dist_e1~t_barberi$sex, paired=F)  # tests Ho: medians are equal
wilcox.test(t_barberi$dist_e2~t_barberi$sex, paired=F)
wilcox.test(t_barberi$dist_e3~t_barberi$sex, paired=F)
wilcox.test(t_barberi$dist_e4~t_barberi$sex, paired=F)
wilcox.test(t_barberi$dist_e5~t_barberi$sex, paired=F)
wilcox.test(t_barberi$dist_e6~t_barberi$sex, paired=F)
wilcox.test(t_barberi$dist_e7~t_barberi$sex, paired=F)
wilcox.test(t_barberi$dist_e8~t_barberi$sex, paired=F)
wilcox.test(t_barberi$dist_e9~t_barberi$sex, paired=F)
wilcox.test(t_barberi$dist_e10~t_barberi$sex, paired=F)

# let's look at the Kolmogorov-Smirnov 2-sample test (how is the sample distribution?)

#T_barberi
ks.test(t_barberi$dist_e1[t_barberi$sex=="female"],
        t_barberi$dist_e1[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e2[t_barberi$sex=="female"],
        t_barberi$dist_e2[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e3[t_barberi$sex=="female"],
        t_barberi$dist_e3[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e4[t_barberi$sex=="female"],
        t_barberi$dist_e4[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e5[t_barberi$sex=="female"],
        t_barberi$dist_e5[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e6[t_barberi$sex=="female"],
        t_barberi$dist_e6[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e7[t_barberi$sex=="female"],
        t_barberi$dist_e7[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e8[t_barberi$sex=="female"],
        t_barberi$dist_e8[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e9[t_barberi$sex=="female"],
        t_barberi$dist_e9[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same
ks.test(t_barberi$dist_e10[t_barberi$sex=="female"],
        t_barberi$dist_e10[t_barberi$sex=="male"], paired=F) # tests Ho: distributions are same

########################
##  BOOTSTRAPPING... ###
########################

# let's bootstrap... (EXAMPLE FOR 1 STRIAE)
#T.barberi
#striae1
set.seed(112358)   # for reproducibility
ntb <- length(t_barberi$sex)  # the number of observations to sample
ntb
B <- 10000  # the number of bootstrap samples
variabletbe1 <- t_barberi$dist_e1  # the variable we will resample from

# now, create the matrix to do bootstrap
BootstrapSamplestbe1 <- matrix( sample(variabletbe1, size= ntb*B, replace=TRUE), 
                                nrow=ntb, ncol=B)
# let's take a moment to discuss what that code is doing...
dim(BootstrapSamplestbe1)

# now, calculate the means (Xf and Xm) for each of the bootstrap samples
# initialize the vector to store the TEST-STATS
Boot.test.stat1tbe1 <- rep(0,B)
Boot.test.stat2tbe1 <- rep(0,B)
# run through a loop, each time calculating the bootstrap test.stat
for (i in 1:B){
  # calculate the boot-test-stat1 and save it
  Boot.test.stat1tbe1[i] <- abs( mean(BootstrapSamplestbe1[1:6,i]) - 
                                   mean(BootstrapSamplestbe1[7:12,i]) )
  # calculate the boot-test-stat2 and save it
  Boot.test.stat2tbe1[i] <- abs( median(BootstrapSamplestbe1[1:6,i]) - 
                                   median(BootstrapSamplestbe1[7:12,i])  )
}

# before going too far, let's remind ourselves of the OBSERVED TEST STATS
test.stat1tbe1; test.stat2tbe1
# and, take a look at the first 20 Bootstrap-TEST STATS for 1 and 2
round(Boot.test.stat1tbe1[1:20], 1)
round(Boot.test.stat2tbe1[1:20], 1)

# and, let's calculate the bootstrap p-value...
# notice how we can ask R a true/false question...(for the first 20)
(Boot.test.stat1tbe1 >= test.stat1tbe1)[1:20]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE

#run just to see additional values
Boot.test.stat1tbe1; Boot.test.stat2tbe1

#...calculate the p-value
mean( Boot.test.stat1tbe1 >= test.stat1tbe1)

# let's calculate the p-value for test statistic 2 (abs diff in medians)
mean( Boot.test.stat2tbe1 >= test.stat2tbe1)

# let's take a look at a density plot of all the Bootstrap test-stats, and 
# add in our Observed test stat
# Plot the density curve
plot(density(Boot.test.stat1tbe1), 
     xlab=expression( group("|", bar(Xf) - bar(Xm), "|") ),
     main="Bootstrap Test Stats striae 1",
     las=1) +
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 22, angle = 0, face = "bold"),
        axis.title.y = element_text(size = 22, angle = 90, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 25, face = "bold"))


library(ggplot2)

# Assuming Boot.test.stat1tbe1 is your data
density_e1_tb <- ggplot(data = NULL, aes(x = Boot.test.stat1tbe1)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(x = expression(group("|", bar(Xf) - bar(Xm), "|")),
       y = "Density",
       title = "Bootstrap Test Stats striae 1") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 22, angle = 0, face = "bold"),
        axis.title.y = element_text(size = 22, angle = 90, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 25, face = "bold")) +
  geom_vline(xintercept = test.stat1tbe1, color = "deeppink", linetype = "dotted") +
  geom_text(x = 0.5, y = 1.5, label = "p-value", color = "blue", size = 8)
density_e1_tb

### Assuming Boot.test.stat2tbe1 is your data
density_e1_tb2 <- ggplot(data = NULL, aes(x = Boot.test.stat2tbe1)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(x = expression(group("|", bar(MEDf) - bar(MEDm), "|")),
       y = "Density",
       title = "Bootstrap Test Stats striae 1") +
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 22, angle = 0, face = "bold"),
        axis.title.y = element_text(size = 22, angle = 90, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 25, face = "bold")) +
  geom_vline(xintercept = test.stat1tbe1, color = "deeppink", linetype = "dotted") +
  geom_text(x = 0.5, y = 1.5, label = "p-value", color = "blue", size = 8)
density_e1_tb2