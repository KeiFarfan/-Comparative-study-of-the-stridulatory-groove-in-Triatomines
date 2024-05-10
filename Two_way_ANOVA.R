### Comparative study of the stridulatory groove in species of Triatoma genus (Hemiptera: Reduviidae) from Mexico ###

### Nancy Rivas 		ORCID 0000-0001-6408-3130
### Alberto Antonio-Campos 		ORCID 0000-0002-2224-559X
### Keity J. Farfán-Pira		ORCID   0000-0002-5010-4241
### Ricardo Alejandre-Aguilar	ORCID 0000-0003-4930-2440

## Two way Anova analysis based on striae and their differences between ##
## males and females ##
### Based on the information of this URL: https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/ ###

## Load packages ##
library(tidyverse)
library(ggpubr)
library(rstatix)

## Establish working directory ##
setwd("~/MEGA/Beto_data_Triatomines")


## Load in the Triatomine striae data (save it in "d") ##
read.table(file="striae_distance.csv", header=T, sep=",")

## Save d as an object ##
d <- read.table(file="striae_distance.csv", header=T, sep=",") %>% 
  convert_as_factor(id, sex, species)
summary(d)

#########################################################################################################################
### STRIAE 1 ANALYSIS ###
### Start with summary statistics: Group the data by speciess and sex, and then ###
### compute some summary statistics of the groove_distance variable: mean and standard deviation ###
d %>% 
  group_by(species, sex) %>% 
  get_summary_stats(groove_distance, type = "mean_sd")

### Visualization of the plot colored by sex groups ##
boxplt <- ggboxplot(
  d, x = "species", y = "groove_distance", size = 0.7,
  color = "sex", palette = c("#BB3099","#00AFBB"), add = "jitter",
  xlab ="Triatomine species", ylab = "Inter-ridge distance (μm)"
) +
  theme(plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 22, angle = 0, face = "bold"),
        axis.title.y = element_text(size = 22, angle = 90, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 25, face = "bold"),
        panel.background = element_rect(color="black", fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 18, face = "bold"),
        legend.margin = margin(r = 0.5, l = 0.5, unit = "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.background = element_rect(fill = "white"),
        legend.position = "bottom", legend.direction = "horizontal")
boxplt

### Check the assumptions: Outliers ###
d %>% 
  group_by(sex, species) %>% 
  identify_outliers(groove_distance) ### T. protracta nahuatlae has outliers, but are not extreme outliers ##

## Check the assumptions: Normality ###
# Compute Shapiro-Wilk test for each combinations of factor levels ##
d %>% 
  group_by(sex, species) %>% 
  shapiro_test(groove_distance)

## Create QQ plot for each cell of design ##
ggqqplot(d, "groove_distance", ggtheme = theme_bw())+
  facet_grid(species ~ sex, labeller = "label_both") ## for the generated plot, as all the points fallapproximately along the reference line, we can assume normality ###

### Compute Anova ###
res.aov  <- d %>% anova_test(groove_distance ~ species*sex)
res.aov

### Group the data by sex and fit anova ###
model <- lm(groove_distance ~ species * sex, data = d)
d %>% 
  group_by(species) %>% 
  anova_test(groove_distance ~ sex, error = model) # the effect of sex in distance striae is statistically significant in T_lecticularia #

### Compute pairwise comparisons to determine which groups are different in sex groups by species ##
library(emmeans)
pwc <- d %>% 
  group_by(species) %>% 
  emmeans_test(groove_distance ~ sex, p.adjust.method = "bonferroni")
pwc  ## there is a significant difference of distance of striae e1 between females and males of T. lecticularia ##

### Report ###
#Visualization: box plots with p-values#
pwc <- pwc %>% add_xy_position(x = "species")
p <- boxplt +
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = FALSE),
    caption = get_pwc_label(pwc)
  )
p
png("avg_modified.png", width = 35, height = 20, units = "cm", res = 600)
print(p)
dev.off()
