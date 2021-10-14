## Statistics for the LE History & Design couse project
## Project topic: effects of urban matrix on lichen edge effects

## Stockholm University - Landscape Ecology master's program 2021-2023 
## Emma Gemal, emmagemal@outlook.com
## Edited 14/10/2021 


## Library ----
library(tidyverse)


## Loading the data ----
lichen <- read.csv("Data/raw_data.csv")

str(lichen)

# calculating diameter from circumference 
lichen <- lichen %>% 
            mutate(tree_dia_cm = tree_circum_cm/pi)


## Checking assumptions ----
# normality of data distribution 
hist(lichen$sp_richness)   # not normal
hist(lichen$coverage_perc)   # not normal

shapiro.test(lichen$sp_richness)
shapiro.test(lichen$coverage_perc)

# homoscedasticity 
plot(lm(sp_richness ~ type, data = lichen))
plot(lm(sp_richness ~ location, data = lichen))
plot(lm(coverage_perc ~ type, data = lichen))
plot(lm(coverage_perc ~ location, data = lichen))

# normality of residuals 
lm1 <- lm(sp_richness ~ type, data = lichen)
lm2 <- lm(coverage_perc ~ type, data = lichen)
shapiro.test(residuals(lm2))  # not normal
shapiro.test(residuals(lm2))  # not normal 

# transformation of the data to attempt to make it normal 
lichen_trans <- lichen %>% 
                    mutate(sp_rich_log = log(sp_richness)) %>% 
                    mutate(coverage_log = log(coverage_perc)) %>% 
                    mutate(sp_rich_sqrt = sqrt(sp_richness)) %>% 
                    mutate(coverage_sqrt = sqrt(coverage_perc))

hist(lichen_trans$sp_rich_log)
hist(lichen_trans$coverage_log)
hist(lichen_trans$sp_rich_sqrt)
hist(lichen_trans$coverage_sqrt)

lm3 <- lm(sp_rich_log ~ type, data = lichen_trans)
lm4 <- lm(coverage_log ~ type, data = lichen_trans)
lm5 <- lm(sp_rich_sqrt ~ type, data = lichen_trans)
lm6 <- lm(coverage_sqrt ~ type, data = lichen_trans)

lm7 <- lm(sp_rich_sqrt ~ location, data = lichen_trans)
lm8 <- lm(coverage_sqrt ~ location, data = lichen_trans)

shapiro.test(residuals(lm3))  # still not normal
shapiro.test(residuals(lm4))  # still not normal
shapiro.test(residuals(lm5))  
shapiro.test(residuals(lm6))
shapiro.test(residuals(lm7))  
shapiro.test(residuals(lm8))

plot(lm5)
plot(lm6)
plot(lm7)
plot(lm8)


## Doing a 2-way ANOVA ----
# species richness 
twoway <- aov(sp_rich_sqrt ~ type + location, data = lichen_trans)
int <- aov(sp_rich_sqrt ~ type*location, data = lichen_trans)

AIC(twoway, int)  # interaction not best 

summary(twoway)

# making a model with diameter
twoway_dia <- aov(sp_rich_sqrt ~ type + location + tree_dia_cm, data = lichen_trans)
summary(twoway_dia)   # no significant effect of any variable


# coverage 
twoway_cov <- aov(coverage_sqrt ~ type + location, data = lichen_trans)
int_cov <- aov(coverage_sqrt ~ type*location, data = lichen_trans)
twoway_dia_cov <- aov(coverage_sqrt ~ type + location + tree_dia_cm, data = lichen_trans)

AIC(twoway_cov, int_cov, twoway_dia_cov)

summary(twoway_cov)  # nothing is significant here either 
