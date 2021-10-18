## Statistics for the LE History & Design couse project
## Project topic: effects of urban matrix on lichen edge effects

## Stockholm University - Landscape Ecology master's program 2021-2023 
## Emma Gemal, emmagemal@outlook.com
## Edited 17/10/2021 


### Library ----
library(tidyverse)
library(car)
library(sjPlot)


### Loading the data ----
lichen <- read.csv("Data/raw_data.csv")

str(lichen)

# calculating diameter from circumference 
lichen <- lichen %>% 
            mutate(tree_dia_cm = tree_circum_cm/pi)

# averaging across the 3 trees per plot 
lichen_sum <- lichen %>% 
                group_by(site_name, location) %>%        
                summarize(avg_sp = mean(sp_richness),
                          avg_cov = mean(coverage_perc),
                          type = first(type),
                          avg_dia = mean(tree_dia_cm)) 

### Checking assumptions ----
# normality of data distribution 
hist(lichen_sum$avg_sp)   # not normal
hist(lichen_sum$avg_cov)   # not normal
hist(lichen_sum$avg_dia)

shapiro.test(lichen_sum$avg_sp)    # normal, less power with small sample sizes
shapiro.test(lichen_sum$avg_cov)   # normal
shapiro.test(lichen_sum$avg_dia)   # normal

# homoscedasticity 
plot(lm(avg_sp ~ type, data = lichen_sum))
leveneTest(lichen_sum$avg_sp, lichen_sum$type)  # equal variances, 
                                                # but test is vulnerable to small sample sizes

plot(lm(avg_sp ~ location, data = lichen_sum))
leveneTest(lichen_sum$avg_sp, lichen_sum$location)   # equal variances

plot(lm(avg_cov ~ type, data = lichen_sum))
leveneTest(lichen_sum$avg_cov, lichen_sum$type)   # equal variances

plot(lm(avg_cov ~ location, data = lichen_sum))
leveneTest(lichen_sum$avg_cov, lichen_sum$location)   # equal variances

# normality of residuals 
lm1 <- lm(avg_sp ~ type, data = lichen_sum)
lm2 <- lm(avg_cov ~ type, data = lichen_sum)
lm3 <- lm(avg_sp ~ location, data = lichen_sum)
lm4 <- lm(avg_cov ~ location, data = lichen_sum)
lm5 <- lm(avg_sp ~ avg_dia, data = lichen_sum)
lm6 <- lm(avg_cov ~ avg_dia, data = lichen_sum)

shapiro.test(residuals(lm2))  # normal
shapiro.test(residuals(lm2))  # normal 
shapiro.test(residuals(lm3))  # normal
shapiro.test(residuals(lm4))  # normal
shapiro.test(residuals(lm5))  # normal
shapiro.test(residuals(lm6))  # normal


### Doing a 2-way ANOVA ----
## species richness 
twoway <- lm(avg_sp ~ type + location, data = lichen_sum)
int <- lm(avg_sp ~ type*location, data = lichen_sum)

AIC(twoway, int)  # interaction is not best (twoway is best)

# making a model with diameter
twoway_dia <- lm(avg_sp ~ type + location + avg_dia, data = lichen_sum)
int_dia <- lm(avg_sp ~ type*location*avg_dia, data = lichen_sum)

AIC(twoway, twoway_dia, int_dia)  # int is too complicated, twoway and twoway_dia the same
                                  # twoway is simpler = maybe better?

Anova(twoway, type = "III")
# matrix type vs richness: p = 0.3574, DF = 1, F = 0.9105
# location in forest vs richness: p = 0.4408, DF = 1, F = 0.6323
# residual variance: DF = 13, lots of within-group variance that's unexplained


## coverage 
twoway_cov <- lm(avg_cov ~ type + location, data = lichen_sum)
int_cov <- lm(avg_cov ~ type*location, data = lichen_sum)
twoway_dia_cov <- lm(avg_cov ~ type + location + avg_dia, data = lichen_sum)
int_dia_cov <- lm(avg_cov ~ type*location*avg_dia, data = lichen_sum)

AIC(twoway_cov, int_cov, twoway_dia_cov, int_dia_cov)   # interaction with all is best!

Anova(int_dia_cov, type = "III")  # nothing is significant here either 
# matrix type vs coverage: p = 0.97956, DF = 1, F = 0.0007
# location vs coverage: p = 0.10021, DF = 1, F = 3.4530
# diameter vs coverage: p = 0.04856 (***SIGNIFICANT***), DF = 1, F = 5.4046
# interaction (matrix type:location): p = 0.65435, DF = 1, F = 0.2162
# interaction (matrix type:diameter): p = 0.89309, DF = 1, F = 0.0192
# interaction (location:diameter): p = 0.10314, F = 1, F = 3.3834
# interaction (all): p = 0.83066, DF = 1, F = 0.0488
# residual variance: DF = 8


### Calculating summarized data for results ----
data_sum <- lichen_sum %>% 
              group_by(type, location) %>% 
              summarize(avg_sp = mean(avg_sp),
                        avg_cov = mean(avg_cov))

