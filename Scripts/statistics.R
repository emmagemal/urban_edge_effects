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

shapiro.test(lichen_sum$avg_sp)    # normal, less power with small sample sizes
shapiro.test(lichen_sum$avg_cov)   # normal

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
shapiro.test(residuals(lm2))  # normal
shapiro.test(residuals(lm2))  # normal 


### Doing a 2-way ANOVA ----
## species richness 
twoway <- aov(avg_sp ~ type + location, data = lichen_sum)
int <- aov(avg_sp ~ type*location, data = lichen_sum)

AIC(twoway, int)  # interaction is not best (twoway is best)

# making a model with diameter
twoway_dia <- aov(avg_sp ~ type + location + avg_dia, data = lichen_sum)
int_dia <- aov(avg_sp ~ type*location*avg_dia, data = lichen_sum)

AIC(twoway, twoway_dia, int_dia)  # int is too complicated, twoway and twoway_dia the same
                                  # twoway is simpler = maybe better?

summary(twoway)
summary(twoway_dia)  # this model explains slightly more of the variation
# nothing is significant though

# for model with tree diameter: 
# matrix type vs richness: p = 0.350, DF = 1, F = 0.948
# location in forest vs richness: p = 0.433, DF = 1, F = 0.658
# diameter vs richness: p = 0.240, DF = 1, F = 1.530
# residual variance: DF = 12, sum of squares = 12.664 (not sure if relevant)


## coverage 
twoway_cov <- aov(avg_cov ~ type + location, data = lichen_sum)
int_cov <- aov(avg_cov ~ type*location, data = lichen_sum)
twoway_dia_cov <- aov(avg_cov ~ type + location + avg_dia, data = lichen_sum)
int_dia_cov <- aov(avg_cov ~ type*location*avg_dia, data = lichen_sum)

AIC(twoway_cov, int_cov, twoway_dia_cov, int_dia_cov)   # interaction with all is best!

summary(int_dia_cov)  # nothing is significant here either 

# plotting the predicted values to visualize the interaction 
fit <- lm(avg_cov ~ type*location*avg_dia, data = lichen_sum)
plot_model(fit, type = "pred", terms = c("type", "avg_dia", "location"))


# basic plot of coverage vs diameter in order to interpret interaction term 
(cov_dia <- ggplot(lichen_sum, aes(x = avg_dia, y = avg_cov)) +
              geom_point(aes(color = location)) +
              geom_smooth(method = "lm") +
              facet_wrap(~type, scales = "free"))

(cov_dia <- ggplot(lichen_sum, aes(x = avg_dia, y = avg_cov)) +
    geom_point(aes(color = type)) +
    geom_smooth(method = "lm") +
    facet_wrap(~location, scales = "free"))

ggsave("Figures/coverage_diameter_plot.png", plot = cov_dia, width = 8, height = 4.5, units = "in")


### Calculating summarized data for results ----
data_sum <- lichen %>% 
              group_by(type, location) %>% 
              summarize(avg_sp = mean(sp_richness),
                        avg_cov = mean(coverage_perc))

