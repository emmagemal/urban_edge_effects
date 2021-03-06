## Figures for the LE History & Design couse project
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

# averaging across the 3 trees per plot 
lichen_sum <- lichen %>% 
                group_by(site_name, location) %>%        
                summarize(avg_sp = mean(sp_richness),
                          avg_cov = mean(coverage_perc),
                          type = first(type),
                          avg_dia = mean(tree_dia_cm)) 

## Making a boxplot ----
# species richness
(boxplot_sp <- ggplot(lichen_sum, aes(x = location, y = avg_sp)) +
                  geom_boxplot(aes(fill = location)) +
                  ylab("Species richness") +
                  xlab("Location in forest") +
                  facet_wrap(~type) +
                  theme_bw() +
                  theme(panel.grid = element_blank(),
                        axis.title.x = 
                          element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                        axis.title.y = 
                          element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                        legend.position = "none") +
                  theme(plot.margin = unit(c(0.3, 1, 0.3, 0.3), "cm")) +
                  scale_fill_manual(values = c("#DDD78D", "#5f8f2f")))

ggsave("Figures/boxplot_species.png", plot = boxplot_sp, width = 5, height = 4.5, units = "in")


# coverage 
(boxplot_cov <- ggplot(lichen_sum, aes(x = location, y = avg_cov)) +
                  geom_boxplot(aes(fill = location)) +
                  xlab("Location in forest") +
                  ylab("Lichen coverage (%)") +
                  facet_wrap(~type) +
                  theme_bw() +
                  theme(panel.grid = element_blank(),
                        axis.title.x = 
                          element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                        axis.title.y = 
                          element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                        legend.position = "none") +
                  theme(plot.margin = unit(c(0.3, 0.3, 0.3, 1), "cm")) +
                  scale_fill_manual(values = c("#DDD78D", "#5f8f2f")))

ggsave("Figures/boxplot_coverage.png", plot = boxplot_cov, width = 5, height = 4.5, units = "in")


# tree diameter ~ species richness (for presentation)
(sp_dia_plot <- ggplot(lichen, aes(x = tree_dia_cm, y = sp_richness)) +
                    stat_smooth(method = "loess", se = F, color = "red") +
                    geom_point() +
                    xlab("Tree diameter (cm)") +
                    ylab("Species richness") +
                    theme_bw() +
                    theme(panel.grid = element_blank(),
                          axis.title.x = 
                            element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                          axis.title.y = 
                            element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")))

ggsave("Figures/diameter_species.png", plot = sp_dia_plot, width = 6, height = 5.5, units = "in")

# tree diameter ~ coverage (for presentation)
(cov_dia_plot <- ggplot(lichen, aes(x = tree_dia_cm, y = coverage_perc)) +
                    stat_smooth(method = "loess", se = F, color = "red") +
                    geom_point() +
                    xlab("Tree diameter (cm)") +
                    ylab("Lichen coverage (%)") +
                    theme_bw() +
                    theme(panel.grid = element_blank(),
                          axis.title.x = 
                            element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                          axis.title.y = 
                            element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")))

ggsave("Figures/diameter_coverage.png", plot = cov_dia_plot, width = 6, height = 5.5, units = "in")
