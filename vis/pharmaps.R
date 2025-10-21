library(ggpubr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(sf)
library(osmdata)
library(prettymapr)
library(ggspatial)
library(grid)
library(patchwork)
library(RColorBrewer)
library(viridis) 
library(ggtext)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

metro <- st_read(file.path(folder, '..', 'data', 'raw', 'shapefiles', 
                            'cinci_metro.shp'))

df <- read.csv(file.path(folder, '..', 'results', 'pharmacies_geocoded.csv'))

pharmaps <- ggplot() +
  geom_sf(data = metro, fill = "white", color = 'black', size = 0.5, 
          alpha = 0.8, linewidth = 0.05)  +
  geom_sf_text(data = metro, aes(label = namelsad), size = 2.5, color = "black") +
  geom_point(data = df, aes(x = longitude, y = latitude, 
                            color = visit_count), size = 1.5) +
  scale_color_viridis_d(direction = 1) +
  labs(colour = "Number of patient visits",
       title = 'Distribution of Pharmacies.',
       subtitle = 'Spatial distribution of pharmacies in the greater Cincinnati area grouped by number of patient visits.',
       fill = NULL) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(size = 7),
        panel.spacing = unit(0.6, "lines"),
        plot.title = element_text(size = 11, face = 'bold'),
        plot.subtitle = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        axis.title.x = element_blank()) +
  guides(fill = guide_legend(nrow = 2)) +
  annotation_scale(location = "bl", width_hint = 0.5, pad_y = unit(0.01, "in"),
       line_width = 0.4, text_cex = 0.5, line_col = "grey50", 
       bar_cols = c("grey10", "white"), height = unit(0.025, "npc")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  ) 

#########
## AGE ##
#########
df1 <- read.csv(file.path(folder, '..', 'results', 'pharmacy_age.csv')) %>%
  filter(PatInsuranceType != "<UNKNOWN>")

label_totals <- df1 %>%
  group_by(PatInsuranceType) %>%
  summarize(total_value = sum(counts))

age_totals <-
  ggplot(df1, aes(x = PatInsuranceType, y = counts)) +
  geom_bar(stat = "identity", aes(fill = age_group)) + coord_flip() + 
  geom_text(data = label_totals, aes(x = PatInsuranceType, y = total_value, 
     label = sprintf("%.0f", total_value)), size = 3,
     position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  scale_fill_viridis_d(direction = 1) +
  labs(colour = NULL, title = "(b) Age.",
       subtitle = "Pharmacy visits by age broken down by insurance types.",
       x = "Insurance Type", y = bquote("Volume of Patients")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 7),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_markdown(size = 9),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 9)
  )+ expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, title = "Age group")) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
 labels = function(y)format(y, scientific = FALSE), limit = c(0, 319))

#########
## SEX ##
#########
df2 <- read.csv(file.path(folder, '..', 'results', 'pharmacy_sex.csv')) %>%
  mutate(PatSex = factor(PatSex,
                         levels = c(0, 1),
                         labels = c("0", "1")))

label_totals <- df2 %>%
  group_by(age_group) %>%
  summarize(total_value = sum(counts))

sex_totals <-
  ggplot(df2, aes(x = age_group, y = counts)) +
  geom_bar(stat = "identity", aes(fill = PatSex)) + coord_flip() + 
  geom_text(data = label_totals, aes(x = age_group, y = total_value, 
      label = sprintf("%.0f", total_value)), size = 3,
      position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  scale_fill_viridis_d(direction = 1) +
  labs(colour = NULL, title = "(b) Sex",
       subtitle = "Pharmacy visits by sex and broken down by age groups.",
       x = "Age group", y = bquote("Volume of Patients")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 7),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_markdown(size = 9),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 9)
  )+ expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, title = "Patient's Sex")) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 299))

##########
## RACE ##
##########
df3 <- read.csv(file.path(folder, '..', 'results', 'pharmacy_race.csv')) %>%
  mutate(PatRace = factor(PatRace,
                         levels = c(0, 1, 2, 3, 5),
                         labels = c("0", "1", "2", "3", "5")))

label_totals <- df3 %>%
  group_by(PatRace) %>%
  summarize(total_value = sum(counts))

race_totals <-
  ggplot(df3, aes(x = PatRace, y = counts)) +
  geom_bar(stat = "identity", aes(fill = age_group)) + coord_flip() + 
  geom_text(data = label_totals, aes(x = PatRace, y = total_value, 
      label = sprintf("%.0f", total_value)), size = 3,
      position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  scale_fill_viridis_d(direction = 1) +
  labs(colour = NULL, title = "(c) Race",
       subtitle = "Pharmacy visits by race and broken down by age groups.",
       x = "Racial Identity", y = bquote("Volume of Patients")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 7),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_markdown(size = 9),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 9)
  )+ expand_limits(y = 0) +
  guides(fill = guide_legend(ncol = 3, title = "Age group")) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
   labels = function(y)format(y, scientific = FALSE), limit = c(0, 619))

gen_plots <- ggarrange(pharmaps, age_totals, sex_totals, race_totals, ncol = 2,
            nrow = 2, common.legend = FALSE, legend = 'bottom')


path = file.path(folder, 'figures', 'summary_plots.png')
png(path, units="in", width= 8, height=9, res=300)
print(gen_plots)
dev.off()

path = file.path(folder, 'figures', 'pharmaps.png')
png(path, units="in", width= 7, height=8, res=300)
print(pharmaps)
dev.off()

