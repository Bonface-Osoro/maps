library(ggplot2)
library(tidyverse)
library(ggtext)
library(sf)
library(RColorBrewer)
library(gt)
library(readr)
library(patchwork)
library(ggplotify)
library(gridExtra)
library(readxl)
library(haven)
library(ggpubr)
library(scales)
library(ggnewscale)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

data <-
  read.csv(file.path(folder, '..', 'data', 'raw', 'bccp.csv'))

usa_states = st_read(file.path(folder, '..', 'data', 'raw', 'shapefiles',
                                'cb_2018_us_state_500k.shp'))
usa_states <- left_join(usa_states, data, by = 'STUSPS')
usa_states <- st_transform(usa_states, crs = 5070)
usa_states_label <- st_point_on_surface(usa_states)

phys_bins <- c(-Inf, 25, 110, 180, 350, 900, Inf)
usa_states$physician_bin <- cut(usa_states$total_physicians, breaks = phys_bins, 
    labels = c("Below 25", "24 - 110", "111 to 180", "181 to 350", 
               "351 to 900", "Above 900"))
#########################
## 1. Total Physicians ##
#########################
physicians <- ggplot() +
  geom_sf(data = usa_states, aes(fill = physician_bin), linewidth = 0.001) +
  scale_fill_brewer(palette = "OrRd") +
  geom_sf_text(data = usa_states_label, aes(label = STUSPS), size = 2, 
               color = "white", fontface = "bold") +
  labs(title = "(A) Total Physicians.",
       subtitle = "Number of registered cardiovascular physicians in each state.", 
       fill = "Totals", x = NULL, y = NULL) + theme_minimal() +
  theme(legend.position = 'bottom',
        plot.margin = margin(0, 0, 0, 0),              
        plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.line.x  = element_line(size = 0.15),
        axis.line.y  = element_line(size = 0.15),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 4)) +
  coord_sf(expand = FALSE)

#############
## 2. BCCP ##
#############
bc_bins <- c(-Inf, 3, 10, 25, 40, Inf)
usa_states$bccp_bin <- cut(usa_states$bccp, breaks = bc_bins, 
    labels = c("Below 3", "4 - 10", "11 to 25", "26 to 40", "Above 40"))

total_bccp <- ggplot() +
  geom_sf(data = usa_states, aes(fill = bccp_bin), linewidth = 0.001) +
  scale_fill_brewer(palette = "OrRd")+
  geom_sf_text(data = usa_states_label, aes(label = STUSPS), size = 2, 
               color = "white", fontface = "bold") +
  labs(title = "(B) Total BCCP.",
       subtitle = "Number of registered cardiovascular pharmacists in each state.", 
       fill = "Totals", x = NULL, y = NULL) + theme_minimal() +
  theme(legend.position = 'bottom',
        plot.margin = margin(0, 0, 0, 0),              
        plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.line.x  = element_line(size = 0.15),
        axis.line.y  = element_line(size = 0.15),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 3)) +
  coord_sf(expand = FALSE)

###########################
## 3. BCCP vs Physicians ##
###########################
usa_states$bccp_phys <- (usa_states$bccp / usa_states$total_physicians) * 100
ph_bc_bins <- c(-Inf, 1, 2, 3, 4, 5, 9, Inf)

usa_states$phys_bcc_bin <- cut(usa_states$bccp_phys, breaks = ph_bc_bins, 
   labels = c("Below 1", "1.1 - 2", "2.1 - 3", "3.1 - 4", "4.1 - 5",
              "5.1 - 9", "Above 9"))
bccp_phy <- ggplot() +
  geom_sf(data = usa_states, aes(fill = phys_bcc_bin), linewidth = 0.001) +
  scale_fill_brewer(palette = "OrRd") +
  geom_sf_text(data = usa_states_label, aes(label = STUSPS), size = 2, 
               color = "white", fontface = "bold") +
  labs(title = "(C) BCCP Ratio.",
       subtitle = "The ratio of registered cardiovascular pharmacists physicians to total \nnumber of cardiovascular physicians.", 
       fill = "Ratio (%)", x = NULL, y = NULL) + theme_minimal() +
  theme(legend.position = 'bottom',
        plot.margin = margin(0, 0, 0, 0),              
        plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.line.x  = element_line(size = 0.15),
        axis.line.y  = element_line(size = 0.15),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 6)) +
  coord_sf(expand = FALSE)


###########################
## 4. Deaths per 100##
###########################
death_bins <- c(-Inf, 140, 160, 180, 220, Inf)
usa_states$death_100k_bin <- cut(usa_states$deaths_per_100k, breaks = death_bins, 
   labels = c("Below 140", "141 - 160", "161 - 180", "181 - 220", "Above 220"))

death_100k <- ggplot() +
  geom_sf(data = usa_states, aes(fill = death_100k_bin), linewidth = 0.001) +
  scale_fill_brewer(palette = "OrRd") +
  geom_sf_text(data = usa_states_label, aes(label = STUSPS), size = 2, 
               color = "white",  fontface = "bold") +
  labs(title = "(D) Deaths.",
       subtitle = "Number of heart-related deaths per 100,000 people.", 
       fill = "Deaths", x = NULL, y = NULL) + theme_minimal() +
  theme(legend.position = 'bottom',
        plot.margin = margin(0, 0, 0, 0),              
        plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.line.x  = element_line(size = 0.15),
        axis.line.y  = element_line(size = 0.15),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 4)) +
  coord_sf(expand = FALSE)

combined_plots <- ggarrange(physicians, total_bccp, bccp_phy, death_100k,
  ncol = 2, nrow = 2, align = "hv", common.legend = FALSE,
  legend = "bottom", heights = c(1, 1))

path = file.path(folder, 'figures', 'combined_plots.png')
png(path, units="in", width=9, height=7, res=720)
print(combined_plots)
dev.off()




