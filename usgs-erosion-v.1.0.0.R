#### LIBRARY IMPORTS ####
library(data.table)

library(ggplot2)
library(scales)
library(ggthemes)
library(ggpubr)
library(gstat)

library(dplyr)
library(zoo)
library(broom)

library(tidyverse)
library(tidyquant)
library(tidyr)
library(segmented)

library(readr)
library(readxl)

library(RColorBrewer)
library(reshape2)

library(sp)
library(USAboundaries)
library(sf)
library(rgeos)
library(raster)
library(rgdal)

library(lubridate)
library(dataRetrieval)

#### THEMES ####
theme_evan <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey70'),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    text = element_text(size=8),
    axis.text = element_text(size = 8), 
    plot.title = element_text(size = 9)
  )

theme_evan_facet <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    # legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    strip.background = element_rect(fill = 'white'),
    text = element_text(size=12),
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 13)
  )
season_facet <- theme_evan_facet + theme(
  legend.position = 'none', 
  strip.background = element_blank(),
  strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
)
park_palettes <- list(
  SmokyMountains = c("#D58A60", "#40663F", "#497381", "#548F01", "#CFA3EE", "#4E5462"),
  RockyMountains = c("#EBECF0", "#DDC70F", "#4B4E55", "#62589F", "#2B313F"),
  Yellowstone = c("#8CBEB1", "#FAFAF2","#EEEAA0", "#999275", "#E8C533", "#3A5836"),
  Arches = c("#A8CDEC", "#F6955E", "#682C37", "#9B6981", "#7887A4", "#A89F8E"),
  ArcticGates = c("#F4E7C5", "#678096", "#ACC2CF", "#979461", "#CD5733", "#A12A19"),
  MtMckinley = c("#D5AE63", "#6E6C81", "#F7ECD8", "#3F3939", "#93AD90", "#C9B793"),
  GeneralGrant = c("#FBE697", "#F3AE6D", "#516888", "#C9DACA", "#14232A", "#557780", "#1F304A", "#802729"),
  Hawaii = c("#D67B44", "#34273B", "#D95B42", "#150718", "#F2E191"),
  CraterLake = c("#7DCCD3", "#4E7147", "#BE9C9D", "#F7ECD8", "#376597", "#9888A5", "#DBA662"),
  Saguaro = c("#847CA3", "#E45A5A", "#F4A65E", "#80792B", "#F2D56F", "#1A1237"),
  GrandTeton = c("#F0EEE2", "#5B6C88", "#48594E", "#A8D0CF", "#BABBB1"),
  BryceCanyon = c("#E39B38", "#C7D8C8", "#B6BDCC", "#BFC4C5", "#9B593F"),
  MtRainier = c("#466D53", "#83CDC0", "#D3A3A1", "#A79CA5", "#FBEAD6"),
  Badlands = c("#5495CF", "#F5AF4D", "#DB4743", "#7C873E", "#FEF4D5"),
  Redwoods = c("#769370", "#BDB2A7", "#F1C646", "#6E687E", "#F17236"),
  Everglades = c("#91D5DE", "#2E8289", "#B4674E", "#EAAE37", "#565F41"),
  Voyageurs = c("#8FC0CE", "#F6F18F", "#FDFCDE", "#238451", "#359F8B"),
  BlueRidgePkwy = c("#EC8FA3", "#FCBA65", "#FAECCF", "#8D7F99", "#8C9D57", "#163343"),
  Denali = c("#73979D", "#DADCD7", "#43200E", "#E16509", "#747669"),
  GreatBasin = c("#6BBAE5", "#E3EEF4", "#454B68", "#F9F5EA", "#81974C", "#553F31"),
  ChannelIslands = c("#F5D2E6", "#554C6C", "#EB8D43", "#70646E", "#7397CB", "#CEA347"),
  Yosemite = c("#9FC2B2", "#DFDED3", "#A49A69", "#3F5B66", "#869144"),
  Acadia = c("#FED789", "#023743", "#72874E", "#476F84", "#A4BED5", "#453947"),
  DeathValley = c("#B23539", "#FAB57C", "#F7E790", "#73652D", "#E79498", "#514289"),
  Zion = c("#469BEC", "#C9FAFF", "#F1E3B6", "#C4878C", "#6D882B")
)
# Converts log10 axis values to format 10^x
fancy_scientific <- function(l) { 
  # turn in to character string in scientific notation 
  l <- log10(l)
  # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
  return(parse(text = paste("10^",as.character(l),sep = "")))
} 

#### SET DIRECTORIES ####
# Set root directory
wd_root <- "~/usgs-erosion"

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/usgs-erosion-imports/")
# Exports folder (save all figures, tables here)
wd_exports <- paste0(wd_root,"/usgs-erosion-exports/")

wd_figures <- paste0(wd_exports, "usgs-erosion-figures/")

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_exports, wd_figures)
for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}
#### IMPORT SEDIMENT TRANSPORT DATA ####
setwd(wd_imports)

# Import USGS daily discharge and suspended sediment data
usgs_import <- fread('usgs_ssc_allSites.csv', colClasses = c(site_no = 'character'))
# Import USGS station information
usgs_stns <- fread('usgs_concavity_w2yr.csv', colClasses = c(site_no = 'character'))

# Join USGS daily data with station information
# Prepare join with key column
setkey(usgs_import, site_no)
setkey(usgs_stns, site_no)
# Make join based on key column
usgs_data <- usgs_import[, ':='(
  Q_cms = ifelse(!is.na(p00061), p00061, ifelse(!is.na(p00060), p00060, p72137)), # choose from available discharge columns
  SSC_mgL = p80154)][, ':='(
  Qss_td = Q_cms * SSC_mgL * 0.0850354) # Compute flux in tons/day
][usgs_stns[!duplicated(usgs_stns$site_no)], on = 'site_no']

# Add pre/post dam for dammed sites
usgs_data <- usgs_data[,':='(pre_post_dam = ifelse(closure_year > year(sample_dt), 'Pre-dam', 
                                                   ifelse(closure_year <= year(sample_dt), 'Post-dam',
                                                          'No-dam')))]
# Create sediment breaks for plotting
SSC_breaks <- data.table(m = rep(1,6), SSC_mgL = c(1,10,100,1000, 10000, 100000))[,
                         ':='(b0 = log10(c(1,10,100,1000, 10000, 100000)*0.0850354),
                              log10_SSC_mgL = log10(SSC_mgL))
                         ]
#### EXAMPLE PLOT AND SAVE ####
# Make example data
example_stn_data <- usgs_data[
  site_no == '06250000'][
    ,':='(log10_Q_cms = log10(Q_cms),
          log10_Qss_td = log10(Qss_td),
          log10_SSC_mgL = log10(SSC_mgL))
  ]

# Compute example linear model for Qss, with pre- and post-dam as categorical variables
example_lm <- lm(log10_Qss_td ~ log10_Q_cms + pre_post_dam, data = example_stn_data)
# Show model details and statistics
tidy(example_lm)
glance(example_lm)

# Plot example USGS station discharge-sediment flux rating curve
example_Q_Qss_plot <- ggplot(example_stn_data[order(pre_post_dam)],
  aes(x = Q_cms, 
      y = Qss_td, 
      fill = pre_post_dam)) + # color by pre/post dam, if there is a dam emplacement mid-record
  geom_point(size = 2.5, pch = 21, color = 'black', stroke = 0.2) + # control plotting parameters for the points
  geom_smooth(method = 'loess', formula = y~x, # add a fit line, this is a loess smoother but can be changed
    aes(group = pre_post_dam), se = F, color = 'black', lty = 'dashed') + # line fits each group (pre/post dam)
  # geom_abline(data = SSC_breaks, # Optional add contour lines for SSC values of a certain level
  #             aes(intercept = b0, slope = m, color = log10_SSC_mgL), lty = 'dashed') +
  scale_x_log10(labels = fancy_scientific) + # label x-axis with clean power labels
  scale_y_log10(labels = fancy_scientific) + # label x-axis with clean power labels
  scale_fill_manual(values = c('#CCD96C','#618C03','grey90')) + # specify colors for filled points
  scale_color_gradientn( # specify colors for gradient lines
    colors = c('#622BD9','#4886D9','#F2BD1D','#F24E29', '#E0142C', 'black'), oob = squish) +
  season_facet + # add theme for good looking plot
  guides(color = FALSE) + # remove legend for SSC contours
  theme(legend.position = c(0.2, 0.8)) + # position legend on top left part of graph (x, y coordinates given)
  labs( # label graph
    x = 'Discharge (cms)',
    y = 'Suspended sediment flux (tons/d)',
    fill = 'Pre/Post Dam', # title of legend
    title = example_stn_data$station_nm[1] # title of graph
  )

ggsave(example_Q_Qss_plot, filename = paste0(wd_figures, 'usgs_',example_stn_data$site_no[1],'_Q_Qss.pdf'),
                                             width = 5, height = 5)
