setwd("E:/Master/sphagnum/All_Steps_2/Debt/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(rdacca.hp)
#################################
Land_names <- c("Wetland")
periods <- periods <- c("1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")

# Wetland
for (Land in Land_names) {
  SR_shp_1911 <- st_read(paste0("./",Land,"/1911_1920/",Land,"_1911_1920_summary_lm.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1 & SR_shp_1911$Locl_R2 >= 0.3]
  
  
  for (period in periods) {
    SR_shp <- st_read(paste0("./",Land,"/",period,"/",Land,"_",period,"_summary_lm.shp"))
    SR_shp_sig <- SR_shp[SR_shp$ID %in% sig_ID,]
    st_write(SR_shp_sig, paste0("./",Land,"/",period,"/",Land,"_",period,"_lm_sig_R.shp"))
    SR_shp_sig_df <- as.data.frame(SR_shp_sig)
    write.csv(SR_shp_sig,paste0("./",Land,"/",period,"/",Land,"_",period,"_lm_sig_R.csv"))
  }
}

# Forest Grass New
Land_names <- c("Forest","Grass")
periods <- periods <- c("1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")
# Species Richness
for (Land in Land_names) {
  SR_shp_1911 <- st_read(paste0("./",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_2.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1 & SR_shp_1911$Locl_R2 >= 0.3]
  
  for (period in periods) {
    SR_shp <- st_read(paste0("./",Land,"/",period,"/",Land,"_",period,"_summary_lm_2.shp"))
    SR_shp_sig <- SR_shp[SR_shp$ID %in% sig_ID,]
    st_write(SR_shp_sig, paste0("./",Land,"/",period,"/",Land,"_",period,"_lm_2_sig_R.shp"),delete_layer = TRUE)
    SR_shp_sig_df <- as.data.frame(SR_shp_sig)
    write.csv(SR_shp_sig,paste0("./",Land,"/",period,"/",Land,"_",period,"_lm_2_sig_R.csv"))
  }
}



###### In continent
setwd("E:/Master/sphagnum/All_Steps_2/")
getwd()
# R version 4.3.1
# Encoding = UTF-8
#################################
library(terra)
library(raster)
library(sf)
library(dplyr)
library(exactextractr)
library(units)
library(tidyr)
#################################
# Global grid system
grid <- st_read("./Equal_50km_grids.shp")

# Grid by continents
grid_continent <- st_read("./grid_continent.shp")
grid_with_continent <- grid_continent[!is.na(grid_continent$Continent), ]
# st_write(grid_with_continent,"./Equal_50km_grids_with_continnet.shp")

Land_names <- c("Wetland","Forest","Grass")
periods <- c("1901_1910","1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")
grid_with_continent <- st_read("./Equal_50km_grids_with_continnet.shp")
crs_grid <- st_crs(grid_with_continent)$proj4string
raster_template <-  rast(extent=c(st_bbox(grid_with_continent)$xmin, st_bbox(grid_with_continent)$xmax, st_bbox(grid_with_continent)$ymin, st_bbox(grid_with_continent)$ymax), 
                         resolution=50000, crs=crs_grid)
polygons <- as(grid_with_continent, "Spatial")
continent_sel_ID <- as.data.frame(grid_with_continent$ID)

#Wetland
for (n in 1){
  for (i in 2:8) {
    Debt_shp <- st_read(paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_sig_R.shp"))
    Debt_select <- Debt_shp[Debt_shp$ID %in% grid_with_continent$ID,]
    st_write(Debt_select,paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_sig_select_R.shp"), delete_layer = TRUE)
    Debt_select_df <- as.data.frame(Debt_select)
    write.csv(Debt_select_df,paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_sig_select_R.csv"))
    cat(Land_names[n],":",periods[n],"Debt sig finish","\n")
  }
  cat(Land_names[n],":Debt sig finish","\n")
}

# Forest Grass New
for (n in 2:3){
  for (i in 2:8) {
    Debt_shp <- st_read(paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_2_sig_R.shp"))
    Debt_select <- Debt_shp[Debt_shp$ID %in% grid_with_continent$ID,]
    st_write(Debt_select,paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_2_sig_select_R.shp"), delete_layer = TRUE)
    Debt_select_df <- as.data.frame(Debt_select)
    write.csv(Debt_select_df,paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_2_sig_select_R.csv"))
    cat(Land_names[n],":",periods[n],"Debt sig finish","\n")
  }
  cat(Land_names[n],":Debt sig finish","\n")
}



##### Debt Table
setwd("E:/Master/sphagnum/All_Steps_2/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
#################################
Land_names <- c("Wetland","Forest","Grass")
periods <- c("1901_1910","1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")
grid_with_continent<- st_read("./Equal_50km_grids_with_continnet.shp")
grid_con_ID <- grid_with_continent$ID

# Wetland
for (n in 1) {
  Debt_SR_df  <- data.frame(ID = grid_con_ID)
  for (i in 2:8) {
    GWR <- st_read(paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_sig_select_R.shp"))
    Debt_df <- as.data.frame(list(ID=GWR$ID, Predict_SR=GWR$Delt_SR))
    names(Debt_df) <- c("ID",periods[i])
    
    Debt_SR_df <- merge(Debt_SR_df, Debt_df, by = "ID", all.x = TRUE)
  }
  write.csv(Debt_SR_df,paste0("./Tables/Debts/",Land_names[n],"_lm_Debt_SR_sig_df_select_R.csv"))
}

# Forest Grass New
for (n in 2:3) {
  Debt_SR_df  <- data.frame(ID = grid_con_ID)
  for (i in 2:8) {
    GWR <- st_read(paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_2_sig_select_R.shp"))
    Debt_df <- as.data.frame(list(ID=GWR$ID, Predict_SR=GWR$Delt_SR))
    names(Debt_df) <- c("ID",periods[i])
    
    Debt_SR_df <- merge(Debt_SR_df, Debt_df, by = "ID", all.x = TRUE)
  }
  write.csv(Debt_SR_df,paste0("./Tables/Debts/",Land_names[n],"_lm_Debt_SR_sig_df_select_2_R.csv"))
}



##### GWR Model R2 and t
setwd("E:/Master/sphagnum/All_Steps_2/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(reshape2)
library(wesanderson)
#################################
# Wetland
Land_names <- c("Wetland","Forest","Grass")
periods <- c("1901_1910","1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")
for (n in 1) {
  R2_df  <- data.frame(ID = 1:204510)
  t_df  <- data.frame(ID = 1:204510)
  for (i in 2:8) {
    GWR <- st_read(paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_sig_select_R.shp"))
    R2_period_df <- as.data.frame(list(ID=GWR$ID, Predict_SR=GWR$Locl_R2))
    names(R2_period_df) <- c("ID",periods[i])
    t_period_df <- as.data.frame(list(ID=GWR$ID, Predict_SR=GWR$t_value))
    names(t_period_df) <- c("ID",periods[i])
    
    R2_df <- merge(R2_df, R2_period_df, by = "ID", all.x = TRUE)
    t_df <- merge(t_df, t_period_df, by = "ID", all.x = TRUE)
  }
  write.csv(R2_df,paste0("./Tables/GWR_results/",Land_names[n],"_GWR_R2_lm_R.csv"))
  write.csv(t_df,paste0("./Tables/GWR_results/",Land_names[n],"_GWR_t_lm_R.csv"))
}

# Forest Grass New
for (n in 2:3) {
  R2_df  <- data.frame(ID = 1:204510)
  t_df  <- data.frame(ID = 1:204510)
  for (i in 2:8) {
    GWR <- st_read(paste0("./Debt/",Land_names[n],"/",periods[i],"/",Land_names[n],"_",periods[i],"_lm_2_sig_select_R.shp"))
    R2_period_df <- as.data.frame(list(ID=GWR$ID, Predict_SR=GWR$Locl_R2))
    names(R2_period_df) <- c("ID",periods[i])
    t_period_df <- as.data.frame(list(ID=GWR$ID, Predict_SR=GWR$t_value))
    names(t_period_df) <- c("ID",periods[i])
    
    R2_df <- merge(R2_df, R2_period_df, by = "ID", all.x = TRUE)
    t_df <- merge(t_df, t_period_df, by = "ID", all.x = TRUE)
  }
  write.csv(R2_df,paste0("./Tables/GWR_results/",Land_names[n],"_GWR_R2_lm_2_R.csv"))
  write.csv(t_df,paste0("./Tables/GWR_results/",Land_names[n],"_GWR_t_lm_2_R.csv"))
}

##### Model boxplots
setwd("E:/Master/sphagnum/All_Steps_2/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggsci)
library(ggprism)
library(lvplot)
library(wesanderson)
#################################
Land_names <- c("Wetland","Forest","Grass")
colors <- c("#bfefff", "#87cefa")
periods <- c("1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")

# Wetland
for (n in 1) {
  R2_df  <- read.csv(paste0("./Tables/GWR_results/",Land_names[n],"_GWR_R2_lm_R.csv"))
  R2_df <- R2_df[,3:9]
  names(R2_df) <- periods
  
  R2_df_long <- pivot_longer(R2_df, cols = everything(), names_to = "Periods", values_to = "Value")
  
  R2_df_long <- R2_df_long %>%
    filter(!is.na(Value) & Value != 0 )
  
  p_R2 <- ggplot(R2_df_long, aes(x = Periods, y = Value)) +
    stat_boxplot(geom="errorbar", position=position_dodge(width=0), width=0.3, linewidth=0.5) + 
    geom_boxplot(position=position_dodge(width =0.2), width=0.7, outlier.shape = NA, size=0.5, fill = colors[1]) + 
    geom_point(aes(group=Periods), position = position_dodge(0.2), alpha=0.5, size =1, color = colors[2]) + 
    scale_x_discrete( guide = "prism_offset") +
    scale_y_continuous( guide = "prism_offset") + 
    # labs(title = paste0(Land_names[n], " GWR model Local R² Boxplot"), x = "Period", y = "Local R²") +
    labs(title = "", x = "", y = "Local R²") +
    theme_prism(base_line_size =0.5) + 
    theme(plot.margin=unit(c(0.2,0.1,0.2,0.1), units="cm"),
          axis.line = element_line(color = "black", linewidth = 0),
          # panel.grid.minor = element_line(color="lightgrey", size=0.2), 
          # panel.grid.major = element_line(color="grey", size=0.3), 
          axis.title.y = element_text(color="black", size=15),
          axis.title.x = element_text(margin = margin(t = 5), color="black", size=15),
          axis.text.y = element_text(color="black", size=15),
          axis.text.x = element_text(angle = 45, margin = margin(t = 5), color="black", size=15),
          legend.position = "none",
          panel.spacing = unit(0,"lines"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          aspect.ratio = 1) + 
    coord_cartesian()
  
  ggsave(filename = paste0("./Figures/Boxplots/Models/",Land_names[n],"_GWR_R2_lm_R.png"), plot = p_R2, width = 7, height = 7, dpi = 300)
  
  
  t_df <- read.csv(paste0("./Tables/GWR_results/",Land_names[n],"_GWR_t_lm_R.csv"))
  t_df <- t_df[,3:9]
  names(t_df) <- periods
  
  t_df_long <- pivot_longer(t_df, cols = everything(), names_to = "Periods", values_to = "Value")
  
  t_df_long <- t_df_long %>%
    filter(!is.na(Value) & Value != 0 )
  
  p_t <- ggplot(t_df_long, aes(x = Periods, y = Value)) +
    stat_boxplot(geom="errorbar", position=position_dodge(width=0), width=0.3, linewidth=0.5) + 
    geom_boxplot(position=position_dodge(width =0.2), width=0.7, outlier.shape = NA, size=0.5, fill = colors[1]) + 
    geom_point(aes(group=Periods), position = position_dodge(0.2), alpha=0.5, size =1, color = colors[2]) + 
    scale_x_discrete( guide = "prism_offset") +
    scale_y_continuous( guide = "prism_offset") + 
    labs(title = paste0(Land_names[n], " GWR model t value Boxplot"), x = "Period", y = "t value") +
    labs(title = "", x = "", y = "t value") +
    theme_prism(base_line_size =0.5) + 
    theme(plot.margin=unit(c(0.2,0.1,0.2,0.1), units="cm"),
          axis.line = element_line(color = "black", linewidth = 0),
          # panel.grid.minor = element_line(color="lightgrey", size=0.2), 
          # panel.grid.major = element_line(color="grey", size=0.3), 
          axis.title.y = element_text(color="black", size=15),
          axis.title.x = element_text(margin = margin(t = 5), color="black", size=15),
          axis.text.y = element_text(color="black", size=15),
          axis.text.x = element_text(angle = 45, margin = margin(t = 5), color="black", size=15),
          legend.position = "none",
          panel.spacing = unit(0,"lines"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          aspect.ratio = 1) +
    coord_cartesian()
  
  ggsave(filename = paste0("./Figures/Boxplots/Models/",Land_names[n],"_GWR_t_lm_R.png"), plot = p_t, 
         width = 7, height = 7, dpi = 300)
}

# Forest Grass New
for (n in 2:3) {
  R2_df  <- read.csv(paste0("./Tables/GWR_results/",Land_names[n],"_GWR_R2_lm_2_R.csv"))
  R2_df <- R2_df[,3:9]
  names(R2_df) <- periods
  
  R2_df_long <- pivot_longer(R2_df, cols = everything(), names_to = "Periods", values_to = "Value")
  
  R2_df_long <- R2_df_long %>%
    filter(!is.na(Value) & Value != 0 )
  
  p_R2 <- ggplot(R2_df_long, aes(x = Periods, y = Value)) +
    stat_boxplot(geom="errorbar", position=position_dodge(width=0), width=0.3, linewidth=0.5) + 
    geom_boxplot(position=position_dodge(width =0.2), width=0.7, outlier.shape = NA, size=0.5, fill = colors[1]) + 
    geom_point(aes(group=Periods), position = position_dodge(0.2), alpha=0.5, size =1, color = colors[2]) + 
    scale_x_discrete( guide = "prism_offset") +
    scale_y_continuous( guide = "prism_offset") + 
    # labs(title = paste0(Land_names[n], " GWR model Local R² Boxplot"), x = "Period", y = "Local R²") +
    labs(title = "", x = "", y = "Local R²") +
    theme_prism(base_line_size =0.5) + 
    theme(plot.margin=unit(c(0.2,0.1,0.2,0.1), units="cm"),
          axis.line = element_line(color = "black", linewidth = 0),
          # panel.grid.minor = element_line(color="lightgrey", size=0.2), 
          # panel.grid.major = element_line(color="grey", size=0.3), 
          axis.title.y = element_text(color="black", size=15),
          axis.title.x = element_text(margin = margin(t = 5), color="black", size=15),
          axis.text.y = element_text(color="black", size=15),
          axis.text.x = element_text(angle = 45, margin = margin(t = 5), color="black", size=15),
          legend.position = "none",
          panel.spacing = unit(0,"lines"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          aspect.ratio = 1) + 
    coord_cartesian()
  ggsave(filename = paste0("./Figures/Boxplots/Models/",Land_names[n],"_GWR_R2_lm_2_R.png"), plot = p_R2, width = 7, height = 7, dpi = 300)
  
  
  t_df <- read.csv(paste0("./Tables/GWR_results/",Land_names[n],"_GWR_t_lm_2_R.csv"))
  t_df <- t_df[,3:9]
  names(t_df) <- periods
  
  t_df_long <- pivot_longer(t_df, cols = everything(), names_to = "Periods", values_to = "Value")
  
  t_df_long <- t_df_long %>%
    filter(!is.na(Value) & Value != 0 )
  
  p_t <- ggplot(t_df_long, aes(x = Periods, y = Value)) +
    stat_boxplot(geom="errorbar", position=position_dodge(width=0), width=0.3, linewidth=0.5) + 
    geom_boxplot(position=position_dodge(width =0.2), width=0.7, outlier.shape = NA, size=0.5, fill = colors[1]) + 
    geom_point(aes(group=Periods), position = position_dodge(0.2), alpha=0.5, size =1, color = colors[2]) + 
    scale_x_discrete( guide = "prism_offset") +
    scale_y_continuous( guide = "prism_offset") + 
    # labs(title = paste0(Land_names[n], " GWR model t value Boxplot"), x = "Period", y = "t value") +
    labs(title = "", x = "", y = "t value") +
    theme_prism(base_line_size =0.5) + 
    theme(plot.margin=unit(c(0.2,0.1,0.2,0.1), units="cm"),
          axis.line = element_line(color = "black", linewidth = 0),
          # panel.grid.minor = element_line(color="lightgrey", size=0.2), 
          # panel.grid.major = element_line(color="grey", size=0.3), 
          axis.title.y = element_text(color="black", size=15),
          axis.title.x = element_text(margin = margin(t = 5), color="black", size=15),
          axis.text.y = element_text(color="black", size=15),
          axis.text.x = element_text(angle = 45, margin = margin(t = 5), color="black", size=15),
          legend.position = "none",
          panel.spacing = unit(0,"lines"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          aspect.ratio = 1) + 
    coord_cartesian()
  
  ggsave(filename = paste0("./Figures/Boxplots/Models/",Land_names[n],"_GWR_t_lm_2_R.png"), plot = p_t, 
         width = 7, height = 7, dpi = 300)
}

##### Sum_std
setwd("E:/Master/sphagnum/All_Steps_2/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sf)
library(readxl)
library(dplyr)
library(sp)
library(GWmodel)
# library(tmap)
# library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(ggplot2)
library(st)
library(exactextractr)
#################################
# Create a  SpatialPolygonsDataFrame 
polygons <- st_read("./grid_continent.shp")
polygons <- polygons[!is.na(polygons$Continent),]
polygons <- as(polygons, "Spatial")

# Average species richness in different periods
periods <- c("1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")

# bw_df <- c(19,19,24,17,25,19,18)

for (i in 5:7) {
  SR <- raster(paste0("./Species_Richness/",periods[i],"_all_species_raster.tif"))
  # plot(SR, main = paste0("Richness patterns of sphagnum between ",periods[i]))
  
  ID <- 1:ncell(SR)
  
  # Add species richness in the polygons
  SR_extract <- exact_extract(SR,polygons,"sum")
  SR_df <- as.data.frame(SR_extract)
  SR_df <- as.data.frame(lapply(SR_df, function(x) round(as.numeric(x))))
  # SR_df$SR_extract[SR_df$SR_extract<1]<-0
  polygons$Observed_SR <- SR_df$SR_extract
  
  # Calculate Sum area from 1901 to 1910
  land_shp <- st_read("./Land/Land_clean/Land_sum_1901_1910.shp")
  polygons$Sum_1901 <- land_shp$Sum
  polygons$Wetland_1901 <- land_shp$Wetland
  polygons$Forest_1901 <- land_shp$Forest
  polygons$Grass_1901 <- land_shp$Grass
  
  ##### Select observed_SR > 0 and Sum area > 0
  SR_Land_sp <- polygons[polygons$Observed_SR > 0 & polygons$Sum_1901 > 0,]
  
  mean_obs <- mean(SR_Land_sp$Observed_SR)
  sd_obs <- sd(SR_Land_sp$Observed_SR)
  mean_wet <- mean(SR_Land_sp$Wetland_1901)
  sd_wet <- sd(SR_Land_sp$Wetland_1901)
  mean_for <- mean(SR_Land_sp$Forest_1901)
  sd_for <- sd(SR_Land_sp$Forest_1901)
  mean_gra <- mean(SR_Land_sp$Grass_1901)
  sd_gra <- sd(SR_Land_sp$Grass_1901)
  
  SR_Land_sp$Observed_SR_std <- (SR_Land_sp$Observed_SR - mean_obs) / sd_obs
  SR_Land_sp$Wetland_1901_std <- (SR_Land_sp$Wetland_1901 - mean_wet) / sd_wet
  SR_Land_sp$Forest_1901_std <- (SR_Land_sp$Forest_1901 - mean_for) / sd_for
  SR_Land_sp$Grass_1901_std <- (SR_Land_sp$Grass_1901 - mean_gra) / sd_gra
  
  
  #choose ID with SR
  ID_sel <- as.data.frame(SR_Land_sp$ID)
  names(ID_sel) <- "ID"
  
  
  # Load necessary libraries
  library(GWmodel)
  library(parallel)
  library(doParallel)
  
  # Create a cluster with the desired number of cores
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  
  # Register the cluster
  registerDoParallel(cl)
  
  #List of functions
  # functions <- c("gaussian","exponential","bisquare","tricube","boxcar")
  functions <- c("gaussian")
  # gaussian: wgt = exp(-.5*(vdist/bw)^2);
  # exponential: wgt = exp(-vdist/bw);
  # bisquare: wgt = (1-(vdist/bw)^2)^2 if vdist < bw, wgt=0 otherwise;
  # tricube: wgt = (1-(vdist/bw)^3)^3 if vdist < bw, wgt=0 otherwise;
  # boxcar: wgt=1 if dist < bw, wgt=0 otherwise
  
  # Run the bw.gwr function
  # Initialize an empty data frame
  bw <- data.frame()
  
  # Loop through each function in the list of functions
  for (choose_func in functions) {
    tryCatch({
      # Redirect the output to a file
      sink_file <- paste0("./Debt/Land_weight/",periods[i],"/Sum_weight_bw_", choose_func, "_lm.txt")
      sink(sink_file)
      
      # Execute the bw.gwr function and store the result
      bw.gwr.func <- bw.gwr(Observed_SR ~ Wetland_1901+Forest_1901+Grass_1901,
                            data = SR_Land_sp,
                            approach = "AICc",
                            kernel = choose_func,
                            adaptive = TRUE,
                            longlat = FALSE,
                            parallel.method = "cluster",
                            parallel.arg = cl)
      
      # Print the result (optional, for checking purposes)
      print(bw.gwr.func)
      
      # Add the result to the data frame
      bw <- rbind(bw, data.frame(bw.gwr.func))
      
      # Stop redirecting the output
      sink()
    }, error = function(e) {
      # Ensure to stop redirecting the output if an error occurs
      if (sink.number() > 0) {
        sink()
      }
      
      # Print the error message and more detailed debug info
      message("Error with function ", choose_func, ": ", e$message)
      message("Details: ", capture.output(print(e)))
      
      # Continue to the next iteration
      next
    })
  }
  # Stop the cluster after computation is complete
  stopCluster(cl)
  #
  #
  #
  # row.names(bw) <- c("gaussian","exponential","bisquare","tricube","boxcar")
  row.names(bw) <- c("gaussian")
  print(bw)
  # gaussian:Adaptive bandwidth (number of nearest neighbours): 16 AICc value: 249259.7
  # exponential：Adaptive bandwidth (number of nearest neighbours): 16 AICc value: 250888.1
  # bisquare:Adaptive bandwidth (number of nearest neighbours): 23 AICc value: 241324
  # tricube:Adaptive bandwidth (number of nearest neighbours): 23 AICc value: 241225.9
  # boxcar:Adaptive bandwidth (number of nearest neighbours): 16 AICc value: 245429.3
  
  bw.gwr.Sum <- bw$bw.gwr.func
  
  # bw.gwr.Sum <- bw_df[i]
  # Run the gwr.basic function
  # Create a cluster with the desired number of cores
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  
  # Register the cluster
  registerDoParallel(cl)
  
  
  gwr.res.Sum <- gwr.basic(Observed_SR ~ Wetland_1901+Forest_1901+Grass_1901,
                           data=SR_Land_sp,
                           bw = bw.gwr.Sum,
                           adaptive = TRUE,
                           longlat = FALSE,
                           kernel ="gaussian",
                           parallel.method="cluster",
                           parallel.arg = cl)
  
  stopCluster(cl)
  # gwr.res.Sum <- readRDS(paste0("./Debt/Land_weight/",periods[i],"/gwr.res.Sum_weight_lm.rds"))
  
  saveRDS(gwr.res.Sum,paste0("./Debt/Land_weight/",periods[i],"/gwr.res.Sum_weight_lm.rds"))
  sink(paste0("./Debt/Land_weight/",periods[i],"/gwr.res.Sum_weight_lm.txt"))
  print(gwr.res.Sum)
  sink()
  
  
  
  # Create a cluster with the desired number of cores
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  
  # Register the cluster
  registerDoParallel(cl)
  
  gwr.res.std <- gwr.basic(Observed_SR_std ~ Wetland_1901_std + Forest_1901_std + Grass_1901_std,
                           data = SR_Land_sp,
                           bw = bw.gwr.Sum, 
                           adaptive = TRUE,
                           longlat = FALSE,
                           kernel = "gaussian",
                           parallel.method = "cluster",
                           parallel.arg = cl)
  stopCluster(cl)
  # gwr.res.std <- readRDS(paste0("./Debt/Land_weight/",periods[i],"/gwr.res.Sum_weight_lm_std.rds"))
  saveRDS(gwr.res.std,paste0("./Debt/Land_weight/",periods[i],"/gwr.res.Sum_weight_lm_std.rds"))
  sink(paste0("./Debt/Land_weight/",periods[i],"/gwr.res.Sum_weight_lm_std.txt"))
  print(gwr.res.std)
  sink()
  
  # GWR coefficients dataframe
  gwr.coef.Sum <- as.data.frame(gwr.res.Sum$SDF) 
  gwr.coef.std <- as.data.frame(gwr.res.std$SDF)  
  
  gwr.coef.Sum$ID <- ID_sel$ID
  gwr.coef.std$ID <- ID_sel$ID
  
  gwr.coef.Sum <- gwr.coef.Sum[, c("ID", setdiff(names(gwr.coef.Sum), "ID"))]
  
  gwr.coef.std <- gwr.coef.std[, c("ID", setdiff(names(gwr.coef.std), "ID"))]
  names(gwr.coef.std) <- gsub("_std$", "_coef", names(gwr.coef.std))
  
  # SDF：
  # Intercept:intercept
  # log_Sum:coefficient of ln(Sum_1901)
  # y:obersved SR
  # yhat:predict obersved SR where land area is Sum_1901
  # resIDual:resIDual
  # CV_Score:coefficient of variation
  # Stud_resIDual:student residual test
  # Intercept_SE:coefficient standard errors of intercept
  # log_Sum_SE:coefficient standard errors of Sum_1901
  # Intercept_TV:t-value of intercept
  # log_Sum_TV:t_value of Sum_1901       
  # Local_R2:Local Coefficient of Determination
  
  ##### Predict species richness
  # Calculae Sum area during different periods
  land_shp <- st_read(paste0("./Land/Land_clean/Land_sum_",periods[i],".shp"))
  land_area <- as.data.frame(land_shp)
  land_area <- land_area[land_area$ID %in% SR_Land_sp$ID,]
  # land_area <- land_area[land_area$Sum >0,]
  
  col_name <- paste0("Sum_",1901+i*10)
  SR_Land_sp@data[col_name] <- land_area$Sum
  col_name <- paste0("Wetland_",1901+i*10)
  SR_Land_sp@data[col_name] <- land_area$Wetland
  col_name <- paste0("Forest_",1901+i*10)
  SR_Land_sp@data[col_name] <- land_area$Forest
  col_name <- paste0("Grass_",1901+i*10)
  SR_Land_sp@data[col_name] <- land_area$Grass
  
  col_name <- paste0("Sum_",1901+i*10)
  SR_Land_sp <- SR_Land_sp[SR_Land_sp[[col_name]]>0,]
  
  # Select gwr.coef.Sum
  gwr.coef.Sum.sel <- gwr.coef.Sum[gwr.coef.Sum$ID %in% SR_Land_sp$ID, ]
  
  #Predict Species Richness
  SR_Land_sp$Predict_SR <- NA
  for (n in 1:nrow(SR_Land_sp)) {
    SR_Land_sp$Predict_SR[n] <- round(gwr.coef.Sum.sel$Intercept[n] + 
                                        gwr.coef.Sum.sel$Wetland_1901[n] * SR_Land_sp[[paste0("Wetland_",1901+i*10)]][n]+
                                        gwr.coef.Sum.sel$Forest_1901[n] * SR_Land_sp[[paste0("Forest_",1901+i*10)]][n]+
                                        gwr.coef.Sum.sel$Grass_1901[n] * SR_Land_sp[[paste0("Grass_",1901+i*10)]][n])
  }
  
  SR_Land_sp$Predict_SR[SR_Land_sp$Predict_SR < 0] <- 0
  
  # significant t_value
  signif_t <- data.frame(
    ID = gwr.coef.Sum.sel$ID,
    Wetland_t_value = gwr.coef.Sum.sel$Wetland_1901_TV,
    Forest_t_value = gwr.coef.Sum.sel$Forest_1901_TV,
    Grass_t_value = gwr.coef.Sum.sel$Grass_1901_TV
  )
  
  signif_t <- signif_t %>%
    mutate(Wetland_significant = case_when(
      abs(Wetland_t_value) < 1.96 ~ 0, # p > 0.05
      abs(Wetland_t_value) >= 1.96 & abs(Wetland_t_value) < 2.58 ~ 1, # 0.05 > p > 0.01
      abs(Wetland_t_value) >= 2.58 & abs(Wetland_t_value) < 3.29 ~ 2, # 0.01 > p > 0.001
      abs(Wetland_t_value) >= 3.29 & abs(Wetland_t_value) < 3.89 ~ 3, # 0.001 > p > 0.0001
      abs(Wetland_t_value) >= 3.89 ~ 4 # p <= 0.0001
    ),
    Forest_significant = case_when(
      abs(Forest_t_value) < 1.96 ~ 0,
      abs(Forest_t_value) >= 1.96 & abs(Forest_t_value) < 2.58 ~ 1,
      abs(Forest_t_value) >= 2.58 & abs(Forest_t_value) < 3.29 ~ 2,
      abs(Forest_t_value) >= 3.29 & abs(Forest_t_value) < 3.89 ~ 3,
      abs(Forest_t_value) >= 3.89 ~ 4
    ),
    Grass_significant = case_when(
      abs(Grass_t_value) < 1.96 ~ 0,
      abs(Grass_t_value) >= 1.96 & abs(Grass_t_value) < 2.58 ~ 1,
      abs(Grass_t_value) >= 2.58 & abs(Grass_t_value) < 3.29 ~ 2,
      abs(Grass_t_value) >= 3.29 & abs(Grass_t_value) < 3.89 ~ 3,
      abs(Grass_t_value) >= 3.89 ~ 4
    ))
  names(signif_t) <- c("ID","Wetland_t_value","Forest_t_value","Grass_t_value",
                       "Wetland_significant","Forest_significant","Grass_significant")
  
  
  
  # Summary result
  Result <- SR_Land_sp
  # Delta SR
  Result$Delta_SR <- Result$Predict_SR - Result$Observed_SR
  Result$Perc_Delta <- (Result$Predict_SR - Result$Observed_SR)/Result$Predict_SR
  
  # Delta  area
  Result$Delta_Sum <- Result$Sum_1901-Result[[paste0("Sum_",1901+i*10)]]
  Result$Delta_Wetland <- Result$Wetland_1901-Result[[paste0("Wetland_",1901+i*10)]]
  Result$Delta_Forest <- Result$Forest_1901-Result[[paste0("Forest_",1901+i*10)]]
  Result$Delta_Grass <- Result$Grass_1901-Result[[paste0("Grass_",1901+i*10)]]
  
  # Summary result
  Result <- merge(Result,signif_t,by ="ID")
  Result <- merge(Result,gwr.coef.Sum.sel,by="ID")
  names(Result)
  names(Result)[names(Result)=="Sum_1901.x"] <- "Sum_1901"
  names(Result)[names(Result)=="Wetland_1901.x"] <- "Wetland_1901"
  names(Result)[names(Result)=="Forest_1901.x"] <- "Forest_1901"
  names(Result)[names(Result)=="Grass_1901.x"] <- "Grass_1901"
  
  
  
  gwr.coef.std.sel <- gwr.coef.std[gwr.coef.std$ID %in% SR_Land_sp$ID, ]
  gwr.coef.std.sel_coef <- gwr.coef.std.sel[, c(1, 3:5)]
  names(gwr.coef.std.sel_coef) <- c("ID","Wetland_coef","Forest_coef","Grass_coef")
  Result <- merge(Result,gwr.coef.std.sel_coef ,by="ID")
  
  # names(Result)
  # "ID":cell ID
  # "Area": feature area
  # "Observed_SR":1971020 Species Richness
  # "Sum_1901": Mean Sum area during 1901 to 1910
  # "log_SR":ln(Observed_SR)         
  # "log_Sum":ln(Sum_1901) 
  # "Sum_present": Mean Sum area during 1901 to 1910
  # "log_Sum_present":ln(Sum_presnt)
  # "Predict_SR": preficted Species richness at present based on mean Sum area during 1971 to 2019
  # "Delta_SR":Predict_SR - Observed_SR         
  # "Perc_Delta":(Predict_SR - Observed_SR)/Predict_SR 
  # "Delta_Sum":Sum_present - Sum_1901
  # "t_value":t_value
  # "significant":
  #        |t_value|< 1.96 signif=0
  #        1.96 <= |t_value|< 2.58 signif=1
  #        2.58<= |t_value|< 3.29 signif=2
  #        3.29 <= |t_value|< 3.89 signif=3
  #        3.89 <= |t_value| signif=4
  # Intercept:intercept
  # log_Sum.y:coefficient of ln(Sum_1901)
  # y:obersved SR
  # yhat:predict obersved SR where land area is Sum_1901
  # residual:residual
  # CV_Score:coefficient of variation
  # Stud_resodual:student residual test
  # Intercept_SE:coefficient standard errors of intercept
  # log_Sum_SE:coefficient standard errors of Sum_1901
  # Intercept_TV:t-value of intercept
  # log_Sum_TV:t_value of Sum_1901       
  # Local_R2:Local Coefficient of Determination
  
  Result_df <- as.data.frame(Result)
  write.csv(Result_df,paste0("./Debt/Land_weight/",periods[i],"/Sum_weight_",periods[i],"_lm_std.csv"))
  Result_sf <- st_as_sf(Result)
  names(Result_sf)
  st_write(Result_sf, paste0("./Debt/Land_weight/",periods[i],"/Sum_weight_",periods[i],"_lm_std.shp"),delete_dsn = TRUE)
  
}



setwd("E:/Master/sphagnum/All_Steps_2/Debt/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(rdacca.hp)
#################################
Land_names <- c("Sum")
periods <- periods <- c("1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")

# Sum
for (Land in Land_names) {
  SR_shp_1911 <- st_read(paste0("./Land_weight/1911_1920/",Land,"_weight_1911_1920_lm_std.shp"))
  sig_ID <- SR_shp_1911$ID[((SR_shp_1911$Wtlnd_s >= 1)|(SR_shp_1911$Frst_sg >= 1)|(SR_shp_1911$Grss_sg >= 1))
                           & (SR_shp_1911$Locl_R2>= 0.3) ]
  
  
  for (period in periods) {
    SR_shp <- st_read(paste0("./Land_weight/",period,"/",Land,"_weight_",period,"_lm_std.shp"))
    SR_shp_sig <- SR_shp[SR_shp$ID %in% sig_ID,]
    st_write(SR_shp_sig, paste0("./Land_weight/",period,"/",Land,"_weight_",period,"_lm_sig_R_std.shp"),delete_dsn = TRUE)
    SR_shp_sig_df <- as.data.frame(SR_shp_sig)
    write.csv(SR_shp_sig,paste0("./Land_weight/",period,"/",Land,"_weight_",period,"_lm_sig_R_std.csv"))
  }
  
}

