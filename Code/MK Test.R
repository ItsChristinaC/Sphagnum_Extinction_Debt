##### Debt Mk tets
##### Extract Delta SR with R
setwd("E:/Master/sphagnum/All_Steps_2/Debt/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sf)
library(dplyr)
#################################
# grid
grid <- st_read("E:/Master/sphagnum/All_Steps_2/Equal_50km_grids.shp")
crs_grid <- st_crs(grid)$proj4string
sample_rast <- rast("E:/Master/sphagnum/All_Steps_2/template_5km_grid.tif")

# Stack Delta_SR in different period
Land_names <- c("Wetland","Forest","Grass")
periods <- c("1911_1920","1921_1930","1931_1940","1941_1950","1951_1960","1961_1970","1971_2020")

# Wetland
for (i in 1){
  Delta_SR_stack <- raster()
  for (period in periods) {
    Land_debt_shp <- st_read(paste0("./",Land_names[i],"/",period,"/",Land_names[i],"_",period,"_lm_sig_select_R.shp"))
    filed_chosen <-  names(Land_debt_shp)[7]
    SR_debt_raster <- raster(rasterize(Land_debt_shp,sample_rast,field = filed_chosen))
    names(SR_debt_raster) <- paste0(period)
    Delta_SR_stack <- stack(Delta_SR_stack,SR_debt_raster)
  }
  writeRaster(Delta_SR_stack,paste0("./",Land_names[i],"/",Land_names[i],"_Delta_SR_lm(sig)_select_R.tif"),overwrite=TRUE)
}

for (i in 1){
  grid_og <- grid
  for (period in periods) {
    Land_debt_shp <- st_read(paste0("./",Land_names[i],"/",period,"/",Land_names[i],"_",period,"_lm_sig_select_R.shp"))
    debt_df <- data.frame(ID=Land_debt_shp[[1]],Del_SR=Land_debt_shp[[7]])
    merged_df <- merge(grid_og, debt_df[, c("ID", "Del_SR")], by = "ID", all.x = TRUE)
    grid_og[paste0(period)] <- merged_df$Del_SR
  }
  st_write(grid_og,paste0("./",Land_names[i],"/",Land_names[i],"_Delta_SR_lm(sig)_select_R.shp"),overwrite=TRUE)
}

# Forest Grass New
for (i in 2:3){
  Delta_SR_stack <- raster()
  for (period in periods) {
    Land_debt_shp <- st_read(paste0("./",Land_names[i],"/",period,"/",Land_names[i],"_",period,"_lm_2_sig_select_R.shp"))
    filed_chosen <-  names(Land_debt_shp)[7]
    SR_debt_raster <- raster(rasterize(Land_debt_shp,sample_rast,field = filed_chosen))
    names(SR_debt_raster) <- paste0(period)
    Delta_SR_stack <- stack(Delta_SR_stack,SR_debt_raster)
  }
  writeRaster(Delta_SR_stack,paste0("./",Land_names[i],"/",Land_names[i],"_Delta_SR_lm(sig)_2_select_R.tif"),overwrite=TRUE)
}

for (i in 2:3){
  grid_og <- grid
  for (period in periods) {
    Land_debt_shp <- st_read(paste0("./",Land_names[i],"/",period,"/",Land_names[i],"_",period,"_lm_2_sig_select_R.shp"))
    debt_df <- data.frame(ID=Land_debt_shp[[1]],Del_SR=Land_debt_shp[[7]])
    merged_df <- merge(grid_og, debt_df[, c("ID", "Del_SR")], by = "ID", all.x = TRUE)
    grid_og[paste0(period)] <- merged_df$Del_SR
  }
  st_write(grid_og,paste0("./",Land_names[i],"/",Land_names[i],"_Delta_SR_lm(sig)_2_select_R.shp"),overwrite=TRUE)
}





##### Delta SR MK test
setwd("E:/Master/sphagnum/All_Steps_2/Debt/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sp)
library(dplyr)
library(trend)
#################################
# grid
grid <- st_read("E:/Master/sphagnum/All_Steps_2/Equal_50km_grids.shp")
sample_rast <- rast("E:/Master/sphagnum/All_Steps_2/template_5km_grid.tif")

Land_names <- c("Wetland","Forest","Grass")
time_stamps <- c("1911","1921","1931","1941","1951","1961","1971")

for (i in 1) {
  Land_name <- Land_names[i]
  dSR_shp <- st_read(paste0("./",Land_name,"/",Land_name,"_Delta_SR_lm(sig)_select_R.shp"))
  dSR_shp <- na.omit(dSR_shp)
  
  # dSR_shp <- Result_shp[[i]]
  dSR_df <- as.data.frame(dSR_shp[2:9])
  dSR_df <- dSR_df[, -ncol(dSR_df)]
  
  mk_df <- data.frame(ID=dSR_shp$ID)
  slope_list <- list()
  p_value_list <- list()
  Zs_list <- list()
  
  grid_og <- grid
  
  for (n in 1:nrow(dSR_df)) {
    Delta_SR_row <- dSR_df[n,]
    
    value <- t(Delta_SR_row)
    
    ts_mean_values <- ts(value, start = 1911, frequency = 1)
    
    MK_estimate <- sens.slope(ts_mean_values, conf.level = 0.95)
    
    slope <- MK_estimate$estimate
    MK_test <- MK_estimate$p.value
    Zs <- MK_estimate$statistic
    
    slope_list[[n]] <- slope
    p_value_list[[n]] <- MK_test
    Zs_list[[n]] <- Zs
  }
  mk_df$slope <- unlist(slope_list)
  mk_df$p_value <- unlist(p_value_list)
  mk_df$Zs <- unlist(Zs_list)
  
  grid_og <- grid_og %>%
    left_join(mk_df, by = "ID")
  
  st_write(grid_og,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_MK_test_select(sig)_R.shp"))
  
  slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
  plot(slope_raster)
  writeRaster(slope_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_slope_select(sig)_R.tif"),overwrite=TRUE)
  
  
  p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
  plot(p_value_raster)
  writeRaster(p_value_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_p_value_select(sig)_R.tif"),overwrite=TRUE)
  
  Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
  plot(Zs_raster)
  writeRaster(Zs_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_Zs_select(sig)_R.tif"),overwrite=TRUE)
}

##### Forest Grass New
for (i in 2:3) {
  Land_name <- Land_names[i]
  dSR_shp <- st_read(paste0("./",Land_name,"/",Land_name,"_Delta_SR_lm(sig)_2_select_R.shp"))
  dSR_shp <- na.omit(dSR_shp)
  
  # dSR_shp <- Result_shp[[i]]
  dSR_df <- as.data.frame(dSR_shp[2:9])
  dSR_df <- dSR_df[, -ncol(dSR_df)]
  
  mk_df <- data.frame(ID=dSR_shp$ID)
  slope_list <- list()
  p_value_list <- list()
  Zs_list <- list()
  
  grid_og <- grid
  
  for (n in 1:nrow(dSR_df)) {
    Delta_SR_row <- dSR_df[n,]
    
    value <- t(Delta_SR_row)
    
    ts_mean_values <- ts(value, start = 1911, frequency = 1)
    
    MK_estimate <- sens.slope(ts_mean_values, conf.level = 0.95)
    
    slope <- MK_estimate$estimate
    MK_test <- MK_estimate$p.value
    Zs <- MK_estimate$statistic
    
    slope_list[[n]] <- slope
    p_value_list[[n]] <- MK_test
    Zs_list[[n]] <- Zs
  }
  mk_df$slope <- unlist(slope_list)
  mk_df$p_value <- unlist(p_value_list)
  mk_df$Zs <- unlist(Zs_list)
  
  grid_og <- grid_og %>%
    left_join(mk_df, by = "ID")
  
  st_write(grid_og,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_2_MK_test_select(sig)_R.shp"))
  
  slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
  plot(slope_raster)
  writeRaster(slope_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_2_slope_select(sig)_R.tif"),overwrite=TRUE)
  
  
  p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
  plot(p_value_raster)
  writeRaster(p_value_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_2_p_value_select(sig)_R.tif"),overwrite=TRUE)
  
  Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
  plot(Zs_raster)
  writeRaster(Zs_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_2_Zs_select(sig)_R.tif"),overwrite=TRUE)
}


# Class
Land_names <- c("Wetland","Forest","Grass")
for (i in 1) {
  Land <- Land_names[i]
  dSR_mk_shp <- st_read(paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_MK_test_select(sig)_R.shp"))
  
  dSR_mk_shp <-  dSR_mk_shp[2:5]
  # names(dSR_mk_shp) <- c("ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  dSR_mk_shp <- dSR_mk_shp %>%
    filter(!is.na(slope), !is.na(Zs))
  
  dSR_mk_shp <- dSR_mk_shp %>%
    mutate(slope_class = case_when(
      slope > 0 ~ 1,
      slope < 0 ~ -1, 
      slope == 0 ~ 0,
      # TRUE ~ NA_real_
    )) %>%
    mutate(Zs_class = case_when(
      abs(Zs) >= 1.96 ~ 2,
      Zs > -1.96 & Zs < 1.96 ~ 1
    )) %>%
    mutate(class_column = slope_class * Zs_class)
  
  
  
  names(dSR_mk_shp) <- c("ID","dSR_slope","dSR_p","dSR_Zs","geometry","slope_class","Zs_class","dSR_class")
  dSR_mk_shp_df <- st_drop_geometry(dSR_mk_shp)
  dSR_mk_shp_df <- as.data.frame(dSR_mk_shp_df)
  
  write.csv(dSR_mk_shp_df,paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_class_select(sig)_R.csv"))
  
  st_write(dSR_mk_shp,paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_class_select(sig)_R.shp"),delete_dsn = TRUE, delete_layer = TRUE)
  
}

# Forest Grass New
for (i in 2:3) {
  Land <- Land_names[i]
  dSR_mk_shp <- st_read(paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select(sig)_R.shp"))
  
  dSR_mk_shp <-  dSR_mk_shp[2:5]
  # names(dSR_mk_shp) <- c("ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  dSR_mk_shp <- dSR_mk_shp %>%
    filter(!is.na(slope), !is.na(Zs))
  
  dSR_mk_shp <- dSR_mk_shp %>%
    mutate(slope_class = case_when(
      slope > 0 ~ 1,
      slope < 0 ~ -1, 
      slope == 0 ~ 0,
      # TRUE ~ NA_real_
    )) %>%
    mutate(Zs_class = case_when(
      abs(Zs) >= 1.96 ~ 2,
      Zs > -1.96 & Zs < 1.96 ~ 1
    )) %>%
    mutate(class_column = slope_class * Zs_class)
  
  
  
  names(dSR_mk_shp) <- c("ID","dSR_slope","dSR_p","dSR_Zs","geometry","slope_class","Zs_class","dSR_class")
  dSR_mk_shp_df <- st_drop_geometry(dSR_mk_shp)
  dSR_mk_shp_df <- as.data.frame(dSR_mk_shp_df)
  
  write.csv(dSR_mk_shp_df,paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_class_2_select(sig)_R.csv"))
  st_write(dSR_mk_shp,paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_class_2_select(sig)_R.shp"),delete_dsn = TRUE, delete_layer = TRUE)
}




##### Wetland 2
for (i in 1) {
  Land_name <- Land_names[i]
  dSR_shp <- st_read(paste0("./",Land_name,"/",Land_name,"_Delta_SR_lm(sig)_2_select_R.shp"))
  dSR_shp <- na.omit(dSR_shp)
  
  # dSR_shp <- Result_shp[[i]]
  dSR_df <- as.data.frame(dSR_shp[2:9])
  dSR_df <- dSR_df[, -ncol(dSR_df)]
  
  mk_df <- data.frame(ID=dSR_shp$ID)
  slope_list <- list()
  p_value_list <- list()
  Zs_list <- list()
  
  grid_og <- grid
  
  for (n in 1:nrow(dSR_df)) {
    Delta_SR_row <- dSR_df[n,]
    
    value <- t(Delta_SR_row)
    
    ts_mean_values <- ts(value, start = 1911, frequency = 1)
    
    MK_estimate <- sens.slope(ts_mean_values, conf.level = 0.95)
    
    slope <- MK_estimate$estimate
    MK_test <- MK_estimate$p.value
    Zs <- MK_estimate$statistic
    
    slope_list[[n]] <- slope
    p_value_list[[n]] <- MK_test
    Zs_list[[n]] <- Zs
  }
  mk_df$slope <- unlist(slope_list)
  mk_df$p_value <- unlist(p_value_list)
  mk_df$Zs <- unlist(Zs_list)
  
  grid_og <- grid_og %>%
    left_join(mk_df, by = "ID")
  
  st_write(grid_og,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_2_MK_test_select(sig)_R.shp"))
  
  slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
  plot(slope_raster)
  writeRaster(slope_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_2_slope_select(sig)_R.tif"),overwrite=TRUE)
  
  
  p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
  plot(p_value_raster)
  writeRaster(p_value_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_2_p_value_select(sig)_R.tif"),overwrite=TRUE)
  
  Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
  plot(Zs_raster)
  writeRaster(Zs_raster,paste0("./MK_test/Debt_SR/",Land_name,"/",Land_name,"_Del_SR_lm_2_Zs_select(sig)_R.tif"),overwrite=TRUE)
}


for (i in 1) {
  Land <- Land_names[i]
  dSR_mk_shp <- st_read(paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select(sig)_R.shp"))
  
  dSR_mk_shp <-  dSR_mk_shp[2:5]
  # names(dSR_mk_shp) <- c("ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  dSR_mk_shp <- dSR_mk_shp %>%
    filter(!is.na(slope), !is.na(Zs))
  
  dSR_mk_shp <- dSR_mk_shp %>%
    mutate(slope_class = case_when(
      slope > 0 ~ 1,
      slope < 0 ~ -1, 
      slope == 0 ~ 0,
      # TRUE ~ NA_real_
    )) %>%
    mutate(Zs_class = case_when(
      abs(Zs) >= 1.96 ~ 2,
      Zs > -1.96 & Zs < 1.96 ~ 1
    )) %>%
    mutate(class_column = slope_class * Zs_class)
  
  
  
  names(dSR_mk_shp) <- c("ID","dSR_slope","dSR_p","dSR_Zs","geometry","slope_class","Zs_class","dSR_class")
  dSR_mk_shp_df <- st_drop_geometry(dSR_mk_shp)
  dSR_mk_shp_df <- as.data.frame(dSR_mk_shp_df)
  
  write.csv(dSR_mk_shp_df,paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_class_2_select(sig)_R.csv"))
  st_write(dSR_mk_shp,paste0("./MK_test/Debt_SR/",Land,"/",Land,"_Del_SR_lm_class_2_select(sig)_R.shp"),delete_dsn = TRUE, delete_layer = TRUE)
}


##### Land Sum
dSR_shp <- st_read(paste0("./Land_weight/Sum_weight_Delta_SR_lm(sig)_select_R.shp"))
dSR_shp <- na.omit(dSR_shp)

# dSR_shp <- Result_shp[[i]]
dSR_df <- as.data.frame(dSR_shp[2:9])
dSR_df <- dSR_df[, -ncol(dSR_df)]

mk_df <- data.frame(ID=dSR_shp$ID)
slope_list <- list()
p_value_list <- list()
Zs_list <- list()

grid_og <- grid

for (n in 1:nrow(dSR_df)) {
  Delta_SR_row <- dSR_df[n,]
  
  value <- t(Delta_SR_row)
  
  ts_mean_values <- ts(value, start = 1911, frequency = 1)
  
  MK_estimate <- sens.slope(ts_mean_values, conf.level = 0.95)
  
  slope <- MK_estimate$estimate
  MK_test <- MK_estimate$p.value
  Zs <- MK_estimate$statistic
  
  slope_list[[n]] <- slope
  p_value_list[[n]] <- MK_test
  Zs_list[[n]] <- Zs
}
mk_df$slope <- unlist(slope_list)
mk_df$p_value <- unlist(p_value_list)
mk_df$Zs <- unlist(Zs_list)

grid_og <- grid_og %>%
  left_join(mk_df, by = "ID")

st_write(grid_og,paste0("./MK_test/Debt_SR/Sum/Sum_weight_Del_SR_lm_MK_test_select(sig)_R.shp"))

slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
plot(slope_raster)
writeRaster(slope_raster,paste0("./MK_test/Debt_SR/Sum/Sum_weight_Del_SR_lm_slope_select(sig)_R.tif"),overwrite=TRUE)


p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
plot(p_value_raster)
writeRaster(p_value_raster,paste0("./MK_test/Debt_SR/Sum/Sum_weight_Del_SR_lm_p_value_select(sig)_R.tif"),overwrite=TRUE)

Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
plot(Zs_raster)
writeRaster(Zs_raster,paste0("./MK_test/Debt_SR/Sum/Sum_weight_Del_SR_lm_Zs_select(sig)_R.tif"),overwrite=TRUE)


#Class
dSR_mk_shp <- st_read(paste0("./MK_test/Debt_SR/Sum/Sum_weight_Del_SR_lm_MK_test_select(sig)_R.shp"))

dSR_mk_shp <-  dSR_mk_shp[2:5]
# names(dSR_mk_shp) <- c("ID","dSR_slope","dSR_p","dSR_Zs","geometry")
dSR_mk_shp <- dSR_mk_shp %>%
  filter(!is.na(slope), !is.na(Zs))

dSR_mk_shp <- dSR_mk_shp %>%
  mutate(slope_class = case_when(
    slope > 0 ~ 1,
    slope < 0 ~ -1, 
    slope == 0 ~ 0,
    # TRUE ~ NA_real_
  )) %>%
  mutate(Zs_class = case_when(
    abs(Zs) >= 1.96 ~ 2,
    Zs > -1.96 & Zs < 1.96 ~ 1
  )) %>%
  mutate(class_column = slope_class * Zs_class)



names(dSR_mk_shp) <- c("ID","dSR_slope","dSR_p","dSR_Zs","geometry","slope_class","Zs_class","dSR_class")
dSR_mk_shp_df <- st_drop_geometry(dSR_mk_shp)
dSR_mk_shp_df <- as.data.frame(dSR_mk_shp_df)

write.csv(dSR_mk_shp_df,paste0("./MK_test/Debt_SR/Sum/Sum_weight_Del_SR_lm_class_select(sig)_R.csv"))
st_write(dSR_mk_shp,paste0("./MK_test/Debt_SR/Sum/Sum_weight_Del_SR_lm_class_select(sig)_R.shp"),delete_dsn = TRUE, delete_layer = TRUE)

##### Climate Mk test
setwd("E:/Master/sphagnum/All_Steps/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sf)
library(sp)
library(dplyr)
library(trend)
library(exactextractr)
#################################
# grid
grid <- st_read("E:/Master/sphagnum/All_Steps/Equal_50km_grids.shp")
sample_rast <- rast("E:/Master/sphagnum/All_Steps/Sphagnum Peatland/template_5km_grid.tif")

Climate_names <- c("pre","tmn","tmx","tmp","pet","vap","wet")
time_stamps <- c("1911","1921","1931","1941","1951","1961","1971")

for (i in 1:7) {
  clim_name <- Climate_names[i]
  dcl_shp <- st_read(paste0("./Climate/",clim_name,"/",clim_name,"_change.shp"))
  dcl_shp <- na.omit(dcl_shp)
  
  dcl_df <- as.data.frame(dcl_shp[2:9])
  dcl_df <- dcl_df[, -ncol(dcl_df)]
  
  mk_df <- data.frame(ID=dcl_shp$ID)
  slope_list <- list()
  p_value_list <- list()
  Zs_list <- list()
  
  grid_og <- grid
  
  for (n in 1:nrow(dcl_df)) {
    Delta_cl_row <- dcl_df[n,]
    
    value <- t(Delta_cl_row)
    
    ts_mean_values <- ts(value, start = 1911, frequency = 1)
    
    MK_estimate <- sens.slopee(ts_mean_values, conf.level = 0.95)
    
    slopee <- MK_estimate$estimate
    MK_test <- MK_estimate$p.value
    Zs <- MK_estimate$statistic
    
    slopee_list[[n]] <- slopee
    p_value_list[[n]] <- MK_test
    Zs_list[[n]] <- Zs
  }
  mk_df$slopee <- unlist(slope_list)
  mk_df$p_value <- unlist(p_value_list)
  mk_df$Zs <- unlist(Zs_list)
  
  grid_og <- grid_og %>%
    left_join(mk_df, by = "ID")
  
  
  st_write(grid_og,paste0("./Debt/MK_test/Climate_Change/",clim_name,"/",clim_name,"_change_MK_test.shp"))
  
  slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
  plot(slope_raster)
  writeRaster(slope_raster,paste0("./Debt/MK_test/Climate_Change/",clim_name,"/",clim_name,"_change_slope.tif"),overwrite=TRUE)
  
  p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
  plot(p_value_raster)
  writeRaster(p_value_raster,paste0("./Debt/MK_test/Climate_Change/",clim_name,"/",clim_name,"_change_p_value.tif"),overwrite=TRUE)
  
  Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
  plot(Zs_raster)
  writeRaster(Zs_raster,paste0("./Debt/MK_test/Climate_Change/",clim_name,"/",clim_name,"_change_Zs.tif"),overwrite=TRUE)
  
}

##### Land Change Mk test
setwd("E:/Master/sphagnum/All_Steps_2/")
getwd()
# R version 4.3.2
# Encoding = UTF-8
#################################
library(raster)
library(terra)
library(sp)
library(dplyr)
library(trend)
#################################
# grid
grid <- st_read("E:/Master/sphagnum/All_Steps_2/Equal_50km_grids.shp")
sample_rast <- rast("E:/Master/sphagnum/All_Steps_2/template_5km_grid.tif")

Land_names <- c("Wetland","Forest","Grass")
time_stamps <- c("1911","1921","1931","1941","1951","1961","1971")

for (i in 1:3) {
  Land_name <- Land_names[i]
  dLand_shp <- st_read(paste0("./Land/Land_Change/",Land_names[i],"/",Land_names[i],"_Change.shp"))
  dLand_shp <- na.omit(dLand_shp)
  
  # dLand_shp <- Result_shp[[i]]
  dLand_df <- as.data.frame(dLand_shp[2:9])
  dLand_df <- dLand_df[, -ncol(dLand_df)]
  
  mk_df <- data.frame(ID=dLand_shp$ID)
  slope_list <- list()
  p_value_list <- list()
  Zs_list <- list()
  
  grid_og <- grid
  
  for (n in 1:nrow(dLand_df)) {
    Delta_Land_row <- dLand_df[n,]
    
    value <- t(Delta_Land_row)
    
    ts_mean_values <- ts(value, start = 1911, frequency = 1)
    
    MK_estimate <- sens.slope(ts_mean_values, conf.level = 0.95)
    
    slope <- MK_estimate$estimate
    MK_test <- MK_estimate$p.value
    Zs <- MK_estimate$statistic
    
    slope_list[[n]] <- slope
    p_value_list[[n]] <- MK_test
    Zs_list[[n]] <- Zs
  }
  mk_df$slope <- unlist(slope_list)
  mk_df$p_value <- unlist(p_value_list)
  mk_df$Zs <- unlist(Zs_list)
  
  grid_og <- grid_og %>%
    left_join(mk_df, by = "ID")
  
  
  st_write(grid_og,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_MK_test.shp"))
  
  slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
  plot(slope_raster)
  writeRaster(slope_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_slope.tif"),overwrite=TRUE)
  
  p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
  plot(p_value_raster)
  writeRaster(p_value_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_p_value.tif"),overwrite=TRUE)
  
  Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
  plot(Zs_raster)
  writeRaster(Zs_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_Zs.tif"),overwrite=TRUE)
  
}


##### Forest Grass New
for (i in 2:3) {
  Land_name <- Land_names[i]
  dLand_shp <- st_read(paste0("./Land/Land_Change/",Land_names[i],"/",Land_names[i],"_Change_2.shp"))
  dLand_shp <- na.omit(dLand_shp)
  
  # dLand_shp <- Result_shp[[i]]
  dLand_df <- as.data.frame(dLand_shp[2:9])
  dLand_df <- dLand_df[, -ncol(dLand_df)]
  
  mk_df <- data.frame(ID=dLand_shp$ID)
  slope_list <- list()
  p_value_list <- list()
  Zs_list <- list()
  
  grid_og <- grid
  
  for (n in 1:nrow(dLand_df)) {
    Delta_Land_row <- dLand_df[n,]
    
    value <- t(Delta_Land_row)
    
    ts_mean_values <- ts(value, start = 1911, frequency = 1)
    
    MK_estimate <- sens.slope(ts_mean_values, conf.level = 0.95)
    
    slope <- MK_estimate$estimate
    MK_test <- MK_estimate$p.value
    Zs <- MK_estimate$statistic
    
    slope_list[[n]] <- slope
    p_value_list[[n]] <- MK_test
    Zs_list[[n]] <- Zs
  }
  mk_df$slope <- unlist(slope_list)
  mk_df$p_value <- unlist(p_value_list)
  mk_df$Zs <- unlist(Zs_list)
  
  grid_og <- grid_og %>%
    left_join(mk_df, by = "ID")
  
  
  st_write(grid_og,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_2_MK_test.shp"), delete_layer = TRUE, overwrite = TRUE)
  
  slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
  plot(slope_raster)
  writeRaster(slope_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_2_slope.tif"),overwrite=TRUE)
  
  p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
  plot(p_value_raster)
  writeRaster(p_value_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_2_p_value.tif"),overwrite=TRUE)
  
  Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
  plot(Zs_raster)
  writeRaster(Zs_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_2_Zs.tif"),overwrite=TRUE)
  
}



##### Wetland 2
for (i in 1) {
  Land_name <- Land_names[i]
  dLand_shp <- st_read(paste0("./Land/Land_Change/",Land_names[i],"/",Land_names[i],"_Change_select_2.shp"))
  dLand_shp <- na.omit(dLand_shp)
  
  # dLand_shp <- Result_shp[[i]]
  dLand_df <- as.data.frame(dLand_shp[2:9])
  dLand_df <- dLand_df[, -ncol(dLand_df)]
  
  mk_df <- data.frame(ID=dLand_shp$ID)
  slope_list <- list()
  p_value_list <- list()
  Zs_list <- list()
  
  grid_og <- grid
  
  for (n in 1:nrow(dLand_df)) {
    Delta_Land_row <- dLand_df[n,]
    
    value <- t(Delta_Land_row)
    
    ts_mean_values <- ts(value, start = 1911, frequency = 1)
    
    MK_estimate <- sens.slope(ts_mean_values, conf.level = 0.95)
    
    slope <- MK_estimate$estimate
    MK_test <- MK_estimate$p.value
    Zs <- MK_estimate$statistic
    
    slope_list[[n]] <- slope
    p_value_list[[n]] <- MK_test
    Zs_list[[n]] <- Zs
  }
  mk_df$slope <- unlist(slope_list)
  mk_df$p_value <- unlist(p_value_list)
  mk_df$Zs <- unlist(Zs_list)
  
  grid_og <- grid_og %>%
    left_join(mk_df, by = "ID")
  
  
  st_write(grid_og,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_2_MK_test_select.shp"), delete_layer = TRUE, overwrite = TRUE)
  
  slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
  plot(slope_raster)
  writeRaster(slope_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_2_slope_select.tif"),overwrite=TRUE)
  
  p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
  plot(p_value_raster)
  writeRaster(p_value_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_2_p_value_select.tif"),overwrite=TRUE)
  
  Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
  plot(Zs_raster)
  writeRaster(Zs_raster,paste0("./Debt/MK_test/Land_Change/",Land_names[i],"/",Land_names[i],"_change_2_Zs_select.tif"),overwrite=TRUE)
  
}



##### Land Sum
dLand_shp <- st_read("./Land/Land_Change/Sum/Sum_Change_select.shp")
dLand_shp <- na.omit(dLand_shp)

# dLand_shp <- Result_shp[[i]]
dLand_df <- as.data.frame(dLand_shp[2:9])
dLand_df <- dLand_df[, -ncol(dLand_df)]

mk_df <- data.frame(ID=dLand_shp$ID)
slope_list <- list()
p_value_list <- list()
Zs_list <- list()

grid_og <- grid

for (n in 1:nrow(dLand_df)) {
  Delta_Land_row <- dLand_df[n,]
  
  value <- t(Delta_Land_row)
  
  ts_mean_values <- ts(value, start = 1911, frequency = 1)
  
  MK_estimate <- sens.slope(ts_mean_values, conf.level = 0.95)
  
  slope <- MK_estimate$estimate
  MK_test <- MK_estimate$p.value
  Zs <- MK_estimate$statistic
  
  slope_list[[n]] <- slope
  p_value_list[[n]] <- MK_test
  Zs_list[[n]] <- Zs
}
mk_df$slope <- unlist(slope_list)
mk_df$p_value <- unlist(p_value_list)
mk_df$Zs <- unlist(Zs_list)

grid_og <- grid_og %>%
  left_join(mk_df, by = "ID")


st_write(grid_og,paste0("./Debt/MK_test/Land_Change/Sum/Sum_change_MK_test_select.shp"), delete_layer = TRUE, overwrite = TRUE)

slope_raster <- rasterize(grid_og,sample_rast,field = "slope")
plot(slope_raster)
writeRaster(slope_raster,paste0("./Debt/MK_test/Land_Change/Sum/Sum_change_slope_select.tif"),overwrite=TRUE)

p_value_raster <- rasterize(grid_og,sample_rast,field = "p_value")
plot(p_value_raster)
writeRaster(p_value_raster,paste0("./Debt/MK_test/Land_Change/Sum/Sum_change_p_value_select.tif"),overwrite=TRUE)

Zs_raster <- rasterize(grid_og,sample_rast,field = "Zs")
plot(Zs_raster)
writeRaster(Zs_raster,paste0("./Debt/MK_test/Land_Change/Sum/Sum_change_Zs_select.tif"),overwrite=TRUE)