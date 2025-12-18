##### Continent
setwd("E:/Master/sphagnum/All_Steps_2/Debt/MK_test/")
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
library(purrr)
#################################
# Global grid system
grid <- st_read("E:/Master/sphagnum/All_Steps_2/Equal_50km_grids.shp")


Land_names <- c("Wetland","Forest","Grass")
Clim_names <-  c("pre","tmn","tmx","tmp","pet","vap","wet")
Continents <- c("Asia","Africa","Antarctica","Europe","Oceania","North_America","South_America")

# Climate
for (n in 1:7) {
  Clim <- Clim_names[n]
  clim_mk_shp <-st_read(paste0("./Climate/",Clim,"/",Clim,"_MK_test_select.shp"))
  clim_mk_shp <- clim_mk_shp[3:5]
  names(clim_mk_shp) <- c(paste0(Clim,"_slope"),paste0(Clim,"_p"),paste0(Clim,"_Zs"),"geometry")
  
  if(n == 1){
    clim_all <- clim_mk_shp
  }else{
    clim_all[[paste0(Clim,"_slope")]] <- clim_mk_shp[[paste0(Clim,"_slope")]]
    clim_all[[paste0(Clim,"_p")]] <- clim_mk_shp[[paste0(Clim,"_p")]]
    clim_all[[paste0(Clim,"_Zs")]] <- clim_mk_shp[[paste0(Clim,"_Zs")]]
  }
}

# Land area
for (i in 1:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  # SR_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/",Land,"_Delta_SR_lm(sig)_select.shp"))
  # SR_shp <- na.omit(SR_shp)
  # sig_ID <- SR_shp$ID
  ####这个和下面的效果一样
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_MK_test_select.shp"))
  # dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_MK_test_select(sig).shp"))
  
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  land_mk_shp <- st_read(paste0("./Land_area/",Land,"/",Land,"_area_MK_test_select.shp"))
  land_mk_shp <- land_mk_shp [3:5]
  names(land_mk_shp) <- c("land_slope","land_p","land_Zs","geometry")
  dSR_land <- dSR_mk_shp %>%
    st_join(land_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_land_clim_all <- dSR_land %>%
    st_join(clim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  # Select significant
  dSR_land_clim <-  dSR_land_clim_all[dSR_land_clim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_land_clim_df <- as.data.frame(dSR_land_clim)
  dSR_land_clim_df <- dSR_land_clim_df %>%
    na.omit()
  
  dSR_land_clim_df_sig <- dSR_land_clim_df %>%
    filter(abs(dSR_Zs) > 1.96)
  
  
  for (m in 1:7) {
    continent <- Continents[m]
    continent_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Continents/grid_",continent,".shp"))
    continent_ID <- continent_shp$ID
    
    dSR_land_clim_con <- dSR_land_clim_df[dSR_land_clim_df$ID %in% continent_ID,] 
    
    if (nrow(dSR_land_clim_con) == 0) {
      message("The dataset for the current continent (dSR_land_clim_con) is empty. Skipping to the next continent.")
      next
    }
    slope_columns <- dSR_land_clim_con %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 continent")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"/",continent,"_lm_rdacc(land&clim)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"(txt)/",continent,"_lm_rdacc(land&clim).txt"))
    # print(avc)
    # sink()
    
    dSR_land_clim_con_sig <- dSR_land_clim_df_sig[dSR_land_clim_df_sig$ID %in% continent_ID,]
    slope_columns_sig <- dSR_land_clim_con_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig%>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 continent")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"/",continent,"_lm_rdacc(land&clim.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"(txt)/",continent,"_lm_rdacc(land&clim.sig)_select.txt"))
    # print(avc)
    # sink()
    cat(Land,continent,":change rdacc finish","\n")
  }
  gc()
  cat(Land,":change rdacc finish","\n")
}



# Climate Change
for (n in 1:7) {
  Clim <- Clim_names[n]
  cclim_mk_shp <-st_read(paste0("./Climate_Change/",Clim,"/",Clim,"_change_MK_test_select.shp"))
  cclim_mk_shp <- cclim_mk_shp[3:5]
  names(cclim_mk_shp) <- c(paste0(Clim,"_c_slope"),paste0(Clim,"_c_p"),paste0(Clim,"_c_Zs"),"geometry")
  
  if(n==1){
    cclim_all <- cclim_mk_shp
  }else{
    cclim_all[[paste0(Clim,"_c_slope")]] <- cclim_mk_shp[[paste0(Clim,"_c_slope")]]
    cclim_all[[paste0(Clim,"_c_p")]] <- cclim_mk_shp[[paste0(Clim,"_c_p")]]
    cclim_all[[paste0(Clim,"_c_Zs")]] <- cclim_mk_shp[[paste0(Clim,"_c_Zs")]]
  }
}



# Land Change 
for (i in 1:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  cland_mk_shp <- st_read(paste0("./Land_Change/",Land,"/",Land,"_change_MK_test_select.shp"))
  cland_mk_shp <- cland_mk_shp [3:5]
  names(cland_mk_shp) <- c("cland_slope","cland_p","cland_Zs","geometry")
  dSR_cland <- dSR_mk_shp %>%
    st_join(cland_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_cland_cclim_all <- dSR_cland %>%
    st_join(cclim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_cland_cclim <-  dSR_cland_cclim_all[dSR_cland_cclim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_cland_cclim_df <- as.data.frame(dSR_cland_cclim)
  dSR_cland_cclim_df <- dSR_cland_cclim_df %>%
    na.omit()
  
  dSR_cland_cclim_df_sig  <- dSR_cland_cclim_df %>%
    filter(abs(dSR_Zs) > 1.96)
  
  for (m in 1:7) {
    continent <- Continents[m]
    continent_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Continents/grid_",continent,".shp"))
    continent_ID <- continent_shp$ID
    
    dSR_cland_cclim_con <- dSR_cland_cclim_df[dSR_cland_cclim_df$ID %in% continent_ID,]
    if (nrow(dSR_cland_cclim_con) == 0) {
      message("The dataset for the current continent (dSR_land_clim_con) is empty. Skipping to the next continent.")
      next
    }
    
    slope_columns <- dSR_cland_cclim_con %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 continent")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"/",continent,"_lm_rdacc(land&climchange)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"(txt)/",continent,"_lm_rdacc(land&climchange)_select.txt"))
    # print(avc)
    # sink()
    
    dSR_cland_cclim_con_sig <- dSR_cland_cclim_df_sig[dSR_cland_cclim_df_sig$ID %in% continent_ID,]
    slope_columns_sig <- dSR_cland_cclim_con_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig %>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 continent")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"/",continent,"_lm_rdacc(land&climchange.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"(txt)/",continent,"_lm_rdacc(land&climchange.sig)_select.txt"))
    # print(avc)
    # sink()
    cat(Land,continent,":change rdacc finish","\n")
  }
  gc()
  cat(Land,":change rdacc finish","\n")
}




##### Forest Grass New
# Climate
for (n in 1:7) {
  Clim <- Clim_names[n]
  clim_mk_shp <-st_read(paste0("./Climate/",Clim,"/",Clim,"_MK_test_select.shp"))
  clim_mk_shp <- clim_mk_shp[3:5]
  names(clim_mk_shp) <- c(paste0(Clim,"_slope"),paste0(Clim,"_p"),paste0(Clim,"_Zs"),"geometry")
  
  if(n == 1){
    clim_all <- clim_mk_shp
  }else{
    clim_all[[paste0(Clim,"_slope")]] <- clim_mk_shp[[paste0(Clim,"_slope")]]
    clim_all[[paste0(Clim,"_p")]] <- clim_mk_shp[[paste0(Clim,"_p")]]
    clim_all[[paste0(Clim,"_Zs")]] <- clim_mk_shp[[paste0(Clim,"_Zs")]]
  }
}

# Land area
for (i in 2:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_2_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  # SR_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/",Land,"_Delta_SR_lm(sig)_select.shp"))
  # SR_shp <- na.omit(SR_shp)
  # sig_ID <- SR_shp$ID
  ####这个和下面的效果一样
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select.shp"))
  # dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select(sig).shp"))
  
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  land_mk_shp <- st_read(paste0("./Land_area/",Land,"/",Land,"_area_2_MK_test_select.shp"))
  land_mk_shp <- land_mk_shp [3:5]
  names(land_mk_shp) <- c("land_slope","land_p","land_Zs","geometry")
  dSR_land <- dSR_mk_shp %>%
    st_join(land_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_land_clim_all <- dSR_land %>%
    st_join(clim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  # Select significant
  dSR_land_clim <-  dSR_land_clim_all[dSR_land_clim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_land_clim_df <- as.data.frame(dSR_land_clim)
  dSR_land_clim_df <- dSR_land_clim_df %>%
    na.omit()
  
  dSR_land_clim_df_sig <- dSR_land_clim_df %>%
    filter(abs(dSR_Zs) > 1.96)
  
  
  for (m in 1:7) {
    continent <- Continents[m]
    continent_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Continents/grid_",continent,".shp"))
    continent_ID <- continent_shp$ID
    
    dSR_land_clim_con <- dSR_land_clim_df[dSR_land_clim_df$ID %in% continent_ID,] 
    
    if (nrow(dSR_land_clim_con) == 0) {
      message("The dataset for the current continent (dSR_land_clim_con) is empty. Skipping to the next continent.")
      next
    }
    slope_columns <- dSR_land_clim_con %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 continent")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"/",continent,"_lm_2_rdacc(land&clim)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"(txt)/",continent,"_lm_2_rdacc(land&clim).txt"))
    # print(avc)
    # sink()
    
    dSR_land_clim_con_sig <- dSR_land_clim_df_sig[dSR_land_clim_df_sig$ID %in% continent_ID,]
    slope_columns_sig <- dSR_land_clim_con_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig%>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 continent")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"/",continent,"_lm_2_rdacc(land&clim.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"(txt)/",continent,"_lm_2_rdacc(land&clim.sig)_select.txt"))
    # print(avc)
    # sink()
    cat(Land,continent,":change rdacc finish","\n")
  }
  gc()
  cat(Land,":change rdacc finish","\n")
}



# Climate Change
for (n in 1:7) {
  Clim <- Clim_names[n]
  cclim_mk_shp <-st_read(paste0("./Climate_Change/",Clim,"/",Clim,"_change_MK_test_select.shp"))
  cclim_mk_shp <- cclim_mk_shp[3:5]
  names(cclim_mk_shp) <- c(paste0(Clim,"_c_slope"),paste0(Clim,"_c_p"),paste0(Clim,"_c_Zs"),"geometry")
  
  if(n==1){
    cclim_all <- cclim_mk_shp
  }else{
    cclim_all[[paste0(Clim,"_c_slope")]] <- cclim_mk_shp[[paste0(Clim,"_c_slope")]]
    cclim_all[[paste0(Clim,"_c_p")]] <- cclim_mk_shp[[paste0(Clim,"_c_p")]]
    cclim_all[[paste0(Clim,"_c_Zs")]] <- cclim_mk_shp[[paste0(Clim,"_c_Zs")]]
  }
}



# Land Change 
for (i in 2:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_2_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  cland_mk_shp <- st_read(paste0("./Land_Change/",Land,"/",Land,"_change_2_MK_test_select.shp"))
  cland_mk_shp <- cland_mk_shp [3:5]
  names(cland_mk_shp) <- c("cland_slope","cland_p","cland_Zs","geometry")
  dSR_cland <- dSR_mk_shp %>%
    st_join(cland_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_cland_cclim_all <- dSR_cland %>%
    st_join(cclim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_cland_cclim <-  dSR_cland_cclim_all[dSR_cland_cclim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_cland_cclim_df <- as.data.frame(dSR_cland_cclim)
  dSR_cland_cclim_df <- dSR_cland_cclim_df %>%
    na.omit()
  
  dSR_cland_cclim_df_sig  <- dSR_cland_cclim_df %>%
    filter(abs(dSR_Zs) > 1.96)
  
  for (m in 1:7) {
    continent <- Continents[m]
    continent_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Continents/grid_",continent,".shp"))
    continent_ID <- continent_shp$ID
    
    dSR_cland_cclim_con <- dSR_cland_cclim_df[dSR_cland_cclim_df$ID %in% continent_ID,]
    if (nrow(dSR_cland_cclim_con) == 0) {
      message("The dataset for the current continent (dSR_land_clim_con) is empty. Skipping to the next continent.")
      next
    }
    
    slope_columns <- dSR_cland_cclim_con %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 continent")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"/",continent,"_lm_2_rdacc(land&climchange)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"(txt)/",continent,"_lm_2_rdacc(land&climchange)_select.txt"))
    # print(avc)
    # sink()
    
    dSR_cland_cclim_con_sig <- dSR_cland_cclim_df_sig[dSR_cland_cclim_df_sig$ID %in% continent_ID,]
    slope_columns_sig <- dSR_cland_cclim_con_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig %>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 continent")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"/",continent,"_lm_2_rdacc(land&climchange.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Continents/MK_test(rdacc)/SR_sig/",Land,"(txt)/",continent,"_lm_2_rdacc(land&climchange.sig)_select.txt"))
    # print(avc)
    # sink()
    cat(Land,continent,":change rdacc finish","\n")
  }
  gc()
  cat(Land,":change rdacc finish","\n")
}

##### Biome
setwd("E:/Master/sphagnum/All_Steps_2/Debt/MK_test/")
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
library(purrr)
#################################
# Global grid system
grid <- st_read("E:/Master/sphagnum/All_Steps_2/Equal_50km_grids.shp")

# Add information
Land_names <- c("Wetland","Forest","Grass")
Clim_names <-  c("pre","tmn","tmx","tmp","pet","vap","wet")

# Climate
for (n in 1:7) {
  Clim <- Clim_names[n]
  clim_mk_shp <-st_read(paste0("./Climate/",Clim,"/",Clim,"_MK_test_select.shp"))
  clim_mk_shp <- clim_mk_shp[3:5]
  names(clim_mk_shp) <- c(paste0(Clim,"_slope"),paste0(Clim,"_p"),paste0(Clim,"_Zs"),"geometry")
  
  if(n == 1){
    clim_all <- clim_mk_shp
  }else{
    clim_all[[paste0(Clim,"_slope")]] <- clim_mk_shp[[paste0(Clim,"_slope")]]
    clim_all[[paste0(Clim,"_p")]] <- clim_mk_shp[[paste0(Clim,"_p")]]
    clim_all[[paste0(Clim,"_Zs")]] <- clim_mk_shp[[paste0(Clim,"_Zs")]]
  }
}

# Land area
for (i in 1:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  land_mk_shp <- st_read(paste0("./Land_area/",Land,"/",Land,"_area_MK_test_select.shp"))
  land_mk_shp <- land_mk_shp [3:5]
  names(land_mk_shp) <- c("land_slope","land_p","land_Zs","geometry")
  dSR_land <- dSR_mk_shp %>%
    st_join(land_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_land_clim_all <- dSR_land %>%
    st_join(clim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_land_clim <-  dSR_land_clim_all[dSR_land_clim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_land_clim_df <- as.data.frame(dSR_land_clim)
  dSR_land_clim_df <- dSR_land_clim_df %>%
    na.omit()
  
  dSR_land_clim_df_sig <- dSR_land_clim_df %>%
    filter(abs(dSR_Zs) >= 1.96)
  
  # Ecoregion grid
  for (m in 1:14) {
    biome_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/grid_biome_",m,".shp"))
    biome_ID <- biome_shp$ID
    
    dSR_land_clim_biome <- dSR_land_clim_df[dSR_land_clim_df$ID %in% biome_ID,]
    if (nrow(dSR_land_clim_biome) == 0) {
      message("The dataset for the current biome (dSR_land_clim_biome) is empty. Skipping to the next ecoregion.")
      next
    }
    
    slope_columns <- dSR_land_clim_biome %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 ecoregion。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"/biome",m,"_lm_rdacc(land&clim)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"(txt)/biome",m,"_lm_rdacc(land&clim)_select.txt"))
    # print(avc)
    # sink()
    
    
    dSR_land_clim_biome_sig <- dSR_land_clim_df_sig [dSR_land_clim_df_sig$ID %in% biome_ID,]
    slope_columns_sig <- dSR_land_clim_biome_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig%>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 ecoregion。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"/biome",m,"_lm_rdacc(land&clim.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"(txt)/biome",m,"_lm_rdacc(land&clim.sig)_select.txt"))
    # print(avc)
    # sink()
    
  }
  gc()
}


# Climate Change
for (n in 1:7) {
  Clim <- Clim_names[n]
  cclim_mk_shp <-st_read(paste0("./Climate_Change/",Clim,"/",Clim,"_change_MK_test_select.shp"))
  cclim_mk_shp <- cclim_mk_shp[3:5]
  names(cclim_mk_shp) <- c(paste0(Clim,"_c_slope"),paste0(Clim,"_c_p"),paste0(Clim,"_c_Zs"),"geometry")
  
  if(n==1){
    cclim_all <- cclim_mk_shp
  }else{
    cclim_all[[paste0(Clim,"_c_slope")]] <- cclim_mk_shp[[paste0(Clim,"_c_slope")]]
    cclim_all[[paste0(Clim,"_c_p")]] <- cclim_mk_shp[[paste0(Clim,"_c_p")]]
    cclim_all[[paste0(Clim,"_c_Zs")]] <- cclim_mk_shp[[paste0(Clim,"_c_Zs")]]
  }
}



for (i in 1:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  cland_mk_shp <- st_read(paste0("./Land_Change/",Land,"/",Land,"_change_MK_test_select.shp"))
  cland_mk_shp <- cland_mk_shp [3:5]
  names(cland_mk_shp) <- c("cland_slope","cland_p","cland_Zs","geometry")
  dSR_cland <- dSR_mk_shp %>%
    st_join(cland_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_cland_cclim_all <- dSR_cland %>%
    st_join(cclim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_cland_cclim <-  dSR_cland_cclim_all[dSR_cland_cclim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_cland_cclim_df <- as.data.frame(dSR_cland_cclim)
  dSR_cland_cclim_df <- dSR_cland_cclim_df %>%
    na.omit()
  
  dSR_cland_cclim_df_sig  <- dSR_cland_cclim_df %>%
    filter(abs(dSR_Zs) > 1.96)
  
  # Ecoregion grid
  for (m in 1:14) {
    biome_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/grid_biome_",m,".shp"))
    biome_ID <- biome_shp$ID
    
    dSR_cland_cclim_biome <- dSR_cland_cclim_df[dSR_cland_cclim_df$ID %in% biome_ID,]
    if (nrow(dSR_cland_cclim_biome) == 0) {
      message("The dataset for the current biome (dSR_cland_cclim_biome) is empty. Skipping to the next ecoregion.")
      next
    }
    slope_columns <- dSR_cland_cclim_biome %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 ecoregion。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"/biome",m,"_lm_rdacc(land&climchange)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"(txt)/biome",m,"_lm_rdacc(land&climchange)_select.txt"))
    # print(avc)
    # sink()
    
    dSR_cland_cclim_biome_sig <- dSR_cland_cclim_df_sig[dSR_cland_cclim_df_sig$ID %in% biome_ID,] 
    slope_columns_sig <- dSR_cland_cclim_biome_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig %>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 ecoregion。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"/biome",m,"_lm_rdacc(land&climchange.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"(txt)/biome",m,"_lm_rdacc(land&climchange.sig)_select.txt"))
    # print(avc)
    # sink()
    
  }
  
  gc()
}


##### Forest Grass New
# Climate
for (n in 1:7) {
  Clim <- Clim_names[n]
  clim_mk_shp <-st_read(paste0("./Climate/",Clim,"/",Clim,"_MK_test_select.shp"))
  clim_mk_shp <- clim_mk_shp[3:5]
  names(clim_mk_shp) <- c(paste0(Clim,"_slope"),paste0(Clim,"_p"),paste0(Clim,"_Zs"),"geometry")
  
  if(n == 1){
    clim_all <- clim_mk_shp
  }else{
    clim_all[[paste0(Clim,"_slope")]] <- clim_mk_shp[[paste0(Clim,"_slope")]]
    clim_all[[paste0(Clim,"_p")]] <- clim_mk_shp[[paste0(Clim,"_p")]]
    clim_all[[paste0(Clim,"_Zs")]] <- clim_mk_shp[[paste0(Clim,"_Zs")]]
  }
}

# Land area
for (i in 2:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_2_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  land_mk_shp <- st_read(paste0("./Land_area/",Land,"/",Land,"_area_2_MK_test_select.shp"))
  land_mk_shp <- land_mk_shp [3:5]
  names(land_mk_shp) <- c("land_slope","land_p","land_Zs","geometry")
  dSR_land <- dSR_mk_shp %>%
    st_join(land_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_land_clim_all <- dSR_land %>%
    st_join(clim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_land_clim <-  dSR_land_clim_all[dSR_land_clim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_land_clim_df <- as.data.frame(dSR_land_clim)
  dSR_land_clim_df <- dSR_land_clim_df %>%
    na.omit()
  
  dSR_land_clim_df_sig <- dSR_land_clim_df %>%
    filter(abs(dSR_Zs) >= 1.96)
  
  # Ecoregion grid
  for (m in 1:14) {
    biome_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/grid_biome_",m,".shp"))
    biome_ID <- biome_shp$ID
    
    dSR_land_clim_biome <- dSR_land_clim_df[dSR_land_clim_df$ID %in% biome_ID,]
    if (nrow(dSR_land_clim_biome) == 0) {
      message("The dataset for the current biome (dSR_land_clim_biome) is empty. Skipping to the next ecoregion.")
      next
    }
    
    slope_columns <- dSR_land_clim_biome %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 ecoregion。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"/biome",m,"_lm_2_rdacc(land&clim)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"(txt)/biome",m,"_lm_2_rdacc(land&clim)_select.txt"))
    # print(avc)
    # sink()
    
    
    dSR_land_clim_biome_sig <- dSR_land_clim_df_sig [dSR_land_clim_df_sig$ID %in% biome_ID,]
    slope_columns_sig <- dSR_land_clim_biome_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig%>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 ecoregion。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"/biome",m,"_lm_2_rdacc(land&clim.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"(txt)/biome",m,"_lm_2_rdacc(land&clim.sig)_select.txt"))
    # print(avc)
    # sink()
    
  }
  gc()
}


# Climate Change
for (n in 1:7) {
  Clim <- Clim_names[n]
  cclim_mk_shp <-st_read(paste0("./Climate_Change/",Clim,"/",Clim,"_change_MK_test_select.shp"))
  cclim_mk_shp <- cclim_mk_shp[3:5]
  names(cclim_mk_shp) <- c(paste0(Clim,"_c_slope"),paste0(Clim,"_c_p"),paste0(Clim,"_c_Zs"),"geometry")
  
  if(n==1){
    cclim_all <- cclim_mk_shp
  }else{
    cclim_all[[paste0(Clim,"_c_slope")]] <- cclim_mk_shp[[paste0(Clim,"_c_slope")]]
    cclim_all[[paste0(Clim,"_c_p")]] <- cclim_mk_shp[[paste0(Clim,"_c_p")]]
    cclim_all[[paste0(Clim,"_c_Zs")]] <- cclim_mk_shp[[paste0(Clim,"_c_Zs")]]
  }
}



for (i in 2:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  cland_mk_shp <- st_read(paste0("./Land_Change/",Land,"/",Land,"_change_2_MK_test_select.shp"))
  cland_mk_shp <- cland_mk_shp [3:5]
  names(cland_mk_shp) <- c("cland_slope","cland_p","cland_Zs","geometry")
  dSR_cland <- dSR_mk_shp %>%
    st_join(cland_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_cland_cclim_all <- dSR_cland %>%
    st_join(cclim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_cland_cclim <-  dSR_cland_cclim_all[dSR_cland_cclim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_cland_cclim_df <- as.data.frame(dSR_cland_cclim)
  dSR_cland_cclim_df <- dSR_cland_cclim_df %>%
    na.omit()
  
  dSR_cland_cclim_df_sig  <- dSR_cland_cclim_df %>%
    filter(abs(dSR_Zs) > 1.96)
  
  # Ecoregion grid
  for (m in 1:14) {
    biome_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/grid_biome_",m,".shp"))
    biome_ID <- biome_shp$ID
    
    dSR_cland_cclim_biome <- dSR_cland_cclim_df[dSR_cland_cclim_df$ID %in% biome_ID,]
    if (nrow(dSR_cland_cclim_biome) == 0) {
      message("The dataset for the current biome (dSR_cland_cclim_biome) is empty. Skipping to the next ecoregion.")
      next
    }
    slope_columns <- dSR_cland_cclim_biome %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 ecoregion。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"/biome",m,"_lm_2_rdacc(land&climchange)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"(txt)/biome",m,"_lm_2_rdacc(land&climchange)_select.txt"))
    # print(avc)
    # sink()
    
    dSR_cland_cclim_biome_sig <- dSR_cland_cclim_df_sig[dSR_cland_cclim_df_sig$ID %in% biome_ID,] 
    slope_columns_sig <- dSR_cland_cclim_biome_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig %>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (biome ", m, ")。跳过当前 ecoregion。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"/biome",m,"_lm_2_rdacc(land&climchange.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Ecoregions/MK_test(rdacc)/SR_sig/",Land,"(txt)/biome",m,"_lm_2_rdacc(land&climchange.sig)_select.txt"))
    # print(avc)
    # sink()
    
  }
  
  gc()
}


##### KG Climate Zone
setwd("E:/Master/sphagnum/All_Steps_2/Debt/MK_test/")
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
library(purrr)
#################################
# Global grid system
grid <- st_read("E:/Master/sphagnum/All_Steps_2/Equal_50km_grids.shp")

# Add information
Land_names <- c("Wetland","Forest","Grass")
Clim_names <-  c("pre","tmn","tmx","tmp","pet","vap","wet")

# Climate
for (n in 1:7) {
  Clim <- Clim_names[n]
  clim_mk_shp <-st_read(paste0("./Climate/",Clim,"/",Clim,"_MK_test_select.shp"))
  clim_mk_shp <- clim_mk_shp[3:5]
  names(clim_mk_shp) <- c(paste0(Clim,"_slope"),paste0(Clim,"_p"),paste0(Clim,"_Zs"),"geometry")
  
  if(n == 1){
    clim_all <- clim_mk_shp
  }else{
    clim_all[[paste0(Clim,"_slope")]] <- clim_mk_shp[[paste0(Clim,"_slope")]]
    clim_all[[paste0(Clim,"_p")]] <- clim_mk_shp[[paste0(Clim,"_p")]]
    clim_all[[paste0(Clim,"_Zs")]] <- clim_mk_shp[[paste0(Clim,"_Zs")]]
  }
}

# Land area
for (i in 1:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  land_mk_shp <- st_read(paste0("./Land_area/",Land,"/",Land,"_area_MK_test_select.shp"))
  land_mk_shp <- land_mk_shp [3:5]
  names(land_mk_shp) <- c("land_slope","land_p","land_Zs","geometry")
  dSR_land <- dSR_mk_shp %>%
    st_join(land_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_land_clim_all <- dSR_land %>%
    st_join(clim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_land_clim <-  dSR_land_clim_all[dSR_land_clim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_land_clim_df <- as.data.frame(dSR_land_clim)
  dSR_land_clim_df <- dSR_land_clim_df %>%
    na.omit()
  
  dSR_land_clim_df_sig <- dSR_land_clim_df %>%
    filter(abs(dSR_Zs) >= 1.96)
  
  # climate region grid
  num <- c(1:9,11,12,14:30)
  for (m in num) {
    koppen_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/grid_koppen_",m,".shp"))
    koppen_ID <- koppen_shp$ID
    
    dSR_land_clim_koppen <- dSR_land_clim_df[dSR_land_clim_df$ID %in% koppen_ID,]
    if (nrow(dSR_land_clim_koppen) == 0) {
      message("The dataset for the current koppen (dSR_land_clim_koppen) is empty. Skipping to the next climate region.")
      next
    }
    
    slope_columns <- dSR_land_clim_koppen %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (koppen ", m, ")。跳过当前 climate region。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_rdacc(land&clim)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_rdacc(land&clim)_select.txt"))
    # print(avc)
    # sink()
    
    
    dSR_land_clim_koppen_sig <- dSR_land_clim_df_sig [dSR_land_clim_df_sig$ID %in% koppen_ID,]
    slope_columns_sig <- dSR_land_clim_koppen_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig%>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (koppen ", m, ")。跳过当前 climate region。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_rdacc(land&clim.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_rdacc(land&clim.sig)_select.txt"))
    # print(avc)
    # sink()
    
  }
  gc()
}


# Climate Change
for (n in 1:7) {
  Clim <- Clim_names[n]
  cclim_mk_shp <-st_read(paste0("./Climate_Change/",Clim,"/",Clim,"_change_MK_test_select.shp"))
  cclim_mk_shp <- cclim_mk_shp[3:5]
  names(cclim_mk_shp) <- c(paste0(Clim,"_c_slope"),paste0(Clim,"_c_p"),paste0(Clim,"_c_Zs"),"geometry")
  
  if(n==1){
    cclim_all <- cclim_mk_shp
  }else{
    cclim_all[[paste0(Clim,"_c_slope")]] <- cclim_mk_shp[[paste0(Clim,"_c_slope")]]
    cclim_all[[paste0(Clim,"_c_p")]] <- cclim_mk_shp[[paste0(Clim,"_c_p")]]
    cclim_all[[paste0(Clim,"_c_Zs")]] <- cclim_mk_shp[[paste0(Clim,"_c_Zs")]]
  }
}



for (i in 1:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  cland_mk_shp <- st_read(paste0("./Land_Change/",Land,"/",Land,"_change_MK_test_select.shp"))
  cland_mk_shp <- cland_mk_shp [3:5]
  names(cland_mk_shp) <- c("cland_slope","cland_p","cland_Zs","geometry")
  dSR_cland <- dSR_mk_shp %>%
    st_join(cland_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_cland_cclim_all <- dSR_cland %>%
    st_join(cclim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_cland_cclim <-  dSR_cland_cclim_all[dSR_cland_cclim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_cland_cclim_df <- as.data.frame(dSR_cland_cclim)
  dSR_cland_cclim_df <- dSR_cland_cclim_df %>%
    na.omit()
  
  dSR_cland_cclim_df_sig  <- dSR_cland_cclim_df %>%
    filter(abs(dSR_Zs) > 1.96)
  
  # climate region grid
  num <- c(1:9,11,12,14:30)
  for (m in num) {
    koppen_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/grid_koppen_",m,".shp"))
    koppen_ID <- koppen_shp$ID
    
    dSR_cland_cclim_koppen <- dSR_cland_cclim_df[dSR_cland_cclim_df$ID %in% koppen_ID,]
    if (nrow(dSR_cland_cclim_koppen) == 0) {
      message("The dataset for the current koppen (dSR_cland_cclim_koppen) is empty. Skipping to the next climate region.")
      next
    }
    slope_columns <- dSR_cland_cclim_koppen %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (koppen ", m, ")。跳过当前 climate region。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_rdacc(land&climchange)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_rdacc(land&climchange)_select.txt"))
    # print(avc)
    # sink()
    
    dSR_cland_cclim_koppen_sig <- dSR_cland_cclim_df_sig[dSR_cland_cclim_df_sig$ID %in% koppen_ID,] 
    slope_columns_sig <- dSR_cland_cclim_koppen_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig %>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (koppen ", m, ")。跳过当前 climate region。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_rdacc(land&climchange.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_rdacc(land&climchange.sig)_select.txt"))
    # print(avc)
    # sink()
    
  }
  
  gc()
}

##### Forest Grass New
# Climate
for (n in 1:7) {
  Clim <- Clim_names[n]
  clim_mk_shp <-st_read(paste0("./Climate/",Clim,"/",Clim,"_MK_test_select.shp"))
  clim_mk_shp <- clim_mk_shp[3:5]
  names(clim_mk_shp) <- c(paste0(Clim,"_slope"),paste0(Clim,"_p"),paste0(Clim,"_Zs"),"geometry")
  
  if(n == 1){
    clim_all <- clim_mk_shp
  }else{
    clim_all[[paste0(Clim,"_slope")]] <- clim_mk_shp[[paste0(Clim,"_slope")]]
    clim_all[[paste0(Clim,"_p")]] <- clim_mk_shp[[paste0(Clim,"_p")]]
    clim_all[[paste0(Clim,"_Zs")]] <- clim_mk_shp[[paste0(Clim,"_Zs")]]
  }
}

# Land area
for (i in 2:3) {
  Land <- Land_names[i]
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_2_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  land_mk_shp <- st_read(paste0("./Land_area/",Land,"/",Land,"_area_2_MK_test_select.shp"))
  land_mk_shp <- land_mk_shp [3:5]
  names(land_mk_shp) <- c("land_slope","land_p","land_Zs","geometry")
  dSR_land <- dSR_mk_shp %>%
    st_join(land_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_land_clim_all <- dSR_land %>%
    st_join(clim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_land_clim <-  dSR_land_clim_all[dSR_land_clim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_land_clim_df <- as.data.frame(dSR_land_clim)
  dSR_land_clim_df <- dSR_land_clim_df %>%
    na.omit()
  
  dSR_land_clim_df_sig <- dSR_land_clim_df %>%
    filter(abs(dSR_Zs) >= 1.96)
  
  # climate region grid
  num <- c(1:9,11,12,14:30)
  for (m in num) {
    koppen_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/grid_koppen_",m,".shp"))
    koppen_ID <- koppen_shp$ID
    
    dSR_land_clim_koppen <- dSR_land_clim_df[dSR_land_clim_df$ID %in% koppen_ID,]
    if (nrow(dSR_land_clim_koppen) == 0) {
      message("The dataset for the current koppen (dSR_land_clim_koppen) is empty. Skipping to the next climate region.")
      next
    }
    
    slope_columns <- dSR_land_clim_koppen %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (koppen ", m, ")。跳过当前 climate region。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_2_rdacc(land&clim)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_2_rdacc(land&clim)_select.txt"))
    # print(avc)
    # sink()
    
    
    dSR_land_clim_koppen_sig <- dSR_land_clim_df_sig [dSR_land_clim_df_sig$ID %in% koppen_ID,]
    slope_columns_sig <- dSR_land_clim_koppen_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig%>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (koppen ", m, ")。跳过当前 climate region。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_2_rdacc(land&clim.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_2_rdacc(land&clim.sig)_select.txt"))
    # print(avc)
    # sink()
    
  }
  gc()
}


# Climate Change
for (n in 1:7) {
  Clim <- Clim_names[n]
  cclim_mk_shp <-st_read(paste0("./Climate_Change/",Clim,"/",Clim,"_change_MK_test_select.shp"))
  cclim_mk_shp <- cclim_mk_shp[3:5]
  names(cclim_mk_shp) <- c(paste0(Clim,"_c_slope"),paste0(Clim,"_c_p"),paste0(Clim,"_c_Zs"),"geometry")
  
  if(n==1){
    cclim_all <- cclim_mk_shp
  }else{
    cclim_all[[paste0(Clim,"_c_slope")]] <- cclim_mk_shp[[paste0(Clim,"_c_slope")]]
    cclim_all[[paste0(Clim,"_c_p")]] <- cclim_mk_shp[[paste0(Clim,"_c_p")]]
    cclim_all[[paste0(Clim,"_c_Zs")]] <- cclim_mk_shp[[paste0(Clim,"_c_Zs")]]
  }
}



for (i in 2:3) {
  SR_shp_1911 <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Debt/",Land,"/1911_1920/",Land,"_1911_1920_summary_lm_2_select.shp"))
  sig_ID <- SR_shp_1911$ID[SR_shp_1911$sgnfcnt>=1]
  
  # Delta SR & Land Area
  dSR_mk_shp <- st_read(paste0("./Debt_SR/",Land,"/",Land,"_Del_SR_lm_2_MK_test_select.shp"))
  names(dSR_mk_shp) <- c("Area","ID","dSR_slope","dSR_p","dSR_Zs","geometry")
  cland_mk_shp <- st_read(paste0("./Land_Change/",Land,"/",Land,"_change_2_MK_test_select.shp"))
  cland_mk_shp <- cland_mk_shp [3:5]
  names(cland_mk_shp) <- c("cland_slope","cland_p","cland_Zs","geometry")
  dSR_cland <- dSR_mk_shp %>%
    st_join(cland_mk_shp) %>%
    distinct(geometry, .keep_all = TRUE)
  
  # All factors
  dSR_cland_cclim_all <- dSR_cland %>%
    st_join(cclim_all)%>%
    distinct(geometry, .keep_all = TRUE)
  
  # Select significant
  dSR_cland_cclim <-  dSR_cland_cclim_all[dSR_cland_cclim_all$ID %in% sig_ID,]
  
  # rdacc
  dSR_cland_cclim_df <- as.data.frame(dSR_cland_cclim)
  dSR_cland_cclim_df <- dSR_cland_cclim_df %>%
    na.omit()
  
  dSR_cland_cclim_df_sig  <- dSR_cland_cclim_df %>%
    filter(abs(dSR_Zs) > 1.96)
  
  # climate region grid
  num <- c(1:9,11,12,14:30)
  for (m in num) {
    koppen_shp <- st_read(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/grid_koppen_",m,".shp"))
    koppen_ID <- koppen_shp$ID
    
    dSR_cland_cclim_koppen <- dSR_cland_cclim_df[dSR_cland_cclim_df$ID %in% koppen_ID,]
    if (nrow(dSR_cland_cclim_koppen) == 0) {
      message("The dataset for the current koppen (dSR_cland_cclim_koppen) is empty. Skipping to the next climate region.")
      next
    }
    slope_columns <- dSR_cland_cclim_koppen %>%
      select(contains("slope"))
    dSR_slope_df <- slope_columns%>%
      select(1)
    factors_slope_df <- slope_columns %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (koppen ", m, ")。跳过当前 climate region。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    
    # avc <- rdacca.hp(dSR_slope_df,factors_slope_df ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_2_rdacc(land&climchange)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_2_rdacc(land&climchange)_select.txt"))
    # print(avc)
    # sink()
    
    dSR_cland_cclim_koppen_sig <- dSR_cland_cclim_df_sig[dSR_cland_cclim_df_sig$ID %in% koppen_ID,] 
    slope_columns_sig <- dSR_cland_cclim_koppen_sig %>%
      select(contains("slope"))
    dSR_slope_df_sig <- slope_columns_sig %>%
      select(1)
    factors_slope_df_sig <- slope_columns_sig %>%
      select(-1)
    avc <- tryCatch(
      {
        avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
      },
      error = function(e) {
        message("遇到错误: ", e$message, " (koppen ", m, ")。跳过当前 climate region。")
        NULL
      }
    )
    
    if (is.null(avc)) {
      next
    }
    # avc <- rdacca.hp(dSR_slope_df_sig,factors_slope_df_sig ,method="RDA",type="adjR2")
    saveRDS(avc,paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_2_rdacc(land&climchange.sig)_select.rds"))
    # sink(paste0("E:/Master/sphagnum/All_Steps_2/Koppen/MK_test(rdacc)/SR_sig/",Land,"/koppen",m,"_lm_2_rdacc(land&climchange.sig)_select.txt"))
    # print(avc)
    # sink()
    
  }
  
  gc()
}
