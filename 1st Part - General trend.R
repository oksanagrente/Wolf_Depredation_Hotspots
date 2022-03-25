########################################################################################
################################ Wolf Attack Hotspots ##################################
######################## First Analysis: General clustering ############################
########################################################################################
 
# Packages
library(sf)
library(dplyr)
library(spatstat)

# Working directory
setwd("C:/Users/grente/Documents/Etudes/Thèse Loup/Points chauds/Dossier revu/Data")

# Datasets
Mercantour <- FALSE #FALSE for the general dataset, choose TRUE for the restricted dataset

if(Mercantour == FALSE){
  load("UP_Loup_sf_liste.RData")
  load("Attaques_sf_liste.RData")
  load("Maillage_Loup_sf_liste.RData")
  france_departements <- st_read("DEPARTEMENT.shp",crs=2154)
  Study_area <- france_departements[france_departements$NOM_DEP %in% c("AIN","ALLIER","ALPES-DE-HAUTE-PROVENCE","HAUTES-ALPES",
                                                                       "ALPES-MARITIMES","ARDECHE","BOUCHES-DU-RHONE","CANTAL",
                                                                       "DROME","ISERE","LOIRE","HAUTE-LOIRE","PUY-DE-DOME","RHONE","SAVOIE",
                                                                       "HAUTE-SAVOIE","VAR","VAUCLUSE"),]  
  Study_area <- st_union(Study_area) %>% 
                st_transform(.,crs=2154)}

if(Mercantour == TRUE){
  load("UP_Loup_Mercantour_sf_liste.RData")
  load("Attaques_Mercantour_sf_liste.RData")
  UP_Loup_sf_liste <- UP_Loup_Mercantour_sf_liste
  Attaques_sf_liste <- Attaques_Mercantour_sf_liste}

#######################################################################################################
################################ 1. Preparation of data ###############################################
#######################################################################################################

########## Calculation of the annual Observation Window ###########

# Without the pastoral surfaces
ObsWindowNull_fonction <- function(x) { 
  Maillage_total_Annnee <- Maillage_Loup_sf_liste[[as.character(x)]]
  Maillage_analyse_Annnee <- Maillage_total_Annnee[as.data.frame(st_intersects(Maillage_total_Annnee,Study_area))$row.id,]
  ObsWindowNull_owin_liste <- list()
  Coord_Maillage_df_liste <- list()
  Coord_inv_Maillage_df_liste <- list()
  Coord_Maillage_df <- as.data.frame(st_coordinates(st_cast(Maillage_analyse_Annnee, "POLYGON"))) #Extract XY coordinates from the wolf distribution file
  for(r in 1:max(Coord_Maillage_df[,"L2"])) {Coord_Maillage_df_liste[[r]] <- list(x=Coord_Maillage_df[Coord_Maillage_df$"L2"==r,"X"],
                                                                                  y=Coord_Maillage_df[Coord_Maillage_df$"L2"==r,"Y"])
  Coord_inv_Maillage_df_liste[[r]] <- with(Coord_Maillage_df_liste[[r]],data.frame(x=rev(x),y=rev(y)))} #Use of reversed coordinates (owin requirement)
  ObsWindowNull_owin_liste[[x]] <- owin(poly=Coord_inv_Maillage_df_liste,unitname=c("meter", "meters"))
}

if(Mercantour == FALSE){
  ObsWindowNull_owin_liste <- lapply(1995:2018, function(x) ObsWindowNull_fonction(x)) 
  names(ObsWindowNull_owin_liste) <- c(1995:2018)
  save(ObsWindowNull_owin_liste,file="ObsWindowNull_owin_liste.RData")}

# With the pastoral surfaces
ObsWindow_fonction <- function(x) { 
  ObsWindow_owin_liste <- list()
  Coord_UP_df_liste <- list()
  Coord_inv_UP_df_liste <- list()
  Coord_UP_df <- as.data.frame(st_coordinates(st_cast(UP_Loup_sf_liste[[as.character(x)]], "MULTIPOLYGON"))) #Extract XY coordinates from the UP file
  for(r in 1:max(Coord_UP_df[,"L3"])) {Coord_UP_df_liste[[r]] <- list(x=Coord_UP_df[Coord_UP_df$"L3"==r,"X"],y=Coord_UP_df[Coord_UP_df$"L3"==r,"Y"])
  Coord_inv_UP_df_liste[[r]] <- with(Coord_UP_df_liste[[r]],data.frame(x=rev(x),y=rev(y)))} #Use of reversed coordinates (owin requirement)
  ObsWindow_owin_liste[[x]] <- owin(poly=Coord_inv_UP_df_liste,unitname=c("meter", "meters"))
}

if(Mercantour == FALSE){
  ObsWindow_owin_liste <- lapply(1995:2018, function(x) ObsWindow_fonction(x)) 
  names(ObsWindow_owin_liste) <- c(1995:2018)
  # save(ObsWindow_owin_liste,file="ObsWindow_owin_liste.RData")
  }

if(Mercantour == TRUE){
  ObsWindow_owin_liste <- lapply(1995:2018, function(x) ObsWindow_fonction(x)) 
  names(ObsWindow_owin_liste) <- c(1995:2018)
  ObsWindow_Mercantour_owin_liste <- ObsWindow_owin_liste
  save(ObsWindow_Mercantour_owin_liste,file="ObsWindow_owin_Mercantour_liste.RData")}

########## Create a raster with sheep numbers and grazing time attributed per pixel ###########

Intensite_df_fonction <- function(x) {
  Intensite_df_liste <- list()
  # Creation of a mask of pixels for the annual observation window     
  Masque_owin <- as.mask(ObsWindow_owin_liste[[as.character(x)]], eps=200, dimyx=NULL, xy=NULL) 
  Pixel_im <- as.im(Masque_owin) #Transform the mask into a pixel image (spatstat object)
  Pixel_df <- as.data.frame.im(Pixel_im) #Transform the pixel image into a data.frame
  Pixel_sf <- st_as_sf(Pixel_df,coords = c('x','y'),crs= 2154) #Transform the pixel images into spatial points
  Coord_Pixel_df <- as.data.frame(st_coordinates(Pixel_sf)) #Obtain the pixel XY coordinates 
  # Identify which pixels (row.id) are inside which pastoral units (col.id)
  Pixel_UP_df <- as.data.frame(st_intersects(Pixel_sf,UP_Loup_sf_liste[[as.character(x)]])) #A bit less kept pixels than in the original mask
  if(length(which(duplicated(Pixel_UP_df$row.id)))>0){Pixel_UP_df <- Pixel_UP_df[-c(which(duplicated(Pixel_UP_df$row.id))),]} #Keep only one pastoral unit per pixel
  Pixel_UP_df <- Pixel_UP_df[order(Pixel_UP_df$row.id),] #Be sure the data.frame is ordered by the pixel rows
  Pixel_UP_df$x <- Coord_Pixel_df[c(Pixel_UP_df$row.id),"X"] #Attribute the X coordinate to the pixel in the correspondance table pixel-units
  Pixel_UP_df$y <- Coord_Pixel_df[c(Pixel_UP_df$row.id),"Y"] #Attribute the Y coordinate to the pixel in the correspondance table pixel-units
  # Associate sheep numbers and grazing time of the corresponding pastoral units to pixels
  UP_Loup_Annee_df <- UP_Loup_sf_liste[[as.character(x)]] #Extract pastoral units of the year
  st_geometry(UP_Loup_Annee_df) <- NULL #Remove geometry
  for(i in 1:max(Pixel_UP_df$col.id)) {
    # Sheep number
    Pixel_UP_df[Pixel_UP_df$col.id==i,"EFF_OV_UP"] <- UP_Loup_Annee_df[i,"EFF_OV"] 
    # Grazing time
    Pixel_UP_df[Pixel_UP_df$col.id==i,"DUREE_PAT_UP"] <- UP_Loup_Annee_df[i,"DUREE_PAT"] 
    # Grazing time ranging from 0 to 1 (probability)
    Pixel_UP_df[Pixel_UP_df$col.id==i,"DUREE_PAT_UP_PROB"] <- (UP_Loup_Annee_df[i,"DUREE_PAT"]-min(UP_Loup_Annee_df$DUREE_PAT))/diff(range(UP_Loup_Annee_df$DUREE_PAT))+1
    # Number of cells within the same pastoral area
    Pixel_UP_df[Pixel_UP_df$col.id==i,"NB_CELL"] <- nrow(Pixel_UP_df[Pixel_UP_df$col.id==i,])
  }
  # Caculate the intensity of each pixel as the related number of sheep weighted by the grazing time
  Pixel_UP_df$INTENSITE <- ceiling(Pixel_UP_df$EFF_OV_UP*Pixel_UP_df$DUREE_PAT_UP_PROB)
  Pixel_UP_df$INTENSITE_EFF <- ceiling(Pixel_UP_df$EFF_OV_UP)
  Pixel_UP_df$INTENSITE_DUREE <- ceiling(Pixel_UP_df$DUREE_PAT_UP)
  Intensite_df_liste[[x]] <- Pixel_UP_df
}

if(Mercantour == FALSE){
  Intensite_df_liste <- lapply(1995:2018, function(x) Intensite_df_fonction(x)) 
  names(Intensite_df_liste) <- c(1995:2018)
  #save(Intensite_df_liste,file="Intensite_df_liste.RData")
  }

if(Mercantour == TRUE){
  Intensite_df_liste <- lapply(1995:2018, function(x) Intensite_df_fonction(x)) 
  names(Intensite_df_liste) <- c(1995:2018)
  Intensite_Mercantour_df_liste <- Intensite_df_liste
  save(Intensite_Mercantour_df_liste,file="Intensite_Mercantour_df_liste.RData")}

########## Create an intensity function with sheep numbers only ###########

Intensite_eff_im_fonction <- function(x) {
  Intensite_eff_im_liste <- list()
  Intensite_eff_im <- as.im.data.frame(Intensite_df_liste[[as.character(x)]][,c("x","y","INTENSITE_EFF")])
  unitname(Intensite_eff_im) <- c("meter", "meters") #Set the units
  Intensite_eff_im_liste[[x]] <- Intensite_eff_im
} 

if(Mercantour == FALSE){
  Intensite_eff_im_liste <- lapply(1995:2018, function(x) Intensite_eff_im_fonction(x)) 
  names(Intensite_eff_im_liste) <- c(1995:2018)
  save(Intensite_eff_im_liste,file="Intensite_eff_im_liste.RData")}

if(Mercantour == TRUE){
  Intensite_eff_im_liste <- lapply(1995:2018, function(x) Intensite_eff_im_fonction(x)) 
  names(Intensite_eff_im_liste) <- c(1995:2018)
  Intensite_eff_im_Mercantour_liste <- Intensite_eff_im_liste
  save(Intensite_eff_im_Mercantour_liste,file="Intensite_eff_im_Mercantour_liste.RData")}

########## Create an intensity function with grazing time only ###########

Intensite_time_im_fonction <- function(x) {
  Intensite_time_im_liste <- list()
  Intensite_time_im <- as.im.data.frame(Intensite_df_liste[[as.character(x)]][,c("x","y","INTENSITE_DUREE")])
  unitname(Intensite_time_im) <- c("meter", "meters") #Set the units
  Intensite_time_im_liste[[x]] <- Intensite_time_im
} 

if(Mercantour == FALSE){
  Intensite_time_im_liste <- lapply(1995:2018, function(x) Intensite_time_im_fonction(x)) 
  names(Intensite_time_im_liste) <- c(1995:2018)
  save(Intensite_time_im_liste,file="Intensite_time_im_liste.RData")}

if(Mercantour == TRUE){
  Intensite_time_im_liste <- lapply(1995:2018, function(x) Intensite_time_im_fonction(x)) 
  names(Intensite_time_im_liste) <- c(1995:2018)
  Intensite_time_im_Mercantour_liste <- Intensite_time_im_liste
  save(Intensite_time_im_Mercantour_liste,file="Intensite_time_im_Mercantour_liste.RData")}

########## Generate a ppp of the observed attacks with the same observation window ###########

# With the observation window without the pastoral surfaces
AttaquesNull_ppp_fonction <- function(x){
  AttaquesNull_ppp_liste <- list()
  Coord_Attaques_df <- as.data.frame(st_coordinates(Attaques_sf_liste[[as.character(x)]]))
  if(length(which(duplicated(Coord_Attaques_df)))>0){
    Coord_Attaques_df <- Coord_Attaques_df[-c(which(duplicated(Coord_Attaques_df))),]} #Remove the attacks with duplicated locations for analyses 
  Attaques_ppp <- as.ppp(Coord_Attaques_df, ObsWindowNull_owin_liste[[as.character(x)]]) 
  AttaquesNull_ppp_liste[[x]] <- Attaques_ppp
}

if(Mercantour == FALSE){
  AttaquesNull_ppp_liste <- lapply(1995:2018, function(x) AttaquesNull_ppp_fonction(x)) 
  names(AttaquesNull_ppp_liste) <- c(1995:2018)
  save(AttaquesNull_ppp_liste,file="AttaquesNull_ppp_liste.RData")}

# With the observation window with the pastoral surfaces
Attaques_ppp_fonction <- function(x){
  Attaques_ppp_liste <- list()
  Coord_Attaques_df <- as.data.frame(st_coordinates(Attaques_sf_liste[[as.character(x)]]))
  if(length(which(duplicated(Coord_Attaques_df)))>0){
    Coord_Attaques_df <- Coord_Attaques_df[-c(which(duplicated(Coord_Attaques_df))),]} #Remove the attacks with duplicated locations for analyses 
  Attaques_ppp <- as.ppp(Coord_Attaques_df, ObsWindow_owin_liste[[as.character(x)]]) 
  Attaques_ppp_liste[[x]] <- Attaques_ppp
}

if(Mercantour == FALSE){
  Attaques_ppp_liste <- lapply(1995:2018, function(x) Attaques_ppp_fonction(x)) 
  names(Attaques_ppp_liste) <- c(1995:2018)
  save(Attaques_ppp_liste,file="Attaques_ppp_liste.RData")}

if(Mercantour == TRUE){
  Attaques_ppp_liste <- lapply(1995:2018, function(x) Attaques_ppp_fonction(x)) 
  names(Attaques_ppp_liste) <- c(1995:2018)
  Attaques_ppp_Mercantour_liste <- Attaques_ppp_liste
  save(Attaques_ppp_Mercantour_liste,file="Attaques_ppp_Mercantour_liste.RData")}

#######################################################################################################
############################## 2. Calculation of K-inhom and K ########################################
#######################################################################################################

if(Mercantour == FALSE){
  load("Intensite_eff_im_liste.RData")
  load("Intensite_time_im_liste.RData")
  load("Attaques_ppp_liste.RData")
  load("AttaquesNull_ppp_liste.RData")}

if(Mercantour == TRUE){
  load("Intensite_eff_im_Mercantour_liste.RData")
  load("Intensite_time_im_Mercantour_liste.RData")
  load("Attaques_ppp_Mercantour_liste.RData")
  Intensite_eff_im_liste <- Intensite_eff_im_Mercantour_liste
  Intensite_time_im_liste <- Intensite_time_im_Mercantour_liste
  Attaques_ppp_liste <- Attaques_ppp_Mercantour_liste}

########## First method: full model ###########

# Create the PPP model with sheep numbers and grazing time as covariates
Full_model_fonction <- function(x){
  Full_model_liste <- list()
Full_model <- ppm(Attaques_ppp_liste[[as.character(x)]],
                  ~ log(pop) + log(time), 
                  covariates = list(pop = Intensite_eff_im_liste[[as.character(x)]],
                                        time = Intensite_time_im_liste[[as.character(x)]]))
Full_model_quadcorr <- quad.ppm(Full_model, drop = TRUE)
Full_model_liste[[x]] <- ppm(Full_model_quadcorr, ~ log(pop) + log(time), 
                      covariates = list(pop = Intensite_eff_im_liste[[as.character(x)]],
                                        time = Intensite_time_im_liste[[as.character(x)]]))}

if(Mercantour == FALSE){
  Full_model_liste <- lapply(1995:2018, function(x) Full_model_fonction(x)) 
  names(Full_model_liste) <- c(1995:2018)
  save(Full_model_liste,file="Full_model_liste.RData")}

if(Mercantour == TRUE){
  Full_model_liste <- lapply(1995:2018, function(x) Full_model_fonction(x)) 
  names(Full_model_liste) <- c(1995:2018)
  Full_model_Mercantour_liste <- Full_model_liste
  save(Full_model_Mercantour_liste,file="Full_model_Mercantour_liste.RData")}

# Extract the intensity surface from the model
Full_model_im_fonction <- function(x){
  Full_model_im_liste <- list()
Full_model_im_liste[[x]] <- predict.ppm(Full_model_liste[[as.character(x)]],
                                        type="trend", 
                                        covariates = list(pop = Intensite_eff_im_liste[[as.character(x)]],
                                                          time = Intensite_time_im_liste[[as.character(x)]]))}

if(Mercantour == FALSE){
  Full_model_im_liste <- lapply(1995:2018, function(x) Full_model_im_fonction(x)) 
  names(Full_model_im_liste) <- c(1995:2018)
  save(Full_model_im_liste,file="Full_model_im_liste.RData")}

if(Mercantour == TRUE){
  Full_model_im_liste <- lapply(1995:2018, function(x) Full_model_im_fonction(x)) 
  names(Full_model_im_liste) <- c(1995:2018)
  Full_model_im_Mercantour_liste <- Full_model_im_liste
  save(Full_model_im_Mercantour_liste,file="Full_model_im_Mercantour_liste.RData")}

# Calculate Kinhom with simulation envelope

Full_model_Kinhom_fonction <- function(x){
  Full_model_Kinhom_liste <- list()
Full_model_Kinhom_liste[[x]] <- envelope(Y=Attaques_ppp_liste[[as.character(x)]],
                                         fun=Kinhom,
                                         lambda=Full_model_im_liste[[as.character(x)]],
                                         simulate=expression(rpoispp(Full_model_im_liste[[as.character(x)]])),
                                         correction="translation",
                                         nsim=499,
                                         nrank=50)}

if(Mercantour == FALSE){
  Full_model_Kinhom_liste <- lapply(1995:2018, function(x) Full_model_Kinhom_fonction(x)) 
  names(Full_model_Kinhom_liste) <- c(1995:2018)
  save(Full_model_Kinhom_liste,file="Full_model_Kinhom_liste.RData")}

if(Mercantour == TRUE){
  Full_model_Kinhom_liste <- lapply(1995:2018, function(x) Full_model_Kinhom_fonction(x)) 
  names(Full_model_Kinhom_liste) <- c(1995:2018)
  Full_model_Kinhom_Mercantour_liste <- Full_model_Kinhom_liste
  save(Full_model_Kinhom_Mercantour_liste,file="Full_model_Kinhom_Mercantour_liste.RData")}

########## Second method: null model with Kinhom ###########

# Calculate Kinhom with simulation envelope
# Null_model_Kinhom_fonction <- function(x){   
#   Null_model_Kinhom_liste <- list()
#   Null_model_Kinhom_liste[[x]] <- envelope(Y=Attaques_ppp_liste[[as.character(x)]],
#                                            fun=Kinhom,
#                                            simulate=NULL,
#                                            correction="translation",
#                                            nsim=499,
#                                            nrank=50)}
# 
# if(Mercantour == FALSE){
#   Null_model_Kinhom_liste <- lapply(1995:2018, function(x) Null_model_Kinhom_fonction(x)) 
#   names(Null_model_Kinhom_liste) <- c(1995:2018)
#   save(Null_model_Kinhom_liste,file="Null_model_Kinhom_liste.RData")}

########## Third method: null model with K ###########

#First version: with the observation window calculated with pastoral surfaces of the year
Null_model_K_fonction <- function(x){
  Null_model_K_liste <- list()
  Null_model_K_liste[[x]] <- envelope(Y=Attaques_ppp_liste[[as.character(x)]],
                                           fun=Kest,
                                           simulate=NULL,
                                           correction="translation",
                                           nsim=499,
                                           nrank=50)}

if(Mercantour == FALSE){
  Null_model_K_liste <- lapply(1995:2018, function(x) Null_model_K_fonction(x)) 
  names(Null_model_K_liste) <- c(1995:2018)
  save(Null_model_K_liste,file="Null_model_K_liste.RData")}

#Second version: with the observation window only calculated with wolf distribution
Null2_model_K_fonction <- function(x){
  Null2_model_K_liste <- list()
  Null2_model_K_liste[[x]] <- envelope(Y=AttaquesNull_ppp_liste[[as.character(x)]],
                                      fun=Kest,
                                      simulate=NULL,
                                      correction="translation",
                                      nsim=499,
                                      nrank=50)}

if(Mercantour == FALSE){
  Null2_model_K_liste <- lapply(1995:2018, function(x) Null2_model_K_fonction(x)) 
  names(Null2_model_K_liste) <- c(1995:2018)
  save(Null2_model_K_liste,file="Null2_model_K_liste.RData")}
