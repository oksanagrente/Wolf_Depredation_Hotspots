########################################################################################
################################ Wolf Attack Hotspots ##################################
##################### Second Analysis: Cluster identification ##########################
########################################################################################

#Packages
library(sf)
library(dplyr)
library(ggplot2)
library(SpatialEpi)

#Kulldorff corrected function "kulldorff2"
source("C:/Users/grente/Documents/Etudes/Points chauds/Dossier revu/Codes R/Analyse/2nd Part - kulldorff2 function.R")
source("C:/Users/grente/Documents/Etudes/Points chauds/Dossier revu/Codes R/Analyse/2nd Part - kulldorff3 function.R")

setwd("C:/Users/grente/Documents/Etudes/Points chauds/Dossier revu/Data")

load("UP_Loup_sf_liste.RData")
load("Attaques_sf_liste.RData")

#######################################################################################################
########################### 1. Data preparation for analyses ##########################################
#######################################################################################################

Donnees_Cluster_UP_df_fonction <- function(x){
    Donnees_Cluster_UP_df_liste <- list()
  UP_Loup_Annee_df <- UP_Loup_sf_liste[[as.character(x)]] #Extract pastoral units of the year
  Attaques_Annee_df <- Attaques_sf_liste[[as.character(x)]] #Extract attacks of the year
  # Identify which attack is within which pastoral unit
  Attaques_UP_df <- as.data.frame(st_contains(UP_Loup_Annee_df,Attaques_Annee_df))
  # Get the centroids of pastoral units
  UP_Loup_Annee_df["CENTROID_X"] <- as.data.frame(st_coordinates(st_centroid(UP_Loup_Annee_df["geometry"])))$X 
  UP_Loup_Annee_df["CENTROID_Y"] <- as.data.frame(st_coordinates(st_centroid(UP_Loup_Annee_df["geometry"])))$Y
  st_geometry(UP_Loup_Annee_df) <- NULL #Remove geometry
  # Identify pastoral units which have been attacked during the year
  UP_Loup_Annee_df[c(unique(Attaques_UP_df$row.id)),"ATTAQUES"] <- as.data.frame(table(Attaques_UP_df$row.id))$Freq
  UP_Loup_Annee_df[-c(unique(Attaques_UP_df$row.id)),"ATTAQUES"] <- 0
  # Calculate the log-transformed number of sheep per unit, weighted by grazing time
  UP_Loup_Annee_df["DISPO_OBS"] <- ceiling(UP_Loup_Annee_df$EFF_OV*UP_Loup_Annee_df$DUREE_PAT*UP_Loup_Annee_df$SURFACE)
  # Create a warning message in case there is less population at risk than observed attacks
  if(nrow(UP_Loup_Annee_df[UP_Loup_Annee_df$DISPO_OBS<UP_Loup_Annee_df$ATTAQUES,])>0)
    warning(paste0('less population at risk than observed attacks in ',x))
  # Create an homogeneous theoretical number of sheep for the depredated units  
  UP_Loup_Annee_df["DISPO_HOMO_FICTIF"] <- round(mean(UP_Loup_Annee_df[UP_Loup_Annee_df$ATTAQUES>0,"EFF_OV"]*UP_Loup_Annee_df[UP_Loup_Annee_df$ATTAQUES>0,"DUREE_PAT"])*UP_Loup_Annee_df$SURFACE)
    # round(mean(UP_Loup_Annee_df[UP_Loup_Annee_df$ATTAQUES>0,"DISPO_OBS"]))
  Donnees_Cluster_UP_df_liste[[x]] <- UP_Loup_Annee_df
  }

Donnees_Cluster_UP_df_liste <- lapply(1995:2018, function(x) Donnees_Cluster_UP_df_fonction(x=x)) 
names(Donnees_Cluster_UP_df_liste) <- c(1995:2018)
save(Donnees_Cluster_UP_df_liste,file="Donnees_Cluster_UP_df_liste.RData")

#######################################################################################################
###################### 2. Calculation of the number of expected attacks ###############################
#######################################################################################################

########## For attacked PU and weighted number of sheep ###########

Attentes_UPpredatees_Effectifpondere_fonction <- function(x){
      Attentes_UPpredatees_Effectifpondere_liste <- list()
  Donnees_Cluster_UPpredatees_df_liste <- Donnees_Cluster_UP_df_liste[[as.character(x)]][Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES>0,]
  Attentes_UPpredatees_Effectifpondere <- expected(Donnees_Cluster_UPpredatees_df_liste$DISPO_OBS,
                                                   Donnees_Cluster_UPpredatees_df_liste$ATTAQUES,
                                                   n.strata=1) #Package SpatialEpi example: equivalent "expected.cases" (no strates for us)
  Attentes_UPpredatees_Effectifpondere_liste[[x]] <- Attentes_UPpredatees_Effectifpondere
}

Attentes_UPpredatees_Effectifpondere_liste <- lapply(1995:2018, function(x) Attentes_UPpredatees_Effectifpondere_fonction(x=x)) 
names(Attentes_UPpredatees_Effectifpondere_liste) <- c(1995:2018)
save(Attentes_UPpredatees_Effectifpondere_liste,file="Attentes_UPpredatees_Effectifpondere_liste.RData")

########## For attacked PU and homogeneous number of sheep ###########

Attentes_UPpredatees_Effectifhomogene_fonction <- function(x){
      Attentes_UPpredatees_Effectifhomogene_liste <- list()
  Donnees_Cluster_UPpredatees_df_liste <- Donnees_Cluster_UP_df_liste[[as.character(x)]][Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES>0,]
  Attentes_UPpredatees_Effectifhomogene <- expected(Donnees_Cluster_UPpredatees_df_liste$DISPO_HOMO_FICTIF,
                                                    Donnees_Cluster_UPpredatees_df_liste$ATTAQUES,
                                                    n.strata=1) #Package SpatialEpi example: equivalent "expected.cases" (no strates for us)
  Attentes_UPpredatees_Effectifhomogene_liste[[x]] <- Attentes_UPpredatees_Effectifhomogene
}

Attentes_UPpredatees_Effectifhomogene_liste <- lapply(1995:2018, function(x) Attentes_UPpredatees_Effectifhomogene_fonction(x=x)) 
names(Attentes_UPpredatees_Effectifhomogene_liste) <- c(1995:2018)
save(Attentes_UPpredatees_Effectifhomogene_liste,file="Attentes_UPpredatees_Effectifhomogene_liste.RData")

#######################################################################################################
################################# 3. Kulldorff analyses ###############################################
#######################################################################################################

########## For attacked PU and weighted number of sheep ###########

# Kulldorff calculations

Kulldorff_UPpredatees_Effectifpondere_fonction <- function(x){
      Kulldorff_UPpredatees_Effectifpondere_liste <- list()
  Donnees_Cluster_UPpredatees_df_liste <- Donnees_Cluster_UP_df_liste[[as.character(x)]][Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES>0,]
  Kulldorff_UPpredatees_Effectifpondere <- kulldorff2(geo=Donnees_Cluster_UPpredatees_df_liste[c("CENTROID_X","CENTROID_Y")], 
                                                  cases= Donnees_Cluster_UPpredatees_df_liste$ATTAQUES, 
                                                  population= Donnees_Cluster_UPpredatees_df_liste$DISPO_OBS, 
                                                  expected.cases= Attentes_UPpredatees_Effectifpondere_liste[[as.character(x)]],
                                                  pop.upper.bound=0.05, 
                                                  n.simulations=499, 
                                                  alpha.level=0.05,
                                                  plot=FALSE)
  Kulldorff_UPpredatees_Effectifpondere_liste[[x]] <- Kulldorff_UPpredatees_Effectifpondere
}

Kulldorff_UPpredatees_Effectifpondere_liste <- lapply(1995:2018, function(x) Kulldorff_UPpredatees_Effectifpondere_fonction(x=x)) 
names(Kulldorff_UPpredatees_Effectifpondere_liste) <- c(1995:2018)
save(Kulldorff_UPpredatees_Effectifpondere_liste,file="Kulldorff_UPpredatees_Effectifpondere_liste.RData") 

# Primary clusters

Cluster_UPpredatees_Effectifpondere_fonction <- function(x){
  Cluster_UPpredatees_Effectifpondere_liste <- list()
  Cluster_UPpredatees_Effectifpondere <- Kulldorff_UPpredatees_Effectifpondere_liste[[as.character(x)]]$most.likely.cluster$location.IDs.included # Warning: new row ids, from the restricted dataset only! 
  UPpredatees_id_Annee <- row.names(Donnees_Cluster_UP_df_liste[[as.character(x)]][Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES>0,]) # Extract old row ids
  Cluster_UPpredatees_Effectifpondere_liste[[x]] <- as.numeric(UPpredatees_id_Annee[c(Cluster_UPpredatees_Effectifpondere)]) # Identify old row ids which are within the primary cluster
}

Cluster_UPpredatees_Effectifpondere_liste <- lapply(1995:2018, function(x) Cluster_UPpredatees_Effectifpondere_fonction(x=x)) 
names(Cluster_UPpredatees_Effectifpondere_liste) <- c(1995:2018)
save(Cluster_UPpredatees_Effectifpondere_liste,file="Cluster_UPpredatees_Effectifpondere_liste.RData")

# Secondary clusters

Cluster2_UPpredatees_Effectifpondere_fonction <- function(x){
  Cluster2_UPpredatees_Effectifpondere_liste <- list()
  Cluster2_df_liste <- list()
  if (length(Kulldorff_UPpredatees_Effectifpondere_liste[[as.character(x)]]$secondary.clusters)>0){
    for(u in 1:length(Kulldorff_UPpredatees_Effectifpondere_liste[[as.character(x)]]$secondary.clusters)){
      Cluster2_UPpredatees_Effectifpondere <- Kulldorff_UPpredatees_Effectifpondere_liste[[as.character(x)]]$secondary.clusters[[u]]$location.IDs.included
      UPpredatees_id_Annee <- row.names(Donnees_Cluster_UP_df_liste[[as.character(x)]][Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES>0,]) # Extract old row ids
      Cluster2_UPpredatees_Effectifpondere_id <- as.numeric(UPpredatees_id_Annee[c(Cluster2_UPpredatees_Effectifpondere)]) # Identify old row ids which are within the primary cluster
      Cluster2_df_liste[[u]] <- data.frame(Niveau=rep(u,length(Cluster2_UPpredatees_Effectifpondere)),IDCluster=Cluster2_UPpredatees_Effectifpondere_id)}
    Cluster2_Annee_df <- bind_rows(Cluster2_df_liste)
    Cluster2_UPpredatees_Effectifpondere_liste[[x]] <- Cluster2_Annee_df}
  else{Cluster2_UPpredatees_Effectifpondere_liste[[x]] <- NULL}
}

Cluster2_UPpredatees_Effectifpondere_liste <- lapply(1995:2018, function(x) Cluster2_UPpredatees_Effectifpondere_fonction(x)) 
names(Cluster2_UPpredatees_Effectifpondere_liste) <- c(1995:2018)
save(Cluster2_UPpredatees_Effectifpondere_liste,file="Cluster2_UPpredatees_Effectifpondere_liste.RData")

########## For attacked PU and homogeneous number of sheep ###########

# Kulldorff calculations

Kulldorff_UPpredatees_Effectifhomogene_fonction <- function(x){
  Kulldorff_UPpredatees_Effectifhomogene_liste <- list()
  Donnees_Cluster_UPpredatees_df_liste <- Donnees_Cluster_UP_df_liste[[as.character(x)]][Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES>0,]
  Kulldorff_UPpredatees_Effectifhomogene <- kulldorff2(geo= Donnees_Cluster_UPpredatees_df_liste[c("CENTROID_X","CENTROID_Y")], 
                                                      cases= Donnees_Cluster_UPpredatees_df_liste$ATTAQUES, 
                                                      population= Donnees_Cluster_UPpredatees_df_liste$DISPO_HOMO_FICTIF, 
                                                      expected.cases= Attentes_UPpredatees_Effectifhomogene_liste[[as.character(x)]],
                                                      pop.upper.bound=0.05, 
                                                      n.simulations=499, 
                                                      alpha.level=0.05,
                                                      plot=FALSE)
  Kulldorff_UPpredatees_Effectifhomogene_liste[[x]] <- Kulldorff_UPpredatees_Effectifhomogene
}

Kulldorff_UPpredatees_Effectifhomogene_liste <- lapply(1995:2018, function(x) Kulldorff_UPpredatees_Effectifhomogene_fonction(x=x)) 
names(Kulldorff_UPpredatees_Effectifhomogene_liste) <- c(1995:2018)
save(Kulldorff_UPpredatees_Effectifhomogene_liste,file="Kulldorff_UPpredatees_Effectifhomogene_liste.RData")

# Primary clusters

Cluster_UPpredatees_Effectifhomogene_fonction <- function(x){
  Cluster_UPpredatees_Effectifhomogene_liste <- list()
  Cluster_UPpredatees_Effectifhomogene <- Kulldorff_UPpredatees_Effectifhomogene_liste[[as.character(x)]]$most.likely.cluster$location.IDs.included # Warning: new row ids, from the restricted dataset only! 
  UPpredatees_id_Annee <- row.names(Donnees_Cluster_UP_df_liste[[as.character(x)]][Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES>0,]) # Extract old row ids
  Cluster_UPpredatees_Effectifhomogene_liste[[x]] <- as.numeric(UPpredatees_id_Annee[c(Cluster_UPpredatees_Effectifhomogene)]) # Identify old row ids which are within the primary cluster
}

Cluster_UPpredatees_Effectifhomogene_liste <- lapply(1995:2018, function(x) Cluster_UPpredatees_Effectifhomogene_fonction(x=x)) 
names(Cluster_UPpredatees_Effectifhomogene_liste) <- c(1995:2018)
save(Cluster_UPpredatees_Effectifhomogene_liste,file="Cluster_UPpredatees_Effectifhomogene_liste.RData")

# Secondary clusters

Cluster2_UPpredatees_Effectifhomogene_fonction <- function(x){
  Cluster2_UPpredatees_Effectifhomogene_liste <- list()
  Cluster2_df_liste <- list()
  if (length(Kulldorff_UPpredatees_Effectifhomogene_liste[[as.character(x)]]$secondary.clusters)>0){
    for(u in 1:length(Kulldorff_UPpredatees_Effectifhomogene_liste[[as.character(x)]]$secondary.clusters)){
      Cluster2_UPpredatees_Effectifhomogene <- Kulldorff_UPpredatees_Effectifhomogene_liste[[as.character(x)]]$secondary.clusters[[u]]$location.IDs.included
      UPpredatees_id_Annee <- row.names(Donnees_Cluster_UP_df_liste[[as.character(x)]][Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES>0,]) # Extract old row ids
      Cluster2_UPpredatees_Effectifhomogene_id <- as.numeric(UPpredatees_id_Annee[c(Cluster2_UPpredatees_Effectifhomogene)]) # Identify old row ids which are within the primary cluster
      Cluster2_df_liste[[u]] <- data.frame(Niveau=rep(u,length(Cluster2_UPpredatees_Effectifhomogene)),IDCluster=Cluster2_UPpredatees_Effectifhomogene_id)}
    Cluster2_Annee_df <- bind_rows(Cluster2_df_liste)
    Cluster2_UPpredatees_Effectifhomogene_liste[[x]] <- Cluster2_Annee_df}
  else{Cluster2_UPpredatees_Effectifhomogene_liste[[x]] <- NULL}
}

Cluster2_UPpredatees_Effectifhomogene_liste <- lapply(1995:2018, function(x) Cluster2_UPpredatees_Effectifhomogene_fonction(x)) 
names(Cluster2_UPpredatees_Effectifhomogene_liste) <- c(1995:2018)
save(Cluster2_UPpredatees_Effectifhomogene_liste,file="Cluster2_UPpredatees_Effectifhomogene_liste.RData")

#######################################################################################################
################################# 4. Complementary analyses ###############################################
#######################################################################################################

Annees <- c(1995:2018)

# Recover pastoral data for PS into hotspots
Results_UPhotspots_sf_fonction <- function(x){
  Results_UPhotspots_sf_liste <- list()
  UP_Loup_annee_sf <- UP_Loup_sf_liste[[as.character(x)]]
  # Identify which PU are depredated
  UP_Loup_annee_sf$ATTAQUES <- Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES
  UP_Loup_nonpredatees_annee_sf <- UP_Loup_annee_sf[UP_Loup_annee_sf$ATTAQUES==0,]
  UP_Loup_predatees_annee_sf <- UP_Loup_annee_sf[UP_Loup_annee_sf$ATTAQUES>0,]
  # Identify which PU are primary or secondary clusters
  UP_Loup_annee_sf[c(as.numeric(row.names(UP_Loup_nonpredatees_annee_sf))),"CLUSTER_POND"] <- "No depredated"
  UP_Loup_annee_sf[c(as.numeric(row.names(UP_Loup_nonpredatees_annee_sf))),"CLUSTER_FICTIF"] <- "No depredated"
  UP_Loup_annee_sf[c(as.numeric(row.names(UP_Loup_predatees_annee_sf))),"CLUSTER_POND"] <- "No hotspot"
  UP_Loup_annee_sf[c(as.numeric(row.names(UP_Loup_predatees_annee_sf))),"CLUSTER_FICTIF"] <- "No hotspot"
  UP_Loup_annee_sf[c(Cluster_UPpredatees_Effectifpondere_liste[[as.character(x)]]),"CLUSTER_POND"] <- "Hotspot"
  UP_Loup_annee_sf[c(Cluster_UPpredatees_Effectifhomogene_liste[[as.character(x)]]),"CLUSTER_FICTIF"] <- "Hotspot"
  UP_Loup_annee_sf[c(Cluster2_UPpredatees_Effectifpondere_liste[[as.character(x)]]$IDCluster),"CLUSTER_POND"] <- "Hotspot"
  UP_Loup_annee_sf[c(Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(x)]]$IDCluster),"CLUSTER_FICTIF"] <- "Hotspot"
  Results_UPhotspots_sf_liste[[x]] <- UP_Loup_annee_sf} 
Results_UPhotspots_sf_liste <- lapply(Annees, function(x) Results_UPhotspots_sf_fonction(x))
names(Results_UPhotspots_sf_liste) <- Annees

# Analyse mean flock size of PS into hotspot or not
Eff_pond_hotspot <- rep(NA,length(Annees))
Eff_homo_hotspot <- rep(NA,length(Annees))
Eff_pond_nonhotspot <- rep(NA,length(Annees))
Eff_homo_nonhotspot <- rep(NA,length(Annees))

for(i in 1:length(Annees)){
  Resultat_annee <- Results_UPhotspots_sf_liste[[as.character(Annees[i])]]
  st_geometry(Resultat_annee) <- NULL
  Eff_pond_hotspot[i] <- mean(Resultat_annee[Resultat_annee$CLUSTER_POND=="Hotspot","EFF_OV"])
  Eff_homo_hotspot[i] <- mean(Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="Hotspot","EFF_OV"])
  Eff_pond_nonhotspot[i] <- mean(Resultat_annee[Resultat_annee$CLUSTER_POND=="No hotspot","EFF_OV"])
  Eff_homo_nonhotspot[i] <- mean(Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="No hotspot","EFF_OV"])}

mean(Eff_pond_hotspot) ; sd(Eff_pond_hotspot)
mean(Eff_pond_nonhotspot) ; sd(Eff_pond_nonhotspot)
mean(Eff_homo_hotspot) ; sd(Eff_homo_hotspot)

# Analyse flock size of PS into hotspot or not
Nb_pond_hotspot <- rep(NA, length(Annees))
Nb_homo_hotspot <- rep(NA,length(Annees))
Nb_pond_nonhotspot <- rep(NA,length(Annees))
Nb_homo_nonhotspot <- rep(NA,length(Annees))

for(i in 1:length(Annees)){
  Resultat_annee <- Results_UPhotspots_sf_liste[[as.character(Annees[i])]]
  st_geometry(Resultat_annee) <- NULL
  Nb_pond_hotspot[i] <- length(Resultat_annee[Resultat_annee$CLUSTER_POND=="Hotspot","EFF_OV"]) 
  Nb_homo_hotspot[i] <- length(Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="Hotspot","EFF_OV"])
  Nb_pond_nonhotspot[i] <- length(Resultat_annee[Resultat_annee$CLUSTER_POND=="No hotspot","EFF_OV"])
  Nb_homo_nonhotspot[i] <- length(Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="No hotspot","EFF_OV"])}

Nbsheep_pond_hotspot <- list()
Nbsheep_homo_hotspot <- list()
Nbsheep_pond_nonhotspot <- list()
Nbsheep_homo_nonhotspot <- list()

for(i in 1:length(Annees)){
  Resultat_annee <- Results_UPhotspots_sf_liste[[as.character(Annees[i])]]
  st_geometry(Resultat_annee) <- NULL
  Nbsheep_pond_hotspot[[i]] <- Resultat_annee[Resultat_annee$CLUSTER_POND=="Hotspot","EFF_OV"]
  Nbsheep_homo_hotspot[[i]] <- Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="Hotspot","EFF_OV"]
  Nbsheep_pond_nonhotspot[[i]] <- Resultat_annee[Resultat_annee$CLUSTER_POND=="No hotspot","EFF_OV"]
  Nbsheep_homo_nonhotspot[[i]] <- Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="No hotspot","EFF_OV"]}

Nbsheep_pond_hotspot <- unlist(Nbsheep_pond_hotspot)
Nbsheep_homo_hotspot <- unlist(Nbsheep_homo_hotspot)
Nbsheep_pond_nonhotspot <- unlist(Nbsheep_pond_nonhotspot)
Nbsheep_homo_nonhotspot <- unlist(Nbsheep_homo_nonhotspot)

mean(Nbsheep_pond_hotspot) ; sd(Nbsheep_pond_hotspot)
mean(Nbsheep_homo_hotspot) ; sd(Nbsheep_homo_hotspot)
mean(Nbsheep_pond_nonhotspot) ; sd(Nbsheep_pond_nonhotspot)

# Analyse mean grazing time of PS into hotspot
Temps_pond_hotspot <- rep(NA,length(Annees))
Temps_homo_hotspot <- rep(NA,length(Annees))
Temps_pond_nohotspot <- rep(NA,length(Annees))
Temps_homo_nohotspot <- rep(NA,length(Annees))

for(i in 1:length(Annees)){
  Resultat_annee <- Results_UPhotspots_sf_liste[[as.character(Annees[i])]]
  st_geometry(Resultat_annee) <- NULL
  Temps_pond_hotspot[i] <- mean(Resultat_annee[Resultat_annee$CLUSTER_POND=="Hotspot","DUREE_PAT"])
  Temps_homo_hotspot[i] <- mean(Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="Hotspot","DUREE_PAT"])
  Temps_pond_nohotspot[i] <- mean(Resultat_annee[Resultat_annee$CLUSTER_POND=="No hotspot","DUREE_PAT"])
  Temps_homo_nohotspot[i] <- mean(Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="No hotspot","DUREE_PAT"])}

mean(Temps_pond_hotspot) ; sd(Temps_pond_hotspot)
mean(Temps_pond_nohotspot) ; sd(Temps_pond_nohotspot)
mean(Temps_homo_hotspot) ; sd(Temps_homo_hotspot)

# Analyse grazing time of PS into hotspot or not
Temps_pond_hotspot <- list()
Temps_homo_hotspot <- list()
Temps_pond_nohotspot <- list()
Temps_homo_nohotspot <- list()

for(i in 1:length(Annees)){
  Resultat_annee <- Results_UPhotspots_sf_liste[[as.character(Annees[i])]]
  st_geometry(Resultat_annee) <- NULL
  Temps_pond_hotspot[[i]] <- Resultat_annee[Resultat_annee$CLUSTER_POND=="Hotspot","DUREE_PAT"]
  Temps_homo_hotspot[[i]] <- Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="Hotspot","DUREE_PAT"]
  Temps_pond_nohotspot[[i]] <- Resultat_annee[Resultat_annee$CLUSTER_POND=="No hotspot","DUREE_PAT"]
  Temps_homo_nohotspot[[i]] <- Resultat_annee[Resultat_annee$CLUSTER_FICTIF=="No hotspot","DUREE_PAT"]}

Temps_pond_hotspot <- unlist(Temps_pond_hotspot)
Temps_homo_hotspot <- unlist(Temps_homo_hotspot)
Temps_pond_nohotspot <- unlist(Temps_pond_nohotspot)
Temps_homo_nohotspot <- unlist(Temps_homo_nohotspot)

mean(Temps_pond_hotspot) ; sd(Temps_pond_hotspot)
mean(Temps_homo_hotspot) ; sd(Temps_homo_hotspot)
mean(Temps_pond_nohotspot) ; sd(Temps_pond_nohotspot)
mean(Temps_homo_nohotspot) ; sd(Temps_homo_nohotspot)

# Improved t-test function
rquery.t.test<-function(x, y = NULL, paired = FALSE, 
                        graph = TRUE, ...)
{
  # I. Preliminary test : normality and variance tests
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  var.equal = FALSE # by default
  
  # I.1 One sample t test
  if(is.null(y)){
    if(graph) par(mfrow=c(1,2))
    shapiro.px<-normaTest(x, graph, 
                          hist.title="X - Histogram",
                          qq.title="X - Normal Q-Q Plot")
    if(shapiro.px < 0.05)
      warning("x is not normally distributed :",
              " Shapiro-Wilk test p-value : ", shapiro.px, 
              ".\n Use a non-parametric test like Wilcoxon test.")
  }
  
  # I.2 Two samples t test
  if(!is.null(y)){
    
    # I.2.a unpaired t test
    if(!paired){
      if(graph) par(mfrow=c(2,2))
      # normality test
      shapiro.px<-normaTest(x, graph, 
                            hist.title="X - Histogram",
                            qq.title="X - Normal Q-Q Plot")
      shapiro.py<-normaTest(y, graph,
                            hist.title="Y - Histogram",
                            qq.title="Y - Normal Q-Q Plot")
      if(shapiro.px < 0.05 | shapiro.py < 0.05){
        warning("x or y is not normally distributed :",
                " Shapiro test p-value : ", shapiro.px,
                " (for x) and ", shapiro.py, " (for y)",
                ".\n Use a non parametric test like Wilcoxon test.")
      }
      # Check for equality of variances
      if(var.test(x,y)$p.value >= 0.05) var.equal=TRUE
    } 
    
    # I.2.b Paired t-test
    else {
      if(graph) par(mfrow=c(1,2))
      d = x-y 
      shapiro.pd<-normaTest(d, graph, 
                            hist.title="D - Histogram",
                            qq.title="D - Normal Q-Q Plot")
      if(shapiro.pd < 0.05 )
        warning("The difference d ( = x-y) is not normally distributed :",
                " Shapiro-Wilk test p-value : ", shapiro.pd, 
                ".\n Use a non-parametric test like Wilcoxon test.")
    } 
    
  }
  
  # II. Student's t-test
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- t.test(x, y, paired=paired, var.equal=var.equal, ...)
  return(res)
}
normaTest<-function(x, graph=TRUE, 
                    hist.title="Histogram", 
                    qq.title="Normal Q-Q Plot",...)
{  
  # Significance test
  #++++++++++++++++++++++
  shapiro.p<-signif(shapiro.test(x)$p.value,1) 
  
  if(graph){
    # Plot : Visual inspection
    #++++++++++++++++
    h<-hist(x, col="lightblue", main=hist.title, 
            xlab="Data values", ...)
    m<-round(mean(x),1)
    s<-round(sd(x),1)
    mtext(paste0("Mean : ", m, "; SD : ", s),
          side=3, cex=0.8)
    # add normal curve
    xfit<-seq(min(x),max(x),length=40)
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit, col="red", lwd=2)
    # qq plot
    qqnorm(x, pch=19, frame.plot=FALSE,main=qq.title)
    qqline(x)
    mtext(paste0("Shapiro-Wilk, p-val : ", shapiro.p),
          side=3, cex=0.8)
  }
  return(shapiro.p)
}

# Comparison between PS within hotspots or outside

rquery.t.test(Nbsheep_pond_hotspot, Nbsheep_pond_nonhotspot[0:4900], paired=FALSE) #x-y normally distributed
library(nortest)
t.test(Nbsheep_pond_hotspot, Nbsheep_pond_nonhotspot, paired = FALSE, alternative="less") #significant
wilcox.test(Nbsheep_pond_hotspot, Nbsheep_pond_nonhotspot, paired = FALSE, alternative = "less") #significant

rquery.t.test(Temps_pond_hotspot, Temps_pond_nohotspot, paired=FALSE) #x-y not normally distributed
wilcox.test(Temps_pond_hotspot, Temps_pond_nohotspot, paired = FALSE, alternative = "less") #significant

# Comparison between analysis with sheep availability and the one without

rquery.t.test(Nbsheep_pond_hotspot, Nbsheep_homo_hotspot, paired=FALSE) #x-y normally distributed
wilcox.test(Nbsheep_pond_hotspot, Nbsheep_homo_hotspot, paired = FALSE, alternative = "less") #significant

rquery.t.test(Temps_pond_hotspot, Temps_homo_hotspot, paired=FALSE) #x-y not normally distributed
wilcox.test(Temps_pond_hotspot, Temps_homo_hotspot, paired=FALSE, alternative="less") #significant

#wilcox.test(Temps_pond_hotspot, Temps_homo_hotspot, paired = FALSE, alternative = "less") #significant

# rquery.t.test(Eff_homo_hotspot, Eff_homo_nonhotspot, paired=FALSE) #x-y normally distributed
# t.test(Eff_homo_hotspot, Eff_homo_nonhotspot, paired=FALSE) #significant

rquery.t.test(Temps_homo_hotspot, Temps_homo_nohotspot, paired=FALSE) #x-y not normally distributed
wilcox.test(Temps_homo_hotspot, Temps_homo_nohotspot, paired=FALSE,alternative="greater") #significant
