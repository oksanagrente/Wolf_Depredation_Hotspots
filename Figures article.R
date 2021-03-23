################################ Wolf Attack Hotspots ##################################
################################# Article figures ######################################

########################################### Packages ###############################################

library(spatstat)
library(dplyr)
library(sf)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(gridExtra)
library(ggsn)
library(tidyr)
library(ggalt)
library(extrafont)
loadfonts(device = "win")
loadfonts(device = "pdf", quiet = T)
windowsFonts("Segoe UI" = windowsFont("Segoe UI"))
hatch <- function(x, density) {
  # x: polygon object (SpatialPolgyons* or sf)
  # density: approx number of lines to plot
  require(sp)
  require(raster)
  e <- extent(x)
  w <- diff(e[1:2])
  x1 <- seq(xmin(e), xmax(e)+w, length.out=floor(density*2))
  x0 <- seq(xmin(e)-w, xmax(e), length.out=floor(density*2))
  y0 <- rep(ymin(e), floor(density*2))
  y1 <- rep(ymax(e), floor(density*2))
  ll <- spLines(mapply(function(x0, y0, x1, y1) {
    rbind(c(x0, y0), c(x1, y1))
  }, x0, y0, x1, y1, 
  SIMPLIFY=FALSE))  
  if(is(x, 'sf')) {
    require(sf)
    ll <- st_as_sf(ll)
    st_crs(ll) <- st_crs(x)
    st_intersection(ll, x)
  } else {
    proj4string(ll) <- proj4string(x)
    raster::intersect(ll, x)
  }
}

########################################### Data ###############################################

setwd("C:/Users/grente/Documents/Etudes/Points chauds/Dossier revu/Data")
  load("Attaques_Mercantour_sf_liste.RData")
  load("UP1996_sf.RData")
  load("UP2012_sf.RData")
  load("UP1996_Mercantour_sf.RData")
  load("UP2012_Mercantour_sf.RData")
  load("Maillage_Loup_sf_liste.RData")
  load("Maillage_Loup_Mercantour_sf_liste.RData")
  load("Attaques_sf_liste.RData")
  load("Attaques_Mercantour_sf_liste.RData")
  load("Intensite_df_liste.RData")
  load("UP_Loup_sf_liste.RData")
  load("UP_Loup_Mercantour_sf_liste.RData")
  load("Full_model_Kinhom_liste.RData")
  load("Full_model_Kinhom_Mercantour_liste.RData")
  load("Null_model_Kinhom_liste.RData")
  load("Null_model_K_liste.RData")
  load("Null2_model_K_liste.RData")
  load("Donnees_Cluster_UP_df_liste.RData")
  load("Donnees_Cluster_UP_Mercantour_df_liste.RData")
  load("Cluster_UPpredatees_Effectifpondere_liste.RData")
  load("Cluster2_UPpredatees_Effectifpondere_liste.RData")
  load("Cluster_UPpredatees_Effectifhomogene_liste.RData")
  load("Cluster2_UPpredatees_Effectifhomogene_liste.RData")
  france_departements <- st_read("DEPARTEMENT.shp",crs=2154)
  Mercantour_sf <- st_read("PNF_PNM_aout_2015.shp",crs=2154) 
  Mercantour_tot <- st_read("Mercantour_etendu.shp",crs=2154) 
  Europe <- st_read("Europe.shp") %>% 
    st_transform(.,crs=2154)  
  Maillage_sf <- st_read("Maillage_10x10_Km_L93.shp",crs=2154) %>% 
    st_crop(france_departements)

########################################### General setup ###############################################
setwd("C:/Users/grente/Documents/Etudes/Points chauds/Dossier revu/Figures")
theme_set(theme_bw()) 
Annees <- c(1995:2018)

########################################### Study areas ###############################################

Study_area <- france_departements[france_departements$NOM_DEP %in% c("AIN","ALLIER","ALPES-DE-HAUTE-PROVENCE","HAUTES-ALPES",
                                                                     "ALPES-MARITIMES","ARDECHE","BOUCHES-DU-RHONE","CANTAL",
                                                                     "DROME","ISERE","LOIRE","HAUTE-LOIRE","PUY-DE-DOME","RHONE","SAVOIE",
                                                                     "HAUTE-SAVOIE","VAR","VAUCLUSE"),]  
Study_area2 <- st_union(Study_area) %>% 
               st_transform(.,crs=2154)

France_cropped <- st_union(st_crop(france_departements,Study_area))

Europe_cropped <- st_crop(Europe,france_departements)

############################################## Figure 1 ###############################################

ggplot() + 
  geom_sf(data=Europe_cropped, size=0.01) +
  geom_sf(data=Study_area2, fill="white", color="black", size=0.20) +
  geom_sf(data=Mercantour_tot,fill="white",color="black", size=0.2) +
  geom_sf(data=hatch(Mercantour_tot,density = 7),size=0.2) +
  ggsn::scalebar(data=Europe_cropped,transform = FALSE,dist = 100,dist_unit = "km", model = "WGS84",
                 st.dist=0.035, st.size=1.5, border.size=0.20,location = "bottomleft",family="Segoe UI") + 
  north(data=Europe_cropped,location="topright",scale=0.10,symbol=2) +
  theme(axis.title = element_blank(),panel.border = element_blank(),
        axis.text = element_text(size=5),text = element_text(family="Segoe UI"),axis.ticks = element_blank())

ggsave("Figure1.png", device="png", width=70, height= 80, units=c("mm"))

############################################## Figure 2 ###############################################

############## Number of sheep ############## 

Effectifs_classes <- cut(UP2012_sf$EFF_OV, 
                         breaks = c(0, 100, 200, 500, 700, 1000, 2500, +Inf), 
                         labels = c("<100", "100-199", "200-499", "500-699", "700-999", "1000-2499",">2500"), 
                         right = FALSE)

UP2012_sf$Eff_Class <- Effectifs_classes

Figure_Effectifs <-   ggplot() +
  geom_sf(data=Study_area2, fill="white", size=0.01) +
  geom_sf(data=Mercantour_tot,fill="white",color="black",size=0.3) +
  geom_sf(data=hatch(Mercantour_tot,density = 15),size=0.3) +
  geom_sf(data=UP2012_sf, aes(fill=Eff_Class, color=Eff_Class), size=0.3) +
  scale_fill_brewer(palette="Greens") +
  scale_color_brewer(palette="Greens") +
  labs(fill="Number of \nsheep", color="Number of \nsheep") +
  ggsn::scalebar(data=Study_area,transform = FALSE,dist = 50,dist_unit = "km", model = "WGS84",
                 st.dist=0.035, st.size=2, border.size=0.20,location = "bottomleft",family="Segoe UI") + 
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), panel.border = element_blank(),
        text = element_text(family="Segoe UI"),
        legend.key.size = unit(0.03,"npc"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=9),
        legend.position = "right") 

############## Grazing time ############## 

Figure_Duree <-   ggplot() +
  geom_sf(data=Study_area2, fill="white", size=0.01) +
  geom_sf(data=Mercantour_tot,fill="white",color="black",size=0.3) +
  geom_sf(data=hatch(Mercantour_tot,density = 15),size=0.3) +
  geom_sf(data=UP2012_sf, aes(fill=DUREE_PAT, color=DUREE_PAT), size=0.3) +
  scale_fill_gradient(low=brewer.pal(9, "Blues")[1],high=brewer.pal(9,"Blues")[9]) +
  scale_color_gradient(low=brewer.pal(9, "Blues")[1],high=brewer.pal(9,"Blues")[9]) +
  north(data=Study_area,location="topright",scale=0.125,symbol=2) +
  labs(fill="Sheep grazing \ntime (days)",color="Sheep grazing \ntime (days)") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), panel.border = element_blank(),
        text = element_text(family="Segoe UI"),
        legend.key.size = unit(0.03,"npc"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=9),
        legend.spacing.x = unit(0.5, 'cm')) 

FinalFigure <- plot_grid(Figure_Effectifs, Figure_Duree, labels = c('A', 'B'), nrow = 1, label_fontfamily = "Segoe UI",
                        label_size = 15, scale=0.9, label_x = 0.12, label_fontface="plain")

ggsave("Figure2.png", device="png", width=210, height= 70, units=c("mm"))

############################################## Figure 3 ###############################################

############## National scale #############

#Proportion of wolf distribution
Nb_Cell_tot <- length(unique(as.data.frame(st_intersects(Study_area2,Maillage_sf))$col.id))
Nb_Cell <- c(rep(NA,24))
for (i in 1:24){Nb_Cell[i] <- nrow(Maillage_Loup_sf_liste[[as.character(Annees[i])]])}
Prop_Colo <- ceiling(Nb_Cell/Nb_Cell_tot*100)

#Number and proportion of PS at risk 
Nb_PS_risk <- c(rep(NA,24))
for (i in 1:24){Nb_PS_risk[i] <- nrow(UP_Loup_sf_liste[[as.character(Annees[i])]])}
Nb_PS_tot <- c(rep(nrow(UP1996_sf),11),rep(nrow(UP2012_sf),13))
Prop_PS_risk <- ceiling(Nb_PS_risk/Nb_PS_tot*100)

#Number and proportion of attacked PS 
PS_attacked_fonction <- function(x){  
  Nb_PS_Att <- c()
  Attaques_Annee_sf <- Attaques_sf_liste[[as.character(x)]]
  SP_Annee_sf <- UP_Loup_sf_liste[[as.character(x)]]
  SP_Att_df <- as.data.frame(st_intersects(Attaques_Annee_sf,SP_Annee_sf)) 
  Nb_PS_Att[x] <- length(unique(SP_Att_df$col.id))}
Nb_PS_Att <- sapply(1995:2018, function(x) PS_attacked_fonction(x))
Prop_PS_Att <- ceiling(Nb_PS_Att/Nb_PS_risk*100)

#Number of attacks 
Nb_Att <- c(rep(NA,24))
for (i in 1:24){Nb_Att[i] <- nrow(Attaques_sf_liste[[as.character(Annees[i])]])}

#Differences  
# Diff_df <- data.frame(
#   Diff_growth_colo = Prop_Colo-lag(Prop_Colo),
#   Diff_growth_risk = Prop_PS_risk - lag(Prop_PS_risk),
#   Diff_growth_PSatt = Prop_PS_Att - lag(Prop_PS_Att),
#   Diff_growth_Att = Nb_Att - lag(Nb_Att))
# Growth_rate_nat_df <- data.frame(
#   Annees=Annees,
#   Rate_colo = Diff_df$Diff_growth_colo/Prop_Colo * 100,
#   Rate_percent_risk = Diff_df$Diff_growth_risk/Prop_PS_risk * 100,
#   Rate_percent_PSatt = Diff_df$Diff_growth_PSatt/Prop_PS_Att * 100,
#   Rate_percent_NbAtt = Diff_df$Diff_growth_Att/Nb_Att * 100)
# Growth_rate_nat_df <- gather(data=Growth_rate_nat_df, "Type", "Value",-Annees)
# Growth_rate_nat_df$Type <- factor(Growth_rate_nat_df$Type, levels = c("Rate_percent_risk", "Rate_percent_PSatt","Rate_percent_NbAtt"))

#Data.frame of results
Nb_nat_df <- data.frame(
  Annees=Annees,
  Prop_Colo = Prop_Colo,
  Prop_PS_risk = Prop_PS_risk,
  Prop_PS_Att = Prop_PS_Att,
  Nb_Att = Nb_Att/25) #Divided for the secondary axis
Nb_nat_df <- gather(data=Nb_nat_df, "Type", "Value",-Annees)
Nb_nat_df$Type <- factor(Nb_nat_df$Type, levels = c("Prop_Colo", "Prop_PS_risk","Prop_PS_Att","Nb_Att"))
# Growth_rate_nat_df$Raw_nb <- Nb_nat_df$Value

#Plot national scale
PropandNumbers_plot <- ggplot(data=Nb_nat_df,aes(x=Annees, y=Value,color=Type)) +
  geom_xspline(size=0.9) +
  scale_color_manual(values=c(brewer.pal(9,"OrRd")[3], brewer.pal(9,"Greens")[5],brewer.pal(9,"Greens")[8],
                              brewer.pal(11,"Spectral")[1]), labels=c("Area recolonised\n by wolves","Pastoral surfaces\nunder depredation risk",
                                                                      "Pastoral surfaces\nwith depredations", "Number of\ndepredations"),
                     name="") +
  # scale_linetype_manual(values=c("solid","solid","solid","solid"), labels=c("Area recolonised\n by wolves","PS\nunder depredation risk",
  #                                                                     "PS\nwith depredations", "Number of\ndepredations"),
  #                       name="")+
  scale_x_continuous(breaks=c(seq(from=1996, to=2018, by=2)),
                     labels=as.character(c(seq(from=1996, to=2018, by=2))),
                     expand = c(0.015,0)) +
  scale_y_continuous("Proportion (%)", sec.axis = sec_axis(~ . * 25, name = "Number of depredations"),
                     limits = c(0,100)) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 10, family="Segoe UI"),
        legend.text = element_text(size=10),
        text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.ticks = element_line(colour = 'black', size = 0.1),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.text.x = element_text(angle=45,vjust=0.6),
        axis.title.y.left = element_text(size = 13, face = "italic", family="Segoe UI",
                                         margin = unit(c(0, 3, 0, 0), "mm")),
        axis.line.y.right = element_line(color = brewer.pal(11,"Spectral")[1],size=0.65), 
        axis.ticks.y.right = element_line(color = brewer.pal(11,"Spectral")[1]),
        axis.text.y.right = element_text(color = brewer.pal(11,"Spectral")[1], family="Segoe UI"),
        axis.title.y.right = element_text(color = brewer.pal(11,"Spectral")[1],margin = unit(c(0, 0, 0, 3), "mm"),
                                          size = 13, face = "italic", family="Segoe UI"),
        panel.border = element_blank(),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major = element_blank())

# Growthrate_plot <- ggplot(data=Growth_rate_nat_df,aes(x=Annees, y=Value,color=Type,linetype=Type)) +
# geom_rect(aes(xmin= -Inf,
#               xmax = +Inf,
#               ymin = -Inf,
#               ymax = 0), fill = brewer.pal(9,"Greys")[2], alpha = 0.1, color=NA) +
#   geom_xspline(size=0.9) +
#   scale_color_manual(values=c(brewer.pal(9,"Greens")[5],brewer.pal(9,"Greens")[8],
#                               brewer.pal(11,"Spectral")[1])) +
#   scale_linetype_manual(values=c("solid","twodash","solid"))+
#   scale_x_continuous(breaks=c(seq(from=1996, to=2018, by=2)),labels=as.character(c(seq(from=1996, to=2018, by=2))),expand = c(0.015,0)) +
#   labs(y = "Growth rate (%)", x = "") +
#   theme_bw() +
#   theme(axis.text = element_text(size = 10, family="Segoe UI"),
#         legend.position = "none",
#         text = element_text(size = 10),
#         axis.title.y = element_text(size = 13, face = "italic", family="Segoe UI"),
#         axis.title.x = element_blank(),
#         axis.ticks = element_line(colour = 'black', size = 0.1),
#         axis.line = element_line(colour = "black", size = 0.1),
#         axis.text.x = element_text(angle=45,vjust=0.6),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor.x = element_line(colour = NA),
#         panel.grid.major = element_blank())

############## Mercantour scale #############

#Proportion of wolf distribution
Nb_Cell_tot_Mercantour <- length(unique(as.data.frame(st_intersects(Mercantour_tot,Maillage_sf))$col.id))
Nb_Cell_Mercantour <- c(rep(NA,24))
for (i in 1:24){Nb_Cell_Mercantour[i] <- nrow(Maillage_Loup_Mercantour_sf_liste[[as.character(Annees[i])]])}
Prop_Colo_Mercantour <- ceiling(Nb_Cell_Mercantour/Nb_Cell_tot_Mercantour*100)

#Number and proportion of PS at risk in Mercantour
Nb_PS_risk_Mercantour <- c(rep(NA,24))
for (i in 1:24){Nb_PS_risk_Mercantour[i] <- nrow(UP_Loup_Mercantour_sf_liste[[as.character(Annees[i])]])}
Nb_PS_tot_Mercantour <- c(rep(nrow(UP1996_Mercantour_sf),11),rep(nrow(UP2012_Mercantour_sf),13))
Prop_PS_risk_Mercantour <- ceiling(Nb_PS_risk_Mercantour/Nb_PS_tot_Mercantour*100)

#Number and proportion of attacked PS in Mercantour
PS_attacked_Mercantour_fonction <- function(x){  
  Nb_PS_Att_Mercantour <- c()
  Attaques_Annee_sf <- Attaques_Mercantour_sf_liste[[as.character(x)]]
  SP_Annee_sf <- UP_Loup_Mercantour_sf_liste[[as.character(x)]]
  SP_Att_df <- as.data.frame(st_intersects(Attaques_Annee_sf,SP_Annee_sf)) 
  Nb_PS_Att_Mercantour[x] <- length(unique(SP_Att_df$col.id))}
Nb_PS_Att_Mercantour <- sapply(1995:2018, function(x) PS_attacked_Mercantour_fonction(x))
Prop_PS_Att_Mercantour <- ceiling(Nb_PS_Att_Mercantour/Nb_PS_risk_Mercantour*100)

#Number of attacks 
Nb_Att_Mercantour <- c(rep(NA,24))
for (i in 1:24){Nb_Att_Mercantour[i] <- nrow(Attaques_Mercantour_sf_liste[[as.character(Annees[i])]])}

# #Differences
# Diff_df <- data.frame(
#   Diff_growth_risk_Mercantour = Prop_PS_risk_Mercantour - lag(Prop_PS_risk_Mercantour),
#   Diff_growth_PSatt_Mercantour = Prop_PS_Att_Mercantour - lag(Prop_PS_Att_Mercantour),
#   Diff_growth_Att_Mercantour = Nb_Att_Mercantour - lag(Nb_Att_Mercantour)) 
# #Restricted scale
# Growth_rate_Mercantour_df <- data.frame(
#   Annees=Annees,
#   Rate_percent_risk_Mercantour = Diff_df$Diff_growth_risk_Mercantour/Prop_PS_risk_Mercantour * 100,
#   Rate_percent_PSatt_Mercantour = Diff_df$Diff_growth_PSatt_Mercantour/Prop_PS_Att_Mercantour * 100,
#   Rate_percent_NbAtt_Mercantour = Diff_df$Diff_growth_Att_Mercantour/Nb_Att_Mercantour * 100)
# Growth_rate_Mercantour_df <- gather(data=Growth_rate_Mercantour_df, "Type", "Value",-Annees)
# Growth_rate_Mercantour_df$Type <- factor(Growth_rate_Mercantour_df$Type, levels = c("Rate_percent_risk_Mercantour", 
#                                                                                     "Rate_percent_PSatt_Mercantour",
#                                                                                     "Rate_percent_NbAtt_Mercantour"))

#Data.frame of result
Nb_Mercantour_df <- data.frame(
  Annees=Annees,
  Prop_Colo_Mercantour = Prop_Colo_Mercantour,
  Prop_PS_risk_Mercantour = Prop_PS_risk_Mercantour,
  Prop_PS_Att_Mercantour = Prop_PS_Att_Mercantour,
  Nb_Att_Mercantour = Nb_Att_Mercantour/25)
Nb_Mercantour_df <- gather(data=Nb_Mercantour_df, "Type", "Value",-Annees)
Nb_Mercantour_df$Type <- factor(Nb_Mercantour_df$Type, levels = c("Prop_Colo_Mercantour", "Prop_PS_risk_Mercantour","Prop_PS_Att_Mercantour","Nb_Att_Mercantour"))
#Growth_rate_Mercantour_df$Raw_nb <- Nb_Mercantour_df$Value

#Plot restricted scale
PropandNumbers_Mercantour_plot <- ggplot(data=Nb_Mercantour_df,aes(x=Annees, y=Value,color=Type)) +
  geom_xspline(size=0.9) +
  scale_color_manual(values=c(brewer.pal(9,"OrRd")[3], brewer.pal(9,"Greens")[5],brewer.pal(9,"Greens")[8],
                              brewer.pal(11,"Spectral")[1]), labels=c("Area recolonised\n by wolves","Pastoral surfaces\nunder depredation risk",
                                                                      "Pastoral surfaces\nwith depredations", "Number of\ndepredations"),
                     name="") +
  # scale_linetype_manual(values=c("solid","twodash","solid"), labels=c("Proportion of PS\nunder depredation risk",
  #                                                                     "Proportion of PS\nwith depredations", "Number of\ndepredations"),
  #                       name="")+
  scale_x_continuous(breaks=c(seq(from=1996, to=2018, by=2)),labels=as.character(c(seq(from=1996, to=2018, by=2))),expand = c(0.015,0)) +
  scale_y_continuous("Proportion (%)", sec.axis = sec_axis(~ . * 25, name = "Number of depredations")) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 10, family="Segoe UI"),
        legend.text = element_text(size=10),
        text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.ticks = element_line(colour = 'black', size = 0.1),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.text.x = element_text(angle=45,vjust=0.6),
        axis.title.y.left = element_text(size = 13, face = "italic", family="Segoe UI",
                                         margin = unit(c(0, 3, 0, 0), "mm")),
        axis.line.y.right = element_line(color = brewer.pal(11,"Spectral")[1],size=0.65), 
        axis.ticks.y.right = element_line(color = brewer.pal(11,"Spectral")[1]),
        axis.text.y.right = element_text(color = brewer.pal(11,"Spectral")[1], family="Segoe UI"),
        axis.title.y.right = element_text(color = brewer.pal(11,"Spectral")[1],margin = unit(c(0, 0, 0, 3), "mm"),
                                          size = 13, face = "italic", family="Segoe UI"),
        panel.border = element_blank(),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major = element_blank())

# Growthrate_Mercantour_plot <- ggplot(data=Growth_rate_Mercantour_df,aes(x=Annees, y=Value,color=Type,linetype=Type)) +
# geom_rect(aes(xmin= -Inf,
#               xmax = +Inf,
#               ymin = -Inf,
#               ymax = 0), fill = brewer.pal(9,"Greys")[2], alpha = 0.1, color=NA) +
#   geom_xspline(size=0.9) +
#   scale_color_manual(values=c(brewer.pal(9,"Greens")[5],brewer.pal(9,"Greens")[8],
#                               brewer.pal(11,"Spectral")[1])) +
#   scale_linetype_manual(values=c("solid","twodash","solid"))+
#   scale_x_continuous(breaks=c(seq(from=1996, to=2018, by=2)),labels=as.character(c(seq(from=1996, to=2018, by=2))),expand = c(0.015,0)) +
#   labs(y = "Growth rate (%)", x = "") +
#   theme_bw() +
#   theme(axis.text = element_text(size = 10, family="Segoe UI"),
#         legend.position = "none",
#         text = element_text(size = 10),
#         axis.title.y = element_text(size = 13, face = "italic", family="Segoe UI"),
#         axis.title.x = element_blank(),
#         axis.ticks = element_line(colour = 'black', size = 0.1),
#         axis.line = element_line(colour = "black", size = 0.1),
#         axis.text.x = element_text(angle=45,vjust=0.6),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor.x = element_line(colour = NA),
#         panel.grid.major = element_blank())

############## Merge plots #############

Legende <- get_legend(PropandNumbers_plot + 
                        guides(color = guide_legend(nrow = 2)))

grid1 <- plot_grid(PropandNumbers_plot + theme(legend.position="none"),
                   # Growthrate_plot + theme(legend.position="none"),
                   PropandNumbers_Mercantour_plot + theme(legend.position="none"),
                   # Growthrate_Mercantour_plot + theme(legend.position="none"),
                   labels = c('A', 'B'), 
                   label_y=1.02,
                   label_fontfamily = "Segoe UI",
                   label_fontface = "plain",
                   label_size = 20,
                   nrow=2, align='vh')

FigureFinale <- plot_grid(grid1, Legende, ncol=1, rel_heights = c(1, .15))

ggsave("Figure3.png", device="png", width=130, height= 160, units=c("mm"))

############################################## Figure 4 ###############################################

# Prepare data
Annee <- c(1995,2001,2010,2018)

Modele <- list(Full_model_Kinhom_liste,Null_model_K_liste,Null2_model_K_liste)
names(Modele) <- c("Full model","K null model","K null model 2")

Resultats_Ripley_df_fonction <- function(x) {
  Resultats_Ripley_df_liste <- list()
  Resultats_Ripley_Annee_df_liste <- list()
  for(m in names(Modele)){
    Resultats_Ripley_Annee_df_liste[[m]] <- data.frame(x=Modele[[m]][[as.character(x)]]$r/1000,
                                                       y=Modele[[m]][[as.character(x)]]$obs/10e+8,
                                                       lower=Modele[[m]][[as.character(x)]]$lo/10e+8,
                                                       upper=Modele[[m]][[as.character(x)]]$hi/10e+8,
                                                       model=as.character(m))}
  Resultats_Ripley_Annee_df <- do.call("rbind", Resultats_Ripley_Annee_df_liste)
  Resultats_Ripley_df_liste[[x]] <- Resultats_Ripley_Annee_df}
Resultats_Ripley_df_liste <- lapply(Annee, function(x) Resultats_Ripley_df_fonction(x))
names(Resultats_Ripley_df_liste) <- c(Annee)

# Delimit the x-axis
Rmin_Kinhom <- c(rep(0,4))
for(x in 1:4){
  Resultats_Ripley_Annee <- Resultats_Ripley_df_liste[[as.character(Annee[x])]]
  Rmin_Kinhom[x] <- max(Resultats_Ripley_Annee[Resultats_Ripley_Annee$model=="Full model"&!is.na(Resultats_Ripley_Annee$lower),"x"])}

Resultats2_Ripley_df_fonction <- function(x){
  Resultats2_Ripley_df_liste <- list()
  Resultats_Ripley_Annee <- Resultats_Ripley_df_liste[[as.character(Annee[x])]]
  Resultats2_Ripley_df_liste[[x]] <- Resultats_Ripley_Annee[Resultats_Ripley_Annee$x<Rmin_Kinhom[x],]}
Resultats2_Ripley_df_liste <- lapply(1:4, function(x) Resultats2_Ripley_df_fonction(x))
names(Resultats2_Ripley_df_liste) <- c(Annee)

# Rmin_Ripley <- c(rep(0,4))
# for(x in 1:4){
#   Resultats_Ripley_Annee <- Resultats_Ripley_df_liste[[as.character(Annee[x])]]
#   Rmin_Ripley[x] <- min(c(max(Resultats_Ripley_Annee[Resultats_Ripley_Annee$model=="Full model"&!is.na(Resultats_Ripley_Annee$lower),"x"]),
#                           max(Resultats_Ripley_Annee[Resultats_Ripley_Annee$model=="K null model"&!is.na(Resultats_Ripley_Annee$lower),"x"]),
#                           max(Resultats_Ripley_Annee[Resultats_Ripley_Annee$model=="K null model 2"&!is.na(Resultats_Ripley_Annee$lower),"x"])))}

# Plot results
Resultats_Ripley_plot_fonction <- function(x){
  Resultats_Ripley_plot_liste <- list()
  Resultats_Ripley_plot <- ggplot(Resultats2_Ripley_df_liste[[as.character(Annee[x])]]) +
    geom_ribbon(aes(ymin=lower, ymax=upper, x=x,fill = model),alpha=0.4, size=0.8)+
    geom_line(aes(y=y,x=x, colour = model,linetype=model),size=0.5) + 
    scale_fill_manual(values = c("#D8B365","#01665E","#5AB4AC"),
                      labels = c(bquote(K[inhom]),
                                 bquote(K~with~pastoral~surfaces),bquote(K~without~pastoral~surfaces)),name="") +
    scale_color_manual(values = c("#D8B365","#01665E","#5AB4AC"),
                       labels = c(bquote(K[inhom]),
                                  bquote(K~with~pastoral~surfaces),bquote(K~without~pastoral~surfaces)),name="") +
    scale_linetype_manual(values=c("solid","twodash","twodash"),
                          labels = c(bquote(K[inhom]),
                                     bquote(K~with~pastoral~surfaces),bquote(K~without~pastoral~surfaces)),name="") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)
                       # limits=c(0,Rmin_Kinhom[x])
                       ) +
    labs(x="r (km)", y= bquote(italic(K[inhom]~(10^9)))) +
    theme_bw()+
    theme(axis.text = element_text(size=10, family="Segoe UI"),
          legend.text = element_text(size=10),
          legend.box.margin = margin(0,0,0,0),
          legend.margin=margin(0,0,0,0),
          axis.title = element_text(size=13, face="italic", family="Segoe UI"),
          axis.title.x = element_text(margin = unit(c(t = 3, r = 0, b = 0, l = 0), "mm")),
          axis.ticks = element_line(colour = 'black', size = 0.1),
          axis.line = element_line(colour = "black", size=0.1),
          panel.border = element_blank(), 
          panel.grid.minor.x = element_line(colour = NA),
          panel.grid.major = element_blank())
  Resultats_Ripley_plot_liste[[x]] <- Resultats_Ripley_plot}
Resultats_Ripley_plot_liste <- lapply(1:4, function(x) Resultats_Ripley_plot_fonction(x))

grid1 <- plot_grid(Resultats_Ripley_plot_liste[[1]]+ xlab(NULL) + theme(legend.position="none"),
                   Resultats_Ripley_plot_liste[[2]]+ ylab(NULL)+xlab(NULL) + theme(legend.position="none"),
                   Resultats_Ripley_plot_liste[[3]]+ theme(legend.position="none"),
                   Resultats_Ripley_plot_liste[[4]]+ ylab(NULL) + theme(legend.position="none"),
                   align = 'vh',
                   nrow=2,
                   labels = c(as.character(Annee)), 
                   label_x=0.2,
                   label_fontfamily = "Segoe UI",
                   label_fontface = "plain",
                   label_size = 15)

Legende <- get_legend(
  Resultats_Ripley_plot_liste[[1]] + 
    guides(color = guide_legend(nrow = 2)))

FigureRipley <- plot_grid(grid1, Legende, ncol=1,rel_heights = c(1, .15))

ggsave("Figure4.png", device="png", width=130, height= 160, units=c("mm"))

###################################### Figure 4b Bonus #########################################

Annee <- c(1995,2001,2010,2018)

Attaques_plot_fonction <- function(x){
  Attaques_plot_liste <- list()
  Maillage_total_Annnee <- Maillage_Loup_sf_liste[[as.character(Annee[x])]]
  Maillage_analyse_Annnee <- Maillage_total_Annnee[as.data.frame(st_intersects(Maillage_total_Annnee,Study_area2))$row.id,]
  Attaques_plot_liste[[x]] <- ggplot() +
    geom_sf(data=Study_area2,fill="white",color="black", size=0.2) +
    geom_sf(data=Maillage_analyse_Annnee, color=NA, aes(fill="Wolf presence cell")) +
    geom_sf(data=UP_Loup_sf_liste[[as.character(Annee[x])]],color=NA,aes(fill="Pastoral surfaces \nunder depredation risk")) +
    geom_sf(data=Attaques_sf_liste[[as.character(Annee[x])]],aes(color="Depredation on sheep"),size=0.3)+
    scale_fill_manual(values=c("Pastoral surfaces \nunder depredation risk"=brewer.pal(9,"Greens")[5],
                               "Wolf presence cell"=brewer.pal(9,"OrRd")[2]),name="")+
    scale_color_manual(values=c("Depredation on sheep"=brewer.pal(11,"Spectral")[1]),name="")+
    theme(axis.title = element_blank(),panel.border = element_blank(),
          axis.text = element_blank(),axis.ticks = element_blank(),
          legend.position = "bottom")}
Attaques_plot_liste <- lapply(1:4, function(x) Attaques_plot_fonction(x))

Figure_Attaques <- plot_grid(Attaques_plot_liste[[1]]+ theme(legend.position="none") +
                                      north(data=Study_area,location="topright",scale=0.15,symbol=2) +
                                      ggsn::scalebar(data=Study_area,transform = FALSE,dist = 50,dist_unit = "km", model = "WGS84",
                                                     st.dist=0.025, st.size=2, border.size=0.20,location = "bottomleft",family="Segoe UI"), 
                                    Attaques_plot_liste[[2]]+ theme(legend.position="none"),
                                    Attaques_plot_liste[[3]]+ theme(legend.position="none"),
                                    Attaques_plot_liste[[4]]+ theme(legend.position="none"),
                                    align = 'vh',
                                    nrow=2,
                                    labels = c(as.character(Annee)), 
                                    label_x=0.25,
                                    label_y=0.75,
                                    label_fontfamily = "Segoe UI",
                                    label_fontface = "plain",
                                    label_size = 12)

Legende <- get_legend(Attaques_plot_liste[[1]]+ 
                        guides(color = guide_legend(override.aes = list(size=2)),
                               fill = guide_legend(override.aes = list(size=1))))

Figure_Attaques <- plot_grid(Figure_Attaques, Legende, ncol=1,rel_heights = c(1, .05))

ggsave("Figure4bonus.png", device="png", width=160, height=130, units=c("mm"))

############################################## Figure 5 ###############################################

Annee <- c(1999,2004,2010,2018)

Resultats_Ripley_Mercantour_df_liste <- list()
for(x in Annee){
  Resultats_Ripley_Mercantour_df_liste[[x]] <- data.frame(x=Full_model_Kinhom_Mercantour_liste[[as.character(x)]]$r/1000,
                                                          y=Full_model_Kinhom_Mercantour_liste[[as.character(x)]]$obs/10e+8,
                                                          lower=Full_model_Kinhom_Mercantour_liste[[as.character(x)]]$lo/10e+8,
                                                          upper=Full_model_Kinhom_Mercantour_liste[[as.character(x)]]$hi/10e+8,
                                                          annee=as.factor(x))}
Resultats_Ripley_Mercantour_df <- do.call("rbind", Resultats_Ripley_Mercantour_df_liste)
Resultats_Ripley_Mercantour_df$annee <- factor(Resultats_Ripley_Mercantour_df$annee, 
                                               levels = c("1999", "2010", "2004", "2018"))

# Rmin_Ripley <- min(c(max(Resultats_Ripley_Mercantour_df[Resultats_Ripley_Mercantour_df$annee==as.character(Annee[1]),"x"]),
#                      max(Resultats_Ripley_Mercantour_df[Resultats_Ripley_Mercantour_df$annee==as.character(Annee[2]),"x"]),
#                      max(Resultats_Ripley_Mercantour_df[Resultats_Ripley_Mercantour_df$annee==as.character(Annee[3]),"x"]),
#                      max(Resultats_Ripley_Mercantour_df[Resultats_Ripley_Mercantour_df$annee==as.character(Annee[4]),"x"])))

# ggplot(Resultats_Ripley_Mercantour_df) +
#   geom_ribbon(aes(ymin = lower,ymax = upper,x = x,fill = annee),alpha = 0.6,size = 0.8) +
#   geom_line(aes(y = y, x = x, colour = annee),size = 0.4,linetype = "twodash") +
#   scale_fill_manual(values = c("1999" = "#D8B365","2004" = "#F6E8C3","2010" = "#5AB4AC","2018" = "#01665E"),
#                     breaks = c("1999", "2004", "2010", "2018"),name="") +
#   scale_colour_manual(values = c("1999" = "#8C510A","2004" = "#D8B365","2010" = "#5AB4AC","2018" = "#01665E"),
#                       breaks = c("1999", "2004", "2010", "2018"),name="") +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 6)
#                      # limits=c(0,Rmin_Ripley)
#                       ) +
#   labs(x="r (km)", y= bquote(italic(K[inhom]~(10^9)))) +
#   theme_bw()+
#   theme(axis.text = element_text(size=10, family="Segoe UI"),
#         legend.text = element_text(size=10),
#         legend.box.margin = margin(0,0,0,0),
#         legend.margin=margin(0,0,0,0),
#         axis.title = element_text(size=13, face="italic", family="Segoe UI"),
#         axis.title.x = element_text(margin = unit(c(t = 3, r = 0, b = 0, l = 0), "mm")),
#         axis.ticks = element_line(colour = 'black', size = 0.1),
#         axis.line = element_line(colour = "black", size=0.1),
#         panel.border = element_blank(), 
#         panel.grid.minor.x = element_line(colour = NA),
#         panel.grid.major = element_blank())
# 
# ggsave("Figure5.png", device="png", width=100, height= 80, units=c("mm"))

Resultats_Ripley_Mercantour_plot_fonction <- function(x){
  Resultats_Ripley_Mercantour_plot_liste <- list()
  Resultats_Ripley_Mercantour_plot <- ggplot(Resultats_Ripley_Mercantour_df[Resultats_Ripley_Mercantour_df$annee==Annee[x],]) +
    geom_ribbon(aes(ymin=lower, ymax=upper, x=x),fill="#D8B365",colour=NA,alpha=0.4, size=0.8)+
    geom_line(aes(y=y,x=x),colour="#D8B365", size=0.5,linetype="solid") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
    labs(x="r (km)", y= bquote(italic(K[inhom]~(10^9)))) +
    theme_bw()+
    theme(axis.text = element_text(size=10, family="Segoe UI"),
          legend.text = element_text(size=10),
          legend.box.margin = margin(0,0,0,0),
          legend.margin=margin(0,0,0,0),
          axis.title = element_text(size=13, face="italic", family="Segoe UI"),
          axis.title.x = element_text(margin = unit(c(t = 3, r = 0, b = 0, l = 0), "mm")),
          axis.ticks = element_line(colour = 'black', size = 0.1),
          axis.line = element_line(colour = "black", size=0.1),
          panel.border = element_blank(), 
          panel.grid.minor.x = element_line(colour = NA),
          panel.grid.major = element_blank())
  Resultats_Ripley_Mercantour_plot_liste[[x]] <- Resultats_Ripley_Mercantour_plot}
Resultats_Ripley_Mercantour_plot_liste <- lapply(1:4, function(x) Resultats_Ripley_Mercantour_plot_fonction(x))

FigureRipleyMercantour <- plot_grid(Resultats_Ripley_Mercantour_plot_liste[[1]]+ xlab(NULL) + theme(legend.position="none"),
                   Resultats_Ripley_Mercantour_plot_liste[[2]]+ ylab(NULL)+xlab(NULL) + theme(legend.position="none"),
                   Resultats_Ripley_Mercantour_plot_liste[[3]]+ theme(legend.position="none"),
                   Resultats_Ripley_Mercantour_plot_liste[[4]]+ ylab(NULL) + theme(legend.position="none"),
                   align = 'vh',
                   nrow=2,
                   labels = c(as.character(Annee)), 
                   label_x=0.2,
                   label_fontfamily = "Segoe UI",
                   label_fontface = "plain",
                   label_size = 15)

ggsave("Figure5b.png", device="png", width=130, height= 160, units=c("mm"))

###################################### Figure 5b Bonus #########################################

Annee_Mercantour <- c(1999,2004,2010,2018)

Attaques_Mercantour_plot_fonction <- function(x){
  Attaques_Mercantour_plot_liste <- list()
  Attaques_Mercantour_plot_liste[[x]] <- ggplot() +
    geom_sf(data=Mercantour_tot,fill="white",color="black", size=0.2) +
    geom_sf(data=UP_Loup_Mercantour_sf_liste[[as.character(Annee_Mercantour[x])]],color=NA,aes(fill="Pastoral surfaces \nunder depredation risk")) +
    geom_sf(data=Attaques_Mercantour_sf_liste[[as.character(Annee_Mercantour[x])]],aes(color="Depredation on sheep"),size=0.8)+
    scale_fill_manual(values=c("Pastoral surfaces \nunder depredation risk"=brewer.pal(9,"Greens")[5]),name="")+
    scale_color_manual(values=c("Depredation on sheep"=brewer.pal(11,"Spectral")[1]),name="")+
    theme(axis.title = element_blank(),panel.border = element_blank(),
          axis.text = element_blank(),axis.ticks = element_blank(),
          legend.position = "bottom")}
Attaques_Mercantour_plot_liste <- lapply(1:4, function(x) Attaques_Mercantour_plot_fonction(x))

FigureRipleyMercantour <- plot_grid(Attaques_Mercantour_plot_liste[[1]]+ theme(legend.position="none") +
                                      north(data=Mercantour_tot,location="topright",scale=0.15,symbol=2) +
                                      ggsn::scalebar(data=Mercantour_tot,transform = FALSE,dist = 10,dist_unit = "km", model = "WGS84",
                                                     st.dist=0.025, st.size=2, border.size=0.20,location = "bottomleft",family="Segoe UI"), 
                                    Attaques_Mercantour_plot_liste[[2]]+ theme(legend.position="none"),
                                    Attaques_Mercantour_plot_liste[[3]]+ theme(legend.position="none"),
                                    Attaques_Mercantour_plot_liste[[4]]+ theme(legend.position="none"),
                                    align = 'vh',
                                    nrow=2,
                                    labels = c(as.character(Annee_Mercantour)), 
                                    label_x=0.55,
                                    label_y=0.9,
                                    label_fontfamily = "Segoe UI",
                                    label_fontface = "plain",
                                    label_size = 13)

Legende <- get_legend(Attaques_Mercantour_plot_liste[[1]]+ 
                        guides(color = guide_legend(override.aes = list(size=2))))

FigureMercantour <- plot_grid(FigureRipleyMercantour, Legende, ncol=1,rel_heights = c(1, .05))

ggsave("Figure5Bonus.png", device="png", width=160, height=130, units=c("mm"))

############################################## Figure 6 ###############################################

############## Ratio of PS number in hotspots #############

Annees <- c(1995:2018)

#Pepare ratio of PS in hotspots
Taux_C1_UPpred_pondere <- c()
Taux_C2_UPpred_pondere <- c()
Taux_tot_UPpred_pondere <- c()
Nb_PtChaud_pondere <- list()
for (x in 1:length(Annees)) {
  Taux_C1_UPpred_pondere[x]=length(Cluster_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]])/length(which(Donnees_Cluster_UP_df_liste[[as.character(Annees[x])]]$ATTAQUES>0))*100
  if(!is.null(nrow(Cluster2_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]]))){
    Taux_C2_UPpred_pondere[x]= nrow(Cluster2_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]])/length(which(Donnees_Cluster_UP_df_liste[[as.character(Annees[x])]]$ATTAQUES>0))*100
    Nb_PtChaud_pondere[x]= max(Cluster2_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]]$Niveau+1)} else {Taux_C2_UPpred_pondere[x]=0 ; Nb_PtChaud_pondere[x]=1}}
Taux_tot_UPpred_pondere <- Taux_C1_UPpred_pondere + Taux_C2_UPpred_pondere

Taux_C1_UPpred_homogene <- c()
Taux_C2_UPpred_homogene <- c()
Taux_tot_UPpred_homogene <- c()
Nb_PtChaud_homogene <- list()
for (x in 1:length(Annees)) {
  Taux_C1_UPpred_homogene[x]=length(Cluster_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]])/length(which(Donnees_Cluster_UP_df_liste[[as.character(Annees[x])]]$ATTAQUES>0))*100
  if(!is.null(nrow(Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]]))){
    Taux_C2_UPpred_homogene[x]= nrow(Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]])/length(which(Donnees_Cluster_UP_df_liste[[as.character(Annees[x])]]$ATTAQUES>0))*100
    Nb_PtChaud_homogene[x]= max(Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]]$Niveau+1)} else {Taux_C2_UPpred_homogene[x]=0 ; Nb_PtChaud_homogene[x]=1}}
Taux_tot_UPpred_homogene <- Taux_C1_UPpred_homogene + Taux_C2_UPpred_homogene

Results_df <- data.frame(Annees=Annees,
                         Taux_tot_UPpred_pondere=round(Taux_tot_UPpred_pondere, digits = 1),
                         Taux_tot_UPpred_homogene=round(Taux_tot_UPpred_homogene, digits = 1))
Results_df <- gather(data=Results_df, "Type", "Value",-Annees)
Results_df$Type <- factor(Results_df$Type, levels = c("Taux_tot_UPpred_pondere", "Taux_tot_UPpred_homogene"))

Ratio_nb_hotspots_plot <- ggplot(Results_df, aes(x=Annees, y=Value, group=Type, color=factor(Type))) +
  geom_point(size=2.5) +
  geom_xspline(size=0.8) +
  scale_color_manual(values=c(brewer.pal(11,"BrBG")[4],brewer.pal(11,"BrBG")[9]), name="",
                     labels=c("with observed heterogeneous\nsheep availability","with simulated homogeneous\nsheep availability")) +
  scale_x_continuous(breaks=c(seq(from=1996, to=2018, by=2)),labels=as.character(c(seq(from=1996, to=2018, by=2))),expand = c(0.015,0)) +
  labs(y = "Proportion of depredated \npastoral surfaces into hotspots (%)", x = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 9, family="Segoe UI"),
        axis.text.x = element_text(angle=45,vjust=0.6),
        axis.title = element_text(size = 9, face = "italic", family="Segoe UI"),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
        axis.ticks = element_line(colour = 'black', size = 0.1),
        axis.line = element_line(colour = "black", size = 0.1),
        legend.text = element_text(size=9),
        panel.border = element_blank(),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major = element_blank())

############## Number of hotspots #############

Number_Cluster_df <- data.frame(Annees=Annees,
                                pondere=unlist(Nb_PtChaud_pondere), 
                                homogene=unlist(Nb_PtChaud_homogene))
Number_Cluster_df <- gather(data=Number_Cluster_df, "Type", "Value",-Annees)
Number_Cluster_df$Type <- factor(Number_Cluster_df$Type, levels = c("pondere", "homogene"))

Nb_hotspots_plot <- ggplot(Number_Cluster_df, aes(x=Annees, y=Value, fill=factor(Type))) +
  geom_col(alpha=0.8, width = 0.9, position=position_dodge(width=0.6), color="grey40", size=0.3) +
  scale_fill_manual(values=c(brewer.pal(11,"BrBG")[4],brewer.pal(11,"BrBG")[9]), name="",
                    labels=c("with observed heterogeneous\nsheep availability","with simulated homogeneous\nsheep availability")) +
  scale_x_continuous(breaks=c(seq(from=1996, to=2018, by=2)),labels=as.character(c(seq(from=1996, to=2018, by=2))),expand = c(0.015,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Number of hotspots", x = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 9, family="Segoe UI"),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle=45,vjust=0.6),
        axis.title = element_text(size = 9, face = "italic", family="Segoe UI"),
        axis.ticks = element_line(colour = 'black', size = 0.1),
        axis.line = element_line(colour = "black", size = 0.1),
        panel.border = element_blank(),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major = element_blank())
Nb_hotspots_plot

############## Surfaces of PS in hotspots #############

#Pepare surfaces of PS in hotspots for full model
Superficie_PtChaudPrimaire_pondere <- c()
Superficie_PtChaudSecondaire_pondere <- list()

for (x in 1:length(Annees)) { #Primary clusters
  Cluster1_Annee_id=Cluster_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]]
  Cluster1_Annee=Donnees_Cluster_UP_df_liste[[as.character(Annees[x])]][c(Cluster1_Annee_id),]
  Superficie_PtChaudPrimaire_pondere[x]=sum(Cluster1_Annee$SURFACE)
}
for (x in 1:length(Annees)) { #Secondary clusters
  if(!is.null(nrow(Cluster2_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]]))){
    Superficie_PtChaudSecondaire_Annee <- c()
    for (n in 1:max(Cluster2_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]]$Niveau)){
      Cluster2_Niveau_Annee_id=Cluster2_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]][Cluster2_UPpredatees_Effectifpondere_liste[[as.character(Annees[x])]]$Niveau==n,"IDCluster"]
      Cluster2_Niveau_Annee=Donnees_Cluster_UP_df_liste[[as.character(Annees[x])]][c(Cluster2_Niveau_Annee_id),]
      Superficie_PtChaudSecondaire_Annee[n]=sum(Cluster2_Niveau_Annee$SURFACE)}
    Superficie_PtChaudSecondaire_pondere[[x]] <- Superficie_PtChaudSecondaire_Annee}else{Superficie_PtChaudSecondaire_pondere[[x]] <- 0}
}

Superficie_pondere_df <- data.frame(Superficie = unlist(Superficie_PtChaudSecondaire_pondere), 
                                    Annees = as.numeric(rep(Annees[1:length(Superficie_PtChaudSecondaire_pondere)],times = sapply(Superficie_PtChaudSecondaire_pondere,length))))
Superficie_primaire_pondere_df <- data.frame(Annees=Annees,Superficie=round(Superficie_PtChaudPrimaire_pondere, digits = 1))
Superficie_pondere_df <- rbind(Superficie_pondere_df,Superficie_primaire_pondere_df)
Superficie_pondere_df$Model <- "pondere"

Nb_PtChaudSecondaire_pondere_df <- data.frame(Annees=Annees, Nb_PtChaud_pondere=unlist(Nb_PtChaud_pondere))

#Pepare surfaces of PS in hotspots for null model
Superficie_PtChaudPrimaire_homogene <- c()
Superficie_PtChaudSecondaire_homogene <- list()

for (x in 1:length(Annees)) { #Primary clusters
  Cluster1_Annee_id=Cluster_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]]
  Cluster1_Annee=Donnees_Cluster_UP_df_liste[[as.character(Annees[x])]][c(Cluster1_Annee_id),]
  Superficie_PtChaudPrimaire_homogene[x]=sum(Cluster1_Annee$SURFACE)
}
for (x in 1:length(Annees)) { #Secondary clusters
  if(!is.null(nrow(Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]]))){
    Superficie_PtChaudSecondaire_Annee <- c()
    for (n in 1:max(Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]]$Niveau)){
      Cluster2_Niveau_Annee_id=Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]][Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(Annees[x])]]$Niveau==n,"IDCluster"]
      Cluster2_Niveau_Annee=Donnees_Cluster_UP_df_liste[[as.character(Annees[x])]][c(Cluster2_Niveau_Annee_id),]
      Superficie_PtChaudSecondaire_Annee[n]=sum(Cluster2_Niveau_Annee$SURFACE)}
    Superficie_PtChaudSecondaire_homogene[[x]] <- Superficie_PtChaudSecondaire_Annee}else{Superficie_PtChaudSecondaire_homogene[[x]] <- 0}
}

Superficie_homogene_df <- data.frame(Superficie = unlist(Superficie_PtChaudSecondaire_homogene), 
                                     Annees = as.numeric(rep(Annees[1:length(Superficie_PtChaudSecondaire_homogene)],times = sapply(Superficie_PtChaudSecondaire_homogene,length))))
Superficie_primaire_homogene_df <- data.frame(Annees=Annees,Superficie=round(Superficie_PtChaudPrimaire_homogene, digits = 1))
Superficie_homogene_df <- rbind(Superficie_homogene_df,Superficie_primaire_homogene_df)
Superficie_homogene_df$Model <- "homogene"

Nb_PtChaudSecondaire_homogene_df <- data.frame(Annees=Annees, Nb_PtChaud_homogene=unlist(Nb_PtChaud_homogene))

# Merge both model results
Superficie_df <- rbind(Superficie_pondere_df, Superficie_homogene_df)
Superficie_df$Superficie <- Superficie_df$Superficie
Superficie_df$Model <- factor(Superficie_df$Model, levels = c("pondere", "homogene"))
levels(Superficie_df$Model) <- c("Full model", "Null model")

#Plot
Surface_boxplot <- ggplot(data=Superficie_df, aes(x=as.factor(Annees),y=Superficie, fill=Model)) + 
  geom_boxplot(size=0.3, fatten = 0.99, outlier.size = 1) +
  scale_fill_manual(values=c("Null model"=brewer.pal(11,"BrBG")[9],"Full model"=brewer.pal(11,"BrBG")[4]), name="") +
  scale_x_discrete(breaks=c(seq(from=1996, to=2018, by=2)),labels=as.character(c(seq(from=1996, to=2018, by=2))),expand = c(0.015,0)) +
  labs(x = "", y=expression(atop(NA, atop(textstyle(italic('Pastoral surface of')),
                                          textstyle(italic(hotspots) ~ (km^2)))))) +
  theme(axis.text = element_text(size = 9, family="Segoe UI"),
        text = element_text(size = 10),
        axis.text.x = element_text(angle=45,vjust=0.6),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0,
                                                    unit = "mm")),
        axis.title = element_text(size = 9, face = "italic", family="Segoe UI"),
        axis.ticks = element_line(colour = 'black', size = 0.1),
        axis.line = element_line(colour = "black", size = 0.1),
        panel.border = element_blank(),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major = element_blank())
Surface_boxplot

############## Merge subfigures #############

Legende <- get_legend(Ratio_nb_hotspots_plot + 
                        guides(color = guide_legend(nrow = 1)) +
                        theme(legend.position = "bottom",
                              legend.spacing.x = unit(0.6, 'cm')))

grid1 <- plot_grid(Nb_hotspots_plot + theme(legend.position="none"),
                   Ratio_nb_hotspots_plot + theme(legend.position="none"),
                   Surface_boxplot + theme(legend.position="none"),
                   labels = c('A', 'B', 'C'), 
                   label_x = 0.18,
                   label_fontfamily = "Segoe UI",
                   label_fontface = "plain",
                   label_size = 15,
                   rel_heights = c(0.9, .9,1),
                   nrow=3, align='vh')

Figure6 <- plot_grid(grid1, Legende, ncol=1, rel_heights = c(1, .05))

ggsave("Figure6.png", device="png", width=130, height= 165, units=c("mm"))

############################################## Figure 7 ###############################################

x=2017

UP_Loup_annee_sf <- UP_Loup_sf_liste[[as.character(x)]]

# Identify which PU are depredated
UP_Loup_annee_sf$ATTAQUES <- Donnees_Cluster_UP_df_liste[[as.character(x)]]$ATTAQUES
UP_Loup_nonpredatees_annee_sf <- UP_Loup_annee_sf[UP_Loup_annee_sf$ATTAQUES==0,]
UP_Loup_predatees_annee_sf <- UP_Loup_annee_sf[UP_Loup_annee_sf$ATTAQUES>0,]

# Identify which PU are primary or secondary clusters
UP_Loup_annee_sf[c(as.numeric(row.names(UP_Loup_nonpredatees_annee_sf))),"CLUSTER_POND"] <- "Not depredated"
UP_Loup_annee_sf[c(as.numeric(row.names(UP_Loup_nonpredatees_annee_sf))),"CLUSTER_FICTIF"] <- "Not depredated"
UP_Loup_annee_sf[c(as.numeric(row.names(UP_Loup_predatees_annee_sf))),"CLUSTER_POND"] <- "No hotspot"
UP_Loup_annee_sf[c(as.numeric(row.names(UP_Loup_predatees_annee_sf))),"CLUSTER_FICTIF"] <- "No hotspot"
UP_Loup_annee_sf[c(Cluster_UPpredatees_Effectifpondere_liste[[as.character(x)]]),"CLUSTER_POND"] <- "Primary hotspot"
UP_Loup_annee_sf[c(Cluster_UPpredatees_Effectifhomogene_liste[[as.character(x)]]),"CLUSTER_FICTIF"] <- "Primary hotspot"
UP_Loup_annee_sf[c(Cluster2_UPpredatees_Effectifpondere_liste[[as.character(x)]]$IDCluster),"CLUSTER_POND"] <- "Hotspot"
UP_Loup_annee_sf[c(Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(x)]]$IDCluster),"CLUSTER_FICTIF"] <- "Hotspot"

# Create primary cluster circle 
UPCentre_PtChaudPrimaire_pondere <- UP_Loup_annee_sf[Cluster_UPpredatees_Effectifpondere_liste[[as.character(x)]][1],]
Distance_UPCentre_UPExt_pondere <- max(st_distance(UPCentre_PtChaudPrimaire_pondere,
                                                   UP_Loup_annee_sf[UP_Loup_annee_sf$CLUSTER_POND=="Primary hotspot",]))
Cercle_PtChaudPrimaire_pondere <- st_buffer(UPCentre_PtChaudPrimaire_pondere,Distance_UPCentre_UPExt_pondere)
UPCentre_PtChaudPrimaire_homogene <- UP_Loup_annee_sf[Cluster_UPpredatees_Effectifhomogene_liste[[as.character(x)]][1],]
Distance_UPCentre_UPExt_homogene <- max(st_distance(UPCentre_PtChaudPrimaire_homogene,
                                                    UP_Loup_annee_sf[UP_Loup_annee_sf$CLUSTER_FICTIF=="Primary hotspot",]))
Cercle_PtChaudPrimaire_homogene <-st_buffer(UPCentre_PtChaudPrimaire_homogene,Distance_UPCentre_UPExt_homogene)

# Create secondary cluster circles
Cluster2_Annee_pondere <- Cluster2_UPpredatees_Effectifpondere_liste[[as.character(x)]]
Cercles_PtsChaudsSecondaires_pondere_liste <- list()
if(!is.null(Cluster2_Annee_pondere)){
  for (n in 1:max(Cluster2_Annee_pondere$Niveau)){
    UPCentre_PtChaudSecondaire_pondere <- UP_Loup_annee_sf[Cluster2_Annee_pondere[Cluster2_Annee_pondere$Niveau==n,][1,2],]
    Distance_UPCentre_UPExt_pondere <- max(st_distance(UPCentre_PtChaudSecondaire_pondere,
                                                       UP_Loup_annee_sf[Cluster2_Annee_pondere[Cluster2_Annee_pondere$Niveau==n,][,2],]))
    Cercle_PtChaudSecondaire_pondere <- st_buffer(UPCentre_PtChaudSecondaire_pondere,Distance_UPCentre_UPExt_pondere)
    Cercles_PtsChaudsSecondaires_pondere_liste[[n]] <- Cercle_PtChaudSecondaire_pondere
  }
} 
Cluster2_Annee_homogene <- Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(x)]]
Cercles_PtsChaudsSecondaires_homogene_liste <- list()
if(!is.null(Cluster2_Annee_homogene)){
  for (n in 1:max(Cluster2_Annee_homogene$Niveau)){
    UPCentre_PtChaudSecondaire_homogene <- UP_Loup_annee_sf[Cluster2_Annee_homogene[Cluster2_Annee_homogene$Niveau==n,][1,2],]
    Distance_UPCentre_UPExt_homogene <- max(st_distance(UPCentre_PtChaudSecondaire_homogene,
                                                        UP_Loup_annee_sf[Cluster2_Annee_homogene[Cluster2_Annee_homogene$Niveau==n,][,2],]))
    Cercle_PtChaudSecondaire_homogene <- st_buffer(UPCentre_PtChaudSecondaire_homogene,Distance_UPCentre_UPExt_homogene)
    Cercles_PtsChaudsSecondaires_homogene_liste[[n]] <- Cercle_PtChaudSecondaire_homogene
  }
}

# Merge primary and secondy clusters
UP_Loup_annee_sf$CLUSTER_POND <- factor(UP_Loup_annee_sf$CLUSTER_POND, levels = c("Not depredated","No hotspot","Primary hotspot", "Hotspot"))
levels(UP_Loup_annee_sf$CLUSTER_POND) <- c("Not depredated","No hotspot","Hotspot", "Hotspot")
UP_Loup_annee_sf$CLUSTER_FICTIF <- factor(UP_Loup_annee_sf$CLUSTER_FICTIF, levels = c("Not depredated","No hotspot","Primary hotspot", "Hotspot"))
levels(UP_Loup_annee_sf$CLUSTER_FICTIF) <- c("Not depredated","No hotspot","Hotspot", "Hotspot")

# Plots
Couleur_Cluster_pondere = c("Not depredated" = brewer.pal(12,"Set3")[9], "Hotspot" = brewer.pal(11,"BrBG")[2],"No hotspot" = brewer.pal(11,"BrBG")[4])

Figure_Hotspots_pondere <- ggplot() +
  geom_sf(data=Study_area2, fill="white", size=0.01) +
  geom_sf(data=UP_Loup_annee_sf, aes(fill=as.factor(CLUSTER_POND)), color=NA) +
  geom_sf(data=Cercle_PtChaudPrimaire_pondere, fill=NA, color=brewer.pal(11,"BrBG")[2], size=0.3, alpha=0.5) +
  geom_sf(data=do.call(rbind,Cercles_PtsChaudsSecondaires_pondere_liste), fill=NA, 
          color=brewer.pal(11,"BrBG")[2], size=0.3, alpha=0.5) +
  scale_fill_manual(values = Couleur_Cluster_pondere) +
  annotate(geom="text", x=800000, y=6505000, label = bquote(with~"observed heterogenous"), 
           size=2.1, family="Segoe UI", color=brewer.pal(11,"BrBG")[2], fontface=1) +
  annotate(geom="text", x=800000, y=6485000, label = bquote(sheep~availability),
           size=2.1, color=brewer.pal(11,"BrBG")[2], fontface=1, family="Segoe UI", ) +
  ggsn::scalebar(data=Study_area,transform = FALSE,dist = 50,dist_unit = "km", model = "WGS84",
                 st.dist=0.035, st.size=1.5, border.size=0.20,location = "bottomleft",family="Segoe UI") + 
  labs(fill="") +
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), panel.border = element_blank(),
        legend.key.size = unit(0.02,"npc"),
        legend.text = element_text(size=7),
        legend.position = "bottom")

Couleur_Cluster_homogene = c("Not depredated" = brewer.pal(12,"Set3")[9], "Hotspot" = brewer.pal(11,"BrBG")[10],"No hotspot" = brewer.pal(11,"BrBG")[8])

Figure_Hotspots_homogene <-  ggplot() +
  geom_sf(data=Study_area2, fill="white", size=0.01) +
  geom_sf(data=UP_Loup_annee_sf, aes(fill=as.factor(CLUSTER_FICTIF)), color=NA) +
  geom_sf(data=Cercle_PtChaudPrimaire_homogene, fill=NA, color=brewer.pal(11,"BrBG")[10], size=0.3, alpha=0.5) +
  geom_sf(data=do.call(rbind,Cercles_PtsChaudsSecondaires_homogene_liste), fill=NA, 
          color=brewer.pal(11,"BrBG")[10], size=0.3, alpha=0.05) +
  scale_fill_manual(values = Couleur_Cluster_homogene) +
  annotate(geom="text", x=800000, y=6505000, label = bquote(with~"simulated homogenous"), #bold() pour du gras
           size=2.1, color=brewer.pal(11,"BrBG")[10], fontface=1, family="Segoe UI", ) +
  annotate(geom="text", x=800000, y=6485000, label = bquote(sheep~availability),
           size=2.1, color=brewer.pal(11,"BrBG")[10], fontface=1, family="Segoe UI", ) +
  north(data=Study_area,location="topright",scale=0.10,symbol=2) +
  labs(fill="") +
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), panel.border = element_blank(),
        legend.key.size = unit(0.02,"npc"),
        legend.text = element_text(size=7),
        legend.position = "bottom")

Legende1 <- get_legend(Figure_Hotspots_pondere + 
                        guides(color = guide_legend(nrow = 1)) +
                        theme(legend.position = "bottom"))

Legende2 <- get_legend(Figure_Hotspots_homogene + 
                         guides(color = guide_legend(nrow = 1)) +
                         theme(legend.position = "bottom"))

legends <- plot_grid(Legende1, Legende2, nrow=1, align='vh')

grid1 <- plot_grid(Figure_Hotspots_pondere + theme(legend.position="none"),
                   Figure_Hotspots_homogene + theme(legend.position="none"),
                     labels = c('A', 'B'), label_fontfamily = "Segoe UI",label_fontface="plain",
                     label_size = 10, label_x = 0.1, nrow=1, align='vh')

Figure7 <- plot_grid(grid1, legends,
                     nrow=2, align='vh', rel_heights = c(1, .10))

ggsave("Figure7.png", device="png", width=210, height= 120, units=c("mm"))

###################################### Supplementary Figure 1 #########################################
############## Number of sheep ############## 

Effectifs_classes <- cut(UP1996_sf$EFF_OV, 
                         breaks = c(0, 100, 200, 500, 700, 1000, 2500, +Inf), 
                         labels = c("<100", "100-199", "200-499", "500-699", "700-999", "1000-2499",">2500"), 
                         right = FALSE)

UP1996_sf$Eff_Class <- Effectifs_classes

Figure_Effectifs <-   ggplot() +
  geom_sf(data=Study_area2, fill="white", size=0.01) +
  geom_sf(data=Mercantour_tot,fill="white",color="black",size=0.3) +
  geom_sf(data=hatch(Mercantour_tot,density = 15),size=0.3) +
  geom_sf(data=UP1996_sf, aes(fill=Eff_Class), color=NA) +
  scale_fill_brewer(palette="Greens") +
  labs(fill="Number of \nsheep") +
  ggsn::scalebar(data=Study_area,transform = FALSE,dist = 50,dist_unit = "km", model = "WGS84",
                 st.dist=0.035, st.size=2, border.size=0.20,location = "bottomleft",family="Segoe UI") + 
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), panel.border = element_blank(),
        text = element_text(family="Segoe UI"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=9),
        legend.position = "right") 
Figure_Effectifs

############## Grazing time ############## 

Figure_Duree <-   ggplot() +
  geom_sf(data=Study_area2, fill="white", size=0.01) +
  geom_sf(data=Mercantour_tot,fill="white",color="black",size=0.3) +
  geom_sf(data=hatch(Mercantour_tot,density = 15),size=0.3) +
  geom_sf(data=UP1996_sf, aes(fill=DUREE_PAT), color=NA) +
  scale_fill_gradient(low=brewer.pal(9, "Blues")[1],high=brewer.pal(9,"Blues")[9]) +
  north(data=Study_area,location="topright",scale=0.125,symbol=2) +
  labs(fill="Sheep grazing \ntime (days)") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), panel.border = element_blank(),
        text = element_text(family="Segoe UI"),
        legend.text = element_text(size=7),
        legend.title = element_text(size=9),
        legend.spacing.x = unit(0.5, 'cm')) 
Figure_Duree

FinalSuppFigure <- plot_grid(Figure_Effectifs, Figure_Duree, labels = c('A', 'B'), nrow = 2, label_fontfamily = "Segoe UI",
                         label_size = 15, scale=0.9)

ggsave("SuppFig1.png", device="png", width=210, height= 297, units=c("mm"))

###################################### Supplementary Figure 2 #########################################

setwd("C:/Users/grente/Documents/Etudes/Points chauds/Dossier revu/Data")

#### Regional scale

#Prepare wolf distribution within study area data
Maillage_Annee_fonction <- function(x){
  Maillage_Annee_sf_liste <- list()
  Maillage_Annee_sf_liste[[x]] <- Maillage_Loup_sf_liste[[as.character(x)]][c(as.data.frame(st_intersects(Maillage_Loup_sf_liste[[as.character(x)]],Study_area2))$row.id),]}
Maillage_Annee_sf_liste <- lapply(1995:2018, function(x) Maillage_Annee_fonction(x)) 
names(Maillage_Annee_sf_liste) <- c(1995:2018)
save(Maillage_Annee_sf_liste,file="Maillage_Annee_sf_liste.RData")

#Prepare all PS data
UP_Annee1 <- rep(list(UP1996_sf),length(1995:2005))
UP_Annee2 <- rep(list(UP2012_sf),length(2006:2018))
UP_Annee <- c(UP_Annee1,UP_Annee2)
names(UP_Annee) <- c(1995:2018)
save(UP_Annee,file="UP_Annee.RData")

#Prepare PS under rik data
UP_risk_fonction_1 <- function(x){
  UP_risk_sf_liste_1 <- list()
  UP_risk_sf_liste_1 <- UP1996_sf[c(as.data.frame(st_intersects(UP1996_sf,Maillage_Annee_sf_liste[[as.character(x)]]))$row.id),]
  if(length(which(duplicated(UP_risk_sf_liste_1)))>0){
    UP_risk_sf_liste_1 <- UP_risk_sf_liste_1[-c(which(duplicated(UP_risk_sf_liste_1))),]}
  UP_risk_sf_liste_1[[x]] <- UP_risk_sf_liste_1}
UP_risk_sf_liste_1 <- lapply(1995:2005, function(x) UP_risk_fonction_1(x)) 
names(UP_risk_sf_liste_1) <- c(1995:2005)
save(UP_risk_sf_liste_1,file="UP_risk_sf_liste_1.RData")

UP_risk_fonction_2 <- function(x){
  UP_risk_sf_liste_2 <- list()
  UP_risk_sf_liste_2 <- UP2012_sf[c(as.data.frame(st_intersects(UP2012_sf,Maillage_Annee_sf_liste[[as.character(x)]]))$row.id),]
  if(length(which(duplicated(UP_risk_sf_liste_2)))>0){
    UP_risk_sf_liste_2 <- UP_risk_sf_liste_2[-c(which(duplicated(UP_risk_sf_liste_2))),]}
  UP_risk_sf_liste_2[[x]] <- UP_risk_sf_liste_2}
UP_risk_sf_liste_2 <- lapply(2006:2010, function(x) UP_risk_fonction_2(x)) 
names(UP_risk_sf_liste_2) <- c(2006:2010)
save(UP_risk_sf_liste_2,file="UP_risk_sf_liste_2.RData")

UP_risk_fonction_3 <- function(x){
  UP_risk_sf_liste_3 <- list()
  UP_risk_sf_liste_3 <- UP2012_sf[c(as.data.frame(st_intersects(UP2012_sf,Maillage_Annee_sf_liste[[as.character(x)]]))$row.id),]
  if(length(which(duplicated(UP_risk_sf_liste_3)))>0){
    UP_risk_sf_liste_3 <- UP_risk_sf_liste_3[-c(which(duplicated(UP_risk_sf_liste_3))),]}
  UP_risk_sf_liste_3[[x]] <- UP_risk_sf_liste_3}
UP_risk_sf_liste_3 <- lapply(2011:2015, function(x) UP_risk_fonction_3(x)) 
names(UP_risk_sf_liste_3) <- c(2011:2015)
save(UP_risk_sf_liste_3,file="UP_risk_sf_liste_3.RData")

UP_risk_fonction_4 <- function(x){
  UP_risk_sf_liste_4 <- list()
  UP_risk_sf_liste_4 <- UP2012_sf[c(as.data.frame(st_intersects(UP2012_sf,Maillage_Annee_sf_liste[[as.character(x)]]))$row.id),]
  if(length(which(duplicated(UP_risk_sf_liste_4)))>0){
    UP_risk_sf_liste_4 <- UP_risk_sf_liste_4[-c(which(duplicated(UP_risk_sf_liste_4))),]}
  UP_risk_sf_liste_4[[x]] <- UP_risk_sf_liste_4}
UP_risk_sf_liste_4 <- lapply(2016:2018, function(x) UP_risk_fonction_4(x)) 
names(UP_risk_sf_liste_4) <- c(2016:2018)
save(UP_risk_sf_liste_4,file="UP_risk_sf_liste_4.RData")

UP_risk_sf_liste <- c(UP_risk_sf_liste_1,UP_risk_sf_liste_2,UP_risk_sf_liste_3,UP_risk_sf_liste_4)

#Prepare depredated PS data
PSatt_Annee_fonction <- function(x){
  PSatt_Annee_sf_liste <- list()
  PSatt_Annee_sf_liste[[x]] <- UP_risk_sf_liste[[as.character(x)]][c(as.data.frame(st_intersects(UP_risk_sf_liste[[as.character(x)]],Attaques_sf_liste[[as.character(x)]]))$row.id),]}
PSatt_Annee_sf_liste <- lapply(1995:2018, function(x) PSatt_Annee_fonction(x)) 
names(PSatt_Annee_sf_liste) <- c(1995:2018)
save(PSatt_Annee_sf_liste,file="PSatt_Annee_sf_liste.RData")

#### Local scale

#Prepare wolf distribution within study area data
MaillageM_Annee_fonction <- function(x){
  MaillageM_Annee_sf_liste <- list()
  MaillageM_Annee_sf_liste[[x]] <- Maillage_Loup_sf_liste[[as.character(x)]][c(as.data.frame(st_intersects(Maillage_Loup_sf_liste[[as.character(x)]],Mercantour_sf))$row.id),]}
MaillageM_Annee_sf_liste <- lapply(1995:2018, function(x) MaillageM_Annee_fonction(x)) 
names(MaillageM_Annee_sf_liste) <- c(1995:2018)
save(MaillageM_Annee_sf_liste,file="MaillageM_Annee_sf_liste.RData")

#Prepare all PS data
UP_M_Annee1 <- rep(list(UP1996_Mercantour_sf),length(1995:2005))
UP_M_Annee2 <- rep(list(UP2012_Mercantour_sf),length(2006:2018))
UP_M_Annee <- c(UP_M_Annee1,UP_M_Annee2)
names(UP_M_Annee) <- c(1995:2018)

#Prepare PS under rik data
UP_M_risk_fonction <- function(x){
  UP_M_risk_sf_liste <- list()
  UP_M_risk_sf_liste <- UP_M_Annee[[as.character(x)]][c(as.data.frame(st_intersects(UP_M_Annee[[as.character(x)]],MaillageM_Annee_sf_liste[[as.character(x)]]))$row.id),]
  if(length(which(duplicated(UP_M_risk_sf_liste)))>0){
    UP_M_risk_sf_liste <- UP_M_risk_sf_liste[-c(which(duplicated(UP_M_risk_sf_liste))),]}
  UP_M_risk_sf_liste[[x]] <- UP_M_risk_sf_liste}
UP_M_risk_sf_liste <- lapply(1995:2018, function(x) UP_M_risk_fonction(x)) 
names(UP_M_risk_sf_liste) <- c(1995:2018)
save(UP_M_risk_sf_liste,file="UP_M_risk_sf_liste.RData")

#Prepare depredated PS data
PSatt_M_Annee_fonction <- function(x){
  PSatt_M_Annee_sf_liste <- list()
  PSatt_M_Annee_sf_liste[[x]] <- UP_M_risk_sf_liste[[as.character(x)]][c(as.data.frame(st_intersects(UP_M_risk_sf_liste[[as.character(x)]],Attaques_sf_liste[[as.character(x)]]))$row.id),]}
PSatt_M_Annee_sf_liste <- lapply(1995:2018, function(x) PSatt_M_Annee_fonction(x)) 
names(PSatt_M_Annee_sf_liste) <- c(1995:2018)
save(PSatt_M_Annee_sf_liste,file="PSatt_M_Annee_sf_liste.RData")

###################################### Supplementary Figure 4 #########################################

setwd("C:/Users/grente/Documents/Etudes/Points chauds/Dossier revu/Data")

#General setup
Annees <- c(1995:2018)

# #Prepare data
Modele <- list(Full_model_Kinhom_liste,Null_model_K_liste,Null2_model_K_liste)
names(Modele) <- c("Full model","K null model","K null model 2")

Resultats_Ripley_df_fonction <- function(x) {
  Resultats_Ripley_df_liste <- list()
  Resultats_Ripley_Annee_df_liste <- list()
  for(m in names(Modele)){
    Resultats_Ripley_Annee_df_liste[[m]] <- data.frame(x=Modele[[m]][[as.character(x)]]$r/1000,
                                                       y=Modele[[m]][[as.character(x)]]$obs/10e+8,
                                                       lower=Modele[[m]][[as.character(x)]]$lo/10e+8,
                                                       upper=Modele[[m]][[as.character(x)]]$hi/10e+8,
                                                       model=as.character(m))}
  Resultats_Ripley_Annee_df <- do.call("rbind", Resultats_Ripley_Annee_df_liste)
  Resultats_Ripley_df_liste[[x]] <- Resultats_Ripley_Annee_df}
Resultats_Ripley_df_liste <- lapply(Annees, function(x) Resultats_Ripley_df_fonction(x))
names(Resultats_Ripley_df_liste) <- c(Annees)
save(Resultats_Ripley_df_liste,file="~/Etudes/Points chauds/Dossier revu/Data/Resultats_Ripley_df_liste.RData")

Resultats_Ripley_Mercantour_df_fonction <- function(x) {
  Resultats_Ripley_Annee_Mercantour_df_liste <- list()
  Resultats_Ripley_Annee_Mercantour_df_liste[[x]] <- data.frame(x=Full_model_Kinhom_Mercantour_liste[[as.character(x)]]$r/1000,
                                                                y=Full_model_Kinhom_Mercantour_liste[[as.character(x)]]$obs/10e+8,
                                                                lower=Full_model_Kinhom_Mercantour_liste[[as.character(x)]]$lo/10e+8,
                                                                upper=Full_model_Kinhom_Mercantour_liste[[as.character(x)]]$hi/10e+8)}
Resultats_Ripley_Annee_Mercantour_df_liste <- lapply(Annees, function(x) Resultats_Ripley_Mercantour_df_fonction(x))
names(Resultats_Ripley_Annee_Mercantour_df_liste) <- c(Annees)
save(Resultats_Ripley_Annee_Mercantour_df_liste,file="~/Etudes/Points chauds/Dossier revu/Data/Resultats_Ripley_Annee_Mercantour_df_liste.RData")

###################################### Supplementary Figure 5 #########################################

#Prepare data of UP
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
  UP_Loup_annee_sf[c(Cluster_UPpredatees_Effectifpondere_liste[[as.character(x)]]),"CLUSTER_POND"] <- "Primary hotspot"
  UP_Loup_annee_sf[c(Cluster_UPpredatees_Effectifhomogene_liste[[as.character(x)]]),"CLUSTER_FICTIF"] <- "Primary hotspot"
  UP_Loup_annee_sf[c(Cluster2_UPpredatees_Effectifpondere_liste[[as.character(x)]]$IDCluster),"CLUSTER_POND"] <- "Hotspot"
  UP_Loup_annee_sf[c(Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(x)]]$IDCluster),"CLUSTER_FICTIF"] <- "Hotspot"
  Results_UPhotspots_sf_liste[[x]] <- UP_Loup_annee_sf} 
Results_UPhotspots_sf_liste <- lapply(Annees, function(x) Results_UPhotspots_sf_fonction(x))
names(Results_UPhotspots_sf_liste) <- Annees

#Prepare data of primary cluster circle for heterogeneous sheep availability
Results_PrimaryCluster_hetero_sf_fonction <- function(x){
  Results_PrimaryCluster_hetero_sf_liste <- list()
  UP_Loup_annee_sf <- Results_UPhotspots_sf_liste[[as.character(x)]]
  UPCentre_PtChaudPrimaire_pondere <- UP_Loup_annee_sf[Cluster_UPpredatees_Effectifpondere_liste[[as.character(x)]][1],]
  Distance_UPCentre_UPExt_pondere <- max(st_distance(UPCentre_PtChaudPrimaire_pondere,
                                                     UP_Loup_annee_sf[UP_Loup_annee_sf$CLUSTER_POND=="Primary hotspot",]))
  
  Cercle_PtChaudPrimaire_pondere <- st_buffer(UPCentre_PtChaudPrimaire_pondere,Distance_UPCentre_UPExt_pondere)
  Results_PrimaryCluster_hetero_sf_liste[[x]] <- Cercle_PtChaudPrimaire_pondere} 
Results_PrimaryCluster_hetero_sf_liste <- lapply(Annees, function(x) Results_PrimaryCluster_hetero_sf_fonction(x))
names(Results_PrimaryCluster_hetero_sf_liste) <- Annees
save(Results_PrimaryCluster_hetero_sf_liste, file="~/Etudes/Points chauds/Dossier revu/Data/Results_PrimaryCluster_hetero_sf_liste.RData")

#Prepare data of primary cluster circle for homogeneous sheep availability
Results_PrimaryCluster_homo_sf_fonction <- function(x){
  Results_PrimaryCluster_homo_sf_liste <- list()
  UP_Loup_annee_sf <- Results_UPhotspots_sf_liste[[as.character(x)]]
  UPCentre_PtChaudPrimaire_homogene <- UP_Loup_annee_sf[Cluster_UPpredatees_Effectifhomogene_liste[[as.character(x)]][1],]
  Distance_UPCentre_UPExt_homogene <- max(st_distance(UPCentre_PtChaudPrimaire_homogene,
                                                      UP_Loup_annee_sf[UP_Loup_annee_sf$CLUSTER_FICTIF=="Primary hotspot",]))
  Cercle_PtChaudPrimaire_homogene <-st_buffer(UPCentre_PtChaudPrimaire_homogene,Distance_UPCentre_UPExt_homogene)
  Results_PrimaryCluster_homo_sf_liste[[x]] <- Cercle_PtChaudPrimaire_homogene} 
Results_PrimaryCluster_homo_sf_liste <- lapply(Annees, function(x) Results_PrimaryCluster_homo_sf_fonction(x))
names(Results_PrimaryCluster_homo_sf_liste) <- Annees
save(Results_PrimaryCluster_homo_sf_liste, file="~/Etudes/Points chauds/Dossier revu/Data/Results_PrimaryCluster_homo_sf_liste.RData")

#Prepare data of secondary cluster circles for heterogeneous sheep availability
Results_SecondaryCluster_hetero_sf_fonction <- function(x){
  Results_SecondaryCluster_hetero_sf_liste <- list()
  Cluster2_Annee_pondere <- Cluster2_UPpredatees_Effectifpondere_liste[[as.character(x)]]
  UP_Loup_annee_sf <- Results_UPhotspots_sf_liste[[as.character(x)]]
  Cercles_PtsChaudsSecondaires_pondere_liste <- list()
  if(!is.null(Cluster2_Annee_pondere)){
    for (n in 1:max(Cluster2_Annee_pondere$Niveau)){
      if(nrow(Cluster2_Annee_pondere[Cluster2_Annee_pondere$Niveau==n,])>1){
        UPCentre_PtChaudSecondaire_pondere <- UP_Loup_annee_sf[Cluster2_Annee_pondere[Cluster2_Annee_pondere$Niveau==n,][1,2],]
        Distance_UPCentre_UPExt_pondere <- max(st_distance(UPCentre_PtChaudSecondaire_pondere,
                                                           UP_Loup_annee_sf[Cluster2_Annee_pondere[Cluster2_Annee_pondere$Niveau==n,][,2],]))
        if(as.numeric(Distance_UPCentre_UPExt_pondere)==0){
          Cercle_PtChaudSecondaire_pondere <- st_buffer(UPCentre_PtChaudSecondaire_pondere, sqrt(st_area(st_union(UP_Loup_annee_sf[Cluster2_Annee_pondere[Cluster2_Annee_pondere$Niveau==n,][,2],]))/pi))}
        else{
          Cercle_PtChaudSecondaire_pondere <- st_buffer(UPCentre_PtChaudSecondaire_pondere,Distance_UPCentre_UPExt_pondere)}
        Cercles_PtsChaudsSecondaires_pondere_liste[[n]] <- Cercle_PtChaudSecondaire_pondere}}
    if(nrow(Cluster2_Annee_pondere)!=1 & length(Cercles_PtsChaudsSecondaires_pondere_liste)!=0){
      Cercles_PtsChaudsSecondaires_pondere_liste[sapply(Cercles_PtsChaudsSecondaires_pondere_liste, is.null)] <- NULL}}
  Results_SecondaryCluster_hetero_sf_liste[[x]] <- Cercles_PtsChaudsSecondaires_pondere_liste} 
Results_SecondaryCluster_hetero_sf_liste <- lapply(Annees, function(x) Results_SecondaryCluster_hetero_sf_fonction(x))
names(Results_SecondaryCluster_hetero_sf_liste) <- Annees
save(Results_SecondaryCluster_hetero_sf_liste, file="~/Etudes/Points chauds/Dossier revu/Data/Results_SecondaryCluster_hetero_sf_liste.RData")

#Prepare data of secondary cluster circles for homogeneous sheep availability
Results_SecondaryCluster_homo_sf_fonction <- function(x){
  Results_SecondaryCluster_homo_sf_liste <- list()
  Cluster2_Annee_homogene <- Cluster2_UPpredatees_Effectifhomogene_liste[[as.character(x)]]
  UP_Loup_annee_sf <- Results_UPhotspots_sf_liste[[as.character(x)]]
  Cercles_PtsChaudsSecondaires_homogene_liste <- list()
  if(!is.null(Cluster2_Annee_homogene)){
    for (n in 1:max(Cluster2_Annee_homogene$Niveau)){
      if(nrow(Cluster2_Annee_homogene[Cluster2_Annee_homogene$Niveau==n,])>1){
        UPCentre_PtChaudSecondaire_homogene <- UP_Loup_annee_sf[Cluster2_Annee_homogene[Cluster2_Annee_homogene$Niveau==n,][1,2],]
        Distance_UPCentre_UPExt_homogene <- max(st_distance(UPCentre_PtChaudSecondaire_homogene,                                                          UP_Loup_annee_sf[Cluster2_Annee_homogene[Cluster2_Annee_homogene$Niveau==n,][,2],]))
        if(as.numeric(Distance_UPCentre_UPExt_homogene)==0){
          Cercle_PtChaudSecondaire_homogene <- st_buffer(UPCentre_PtChaudSecondaire_homogene, sqrt(st_area(st_union(UP_Loup_annee_sf[Cluster2_Annee_homogene[Cluster2_Annee_homogene$Niveau==n,][,2],]))/pi))}
        else{
          Cercle_PtChaudSecondaire_homogene <- st_buffer(UPCentre_PtChaudSecondaire_homogene,Distance_UPCentre_UPExt_homogene)}
        Cercles_PtsChaudsSecondaires_homogene_liste[[n]] <- Cercle_PtChaudSecondaire_homogene}}
    if(nrow(Cluster2_Annee_homogene)!=1 & length(Cercles_PtsChaudsSecondaires_homogene_liste)!=0){
      Cercles_PtsChaudsSecondaires_homogene_liste[sapply(Cercles_PtsChaudsSecondaires_homogene_liste, is.null)] <- NULL}}
  Results_SecondaryCluster_homo_sf_liste[[x]] <- Cercles_PtsChaudsSecondaires_homogene_liste} 
Results_SecondaryCluster_homo_sf_liste <- lapply(Annees, function(x) Results_SecondaryCluster_homo_sf_fonction(x))
names(Results_SecondaryCluster_homo_sf_liste) <- Annees
save(Results_SecondaryCluster_homo_sf_liste, file="~/Etudes/Points chauds/Dossier revu/Data/Results_SecondaryCluster_homo_sf_liste.RData")

#Merge primary and secondary PS hotspots into one single column

Results_UPhotspots2_sf_fonction <- function(x){
  Results_UPhotspots2_sf_liste <- list()
  UP_Loup_annee_sf <- Results_UPhotspots_sf_liste[[as.character(x)]]
  UP_Loup_annee_sf$CLUSTER_POND <- factor(UP_Loup_annee_sf$CLUSTER_POND, levels = c("No depredated","No hotspot","Primary hotspot", "Hotspot"))
  levels(UP_Loup_annee_sf$CLUSTER_POND) <- c("No depredated","No hotspot","Hotspot", "Hotspot")
  UP_Loup_annee_sf$CLUSTER_FICTIF <- factor(UP_Loup_annee_sf$CLUSTER_FICTIF, levels = c("No depredated","No hotspot","Primary hotspot", "Hotspot"))
  levels(UP_Loup_annee_sf$CLUSTER_FICTIF) <- c("No depredated","No hotspot","Hotspot", "Hotspot")
  Results_UPhotspots2_sf_liste[[x]] <- UP_Loup_annee_sf}

Results_UPhotspots2_sf_liste_1 <- lapply(1995:2005, function(x) Results_UPhotspots2_sf_fonction(x))
names(Results_UPhotspots2_sf_liste_1) <- 1995:2005
save(Results_UPhotspots2_sf_liste_1, file="~/Etudes/Points chauds/Dossier revu/Data/Results_UPhotspots2_sf_liste_1.RData")

Results_UPhotspots2_sf_liste_2 <- lapply(2006:2010, function(x) Results_UPhotspots2_sf_fonction(x))
names(Results_UPhotspots2_sf_liste_2) <- 2006:2010
save(Results_UPhotspots2_sf_liste_2, file="~/Etudes/Points chauds/Dossier revu/Data/Results_UPhotspots2_sf_liste_2.RData")

Results_UPhotspots2_sf_liste_3 <- lapply(2011:2014, function(x) Results_UPhotspots2_sf_fonction(x))
names(Results_UPhotspots2_sf_liste_3) <- 2011:2014
save(Results_UPhotspots2_sf_liste_3, file="~/Etudes/Points chauds/Dossier revu/Data/Results_UPhotspots2_sf_liste_3.RData")

Results_UPhotspots2_sf_liste_4 <- lapply(2015:2018, function(x) Results_UPhotspots2_sf_fonction(x))
names(Results_UPhotspots2_sf_liste_4) <- 2015:2018
save(Results_UPhotspots2_sf_liste_4, file="~/Etudes/Points chauds/Dossier revu/Data/Results_UPhotspots2_sf_liste_4.RData")
