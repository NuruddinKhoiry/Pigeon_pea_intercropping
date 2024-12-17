# This R script is written by Ahmad Nuruddin Khoiri 
# Email: nuruddinkhoiri34@gmail.com

# This R script is a part of the manuscript entitled 
# "Pigeon pea-mediated soil microbial shifts improve agroecosystem multifunctionality in long-term maize–palisade grass intercropping"

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

## Load data
data <- readRDS("data_ori.rds")

## scale data
Zdata <- data
Zdata[ ,-1] <- scale(Zdata[ , -1])
Zdata <- as.data.frame(Zdata, check.names = F)


#####################################################
#####################################################
## Functional proxies

# Soil physical (SoilPHY)
Zdata$SoilPHY <- rowMeans(Zdata[,c(34:53)])

# Physical fractionation of organic matter (%) (SoilPOM)
Zdata$SoilPOM <- rowMeans(Zdata[,c(54:59)])

# Soil chemical (SoilCHE)
Zdata$SoilCHE <- rowMeans(Zdata[,c(66:87)])

# Soil microbial diversity (SoilDIV)
Zdata$SoilDIV <- rowMeans(Zdata[,c(143:146)])

# Plant roots (Roots)
Zdata$Roots <- rowMeans(Zdata[,c(60:65)])

# Pasture in the OFF-SEASON - oat + palisade grass - chemical composition (nutritional quality) (Pasture)
Zdata$Pasture <- rowMeans(Zdata[,c(88:95)])

# Straw production and nutrient accumulation (Straw)
Zdata$Straw <- rowMeans(Zdata[,c(96:107)])

# Soybean crop season (SoybeanPROD)
Zdata$SoybeanPROD <- rowMeans(Zdata[,c(2:3)])

#Silage dry matter yield_season (maize, pigeon pea, palisade grass) (SilagePROD)
Zdata$SilagePROD <- rowMeans(Zdata[,c(113:115)])

# Macronutrient accumulation in silage residues (SilageRES)
Zdata$SilageRES <- rowMeans(Zdata[,c(116:121)])

# Leaf nutrient contents in soybean (SoybeanLEAF)
Zdata$SoybeanLEAF <- rowMeans(Zdata[,c(4:14)])

# Leaf nutrient contents in maize (MaizeLEAF)
Zdata$MaizeLEAF <- rowMeans(Zdata[,c(122:132)])

# Fatty acid profile in lamb meat (LMeatFAT)
Zdata$LMeatFAT <- rowMeans(Zdata[,c(15:33)])

# Chemical composition and nutritional evaluation of lamb meat (LMeatNUT)
Zdata$LMeatNUT <- rowMeans(Zdata[,c(133:135)])

# Color of meat (LMeatCOL)
Zdata$LMeatCOL <- rowMeans(Zdata[,c(136:138)])

# Color of fat (LMeatFATCOL)
Zdata$LMeatFATCOL <- rowMeans(Zdata[,c(139:141)])

# Warner-Bratzler shear force (WBS)
Zdata$WBS <- Zdata[,c(142)]

# Greenhouse gases (GHGs)
Zdata$GHG <- rowMeans(Zdata[,c(108:110)])

# Microclimate (Microclimate)
Zdata$Microclimate <- rowMeans(Zdata[,c(111:112)])


#####################################################
#####################################################
## Invert undesirable proxies

# GHG --> GHG mitigation
Zdata$GHG_MIT <- Zdata$GHG * -1

# Microclimate --> Climate protection
Zdata$MicroclimatePRO <- Zdata$Microclimate * -1

#####################################################
#####################################################
## scale Variables, Proxies and Functions between 0 and 1 to ease readability
for (i in colnames(Zdata[,2:ncol(Zdata)])) {
    Zdata[,i] = Zdata[,i] + abs(min(Zdata[,i], na.rm = T))
    Zdata[,i] = Zdata[,i] / max(Zdata[,i], na.rm = T)
}


#####################################################
#####################################################
## calculate agroecosystem goods
# Soil health
SoilHEALTH <- c("SoilPHY","SoilCHE","SoilPOM", "SoilDIV")

# Plant productivity
PlantPROD <- c("Roots","Pasture","Straw","SoybeanPROD","SilagePROD","SoybeanLEAF","MaizeLEAF","SilageRES")

# Lamb meat productivity
LambMeatPROD <- c("LMeatFAT","LMeatNUT","LMeatCOL","LMeatFATCOL","WBS")

# Climate protection
ClimPRO <- c("GHG_MIT","MicroclimatePRO")

## Compute final Goods with equal weight of the included functions

Zdata$SoilHEALTH <- rowSums(t(t((Zdata[,SoilHEALTH])) * rep(1/length(SoilHEALTH), length(SoilHEALTH))))

Zdata$PlantPROD <- rowSums(t(t((Zdata[,PlantPROD])) * rep(1/length(PlantPROD), length(PlantPROD))))

Zdata$LambMeatPROD <- rowSums(t(t((Zdata[,LambMeatPROD])) * rep(1/length(LambMeatPROD), length(LambMeatPROD))))

Zdata$ClimPRO <- rowSums(t(t((Zdata[,ClimPRO])) * rep(1/length(ClimPRO), length(ClimPRO))))


#####################################################
#####################################################
## Calculate final EMF index
Zdata$EMFecosystem  =  rowMeans(Zdata[,c("SoilHEALTH", "ClimPRO", "PlantPROD", "LambMeatPROD")])

#####################################################
#####################################################
## Plot results

## Fig. 6
Zdata_fig6 <- Zdata[,c("Treatment","SoilHEALTH", "PlantPROD", "LambMeatPROD", "ClimPRO", "EMFecosystem")] %>%
    reshape2::melt() %>%
    mutate(variable=gsub("SoilHEALTH","Soil Health", variable)) %>%
    mutate(variable=gsub("PlantPROD","Plant Productivity", variable)) %>%
    mutate(variable=gsub("LambMeatPROD","Meat Productivity", variable)) %>%
    mutate(variable=gsub("ClimPRO","Climate Protection", variable)) %>%
    mutate(variable=gsub("EMFecosystem","EMF", variable))

Zdata_fig6$variable <- factor(Zdata_fig6$variable, levels = c("Soil Health","Plant Productivity", "Meat Productivity", "Climate Protection","EMF"))

Zdata_fig6 %>%
    ggplot(aes(x = Treatment, y = value, color = Treatment)) +
    geom_boxplot(aes(color = Treatment)) +
    geom_jitter() +
    stat_compare_means(method = 't.test', show.legend = F) +
    scale_color_manual(values = c("orange","#298c8c")) +
    facet_wrap(~variable, scales = 'free') +
    theme_light() +
    theme(axis.title = element_blank(), legend.position = 'none')


## Supplementary Fig. 4
list_proxies = c("SoilDIV","SoilPHY","SoilCHE","SoilPOM", 
                 "GHG","Microclimate",
                 "Roots","Pasture","Straw","SoybeanPROD","SilagePROD","SoybeanLEAF","MaizeLEAF","SilageRES",
                 "LMeatFAT","LMeatNUT","LMeatCOL","LMeatFATCOL","WBS"
                )

Zdata_sfig4 <- Zdata[,c("Treatment",list_proxies)] %>% reshape2::melt() 

Zdata_sfig4 %>%
    ggplot(aes(x = Treatment, y = value, fill = Treatment)) +
    geom_boxplot(aes(fill = Treatment)) +
    stat_compare_means(method = 't.test', show.legend = F) +
    scale_fill_manual(values = c("orange","#298c8c")) +
    facet_wrap(~variable, scales = 'free') +
    theme(axis.title = element_blank(), legend.position = 'none')