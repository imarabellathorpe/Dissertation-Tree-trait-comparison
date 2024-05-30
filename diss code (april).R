###################################
#combined dataset code 
#' To DO list:
#' clean data 
#' ---- what to do with C130 data that wasn't actually taken at 130, e.g. 90 cm instead? 
#' ---- should i remove small kigelia as it could be outlier in BTratio vs diameter?
#' - cite r packages: https://ropensci.org/blog/2021/11/16/how-to-cite-r-and-r-packages/ citation("...")

#' - Trait Ideas: 
#' ---- refine trait list - for methods :) 

#' - PCA and analysis:
#' ---- 2 x analysis 
#' ---- hierarchical clusetering /kmedoids 

#' - Stats ideas... 
#' ---- pairwise? 
#' 
#' - 1. fix traits in matrix 
#' - 2. PCA trial - standardise variables - weighting...  
#' - 3. 2 types of analsis 
#' - 4. missMDA 
#' - 5. individuals within species analysis? 
#' - 6. create p values with dimdesc() = currently not working... ? 
#' - 7. is missMDA bias? check for this! 
#' - 8. Regressions and stats from PCA 
#' - 9. watch vid: interpreting PCA output: http://www.sthda.com/english/articles/21-courses/65-principal-component-analysis-course-using-factominer/
version$version.string
#' - PROBLEMS:
#' ---- res.desc <- dimdesc(pca_traits1, axes = 1:3, proba = 0.05) =## WARNING - non convenient data ??
#' ---- make bark 140 and 40 one trait? 
#' ---- k-means, number of centers, need to do silhouette analysis or elbow method? 
#' 


# 1. set up ---------------------------------------------------------------
library(tidyverse)
library(vegan)
install.packages("readxl")
library(readxl)

# 2. read in data sets ----------------------------------------------------
#upload excel 
cb <-  read_excel("~/Desktop/Tree species excel ben.xlsx", 
                  sheet = "combined", na = c("na"))

# 3. cleaning -------------------------------------------------------------
#make characters into factors - not sure why... 
cb <- cb %>% mutate_if(is.character, as.factor)
summary(cb)
# check spp names
table(cb$species)

# 4. Trait calculations ---------------------------------------------------

# 4.1 e.g. canopy diameter:stem diameter - not sure how this is normally calculated, might be log(DBH)/log(Canopy width)?
# canopy width
cb$canopy_diam_m = (cb$canopy1 +cb$canopy2)/2 # mean
hist(cb$canopy_diam_m)
summary(cb$canopy_diam_m)

cb %>% ggplot(aes(y = canopy_diam_m))+
  geom_boxplot() + facet_wrap(~species)

# stem diameter 130 
summary(cb$C130)
cb$diam_130_cm <- as.numeric(cb$C130)/pi
hist(cb$diam_130_cm)

cb %>% ggplot(aes(y = diam_130_cm))+
  geom_boxplot() + facet_wrap(~species)

# stem diameter 40 
summary(cb$C40)
cb$diam_40_cm <- as.numeric(cb$C40)/pi
hist(cb$diam_40_cm)

cb %>% ggplot(aes(y = diam_40_cm))+
  geom_boxplot() + facet_wrap(~species)

#stem diameter 10 
summary(cb$C10)
cb$diam_10_cm <- as.numeric(cb$C10)/pi
hist(cb$diam_10_cm)
cb$sqrt_diam_10_cm <- sqrt(cb$diam_10_cm)
hist(cb$sqrt_diam_10_cm)

cb %>% ggplot(aes(y = diam_10_cm))+
  geom_boxplot() + facet_wrap(~species)

# 4.2 RBT (bark thickness VS stem diameter)
cb$bark_thickness_140_mm <- rowMeans(data.frame(cb$`Bark 140 a`, 
                                                cb$`Bark 140 b`, 
                                                cb$`Bark 140 c`, 
                                                cb$`Bark 140 d`), na.rm=TRUE)

#view(cb$bark_thickness_140_mm) 
#boxplot 
cb %>% ggplot(aes(y = bark_thickness_140_mm))+
  geom_boxplot() + facet_wrap(~species)
summary(cb$bark_thickness_140_mm)

# if doing RBT according to Midgley and Lawes (2016) then must create bole diameter:
# create new column, bole diameter = average bark thickness*2?
# RBT = (BT/BD) * 100 

# bark thickness ratio at 140 
cb$bole_diam_140 <-  cb$diam_130_cm - (2*(cb$bark_thickness_140_mm /10) )
cb$BTratio_140 = ((cb$bark_thickness_140_mm/10)/bole_diam_140)*100 
view(cb$BTratio_140)

# bark thickness 40 cm in mm 
cb$bark_thickness_40_mm <- rowMeans(data.frame(cb$`Bark 40 a`, 
                                               cb$`Bark 40 b`, 
                                               cb$`Bark 40 c`, 
                                               cb$`Bark 40 d`), na.rm=TRUE)

# bark thickness ratio at 40 
cb$bole_diam_40 <- cb$diam_40_cm - (2*(cb$bark_thickness_40_mm /10) )
cb$BTratio_40 = ((cb$bark_thickness_40_mm/10)/bole_diam_40)*100 
view(cb$BTratio_40)
hist(cb$BTratio_40)
hist(cb$BTratio_140)
cb$log_BTratio_40 = log(cb$BTratio_40)
hist(log_BTratio_40)
cb$log_BTratio_140 = log(cb$BTratio_140)
hist(log_BTratio_140)

#find mean RBT
cb$RBT_40_and_140 <- (cb$BTratio_40+cb$BTratio_140)/2
view(cb$RBT_40_and_140)
hist(cb$RBT_40_and_140)
cb$log_RBT_40_and_140 = log(cb$RBT_40_and_140)
hist(cb$log_RBT_40_and_140)
#view in boxplot
cb %>% ggplot(aes(y = RBT_40_and_140))+
  geom_boxplot() + facet_wrap(~species)

cb %>% ggplot(aes(y = BTratio_40))+
  geom_boxplot() + facet_wrap(~species)
#what i had previously done... cb$BTratio_40 = ((2 * cb$bark_thickness_40_mm)/(cb$diam_40_cm))*100

# 4.4 Stem height-diameter allometry: short and fat vs tall and slim = height / dbh 
summary(cb$`height (m)`)
hist(cb$`height (m)`)
view(cb$`height (m)`)

# Stem height-diameter allometry: all species compared 
df <- ggplot(data=cb, aes(x = diam_130_cm, y = `height (m)`, colour = species)) +
  geom_point()+ 
  geom_smooth(method =  "lm", se = FALSE)
df

#decide which diameter 10 or 130 height i will use
height_diameter130_slope <- ggplot(data=cb, aes(x = diam_130_cm, y =`height (m)`, colour = species)) +
  geom_point()+ 
  geom_smooth(method =  "lm", se = FALSE)
height_diameter130_slope

# Install and load the viridis package if you haven't already
install.packages("viridis")  # Uncomment this line if you need to install the package
library(viridis)

#install.packages("viridis")  # Uncomment this line if you need to install the package
#library(viridis)
citation("viridis")
library(dplyr)
library(ggplot2)
citation("ggplot2")

# Calculate the R² values for each species
# Calculate the R² values for each species
species_r2 <- cb %>%
  group_by(s.species) %>%
  summarise(R2 = summary(lm(`height (m)` ~ diam_130_cm))$r.squared)

# Determine the end points for the regression lines
line_end_points <- cb %>%
  group_by(s.species) %>%
  summarise(
    x_end = max(diam_130_cm, na.rm = TRUE),
    y_end = predict(lm(`height (m)` ~ diam_130_cm), newdata = data.frame(diam_130_cm = max(diam_130_cm, na.rm = TRUE)))
  )

# Combine the R² values and line end points
annotations <- inner_join(species_r2, line_end_points, by = "s.species")

# Generate the plot
height_diameter130_slope <- ggplot(data = cb, aes(x = diam_130_cm, y = `height (m)`, colour = s.species)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 1) +  # Use viridis color palette
  theme_minimal() +  # Set a minimal theme to start with
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.line = element_line(colour = "grey"),  # Set axis lines to grey
    axis.text = element_text(size = 12),  # Increase axis text size for readability
    axis.title = element_text(size = 14, face = "bold"),  # Increase and bold axis titles
    legend.position = "right",  # Position legend to the right
    legend.title = element_text(size = 12, face = "bold"),  # Style legend title
    legend.text = element_text(size = 10)  # Style legend text
  ) +
  labs(
    title = "Relationship between Diameter at 130cm and Height",
    x = "Stem diameter at 130cm height (cm)",
    y = "Height (m)",
    color = "Species"
  ) +
  geom_text(data = annotations, aes(x = x_end, y = y_end, label = paste0("R²: ", round(R2, 2))),
            hjust = -0.1, vjust = 1.1, size = 3, color = "black")  # Adjust the position of the text

# Print the plot
print(height_diameter130_slope)

# Generate the plot
height_diameter130_slope <- ggplot(data=cb, aes(x = diam_130_cm, y =`height (m)`, colour = s.species)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 1) +  # Use viridis color palette plasma, viridis, cividis
  theme_minimal() +  # Set a minimal theme to start with
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.line = element_line(colour = "grey"),  # Set axis lines to grey
    axis.text = element_text(size = 12),  # Increase axis text size for readability
    axis.title = element_text(size = 14, face = "bold"),  # Increase and bold axis titles
    legend.position = "right",  # Position legend to the right
    legend.title = element_text(size = 12, face = "bold"),  # Style legend title
    legend.text = element_text(size = 10)  # Style legend text
  ) +
  labs(
    title = "Relationship between Diameter at 130cm and Height",
    x = "Stem diameter at 130cm height (cm)",
    y = "Height (m)",
    color = "Species"
  )

# Print the plot
print(height_diameter130_slope)


height_diameter10_slope <- ggplot(data=cb, aes(x = diam_10_cm, y =`height (m)`, colour = species)) +
  geom_point()+ 
  geom_smooth(method =  "lm", se = FALSE)
height_diameter10_slope

# 4.5 canopy_stem_slope 
# 4.5.1 diameter @ 10 cm VS canopy diam
canopy_stem_slope <- ggplot(data=cb, aes(x = diam_10_cm, y = canopy_diam_m, colour = species)) +
  geom_point()+ 
  geom_smooth(method =  "lm", se = FALSE)
canopy_stem_slope

canopy_stem_slope <- ggplot(data=cb, aes(x = diam_10_cm, y = canopy_diam_m, colour = s.species)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 1) +  # Use viridis color palette
  theme_minimal() +  # Set a minimal theme to start with
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.line = element_line(colour = "grey"),  # Set axis lines to grey
    axis.text = element_text(size = 12),  # Increase axis text size for readability
    axis.title = element_text(size = 14, face = "bold"),  # Increase and bold axis titles
    legend.position = "right",  # Position legend to the right
    legend.title = element_text(size = 12, face = "bold"),  # Style legend title
    legend.text = element_text(size = 10)  # Style legend text
  ) +
  labs(
    title = "Relationship between Canopy Diameter and Stem Diameter at 10cm",
    x = "Stem diameter at 10cm height (cm)",
    y = "Canopy diameter mean (m)",
    color = "Species"
  )

# Print the plot
print(canopy_stem_slope)

# Calculate the R² values for each species
species_r2 <- cb %>%
  group_by(s.species) %>%
  summarise(R2 = summary(lm(`canopy_diam_m` ~ diam_10_cm))$r.squared)

# Determine the end points for the regression lines
line_end_points <- cb %>%
  group_by(s.species) %>%
  summarise(
    x_end = max(diam_10_cm, na.rm = TRUE),
    y_end = predict(lm(`canopy_diam_m` ~ diam_10_cm), newdata = data.frame(diam_10_cm = max(diam_10_cm, na.rm = TRUE)))
  )

# Combine the R² values and line end points
annotations <- inner_join(species_r2, line_end_points, by = "s.species")

# Generate the plot
canopy_stem_slope <- ggplot(data = cb, aes(x = diam_10_cm, y = `canopy_diam_m`, colour = s.species)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 1) +  # Use viridis color palette
  theme_minimal() +  # Set a minimal theme to start with
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.line = element_line(colour = "grey"),  # Set axis lines to grey
    axis.text = element_text(size = 12),  # Increase axis text size for readability
    axis.title = element_text(size = 14, face = "bold"),  # Increase and bold axis titles
    legend.position = "right",  # Position legend to the right
    legend.title = element_text(size = 12, face = "bold"),  # Style legend title
    legend.text = element_text(size = 10)  # Style legend text
  ) +
  labs(
    x = "Stem diameter at 10cm height (cm)",
    y = "Canopy diameter mean (m)",
    color = "Species"
  ) +
  geom_text(data = annotations, aes(x = x_end, y = y_end, label = paste0("R²: ", round(R2, 2))),
            hjust = -0.1, vjust = 1.1, size = 3, color = "black")  # Adjust the position of the text

# Print the plot
print(canopy_stem_slope)

# 4.5.2
#height VS canopy diam - can be used for all species as no missing 10 cm values
cb %>% ggplot(aes(x = `height (m)`, y = canopy_diam_m, colour = species)) +
  geom_point()+ 
  geom_smooth(method =  "lm", se = FALSE)

# further exploration ... 
#potentially try presence or absence for multistems vs DABS? not sure best way to present the data! 

# 4.6 thorn density 
#clean thorn density and make average
summary(cb$`thorn density 1`)
summary(cb$`thorn density 2`)
summary(cb$`thorn density 3`)
summary(cb$`thorn density 4`)
summary(cb$`thorn density 5`)

hist(cb$`thorn density 1`)
hist(cb$`thorn density 2`)
hist(cb$`thorn density 3`)
hist(cb$`thorn density 4`)
hist(cb$`thorn density 5`)

# 4.7 create mean of thorn density:

#tt$thorn_density_average <- rowMeans(select(tt, starts_with("thorn_density")), na.rm = TRUE)
#OR
cb$thorn_density_average <-  (cb$`thorn density 1` + 
                                cb$`thorn density 2` + 
                                cb$`thorn density 3` + 
                                cb$`thorn density 4` +
                                cb$`thorn density 5`) / 5

summary(cb$thorn_density_average)
hist(cb$thorn_density_average)

#view(cb$thorn_density_average)

# 4.8 
#DABS -- need to check it is clean! 
summary(cb$`DABS`)
hist(cb$`DABS`)
view(cb$`DABS`)
logDABS <- log(cb$DABS)
hist(logDABS)
cb$sqrt_DABS <- sqrt(cb$DABS)
hist(sqrt_DABS)

summary(cb$DABS_percent)
hist(cb$DABS_percent)
cb$sqrt_DABS_percent <- sqrt(cb$DABS_percent)
hist(sqrt_DABS_percent)
view(sqrt_DABS_percent)

#log is very slightly better 
#sqrt = good 

# 4.9 Tapering
# stem diameter ratio between height at 130 and 10 cm 
# do I also make it a slope? 
cb$tapering <- (cb$diam_130_cm / cb$diam_10_cm)
hist(cb$tapering)
log_tapering <- log(cb$tapering)
hist(log_tapering)

#view(cb$tapering)
#log is no better 

# 4.10 Leaf surface area mean (cm^2)
summary(cb$`mean SA`)
hist(cb$`mean SA`)
log_mean_SA <- log(cb$`mean SA`)
hist(log_mean_SA)
cb$log10_mean_SA <- log10(cb$`mean SA`)
hist(log10_mean_SA)
#log is better 
#log 10 is best ! 
#view(cb$`mean SA`)

#multistem_mean = mean(multistem, na.rm=TRUE), - put this is DF if wanted

# 5 Binomial Traits:

# 5.1 bark texture presence or absence OR scale of 1-5? 
unique(cb$`bark texture (1-5)`)

# 5.2 Resprouting presence
# 5.2.1 basal resprouter:
# how to group by species 
unique(cb$basal)
cb$basal[is.na(cb$basal)] <- 0
unique(cb$basal)
#view(cb$basal)

# 5.2.2 epicormic resprouter:
unique(cb$epicormic)
cb$epicormic[is.na(cb$epicormic)] <- 0
unique(cb$epicormic)

# 5.2.3 axillary resprouter:
unique(cb$axillary)
cb$axillary[is.na(cb$axillary)] <- 0
unique(cb$axillary)

# 5.2.4 root sucker resprouter:
unique(cb$`root suckers`)
cb$`root suckers`[is.na(cb$`root suckers`)] <- 0
unique(cb$`root suckers`)

# 5.3 Multistem presence 
unique(cb$multistem)
cb$multistem[is.na(cb$multistem)] <- 0
unique(cb$multistem)

# 5.4 Mutualism / ant presence 
unique(cb$mutualism)
cb$mutualism[is.na(cb$mutualism)] <- 0
unique(cb$mutualism)

# 5.5 Leaf type: simple
unique(cb$leaf_simple)
cb$leaf_simple[is.na(cb$leaf_simple)] <- 0
unique(cb$leaf_simple)

# 5.6 Leaf hairy? scale 1-5 , 1= none, 2= sparse, 3=dense described in book
unique(cb$`leaf hair scale`)

# 5.7 Bark fissured ?
unique(cb$`bark fissured`)
cb$`bark fissured`[is.na(cb$`bark fissured`)] <- 0
unique(cb$`bark fissured`)

# Presence or absence of thorns:
unique(cb$thorn_presence)
cb$thorn_presence[is.na(cb$thorn_presence)] <- 0
unique(cb$thorn_presence)
view(cb$thorn_presence)

#5.4 Human interaction presence ?


# 5.5 Medicinal /human use ?


#--------------------------------------------------------------------------------
# 6.0 Make a spp level summary of the key traits --------------------------------

#join DABS and multistem data = defence against bark stripping!?

#analysis 
#1: all species and fewer  traits 
#2: all traits and fewer species

#all traits with NAs 
spt_all_traits <- cb %>% group_by(s.species) %>%
  summarise(n_trees = length(`unique tree ID`),
            height_diameter130_slope = 
              ifelse(n_trees >1, coef(lsfit(diam_130_cm, `height (m)`))[2], NA),
            canopy_stem_slope = 
              ifelse(n_trees >1, coef(lsfit(diam_10_cm, canopy_diam_m))[2], NA),
            BTratio_mean= mean((BTratio_40+BTratio_140)/2, na.rm = TRUE),
             #BTratio_140_mean= mean(BTratio_140, na.rm = TRUE), 
            #BTratio_40_mean= mean(BTratio_40, na.rm = TRUE), 
            DABS_mean = mean(DABS, na.rm=TRUE), 
            thorn_presence = mean(thorn_presence),
            #thorn_density_mean = mean(thorn_density_average, na.rm=TRUE), 
            tapering_mean = mean(tapering, na.rm=TRUE),
            mean_leaf_SA = mean(`mean SA`, na.rm=TRUE), 
            resprout_basal = mean(basal),
            resprout_epicormic = mean(epicormic),
            resprout_axillary = mean(axillary),
            resprout_root_suck = mean(`root suckers`), 
            multistem = mean(multistem), 
            mutualism = mean(mutualism),
            leaf_simple = mean(leaf_simple),
            leaf_hair_scale_mean = mean(`leaf hair scale`), 
            bar_texture = mean(`bark texture (1-5)`)
            )
View(spt_all_traits)

#spp.basal2 = ifelse(!is.na(`resprouting number`), `resprouting number`>0, NA)

spt_thorny_species <- cb %>% group_by(s.species) %>%
  summarise(n_trees = length(`unique tree ID`),
            height_diameter130_slope = 
              ifelse(n_trees >1, coef(lsfit(diam_130_cm, `height (m)`))[2], NA),
            canopy_stem_slope = 
              ifelse(n_trees >1, coef(lsfit(diam_10_cm, canopy_diam_m))[2], NA),
            BTratio_mean= mean((BTratio_40+BTratio_140)/2, na.rm = TRUE),
            #BTratio_140_mean= mean(BTratio_140, na.rm = TRUE), 
            #BTratio_40_mean= mean(BTratio_40, na.rm = TRUE), 
            DABS_mean = mean(sqrt_DABS, na.rm=TRUE), 
            thorn_presence = mean(thorn_presence),
            thorn_density_mean = mean(thorn_density_average, na.rm=TRUE), 
            tapering_mean = mean(tapering, na.rm=TRUE),
            log10_leaf_SA = mean(log10_mean_SA, na.rm=TRUE), 
            resprout_basal = mean(basal),
            resprout_epicormic = mean(epicormic),
            resprout_axillary = mean(axillary),
            resprout_root_suck = mean(`root suckers`), 
            multistem = mean(multistem), 
            mutualism = mean(mutualism),
            leaf_simple = mean(leaf_simple),
            leaf_hair_scale_mean = mean(`leaf hair scale`), 
            bar_texture = mean(`bark texture (1-5)`)
  )
View(spt_thorny_species)


#dataframe for missMDA trials 
#had to remove tratis so there are more units than variables ! 
spt_miss_MDA <- cb %>% group_by(s.species) %>%
  summarise(n_trees = length(`unique tree ID`),
            height_diameter130_slope = 
              ifelse(n_trees >1, coef(lsfit(diam_130_cm, `height (m)`))[2], NA),
            canopy_stem_slope = 
              ifelse(n_trees >1, coef(lsfit(diam_10_cm, canopy_diam_m))[2], NA),
            BTratio_mean= mean((BTratio_40+BTratio_140)/2, na.rm = TRUE), 
            DABS_mean= mean(sqrt_DABS, na.rm=TRUE), 
            thorn_density_mean = mean(thorn_density_average, na.rm=TRUE),
            tapering_mean = mean(tapering, na.rm=TRUE),
            log10_leaf_SA = mean(log10_mean_SA, na.rm=TRUE), 
            resprout_basal = mean(basal),
            resprout_epicormic = mean(epicormic),
            resprout_axillary = mean(axillary),
            multistem = mean(multistem), 
            leaf_hair_scale_mean = mean(`leaf hair scale`), 
            bar_texture = mean(`bark texture (1-5)`)
  )
View(spt_miss_MDA)


#dataframe with no NA or imputed values  
spt1 <- cb %>% group_by(s.species) %>%
  summarise(n_trees = length(`unique tree ID`),
            height_diameter130_slope = 
              ifelse(n_trees >1, coef(lsfit(diam_130_cm, `height (m)`))[2], NA),
            canopy_stem_slope = 
              ifelse(n_trees >1, coef(lsfit(diam_10_cm, canopy_diam_m))[2], NA),
            BTratio_mean= mean((BTratio_40+BTratio_140)/2, na.rm = TRUE), 
            DABS_mean= mean(DABS, na.rm=TRUE), 
            tapering_mean = mean(tapering, na.rm=TRUE), 
            leaf_SA = mean(`mean SA`, na.rm=TRUE),
            )
View(spt1)




###########################################
## FAMD ##
# see: https://www.r-bloggers.com/2022/11/pca-for-categorical-variables-in-r/ 

spt_FAMD <- FAMD(spt2, graph=FALSE)
spt_FAMD

# MAIN DATAFRAME ANALYSIS #####################################################################
#MAIN analysis - copy code and use different data frame created ! 
##############################################
#PCA and ordinance
#install.packages("PCAtest") - not available this R version
#install.packages("FactoMineR")
#library("PCAtest")
#library("FactoMineR")
#pca<- PCA(spt1_matrix, graph=TRUE)
#print(pca)
#help("prcomp")
install.packages("FactoMineR")
install.packages("vcd")
install.packages("factoextra")
library(FactoMineR)
library(vcd)
library(factoextra)
library(dplyr)


spt2 <- cb %>% group_by(s.species) %>%
  summarise(n_trees = length(`unique tree ID`),
            SHD = 
              ifelse(n_trees >1, coef(lsfit(diam_130_cm, `height (m)`))[2], NA),
            CSD = 
              ifelse(n_trees >1, coef(lsfit(diam_10_cm, canopy_diam_m))[2], NA),
            RBT = mean(log_RBT_40_and_140, na.rm = TRUE),
            #BTratio_140_mean= mean(BTratio_140, na.rm = TRUE), 
            #BTratio_40_mean= mean(BTratio_40, na.rm = TRUE), 
            DABS = mean(sqrt_DABS_percent, na.rm=TRUE), 
            #thorn_density_mean = mean(thorn_density_average, na.rm=TRUE), 
            "T" = mean(tapering, na.rm=TRUE),
            LSA = mean(log10_mean_SA, na.rm=TRUE), 
            LH = mean(`leaf hair scale`), 
            BT = mean(`bark texture (1-5)`),
            TP = mean(thorn_presence),
            BRP = mean(basal),
            ERP = mean(epicormic),
            #resprout_axillary = mean(axillary),
            #resprout_root_suck = mean(`root suckers`), 
            MP = mean(multistem), 
            #mutualism = mean(mutualism),
            #leaf_simple = mean(leaf_simple),
            
  )
View(spt2)


# Assign row names from the "species" column, remove n_trees
spt3_matrix <- spt2[, !names(spt2) %in% "n_trees"] %>% remove_rownames() %>% column_to_rownames(var="s.species") #works 

view(spt2_matrix)
str(spt3_matrix)
#CORRELATIONS and p values #######################################
citation("psych")
pairs.panels(spt3_matrix, gap=0, pch=21, method="spearman")
# Calculate Spearman correlation matrix for your dataframe
correlation_matrix <- cor(spt3_matrix, method = "spearman")
# View the correlation matrix table.. 
print(correlation_matrix)

# Optional: If you want to view the correlation matrix in a more readable format
# Install the 'corrplot' package if not already installed
if (!require(corrplot)) {
  install.packages("corrplot")
  library(corrplot)
}

# choosing colour hex https://www.color-hex.com/color-palette/35102 
col <- colorRampPalette(c("#0059b3", "grey", "#CC0000"))(200)  # Adjust number for smoothness

# Generate the correlation plot with enhanced color contrast
corrplot(correlation_matrix, method = "number", type = "upper", order = "hclust",
         tl.col = "black", tl.cex = 0.6, tl.srt = 45, col = col,
         number.cex = 1, # Adjust text size for readability
         addCoef.col = "black") # Ensure coefficients are colored black for visibility
text(x = 1, y = 1, labels = "*", col = "red", cex = 2)  # Customize positions and labels

# Use corrplot to visualize the correlation matrix
corrplot(correlation_matrix, method = "number", type = "upper", order = "hclust",
         tl.col = "black", tl.cex = 0.6, tl.srt = 45)


# Assuming 'spt3_matrix' is your data frame with only the relevant variables

#install.packages("Hmisc")
library(Hmisc)  # for rcorr() function that calculates correlations and p-values
citation("Hmisc")
# Assuming 'spt3_matrix' is your data frame with only the relevant variables
correlation_results <- Hmisc::rcorr(as.matrix(spt3_matrix), type="spearman")
# Access correlation matrix
correlations <- correlation_results$r
# Access p-value matrix
p_values <- correlation_results$P

# Using corrplot to visualize correlations with significance levels
library(corrplot)
citation("corrplot")

corrplot(correlations, type="upper", order="hclust",
         p.mat = p_values, sig.level = 0.05, insig = "label_sig",
         method="color", addCoef.col = "black", 
         tl.col="black", tl.srt=45, 
         col=colorRampPalette(c("#0059b3", "lightgrey", "#CC0000"))(200))

#no sig stars!! 
corrplot(correlations, type = "upper", order = "hclust",
         p.mat = p_values, sig.level = 0.05, insig = "n",  # Change insig to 'n' or 'blank'
         method = "color", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#0059b3", "lightgrey", "#CC0000"))(200))
text(x = 1, y = 1, labels = "*", col = "red", cex = 2)  # Customize positions and labels

##
# Format the correlation matrix to have 2 decimal places
formatted_correlations <- round(correlations, 2)

# Use corrplot with the original correlations matrix
corrplot(correlations, type = "upper", order = "hclust",
         p.mat = p_values, sig.level = 0.05, insig = "n",  # Change insig to 'n' or 'blank'
         method = "color", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#0059b3", "lightgrey", "#CC0000"))(200),
         number.cex = 0.7, number.decimal=2)  # Adjust text size if needed
text(x = 1, y = 1, labels = "*", col = "red", cex = 2)  # Customize positions and labels

formatted_correlations <- round(correlations, 2)

# Use corrplot with formatted correlations
corrplot(formatted_correlations, type = "upper", order = "hclust",
         p.mat = p_values, sig.level = 0.05, insig = "n",  # Change insig to 'n' or 'blank'
         method = "color", addCoef.col = "black", 
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#0059b3", "lightgrey", "#CC0000"))(200),
         number.cex = 0.7)  # Adjust text size if needed



# View the table with significance symbols
print(cor_pval_table)
View(cor_pval_table)
#### sig figs 
# Create a dataframe from the correlation matrix and the p-value matrix
cor_pval_table <- expand.grid(Var1 = rownames(correlations), Var2 = colnames(correlations))
cor_pval_table$Correlation <- as.vector(correlations)
cor_pval_table$P_value <- as.vector(p_values)

# Helper function to assign significance symbols
get_significance <- function(p) {
  ifelse(as.numeric(p) < 0.001, "***",
         ifelse(as.numeric(p) < 0.01, "**",
                ifelse(as.numeric(p) <= 0.05, "*", "")))
}

library(dplyr)

# Add the significance column and format both correlations and p-values to two significant figures
cor_pval_table <- cor_pval_table %>%
  mutate(
    Significance = get_significance(P_value),
    P_value = signif(as.numeric(P_value), 2),
    Correlation = signif(as.numeric(Correlation), 2)
  )

# Filter out redundant entries and self-correlations
cor_pval_table <- cor_pval_table[upper.tri(correlations, diag = FALSE),]

# View the table with significance symbols
print(cor_pval_table)
View(cor_pval_table)


#tapering vs canopystemslpe are highly correlated - not sure if they are too similar traits to compare, 

#.rowNamesDF(spt1_matrix, make.names=TRUE) <- cb$species === doesn't work... 
pca_traits <- prcomp(spt3_matrix, center = T, scale = T )
pca_traits
str(spt3_matrix)

# summary of variation explained per axis and cumulatively
summary(pca_traits)


# correlation coef for each variable along each axis
round(pca_traits$rotation, 1)

## FVIZ PCA PLOT ##
# plots just the variables and their arrows
#install.packages("factoextra") 
#library("factoextra")
PCA_1_2 <- fviz_pca_var(pca_traits, col.var = "black", repel = T, axes = c(1, 2))
PCA_1_2

#orthogonality of PCs 
pairs.panels(pca_traits$x, gap=0, pch=21)

## EIGENVALUES AND SCREEPLOT ##
eig.val <- get_eigenvalue(pca_traits)
eig.val
view(eig.val)

eig.val_rounded <- round(eig.val, 2)
# Print the rounded eigenvalues
print(eig.val_rounded)

# View the rounded eigenvalues in RStudio
View(eig.val_rounded)
fviz_eig(pca_traits, addlabels = TRUE, ylim = c(0, 50))



## RESULTS ## 
var <- get_pca_var(pca_traits)
var

## ACCESS COMPONENTS ##
# Coordinates 
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
view(var$contrib)
contribvar <- var$contrib
contribvar <- contribvar[, 1:5]
contribvar<- round(contribvar, 2)
view(contribvar)

# Optional: if you want to use the View function in RStudio
View(var_contrib_first_5_rounded)
## Quality of representation ## 
head(var$cos2)

#visualise cos2 of variables on all dimensions with corrplot package
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

#make it into barplot 
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca_traits, choice = "var", axes = 1:2)
fviz_cos2(pca_traits, choice = "var", axes = 1)
fviz_cos2(pca_traits, choice = "var", axes = 2)

# Color by cos2 values: quality on the factor map
PCA_cos2_colour <- fviz_pca_var(pca_traits, col.var = "cos2",
                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                repel = TRUE # Avoid text overlapping
)

PCA_cos2_colour

# Change the transparency by cos2 values
PCA_transparency <- fviz_pca_var(pca_traits, alpha.var = "cos2")
PCA_transparency


## CONTRIBUTION ## 

head(var$contrib, 4)

# highlight the most contributing variables for each dimension:
corrplot(var$contrib, is.corr=FALSE)  

## CHECK VARIABLE CONTRIBUTIONS ## (from casey's code)
# check contribution of each variable to each PCA axis
fviz_contrib(pca_traits, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC1 
##### change the number 6 to whatever the number of variables you have! 
# The red dashed line on the graph above indicates the expected average contribution. 
# If the contribution of the variables were uniform, the expected value would be 
# 1/length(variables) = 1/6 = 16*%.
fviz_contrib(pca_traits, choice = "var", axes = 1, top = 6) +
  theme(
    axis.text.x = element_text(size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 16), # Increase x-axis title size
    axis.title.y = element_text(size = 16)  # Increase y-axis title size
  ) +
  labs(x = "Traits") + labs(y= "PC1 Contributions (%)") +
  ggtitle(NULL)
# Contributions of variables to PC2
fviz_contrib(pca_traits, choice = "var", axes = 2, top = 6) +
  theme(
    axis.text.x = element_text(size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 16), # Increase x-axis title size
    axis.title.y = element_text(size = 16)  # Increase y-axis title size
  ) +
  labs(x = "Traits") + labs(y= "PC2 Contributions (%)") +
  ggtitle(NULL)

# Contributions both dimensions:
fviz_contrib(pca_traits, choice = "var", axes = 1:2, top = 10)




# Specify x-axis label

## most important contributers hightlighted on PCA plot ##

PCA_contrib <- fviz_pca_var(pca_traits, col.var = "contrib",
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

PCA_contrib


## RESULTS ##
ind <- get_pca_ind(pca_traits)
ind

# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)
#view(ind$contrib)
contribind <- ind$contrib
contribind <- contribind[, 1:5]
contribind <- round(contribind, 2)
view(contribind)

spt4_matrix <- round(spt3_matrix, 2)
view(spt4_matrix)
# PLOTS: QUALITY AND CONTRIBUTION
#ind plot with cos2 in colour 
PCA_ind_colour <- fviz_pca_ind(pca_traits, col.ind = "cos2", 
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                               repel = TRUE # Avoid text overlapping (slow if many points)
)
PCA_ind_colour

#ind plot with cos2 in size  
PCA_ind_size <- fviz_pca_ind(pca_traits, pointsize = "cos2", 
                             pointshape = 21, fill = "#E7B800",
                             repel = TRUE # Avoid text overlapping (slow if many points)
)
PCA_ind_size


#bar plot of the quality of representation (cos2) of individuals on the factor map
fviz_cos2(pca_traits, choice = "ind")

# Total contribution on PC1 and PC2
ind_contrib <- fviz_contrib(pca_traits, choice = "ind", axes = 1:2)
ind_contrib <- fviz_contrib(pca_traits, choice = "ind", axes = 1)
#PC1
fviz_contrib(pca_traits, choice = "ind", axes = 1, top = 8) +
  theme(
    axis.text.x = element_text(size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 16), # Increase x-axis title size
    axis.title.y = element_text(size = 16)  # Increase y-axis title size
  ) +
  labs(x = "Species") + labs(y= "PC1 Contributions (%)") +
  ggtitle(NULL)

#PC2
fviz_contrib(pca_traits, choice = "ind", axes = 2, top = 8) +
  theme(
    axis.text.x = element_text(size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.x = element_text(size = 16), # Increase x-axis title size
    axis.title.y = element_text(size = 16)  # Increase y-axis title size
  ) +
  labs(x = "Species") + labs(y= "PC2 Contributions (%)") +
  ggtitle(NULL)
ind_contrib <- fviz_contrib(pca_traits, choice = "ind", axes = 2)

ind_contrib

citation("tidyr")


#####################
# BIPLOT 
library(ggsci)
citation("ggsci")

# can try colours like "jco" or "rickandmorty"

biplot <- fviz_pca_biplot(pca_traits,
                          geom_var = c("point", "arrow"),
                          repel = T,
                          col.var = "contrib", 
                          col.ind = "#696969",
                          #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          pointsize = "contrib",
                          label = c("var", "ind"),  # Add labels for individuals and traits
                     
                               title = NULL,
                          labelsize = 6,
                          arrowsize = 1) + 
  theme_bw(base_size = 18) +
  theme(panel.grid = element_blank()) +
  xlab("PC1 - Architectural growth forms (33.96 %)") +
  ylab("PC2 - Resilience to disturbance (20.36 %)") 

# Modify the plot object to add two legends
biplot +
  scale_color_continuous(name = "Trait contribution (%)") +
  guides(size = guide_legend(title = "Species contribution (%)"))

#clustering gower's distance ############
#clustering with gowers distance:
str(spt2_matrix)

# Convert specific columns to logical type
library(dplyr)
citation("dplyr")
binary_cols_2 <- c("thorn_presence", "multistem", "resprout_basal", "resprout_epicormic")
spt2_matrix <- spt2_matrix %>%
  mutate(across(all_of(binary_cols_2), as.factor)) # Convert specific columns to logical type
str(daisy.mat)

# Convert binary variables to factors with levels "0" and "1"
#daisy.mat[binary_cols_2] <- lapply(spt2_matrix[binary_cols_2], factor, levels = c(FALSE, TRUE))

#ordinal variables: 
spt2_matrix$bark_texture <- factor(spt2_matrix$bark_texture, ordered = TRUE, levels = c("1", "2", "3", "4", "5"))
spt2_matrix$leaf_hair_scale_mean <- factor(spt2_matrix$leaf_hair_scale_mean, ordered = TRUE, levels = c("1", "2", "3"))

# Check the result

str(spt2_matrix)
view(spt2_matrix)
names(spt2_matrix)

#make a matrix with gower distance 
daisy.mat <- as.matrix(daisy(spt2_matrix,metric="gower"))
str(daisy.mat)

#silhouette clustering with kmedoids 
avg_silhouette <- NA 
for (i in 2:10){
  my.cluster<- pam(daisy.mat, diss=T, k=i)
  avg_silhouette[i]<-my.cluster$silinfo$avg.width
}
avg_silhouette

plot(1:10, avg_silhouette, xlab='total number of clusters',
     ylab = 'average silhouette', bty="n")
lines(1:10, avg_silhouette)

# Create a clusplot with kmedoids 
# https://www.youtube.com/watch?v=0qp7p98Su6U&ab_channel=DigiroofLearning 

my.cluster <- pam(daisy.mat, k=2, diss = T)
clusplot(my.cluster, dist=daisy.mat, color=T, labels=3 , main='k-medoids clustering', lines=0)
#or .. 
clusplot(my.cluster, dist = daisy.mat, color = TRUE, labels = 3, 
         main = 'k-medoids clustering', 
         cex = 1,  # Adjust point size
         cex.lab = 0.75,  # Adjust label size
         cex.main = 1,
         shade=T,
         lines=0
         #xlim = c(-0.5, 0.5),  # Set x limits
         #ylim = c(-0.03, 0.03)
         )  # Set y limits  # Adjust main title size


str(my.cluster)

view(spt2_matrix)

data <- spt2_matrix[, c("resprout_basal", "multistem", "DABS_percent_mean", "thorn_presence", "canopy_stem_slope")]

# Calculate p-values for each pair of numerical variables
results <- data.frame()
for (i in 1:ncol(data)) {
  for (j in i:ncol(data)) {
    if (i != j) {
      test_result <- cor.test(data[[i]], data[[j]], method = "spearman")
      results <- rbind(results, cbind(Var1 = names(data)[i], Var2 = names(data)[j], 
                                      Correlation = test_result$estimate, 
                                      P.value = test_result$p.value))
    }
  }
}

# Convert results to a more readable format
results <- as.data.frame(results)
results$P.value <- as.numeric(as.character(results$P.value))  # Ensure P.value is numeric
results$Correlation <- as.numeric(as.character(results$Correlation))  # Ensure Correlation is numericcorr#summarise the clusters
spt2_matrix$cluster <- my.cluster$clustering
print(spt2_matrix)
library(dplyr)

# Ensure all ordered factors are converted to factors or characters
list_of_summaries <- lapply(list_of_summaries, function(df) {
  df %>%
    mutate(across(where(is.ordered), as.character))  # Convert ordered factors to character
})

# Now attempt to combine them into one dataframe, showing Mode
summary_table <- map_df(list_of_summaries, ~ .x, .id = "Variable")
print(summary_table, n=24)

# define mode 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# exploring proportions of binary data 
summary_resprout_basal <- spt2_matrix %>%
  group_by(cluster) %>%
  summarise(
    Proportion_1 = mean(resprout_basal == 1),  # Proportion of 1's
    Proportion_0 = mean(resprout_basal == 0)   # Proportion of 0's
  )
summary_resprout_basal


summary_resprout_epicormic <- spt2_matrix %>%
  group_by(cluster) %>%
  summarise(
    Proportion_1 = mean(resprout_epicormic == 1),  # Proportion of 1's
    Proportion_0 = mean(resprout_epicormic == 0)   # Proportion of 0's
  )
summary_resprout_epicormic

summary_multistem <- spt2_matrix %>%
  group_by(cluster) %>%
  summarise(
    Proportion_1 = mean(multistem == 1),  # Proportion of 1's
    Proportion_0 = mean(multistem == 0)   # Proportion of 0's
  )
summary_multistem

summary_thorn_presence <- spt2_matrix %>%
  group_by(cluster) %>%
  summarise(
    Proportion_1 = mean(thorn_presence == 1),  # Proportion of 1's
    Proportion_0 = mean(thorn_presence == 0)   # Proportion of 0's
  )
summary_thorn_presence

#exploring proportion for categorical variables on a scale:
summary_bark_texture <- spt2_matrix %>%
  group_by(cluster) %>%
  summarise(
    Proportion_1 = mean(bark_texture == 1),
    Proportion_2 = mean(bark_texture == 2),
    Proportion_3 = mean(bark_texture == 3),
    Proportion_4 = mean(bark_texture == 4),
    Proportion_5 = mean(bark_texture == 5)
  )
summary_bark_texture

summary_leaf_hair <- spt2_matrix %>%
  group_by(cluster) %>%
  summarise(
    Proportion_1 = mean(leaf_hair_scale_mean == 1),
    Proportion_2 = mean(leaf_hair_scale_mean == 2),
    Proportion_3 = mean(leaf_hair_scale_mean == 3)
  )
summary_leaf_hair


# Combine and reshape data
combined_data <- bind_rows(
  mutate(summary_resprout_basal, trait = "Resprout Basal"),
  mutate(summary_resprout_epicormic, trait = "Resprout Epicormic"),
  mutate(summary_multistem, trait = "Multistem"),
  mutate(summary_thorn_presence, trait = "Thorn Presence"),
  mutate(summary_bark_texture, trait = "Bark Texture"),
  mutate(summary_leaf_hair, trait = "Leaf Hair Scale")
) %>%
  pivot_longer(
    cols = starts_with("Proportion"),
    names_to = "Category",
    values_to = "Proportion",
    names_prefix = "Proportion_"
  ) %>%
  mutate(Category = recode(Category, `1` = "Present", `0` = "Absent", 
                           `2` = "Level 2", `3` = "Level 3", `4` = "Level 4", `5` = "Level 5"))

# Ensure 'cluster' is a factor for plotting
combined_data$cluster <- factor(combined_data$cluster)


# Print the summaries
print(data_summary)

str(spt2_matrix)

# Function to compute appropriate summary statistics

summarize_clusters <- function(data, var) {
  if(is.numeric(data[[var]])) {
    data %>%
      group_by(cluster) %>%
      summarise(Mean = mean(.data[[var]], na.rm = TRUE), .groups = 'drop')
  } else {
    # Assuming definition of Mode function if you decide to use mode calculation
    data %>%
      group_by(cluster) %>%
      summarise(Mode = Mode(.data[[var]]), .groups = 'drop')  # Mode function needs to be defined
  }
}


# define more 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Apply this function across variables

list_of_summaries <- lapply(names(spt2_matrix)[-which(names(spt2_matrix) == "cluster")], function(var) {
  summarize_clusters(spt2_matrix, var)
})
str(spt2_matrix)
# Combine all summaries into one table (optional, for easier viewing)
library(purrr)
summary_table <- map_df(list_of_summaries, ~ .x, .id = "Variable")

# Display the table
print(summary_table)
view(summary_table)

library(tidyr)

# Pivot the table to wide format
summary_table_wide <- summary_table %>%
  pivot_wider(names_from = cluster, values_from = c(Mean), names_sep = "_")
summary_table_wide[is.na(summary_table_wide)] <- "N/A"
# Check the new table structure
print(summary_table_wide)
view(summary_table_wide)


# Re-apply the summarization function with a clear label for each variable
list_of_summaries <- lapply(names(spt2_matrix)[-which(names(spt2_matrix) == "cluster")], function(var) {
  summary <- summarize_clusters(spt2_matrix, var)
  summary$Variable <- var  # Add a new column to each summary with the variable name
  return(summary)
})
# visualising binary and categorical proportions: #################### 
# Preparing categorical data summaries for bark texture and leaf hair scale
summary_bark_texture_long <- summary_bark_texture %>%
  pivot_longer(
    cols = starts_with("Proportion"),
    names_to = "Category",
    values_to = "Proportion",
    names_prefix = "Proportion_"
  ) %>%
  mutate(Category = as.character(as.numeric(sub("Proportion_", "", Category))))

summary_bark_texture_long


summary_leaf_hair <- summary_leaf_hair %>%
  mutate(Category = case_when(
    Proportion == 0.333 & cluster == "1" ~ "1",
    Proportion == 0.444 & cluster == "1" ~ "2",
    Proportion == 0.222 & cluster == "1" ~ "3",
    Proportion == 0.667 & cluster == "2" ~ "1",
    Proportion == 0.167 & cluster == "2" ~ "2",
    Proportion == 0.167 & cluster == "2" ~ "3"
  ))

# Ensure it matches expected structure
str(summary_leaf_hair)

# Assigning categories manually
summary_leaf_hair <- summary_leaf_hair %>%
  mutate(Category = rep(1:3, times = 2)) %>%
  group_by(cluster) %>%
  mutate(Category = as.character(Category))  # Ensure it is a character if needed

# Check the structure again to ensure the change
str(summary_leaf_hair)

# Now pivot the data correctly
summary_leaf_hair_long <- summary_leaf_hair %>%
  pivot_longer(
    cols = Proportion,
    names_to = "Category",
    values_to = "Proportion",
    names_prefix = "Proportion_"
  )

# View the pivoted data to ensure it is correct
print(summary_leaf_hair_long)

# Creating a stacked bar plot for a singular variable 
ggplot(summary_leaf_hair, aes(x = cluster, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Pastel1", name = "Leaf Hair Scale") +
  labs(title = "Proportion of Leaf Hair Scale Categories by Cluster",
       x = "Cluster",
       y = "Proportion") +
  theme_minimal()


# Assuming summary data frames are already calculated and just need cleaning

# Prepare each summary data frame
prepare_summary <- function(data, trait_name, levels_present = NULL) {
  # If data already has a 'Category' column, we skip creating it and go directly to adjusting proportions
  if ("Category" %in% names(data)) {
    data <- mutate(data, trait = trait_name)
  } else {
    # Convert proportions columns to long format
    data <- pivot_longer(
      data,
      cols = starts_with("Proportion"),
      names_to = "Category",
      values_to = "Proportion",
      names_prefix = "Proportion_"
    )
    
    if (!is.null(levels_present)) {
      data$Category <- factor(data$Category, levels = levels_present)
    }
    
    data <- mutate(data, trait = trait_name)
  }
  
  return(data)
}

# Apply the function to your 'summary_leaf_hair' assuming it already has 'Category'
summary_leaf_hair <- prepare_summary(summary_leaf_hair, "Leaf Hair Scale", levels_present = c("1", "2", "3"))

# Now check structure
str(summary_leaf_hair)

summary_leaf_hair$Category <- factor(summary_leaf_hair$Category, levels = c("1", "2", "3"))

# Plotting the data
p1 <- ggplot(summary_leaf_hair, aes(x = cluster, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Leaf Hair Scale by Cluster",
       x = "Cluster",
       y = "Proportion") +
  scale_fill_brewer(palette = "Pastel1", name = "Leaf Hair Category") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
p1
# Plotting the data
p2 <- ggplot(summary_bark_texture, aes(x = cluster, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Pastel1", name = "Bark Texture Scale") +
  labs(title = "Proportion of Bark Texture Scale by Cluster",
       x = "Cluster",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
p2

p3 <- ggplot(summary_multistem, aes(x = cluster, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Pastel1", name = "Multistem Presence") +
  labs(title = "Proportion of Multistem Presence by Cluster",
       x = "Cluster",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
p3


p4 <- ggplot(summary_resprout_basal, aes(x = cluster, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Pastel1", name = "Basal Resprouting Presence") +
  labs(title = "Proportion of Basal Resprouting Presence by Cluster",
       x = "Cluster",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
p4


p5 <- ggplot(summary_resprout_epicormic, aes(x = cluster, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Pastel1", name = "Basal Epicormic Presence") +
  labs(title = "Proportion of Basal Epicormic Presence by Cluster",
       x = "Cluster",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
p5

p6 <- ggplot(summary_thorn_presence, aes(x = cluster, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Pastel1", name = "Thorn Presence") +
  labs(title = "Proportion of Thorn Presence by Cluster",
       x = "Cluster",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
p6

# Combine plots

install.packages("patchwork")
library(patchwork)
citation('patchwork')
combined_plot <- p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 2)  # Arrange plots in two columns

# Print combined plot
combined_plot

  #visualising binary and categorical proportions:


# VISUALISE numerical values of CLUSTER GROUPS ########
#ANALYSE AND VISUALISE CLUSTER GROUPS 
#make it pretty?
##Visualize 
fviz_cluster(list(data = spt2_matrix,
                  cluster = my.cluster),
             geom="point") + ggtitle("Agglomerative clustering (average)") 

#analyse cluster groups 
table(my.cluster$clustering)

clustering_matrix <- cbind(spt2_matrix, my.cluster$clustering)
colnames(clustering_matrix)[ncol(clustering_matrix)] <- 'pam_cluster'
aggregate(clustering_matrix[, c('height_diameter130_slope', 
                                'canopy_stem_slope', 
                                'BTratio_mean', 
                                'DABS_percent_mean',
                                'tapering_mean',
                                'mean_leaf_SA')],
          list(clustering_matrix$pam_cluster), mean)

str(clustering_matrix)
#create table... how do i compare cluster values for binary variables? 
#clust.vals <- clustering_matrix %>% group_by(pam_cluster) %>% summarise_all(list(mn = mean, sd = sd))
#View(clust.vals)
#must use if becauase of non-numeric variables 
library(dplyr)
library(ggplot2)

clust.vals <- clustering_matrix %>%
  group_by(pam_cluster) %>%
  summarise_if(is.numeric, list(mn = mean, sd = sd))
View(clust.vals)

#focusing on the numerical vlaues:
clust.vals.se.2 <- clustering_matrix %>%
  group_by(pam_cluster) %>%
  summarise(across(c(height_diameter130_slope, canopy_stem_slope, BTratio_mean, DABS_percent_mean, tapering_mean, mean_leaf_SA),
                   list(mn = mean, sd = sd),
                   .names = "{.col}_{.fn}")) %>%
  mutate(across(ends_with("_sd"), ~ ./ sqrt(n()), .names = "{.col}_se"))  # Calculate standard errors
view(clust.vals.se.2)

#checking number of speceis per cluster as want to validate standard error... 
#cluster 1 = 9 species, cluster 2= 6 species
# Re-calculating based on provided standard deviations and sample sizes:
cluster_1_species_count <- 9
cluster_2_species_count <- 6

# Example recalculation for one variable in R code:
height_diameter130_slope_se_cluster1 <- 0.07886248 / sqrt(cluster_1_species_count)
height_diameter130_slope_se_cluster1
height_diameter130_slope_se_cluster2 <- 0.08527717 / sqrt(cluster_2_species_count)
height_diameter130_slope_se_cluster2


# Modify the dataset to include a new column for sample size
clust.vals.se.3 <- clustering_matrix %>%
  group_by(pam_cluster) %>%
  summarise(across(c(height_diameter130_slope, canopy_stem_slope, BTratio_mean, DABS_percent_mean, tapering_mean, mean_leaf_SA),
                   list(mn = mean, sd = sd),
                   .names = "{.col}_{.fn}"),
            size = n())  # Add a column to check the size of each group
View(clust.vals.se.3)

# View the output to check sizes
# Calculate the mean, standard deviation and also capture the sample size in each cluster
clust.vals.se.3 <- clustering_matrix %>%
  group_by(pam_cluster) %>%
  mutate(size = n()) %>%  # Add a column to capture the size of each group before summarising
  summarise(across(c(height_diameter130_slope, canopy_stem_slope, BTratio_mean, DABS_percent_mean, tapering_mean, mean_leaf_SA),
                   list(mn = mean, sd = sd),
                   .names = "{.col}_{.fn}"),
            size = mean(size))  # Keep the size constant across all summarised results

# Calculate the standard errors using the 'size'
clust.vals.se.3 <- clust.vals.se.3 %>%
  mutate(across(ends_with("_sd"), ~ ./ sqrt(size), .names = "{.col}_se"))  # Calculate standard errors using the correct sample size

# View the updated data frame
print(names(clust.vals.se.3))
view(clust.vals.se.3)

library(ggplot2)
library(gridExtra)  # If you want to arrange multiple plots

# Extracting just the names for the mean columns
variable_names <- c("height_diameter130_slope", "canopy_stem_slope", 
                    "BTratio_mean", "DABS_percent_mean", 
                    "tapering_mean", "mean_leaf_SA")

# Initialize an empty list for storing plots if you want to show them together
plots_list <- list()

# Loop through each variable to create separate plots
for (var in variable_names) {
  mean_col <- paste0(var, "_mn")
  se_col <- paste0(var, "_sd_se")
  
 # Generate a plot for each variable
  p <- ggplot(clust.vals.se.3, aes(x = as.factor(pam_cluster), y = !!sym(mean_col), fill = as.factor(pam_cluster))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = !!sym(mean_col) - !!sym(se_col), ymax = !!sym(mean_col) + !!sym(se_col)),
                  position = position_dodge(width = 0.8), width = 0.25) +
    labs(title = paste("Means and Standard Errors of", var, "by Cluster"),
         x = "Cluster", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Store plot in list
  plots_list[[var]] <- p
}

# Display all plots together (optional)
do.call(gridExtra::grid.arrange, c(plots_list, ncol = 2))

# Loop through each variable to create separate plots
plots_list <- list()  # Initialize an empty list to store plots
for (var in variable_names) {
  mean_col <- paste0(var, "_mn")
  se_col <- paste0(var, "_sd_se")
  
  # Generate a plot for each variable
  p <- ggplot(clust.vals.se.3, aes(x = as.factor(pam_cluster), y = !!sym(mean_col), fill = as.factor(pam_cluster))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = !!sym(mean_col) - !!sym(se_col), ymax = !!sym(mean_col) + !!sym(se_col)),
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = c("1" = "#0059b3", "2" = "#CC0000")) +  # Custom colors for each cluster
    labs(title = paste(var),
         x = "Cluster", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Store plot in list
  plots_list[[var]] <- p
}

# Display all plots together (optional)
do.call(gridExtra::grid.arrange, c(plots_list, ncol = 2))


# PASTEL PLOT ! 
plots_list <- list()  # Initialize an empty list to store plots
for (var in variable_names) {
  mean_col <- paste0(var, "_mn")
  se_col <- paste0(var, "_sd_se")
  
  # Generate a plot for each variable
  p <- ggplot(clust.vals.se.3, aes(x = as.factor(pam_cluster), y = !!sym(mean_col), fill = as.factor(pam_cluster))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = !!sym(mean_col) - !!sym(se_col), ymax = !!sym(mean_col) + !!sym(se_col)),
                  position = position_dodge(width = 0.8), width = 0.25) +
    scale_fill_manual(values = c("1" = "#99c2ff", "2" = "#ff9999")) +  # Custom pastel colors for each cluster
    labs(title = paste("Means and Standard Errors of", var, "by Cluster"),
         x = "Cluster", y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Store plot in list
  plots_list[[var]] <- p
}

# Display all plots together (optional)
do.call(gridExtra::grid.arrange, c(plots_list, ncol = 2))

#boxplot experiment for continuous 

# create a nice table of mean and se ##### 
######Create a nice table of mean and standard error
#pivot to long format 
clust.vals.long <- clust.vals.se.3 %>%
  pivot_longer(
    cols = -pam_cluster,  # Exclude the cluster identifier from the reshaping
    names_to = c("variable", "stat"),  # Define two new columns for the variable name and statistic type
    names_pattern = "(.*)_(mn|se)$",  # Correct regex pattern to capture base variable name and statistic type (mn for mean, se for standard error)
    values_to = "value"  # Store values in this new column
  )

# Checking the structure of the reshaped data
print(clust.vals.long, n=36)
library(dplyr)
clust.vals.se_clean <- clust.vals.long %>%
  filter(!is.na(variable)) 

# View the cleaned and structured data
print(clust.vals.se_clean, n=36)
View(clust.vals.se_clean)

round(clust.vals.se_clean, 0)

clust.vals.wide <- clust.vals.se_clean %>%
  pivot_wider(
    id_cols = pam_cluster,
    names_from = variable,
    values_from = value,
    names_sep = "_",  # Optional, adds a separator between the variable name and the new columns (mn, se)
    values_fn = list(value = mean)  # Handles any accidental duplicates
  )

# View the structured wide format data
print(clust.vals.wide)

# PERMANOVA #####################
#PERMANOVA - STAT ANALYSIS ON CLUSTERS 

if (!require(vegan)) {
  install.packages("vegan")
  library(vegan)
}
#citation("vegan")

spt2_matrix$cluster <- as.factor(spt2_matrix$cluster)

# PERMANOVA test
# Run PERMANOVA using adonis2 with the Gower distance matrix
permanova_result <- adonis2(daisy_mat ~ spt2_matrix$cluster, method = "permutational", permutations = 999)
print(permanova_result)

# Multidimensional Scaling on the Gower distance matrix

#experiment mds #############
mds.dist <- vegdist(daisy_mat)
str(mds.dist)
mds_iso <- isoMDS(mds.dist)
stressplot(mds, mds.dist)
var.mds <- metaMDS(daisy.mat, trace=F)
var.mds
plot(var.mds, type="t")

#procruster rotation
tmp <- wisconsin(sqrt(daisy.mat))
dis <- vegdist(tmp)
mds_iso <- isoMDS(dis, trace = 0)
pro <- procrustes(var.mds, mds_iso)
pro
plot(pro)
identify(pro)

#plot residual differences: 
plot(pro, kind = 2)

# Assuming daisy.mat is your Gower's distance matrix
# Perform NMDS using metaMDS
nmds_result <- metaMDS(daisy.mat, distance = "gower", k = 2, trymax = 100)

# Plot the NMDS results
plot(nmds_result, type = "n")  # Create an empty plot

# Add the points for the sites
points(nmds_result$points, pch = 21, bg = "lightblue", cex = 1.5)

# Add text labels for the sites
text(nmds_result$points, labels = rownames(daisy.mat), cex = 0.8, col = "blue")

# Optionally, you can add stress plot
stressplot(nmds_result)

# Display the NMDS result
print(nmds_result)

#inertia is variance 
var.pca<- rda(spt3_matrix)
var.pca
plot(var.pca)
sum(apply(spt3_matrix, 2, var))
biplot(var.pca, scaling = -1)

#inertia is correlations- = using correlation coefficients 
var.pca <- rda(spt3_matrix, scale = TRUE)
var.pca
plot(var.pca, scaling = 3)
biplot(var.pca, scaling = 3)

dim(spt3_matrix)

#MDS plot #####
mds <- metaMDS(daisy_mat, trymax = 20)

# Plotting
plot(mds$points[,1], mds$points[,2], col = spt2_matrix$cluster, pch = 19, xlab = "MDS1", ylab = "MDS2",
     main = "MDS Plot based on Gower Distance")
legend("topright", legend = levels(spt2_matrix$cluster), col = 1:length(levels(spt2_matrix$cluster)), pch = 19)

install.packages("ggrepel")
library(ggrepel)  # For better label placement
library(ggplot2)

# If there are NA values, you may need to address these. If `cluster` should be a factor, ensure it is:
mds_df$cluster <- as.factor(mds_df$cluster)
names(mds_df)
mds_df <- as.data.frame(mds$points)
mds_df$labels <- rownames(mds$points)  # Preserving original row names as labels
mds_df$cluster <- spt2_matrix$cluster[match(rownames(mds$points), rownames(spt2_matrix))] 

# Now plot using the cleaned data frame
ggplot(mds_df, aes(x = MDS1, y = MDS2, label = labels, color = as.factor(cluster))) +
  geom_point(aes(shape = as.factor(cluster)), size = 3) +
  geom_text_repel(size = 3, box.padding = 0.35, point.padding = 0.5, max.overlaps = Inf) +
  scale_color_manual(values = c("#0059b3", "#CC0000")) + 
  labs(title = "MDS Plot of PAM Clusters with Labels", x = "MDS Dimension 1", y = "MDS Dimension 2", color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_fixed()


#mds_df uses metaMDS() whereas mds_df.1 uses cmdscale() 
#metaMDS is typically used for ecological data 
#pastel colour circular ellipse 
ggplot(mds_df, aes(x = MDS1, y = MDS2, color = cluster)) +
  geom_point() +  # Draw the points
  geom_text(aes(label = labels), vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +  # Add labels, apply only here
  stat_ellipse(aes(group = cluster), geom = "polygon", alpha = 0.2, fill = NA, linetype = "dashed", linewidth = 0.5) +  # Add ellipses
  scale_color_brewer(type = "qual") +  # Use qualitative colors for distinct cluster visualization
  theme_minimal() +
  labs(title = "Cluster Plot with MDS, Labels, and Ellipses", x = "MDS 1", y = "MDS 2")

#changing circle/ellipse to convex hulls 
# Calculate convex hulls for each cluster
hulls_data <- list()
clusters <- unique(mds_df$cluster)

for (cl in clusters) {
  subset_df <- mds_df %>% filter(cluster == cl)
  hull_indices <- chull(subset_df$MDS1, subset_df$MDS2)
  hull_indices <- c(hull_indices, hull_indices[1])  # Close the hull by repeating the first index
  
  # Extract hull points
  hull_points <- subset_df[hull_indices, ]
  hull_points$cluster <- cl  # Ensure cluster identification is clear
  
  hulls_data[[cl]] <- hull_points
}

hulls_df <- do.call(rbind, hulls_data)

#create plot with convex hulls
# perfect plot ... 
ggplot(mds_df, aes(x = MDS1, y = MDS2, color = cluster, label = labels)) +
  geom_point(aes(shape = cluster), size = 3) +
  geom_text_repel(size = 5, box.padding = unit(0.35, "lines"), point.padding = unit(0.5, "lines"), max.overlaps = 50) +
  geom_polygon(data = hulls_df, aes(x = MDS1, y = MDS2, fill = cluster, group = cluster),
               color = "black", alpha = 0.2, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "MDS Plot with Species Labels and Cluster Hulls",
       x = "Dimension 1", y = "Dimension 2")

#even better as matches colours of clusplot  
ggplot(mds_df, aes(x = MDS1, y = MDS2, color = cluster, label = labels)) +
  geom_point(aes(shape = cluster), size = 3) +
  geom_text_repel(size = 5, box.padding = unit(0.35, "lines"), point.padding = unit(0.5, "lines"), max.overlaps = 50) +
  geom_polygon(data = hulls_df, aes(x = MDS1, y = MDS2, fill = cluster, group = cluster),
               color = "black", alpha = 0.2, linetype = "dashed") +
  scale_color_manual(values = c("1" = "#0059b3", "2" = "#CC0000")) +  # Custom colors for points
  scale_fill_manual(values = c("1" = "#0059b3", "2" = "#CC0000")) +  # Custom colors for fills
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "grey80"),
    legend.position = "right"
  ) +
  labs(title = "MDS Plot with Species Labels and Cluster Hulls",
       x = "Dimension 1", y = "Dimension 2")

ggsave("MDS_plot_with_labels.pdf", width = 8, height = 6)  # Save the plot to a file

# HIERACHICAL CLUSTERING EXPERIMENT... ####
#### hierachical clustering:
summary(daisy.mat)
# Check for missing values in the dissimilarity matrix
any(is.na(daisy.mat))
dim(daisy.mat)
hc.cluster <- hclust(as.matrix(daisy.mat), method = 'centroid') #look at different methods, complete, single, etc...
# If there are missing values, remove them
daisy.mat <- na.omit(daisy.mat)

# Perform hierarchical clustering again
hc.cluster <- hclust(daisy.mat, method='complete')
hc.cluster <- hclust(daisy.mat, method='complete')
plot(hc.cluster)
 

# PHYLOGENY.. ################################################
#phylogeny - 

#pgls() function

# chat GPT 
# Assuming you have a data frame called 'my_data' with columns 'y' and 'x'

# Load your phylogenetic tree
# Assuming you have a phylogenetic tree object called 'phy_tree'

# Perform PGLS regression
pgls_model <- pgls(y ~ x, data = my_data, phy = phy_tree)

# MAPPING EXPERIMENT ###############################################
#mapping? 
# ask Lucie? 
?geodata


# MDS plot #################
#more mds but with cmdscale which is not applicable.. 

daisy_mat2 <- daisy(spt2_matrix, metric = "gower")

# Perform PAM clustering
pam_result <- pam(daisy_mat2, k = 2)

# Perform MDS using the same distance matrix
mds_result.1 <- cmdscale(daisy_mat2, k = 2)  # Reduce to two dimensions

# Convert MDS result to a data frame for plotting
mds_df.1 <- as.data.frame(mds_result.1)
mds_df.1$cluster <- as.factor(pam_result$clustering)  # Add cluster assignments
library(ggplot2)

# Adding labels to the plot, assuming you have labels in your data
mds_df.1$labels <- row.names(spt2_matrix)  # Adjust based on your data structure

# Enhanced plot with labels
ggplot(mds_df.1, aes(x = V1, y = V2, color = cluster, label = labels)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_text(aes(label = labels), vjust = 1.5, hjust = 1.5) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_minimal() +
  labs(title = "MDS Plot of PAM Clusters with Labels",
       x = "MDS Dimension 1",
       y = "MDS Dimension 2")

#mds_df.1 uses cmdscale() 
ggplot(mds_df.1, aes(x = V1, y = V2, color = cluster)) +
  geom_point() +  # Draw the points
  geom_text(aes(label = labels), vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +  # Add labels, apply only here
  stat_ellipse(aes(group = cluster), geom = "polygon", alpha = 0.2, fill = NA, linetype = "dashed", linewidth = 0.5) +  # Add ellipses
  scale_color_brewer(type = "qual") +  # Use qualitative colors for distinct cluster visualization
  theme_minimal() +
  labs(title = "Cluster Plot with MDS, Labels, and Ellipses", x = "MDS 1", y = "MDS 2")
