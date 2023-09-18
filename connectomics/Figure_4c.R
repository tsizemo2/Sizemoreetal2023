#################### LOAD PACKAGES ##########################################################################
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("bigmemory", "plyr", "parallel", "parallelly", "Rcpp", "RcppProgress","RcppParallel", "RcppArmadillo", "hdf5r", 
              "xlsx", "readxl", "gh", "gistr", "git2r", "tidyverse", "devtools", "arsenal", "knitr", "dbplyr", "dtplyr", 
              "tidyr", "tidyquant", "rstatix", "ggplot2", "ggdendro", "gcookbook", "ggrepel", "ggthemes", "ggsci", "R.cache", 
              "ComplexHeatmap","ggpubr", "alphashape3d", "apcluster", "drvid", "reshape2", "nat", "nat.utils", "nat.h5reg",
              "nat.flybrains","reticulate", "Morpho", "nat.jrcbrains", "nat.templatebrains", "fafbseg","cachem",  "hemibrainr", 
              "flycircuit", "neuromorphr", "neuprintr","natverse","rflyem", "vfbconnectr", "R.methodsS3", "patchwork", "cluster")
ipak(packages)


# if you want to check whether updates are available for natverse & natverse dependencies
# natverse_update() # if yes, then run: natverse_update(update=TRUE)
#
# if nat.ants is NOT already installed, then the above code will likely not be able to install it for you.
# in that case, you should follow these instructions: https://github.com/jefferis/nat.ants
#
# if you haven't already, download the template brain registrations using the following functions:
# download_saalfeldlab_registrations()
# download_jefferislab_registrations()
#

#################### LOAD DATA ##########################################################################
ExcDriveInhSup_PNdendglomerulusmeshes <- read_excel("path/to/location/where/aggregated/synapse/counts/by/principal/neuron/type/and/in/which/glomerulus/the/synapse(s)/are/located/can/be/found.xlsx")

#################### DATA WRANGLING ##########################################################################
df <- ExcDriveInhSup_PNdendglomerulusmeshes %>% 
  gather(key = cell_type,
         value = input_ratio,
         ORN_input_ratio:Other_Unknown_input_ratio,
         factor_key = TRUE)

#################### BUILD THE PLOT ##########################################################################
mypal = c("#FFD9B2", "#FFC79C", "#FFAB7A", "#A1CCA5", "#709775", "#415D43", "#A38881", "#000000")
names(mypal) = c("lightestorange", "midorange", "darkerorange", "mintgreen", "midgreen", "darkgreen", "tan", "black")

p1 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN1') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(y = input_ratio,
             x = glomeruli,
             fill = factor(cell_type))) + 
  geom_bar(position = "stack",
           stat = "identity",
           fill = rep(mypal, 58),
           show.legend = TRUE) + 
  theme_classic()
p1 + labs(colour = factor(cell_type)) +
  theme(legend.position = "top")
# print(p1) #just here if you want to check out this first plot...

p2 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN2') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p3 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN3') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p4 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN4') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p5 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN5') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p6 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN6') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p7 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN7') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p8 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN8') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p9 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN9') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p10 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN10') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p11 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN11') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p12 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN12') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p13 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN13') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

p14 <- df %>% 
  dplyr::filter(mipln == 'putMIP_LN14') %>% 
  group_by(glomeruli) %>% 
  ggplot(aes(fill = cell_type,
             y = input_ratio,
             x = glomeruli)) + 
  geom_bar(position = "stack",
           stat = "identity") + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank()) + 
  coord_fixed(ratio = 1.5)

#################### COLLATE PLOTS ##########################################################################
combined <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + p13 + p14 & theme(legend.position = "left")
combined + plot_layout(ncol = 2, nrow = 7, guides = "collect")
