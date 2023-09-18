########### LOAD PACKAGES ####################################################################################################
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

########### GET THE DATA #############################################
data <- read.csv("path/to/your/data/file.csv",
                 stringsAsFactors = TRUE)

########### DATA WRANGLING #############################################
data2 <- na.omit(data)

d2_long <- data2 %>% 
  dplyr::select(glomerulus, mipln, ratio)
rownames(d2_long) <- NULL
# data2_long$ratio <- as.numeric(data2_long$ratio)

d2_wide <- d2_long %>%  
  pivot_wider(names_from = mipln, values_from = ratio) %>% 
  replace(is.na(.), 2) #changed all NAs to 2 because the "NAs" are relevant, and therefore those rows throwing too many to be considered for the euclidean dist analysis can't just be tossed out... 

d2w_num <- apply( d2_wide, 2, as.numeric ) #will throw a warning, but no worries...

d2w_num <- d2w_num[,colnames(d2w_num)!="glomerulus"] #removing glomerulus column that is now all "NA"...
rownames(d2w_num) <- d2_wide$glomerulus


# ### DO THIS IF YOU NEED TO HANDLE DATA WITH SIGNIFICANT AMOUNTS OF NAs THAT CAN BE REMOVED BEFORE ANALYZING
# # can check if there are NAs in the distance matrix; if so, euclidean distance can't be calculated.
# print(sum(is.na(as.matrix(dist(d2w_num))))) # this = 4, so some rows need to be removed!!!
# 
#   ### BECAUSE OF THE SIGNIFICANT # OF NAs, THE DENDROGRAM WOULDN'T PRINT RIGHT, ###
#   ###   SO NEED TO REMOVE THE ROWS GIVING THE MOST NAs & REMOVE THEM !!!        ###
# giveNAs = which(is.na(as.matrix(dist(d2w_num))), arr.ind=TRUE)
# head(giveNAs)
# 
# tab = sort(table(c(giveNAs)), decreasing=TRUE)
# checkNA = sapply(1:length(tab), function(i){
#   sum(is.na(as.matrix(dist(d2w_num[-as.numeric(names(tab[1:i])),]))))
# })
# rmv = names(tab)[1:min(which(checkNA==0))]
# 
# d2w_num = d2w_num[-as.numeric(rmv),]
# ###   ######   ######   ######   ######   ######   ######   ######   ######   ######   ###

############# FOR PLOTTING DENDROGRAM WITHOUT SCALING! #############################
# * didn't need to scale values since I normalized the values already!!!
        ### * REDONE FOR 2ND SUBMISSION * ###
              # d2w.scaled <- data2_wide
              # d2w.scaled[, c(2:15)] <- scale(d2w.scaled[, 2:15])
              # d2ws.mat <- as.matrix(d2w.scaled[, -c(1)])
              # rownames(d2ws.mat) <- d2w.scaled$glomerulus
              # d2w_mat <- as.matrix(data2_wide[, -c(1)])
              # rownames(d2w_mat) <- data2_wide$glomerulus

d2wm_dend <- as.dendrogram(hclust(dist(
  d2w_num, method = "euclidean"),
  method = "ward.D2")
  )
dend_plot <- ggdendrogram(data = d2wm_dend, rotate = T) +
  theme(axis.text.y = element_text(size = 4))
print(dend_plot)


############# FOR PLOTTING HEATMAP AND ORDERING ACCORDING TO DENDROGRAM! #############################
#need long format, but did that for "data2_long" :) 
                    # mipln_order = c("putMIP_LN1","putMIP_LN2","putMIP_LN3", "putMIP_LN4", "putMIP_LN5",
                    #                 "putMIP_LN6","putMIP_LN7","putMIP_LN8", "putMIP_LN9", "putMIP_LN10",
                    #                 "putMIP_LN11","putMIP_LN12","putMIP_LN13", "putMIP_LN14")
                    # data2_long$mipln <- factor(data2_long$mipln, levels = mipln_order)

  # Data wrangling
# did this above when I made the 'data2_long' dataframe...

  # Extract order of the dendrogram tips
hm_order <- order.dendrogram(d2wm_dend)

# Order the levels according to their position in the cluster
                    # data2_long$glomerulus <- as.factor(data2_long$glomerulus)
                    # data2_long$mipln <- as.factor(data2_long$mipln)
                    # glomeruli <- d2_wide$glomerulus
d2_long$glomerulus <- factor(x = d2_long$glomerulus,
                          levels = d2_wide$glomerulus[hm_order], 
                          ordered = TRUE)

# hm_order2 <- c("DM5","DA3","VM1","VM4","VA7l","VM2","VM5d","DM2","VA7m","DL4","DP1l","VC4","VM7v","VL2a","DL3","VM7d","VC5","DM6","VP3","VM5v","DA4m","DA2","DA4l","VP1l","DL2d","VA5","VA4","VP2","VM3","DL1","VP4","VP1d","DL5","VP1m","D","DM4","DL2v","V","DP1m","VL2p","DM1","VA2","DC1","DC3","DA1","VA1d","VA6","DC2","VM6","DC4","VC3","VA3","DM3","VL1","VC2","VA1v","VC1")

# hm_order2 <- c(as.integer(rmv), hm_order)
# d2_long$glomerulus <- factor(x = d2_long$glomerulus,
#                              levels = d2_wide$glomerulus[hm_order2], 
#                              ordered = TRUE)


## Create Heatmap Plot...
hm_plot <- ggplot(data = d2_long, aes(x = mipln, y = glomerulus)) +
  geom_tile(aes(fill = ratio)) +
  scale_fill_gradient2(low = "#D100FA", 
                       mid = "#FFFFFF", 
                       high = "#000000", 
                       na.value = "#6E9E75",
                       name = "Input:Output Ratio") +
  theme(axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 4),
        legend.position = "top")
print(hm_plot)


grid.newpage()
print(hm_plot, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(dend_plot, vp = viewport(x = 0.90, y = 0.43, width = 0.2, height = 0.92))

