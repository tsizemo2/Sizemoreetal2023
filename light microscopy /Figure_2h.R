#################### LOAD PACKAGES ##########################################################################
  #check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
  #usage
pkgs <- c("readr", "tidyverse", "readxl", "ComplexHeatmap", "circlize")

# pkgs.2 <- c("readr", "tidyverse", "readxl", "ggtext", "ggdist", "magick",
#               "patchwork", "ggstance", "ggsci", "gcookbook", "ggpubr", 
#               "ggsignif", "ggforce", "ggnetwork", "ggridges", "ggrepel",
#               "ggthemes", "ggplot2", "ggdendro", "reshape2", "grid", 
#               "gridGraphics", "gridExtra", "grDevices", "factoextra",
#               "scales", "stats", "fpc", "cluster", "NbClust")
ipak(pkgs)
rm(pkgs, ipak)


######## LOAD DATA ##########################################################################
data <- read.csv("path/to/where/binary/glomerulus/innervation/values/for/individual/MIPergic/LN/clones/is/located.csv",
                 stringsAsFactors = TRUE)

######## PLOT AND HIERARCHICALLY CLUSTER GLOMERULUS INNERVATION PATTERNS OF EACH MIPERGIC LN CLONE ##########################################################################
    # if you need to install, then run the following:
    # if (!requireNamespace("BiocManager", quietly = TRUE))
    #   install.packages("BiocManager")
    # BiocManager::install("ComplexHeatmap")

  #data wrangling
d2w.mat <- as.matrix(d2.wide[, 2:47])
rownames(d2w.mat) <- data2$reference

  #make it sexy...
cool_col = colorRamp2(c(1, 0), c("#BDBFC1", "#FFFFFF"))

  #make the heatmap + dendrograms in 1 go!
Heatmap(d2w.mat, 
        col = cool_col,
        na_col = "#FF0000",
        color_space = "RGB",
        border_gp = gpar(col = "#000000"),
        name = "MIPergic LN glomerular innervation", #title of legend
        column_title = "Glomerulus", row_title = "Individual MIPergic LNs",
        column_title_side = "bottom",
        cluster_rows = TRUE,
        cluster_row_slices = TRUE,
        clustering_distance_rows = "euclidean",
        clustering_method_rows = "ward.D2",
        row_names_gp = gpar(fontsize = 6), # Text size for row names
        row_dend_side = "right",
        cluster_columns = TRUE,
        cluster_column_slices = TRUE,
        clustering_distance_columns = "euclidean",
        clustering_method_columns = "ward.D2",
        column_dend_side = "top",
        column_names_side = "top",
        column_names_gp = gpar(fontsize = 6))







