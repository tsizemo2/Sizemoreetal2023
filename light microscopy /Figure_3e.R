######## LOAD PACKAGES ###############
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
  # usage
pkgs <- c("bigmemory", "parallel", "parallelly", "tidyverse", "readxl",
              "rstatix", "ggplot2", "ggthemes", "ggstance", "ggsci", "ggthemes",
              "ggpubr", "ggforestplot", "ggrepel", "GGally")
ipak(pkgs)
rm(pkgs, ipak)

######## LOAD DATA ###############
ConcatenatedPunctaDensityData <- read_excel("path/to/where/concatenated/puncta/density/file/is/located.xlsx")

######## DATA WRANGLING ###############
ConcatenatedPunctaDensityData$animal <- as.factor(ConcatenatedPunctaDensityData$animal)
ConcatenatedPunctaDensityData$glomerulus <- as.factor(ConcatenatedPunctaDensityData$glomerulus)
ConcatenatedPunctaDensityData$marker <- as.factor(ConcatenatedPunctaDensityData$marker)

df <- ConcatenatedPunctaDensityData %>% 
  group_by(marker, glomerulus) %>% 
  get_summary_stats(value, type = "mean_se")

######## MAKE THE PLOT ###############
#making a horizontal line at the desired location along the y-axis...
h <- 0.5

#making the plot itself...
g <- ggplot(data = df, aes(x = glomerulus, y = mean, group = glomerulus, color = marker)) + 
  geom_pointrange(aes(ymin = mean-se, ymax = mean+se, shape = marker), 
                  position = position_jitterdodge(jitter.width = 0.8, dodge.width = 0.9)) + 
  geom_hline(aes(yintercept = h,
                 linetype = "dashed",
                 colour = "black")) +
  labs(y = "effector (or anti-MIP) / voxel (normalized)") + 
  theme_bw()

g2 <- g + 
  theme(panel.grid.major.x = element_line(size = 5, colour = "#D3D3D3"))

#let's see what we made...
g2