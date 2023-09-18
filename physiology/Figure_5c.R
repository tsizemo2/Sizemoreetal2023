#################### LOAD PACKAGES ##########################################################################
### ipak function: install and load multiple R packages.
### check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
###usage...
pkgs <- c("readxl", "xlsx", "tidyverse", "lubridate", "magrittr","knitr", "magick", "afex", "foreach","tidyquant", "rstatix", 
          "ggstatsplot", "ggsci", "git2r", "RColorBrewer", "crayon", "shape", "paletteer", "colorspace", "gcookbook", "ggpubr", 
          "ggrepel", "ggthemes", "ggstatsplot", "ggforce", "ggsignif", "ggtext", "ggdist", "patchwork", "ggthemes", "extrafont",
          "reshape2", "grid", "gridExtra", "cowplot", "Polychrome", "scico", "Bolstad2")
ipak(pkgs)

###you might run into the "No Font Name. Skipping." error, like I did some time ago...
### ...if so, then:
### library(remotes)
### remotes::install_version("Rttf2pt1", version = "1.3.8")

rm(pkgs, ipak)


########## LOAD DATA ######################################################################################################################################
ConcatinatedData <- read_excel("path/to/location/where/aggregated/R32F10/physiology/is/located.xlsx")

################### CALCULATE ABSOLUTE AREA UNDER THE CURVE (AUC) ####################################################################################################################
  ###NOW, let's compute the absolute area under the curve (or, the area between the curve and the origin) using Simpson's rule
  ###   for all glomeruli-odor combinations (in this example, let's do it for the DM1 glomerulus & ACV)...

  ###..., but first we need to make sure our values are indeed considered numeric (as opposed to character, or factor, etc.)...
dm1.acv$value <- as.numeric(dm1.acv$value)

  ###the fun stuff...
dm1_acv.auc = dm1.acv %>% 
  dplyr::select(animal, value, time.sec) %>% 
  dplyr::group_by(animal) %>% 
  Bolstad2::sintegral(
    x = time.sec,
    fx = value,
    n.pts = 256
  )

  ###save for later reference...
write.xlsx2(dm1_acv.auc, file = "DM1_R32F10_ACV_SimpsonsAUC.xlsx")

################## PLOT THE DATA #################################################################
  ###once you've performed the steps above for all odors and all glomeruli, then you're ready to concatenate each respective dataframe together
  ### for plotting...
auc.plot <- ggplot(AUC_Concatenated, aes(glomerulus, auc_value)) + 
  geom_boxplot(aes(color = odor,
                   fill = after_scale(desaturate(lighten(color, 0.6), 0.6))),
               size = 1,
               outlier.shape = NA) + 
  geom_dotplot(aes(color = odor), binaxis = "y", stackdir = "center", binpositions = "bygroup", dotsize = 0.25) + 
  geom_jitter(shape=16, position = position_jitter(0.2)) +
  scale_color_brewer(palette = "Dark2", guide = "legend") +
  labs(x = "Glomerulus", y = "Area Under the dF/F Curve")
auc.plot
