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


####### LOAD DATA #########################################
R32F10phys_longformat <- read_excel("path/to/location/where/aggregated/R32F10/physiology/is/located.xlsx")

####### FIND PEAK RESPONSE AFTER ODOR ONSET #########################################
  ###calculate the peak response for each glomerulus-odor combination post-odor onset...
  ### in this example, we'll calculate the peak response to apple cider vinegar (ACV) for all our test glomeruli...
dm1 <- R32F10phys_longformat %>% 
  dplyr::filter(glomerulus =="DM1" & odor == "acv" & frame > 20) %>% 
  dplyr::group_by(animal) %>% 
  summarize(dm1.pkres = max(value))

#################### PLOT THE DATA ##########################################################################
  ###color palette options...
  ###   ...some possibilities you might consider include:
  ###   P10 = createPalette(10, c("#0D736B", "#348E53", "#EF6F6C"))
  ###   ...or...
  ### devtools::install_github("karthik/wesanderson")
  ### library(wesanderson)

  ###once you identify a palette you like, let's make the plot...
pkres.plot <- ggplot(PeakResponses_Concatenated, aes(glomerulus, value)) + 
  geom_boxplot(aes(color = odor,
                   fill = after_scale(desaturate(lighten(color, 0.6), 0.6))),
               size = 1,
               outlier.shape = NA) + 
  geom_dotplot(aes(color = odor), binaxis = "y", stackdir = "center", binpositions = "bygroup", dotsize = 0.25) + 
  geom_jitter(shape=16, position = position_jitter(0.2)) +
  scale_color_brewer(palette = "Dark2", guide = "legend") +
  labs(x = "Glomerulus", y = "Peak Response (dF/F)")
pkres.plot
