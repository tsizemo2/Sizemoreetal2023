#################### LOAD PACKAGES ########################################################################################################################
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


############## LOAD DATA ################################################################################################################################
ConcatinatedData <- read_excel("path/to/where/aggregated/R32F10/phys/is/located.xlsx")

########################## PLOT ACV TRACES... ########################################################
  ###plot them all on the same plot, so that they all have the same size y-axis!!!
  ###all of these expts. were conducted using 1p, so we can just pull the data from
  ###the ConcatinatedData file we initially loaded up...
ACV_plot <- ConcatinatedData %>% 
  filter(odor == "acv") %>% 
  dplyr::select(frame, glomerulus, animal, value) %>% 
  group_by(frame, glomerulus) %>%
  get_summary_stats(value, type = "mean_se") %>% 
  group_by(glomerulus) %>% 
  ggplot(aes(x = frame, y = mean, colour = glomerulus)) + 
  geom_line(lwd=0.8) + 
  geom_ribbon(aes(ymin = mean-se, ymax = mean+se),alpha = 0.2) + 
  geom_hline(yintercept = 0, lwd = 0.5, color = 'black') + 
  theme_gray() +
  theme(panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.minor = element_line(color = "white", size = 0.25))
ACV_plot


##################### PLOT 1-HEXANOL TRACES... ########################################################
dm1.hex <- dm1_join.2 %>%
  dplyr::filter(odor == "1hex") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm2.hex <- dm2_join.2 %>%
  dplyr::filter(odor == "1hex") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm4.hex <- dm4_join.2 %>%
  dplyr::filter(odor == "1hex") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm5.hex <- dm5_join.2 %>%
  dplyr::filter(odor == "1hex") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1l.hex <- dp1l_join.2 %>%
  dplyr::filter(odor == "1hex") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1m.hex <- dp1m_join.2 %>%
  dplyr::filter(odor == "1hex") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

va2.hex <- va2_join.2 %>%
  dplyr::filter(odor == "1hex") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

vm2.hex <- vm2_join.2 %>%
  dplyr::filter(odor == "1hex") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

hex.merged <- list(dm1.hex,dm2.hex,dm4.hex,dm5.hex,dp1l.hex, dp1m.hex, va2.hex, vm2.hex) %>% 
  bind_rows(.id = "glomerulus")
hex.merged$glomerulus <- c(rep("dm1", 40),rep("dm2", 40),rep("dm4", 40),rep("dm5", 40),rep("dp1l", 40),rep("dp1m", 40),rep("va2", 40),rep("vm2", 40))

  ### plot so that all glomeruli have the same y-axis...
HEX_plot <- hex.merged %>% 
  ggplot(aes(x = frame, y = mean, colour = glomerulus)) + 
  geom_line(lwd=0.8) + 
  geom_ribbon(aes(ymin = mean-se, ymax = mean+se),alpha = 0.2) + 
  geom_hline(yintercept = 0, lwd = 0.5, color = 'black') + 
  theme_gray() +
  theme(panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.minor = element_line(color = "white", size = 0.25))
HEX_plot


remove(dm1.hex,dm2.hex,dm4.hex,dm5.hex,dp1l.hex,dp1m.hex,va2.hex,vm2.hex, hex.merged)

########################## PLOT 1-OCTEN-3-OL TRACES... ########################################################
dm1.oct <- dm1_join.2 %>%
  dplyr::filter(odor == "1octen3ol") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm2.oct <- dm2_join.2 %>%
  dplyr::filter(odor == "1octen3ol") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm4.oct <- dm4_join.2 %>%
  dplyr::filter(odor == "1octen3ol") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm5.oct <- dm5_join.2 %>%
  dplyr::filter(odor == "1octen3ol") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1l.oct <- dp1l_join.2 %>%
  dplyr::filter(odor == "1octen3ol") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1m.oct <- dp1m_join.2 %>%
  dplyr::filter(odor == "1octen3ol") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

va2.oct <- va2_join.2 %>%
  dplyr::filter(odor == "1octen3ol") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

vm2.oct <- vm2_join.2 %>%
  dplyr::filter(odor == "1octen3ol") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)


oct.merged <- list(dm1.oct,dm2.oct,dm4.oct,dm5.oct,dp1l.oct, dp1m.oct, va2.oct, vm2.oct) %>% 
  bind_rows(.id = "glomerulus")
oct.merged$glomerulus <- c(rep("dm1", 40),rep("dm2", 40),rep("dm4", 40),rep("dm5", 40),rep("dp1l", 40),rep("dp1m", 40),rep("va2", 40),rep("vm2", 40))

  ### plot so that all glomeruli have the same y-axis...
OCT_plot <- oct.merged %>% 
  ggplot(aes(x = frame, y = mean, colour = glomerulus)) + 
  geom_line(lwd=0.8) + 
  geom_ribbon(aes(ymin = mean-se, ymax = mean+se),alpha = 0.2) + 
  geom_hline(yintercept = 0, lwd = 0.5, color = 'black') + 
  theme_gray() +
  theme(panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.minor = element_line(color = "white", size = 0.25))
OCT_plot

remove(dm1.oct,dm2.oct,dm4.oct,dm5.oct,dp1l.oct,dp1m.oct,va2.oct,vm2.oct, oct.merged)

##################### PLOT BENZALDEHYDE TRACES... ########################################################
dm1.benz <- dm1_join.2 %>%
  dplyr::filter(odor == "benz") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm2.benz <- dm2_join.2 %>%
  dplyr::filter(odor == "benz") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm4.benz <- dm4_join.2 %>%
  dplyr::filter(odor == "benz") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm5.benz <- dm5_join.2 %>%
  dplyr::filter(odor == "benz") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1l.benz <- dp1l_join.2 %>%
  dplyr::filter(odor == "benz") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1m.benz <- dp1m_join.2 %>%
  dplyr::filter(odor == "benz") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

va2.benz <- va2_join.2 %>%
  dplyr::filter(odor == "benz") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

vm2.benz <- vm2_join.2 %>%
  dplyr::filter(odor == "benz") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

benz.merged <- list(dm1.benz,dm2.benz,dm4.benz,dm5.benz,dp1l.benz, dp1m.benz, va2.benz, vm2.benz) %>% 
  bind_rows(.id = "glomerulus")
benz.merged$glomerulus <- c(rep("dm1", 40),rep("dm2", 40),rep("dm4", 40),rep("dm5", 40),rep("dp1l", 40),rep("dp1m", 40),rep("va2", 40),rep("vm2", 40))

  ### plot so that all glomeruli have the same y-axis...
BENZ_plot <- benz.merged %>% 
  ggplot(aes(x = frame, y = mean, colour = glomerulus)) + 
  geom_line(lwd=0.8) + 
  geom_ribbon(aes(ymin = mean-se, ymax = mean+se),alpha = 0.2) + 
  geom_hline(yintercept = 0, lwd = 0.5, color = 'black') + 
  theme_gray() +
  theme(panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.minor = element_line(color = "white", size = 0.25))
BENZ_plot

remove(dm1.benz,dm2.benz,dm4.benz,dm5.benz,dp1l.benz,dp1m.benz,va2.benz,vm2.benz, benz.merged)


########################## PLOT GERANYL ACETATE TRACES... ########################################################
dm1.gace <- dm1_join.2 %>%
  dplyr::filter(odor == "gerace") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm2.gace <- dm2_join.2 %>%
  dplyr::filter(odor == "gerace") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm4.gace <- dm4_join.2 %>%
  dplyr::filter(odor == "gerace") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm5.gace <- dm5_join.2 %>%
  dplyr::filter(odor == "gerace") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1l.gace <- dp1l_join.2 %>%
  dplyr::filter(odor == "gerace") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1m.gace <- dp1m_join.2 %>%
  dplyr::filter(odor == "gerace") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

va2.gace <- va2_join.2 %>%
  dplyr::filter(odor == "gerace") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

vc1.gace <- vc1_TS %>%
  dplyr::filter(odor == "gerace") %>%
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

vm2.gace <- vm2_join.2 %>%
  dplyr::filter(odor == "gerace") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

gerace.merged <- list(dm1.gace,dm2.gace,dm4.gace,dm5.gace,dp1l.gace, dp1m.gace, va2.gace, vc1.gace, vm2.gace) %>% 
  bind_rows(.id = "glomerulus")
gerace.merged$glomerulus <- c(rep("dm1", 40),rep("dm2", 40),rep("dm4", 40),rep("dm5", 40),rep("dp1l", 40),rep("dp1m", 40),rep("va2", 40),rep("vc1", 40),rep("vm2", 40))

  ### plot so that all glomeruli have the same y-axis...
GERACE_plot <- gerace.merged %>% 
  ggplot(aes(x = frame, y = mean, colour = glomerulus)) + 
  geom_line(lwd=0.8) + 
  geom_ribbon(aes(ymin = mean-se, ymax = mean+se),alpha = 0.2) + 
  geom_hline(yintercept = 0, lwd = 0.5, color = 'black') + 
  theme_gray() +
  theme(panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.minor = element_line(color = "white", size = 0.25))
GERACE_plot

remove(dm1.gace, dm2.gace,dm4.gace,dm5.gace,dp1l.gace,dp1m.gace,va2.gace,vc1.gace,vm2.gace,gerace.merged)


##################### PLOT AMMONIUM HYDROXIDE TRACES... ########################################################
dm1.noh <- dm1_join.2 %>%
  dplyr::filter(odor == "nh4oh") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm2.noh <- dm2_join.2 %>%
  dplyr::filter(odor == "nh4oh") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm4.noh <- dm4_join.2 %>%
  dplyr::filter(odor == "nh4oh") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dm5.noh <- dm5_join.2 %>%
  dplyr::filter(odor == "nh4oh") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1l.noh <- dp1l_join.2 %>%
  dplyr::filter(odor == "nh4oh") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

dp1m.noh <- dp1m_join.2 %>%
  dplyr::filter(odor == "nh4oh") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

va2.noh <- va2_join.2 %>%
  dplyr::filter(odor == "nh4oh") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

va3.noh <- va3_TS %>%
  dplyr::filter(odor == "nh4oh") %>%
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

vc1.noh <- vc1_TS %>%
  dplyr::filter(odor == "nh4oh") %>%
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

vm2.noh <- vm2_join.2 %>%
  dplyr::filter(odor == "nh4oh") %>% 
  group_by(frame) %>%
  get_summary_stats(value, type = "mean_se") %>%
  dplyr::select(frame, mean, se)

noh.merged <- list(dm1.noh,dm2.noh,dm4.noh,dm5.noh,dp1l.noh, dp1m.noh, va2.noh, va3.noh, vc1.noh, vm2.noh) %>% 
  bind_rows(.id = "glomerulus")
noh.merged$glomerulus <- c(rep("dm1", 40),rep("dm2", 40),rep("dm4", 40),rep("dm5", 40),rep("dp1l", 40),rep("dp1m", 40),rep("va2", 40),rep("va3", 40),rep("vc1", 40),rep("vm2", 40))

  ### plot so that all glomeruli have the same y-axis...
NH4OH_plot <- noh.merged %>% 
  ggplot(aes(x = frame, y = mean, colour = glomerulus)) + 
  geom_line(lwd=0.8) + 
  geom_ribbon(aes(ymin = mean-se, ymax = mean+se),alpha = 0.2) + 
  geom_hline(yintercept = 0, lwd = 0.5, color = 'black') + 
  theme_gray() +
  theme(panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.minor = element_line(color = "white", size = 0.25))
NH4OH_plot

remove(dm1.noh,dm2.noh,dm4.noh,dm5.noh,dp1l.noh,dp1m.noh,va2.noh,va3.noh,vc1.noh,vm2.noh,noh.merged)


########################## MERGE THE PLOTS TOGETHER: VOLTRON-STYLE ########################################################
phys.plots <- ggarrange(ACV_plot,
                        BENZ_plot,
                        OCT_plot,
                        HEX_plot,
                        GERACE_plot,
                        NH4OH_plot,
                        ncol = 1,
                        nrow = 6,
                        labels = NULL,
                        common.legend = T,
                        align = "hv")
phys.plots
