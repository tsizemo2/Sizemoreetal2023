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

#################### LOAD DATA ########################################################################################################################
pebg4.data <- read_excel("path/to/location/where/the/concatenated/pebGAL4/physio/data/is/located.xlsx")
pebg4_AUC.data <- read_excel("path/to/location/where/the/previously/computed/and/concatenated/pebGAL4/physio/auc/values/are/located.xlsx")

#################### PLOT WT OSN MEAN TRACES WITH +- SEM RIBBON ########################################################################################################################
  ###get the mean & sem for each glomerulus/concentration/treatment/frame combination...
pebg4.data$treatment <- factor(pebg4.data$treatment, levels = c("before", "during", "after"))
pebg4.sum <- pebg4.data %>% 
  group_by(glomerulus, odor, treatment, frame) %>% 
  group_modify(~ {
    .x %>% 
      get_summary_stats(value, type = "mean_se")
  })

  ###create before, during, and after plot for each glomerulus/concentration combination...  
  ###plot responses to 10^-2 apple cider vinegar (ACV)...
neg2.plots <- pebg4.data %>% 
  group_by(glomerulus, odor, treatment, frame) %>% 
  group_modify(~ {
    .x %>% 
      get_summary_stats(value, type = "mean_se")
  }) %>% 
  dplyr::filter(odor == "neg2") %>% 
  ggplot(., aes(x = frame,
                color = factor(treatment, levels = c("before","during","after")),
                group = factor(treatment, levels = c("before","during","after")))) +
  geom_line(aes(y = mean),
            linewidth=1.5) + 
  geom_ribbon(aes(ymin = mean-se,
                  ymax = mean+se,
                  fill = treatment),
              alpha=0.2) +
  facet_wrap(vars(treatment, glomerulus),
             nrow = 3,
             ncol = 5) + 
  geom_hline(yintercept = 0, lwd=0.5, color = 'black')
neg2.plots

neg2.plots_2 <- neg2.plots + scale_color_manual(values = c("gray", "magenta", "darkgray")) + 
  scale_fill_manual(values = c("gray", "magenta", "darkgray"))
neg2.plots_2

  ###plot responses to 10^-6 ACV...
neg6.plots <- pebg4.data %>% 
  group_by(glomerulus, odor, treatment, frame) %>% 
  group_modify(~ {
    .x %>% 
      get_summary_stats(value, type = "mean_se")
  }) %>% 
  dplyr::filter(odor == "neg6") %>% 
  ggplot(., aes(x = frame,
                color = factor(treatment, levels = c("before","during","after")),
                group = factor(treatment, levels = c("before","during","after")))) +
  geom_line(aes(y = mean),
            linewidth=1.5) + 
  geom_ribbon(aes(ymin = mean-se,
                  ymax = mean+se,
                  fill = treatment),
              alpha=0.2) +
  facet_wrap(vars(treatment, glomerulus),
             nrow = 3,
             ncol = 5) + 
  geom_hline(yintercept = 0, lwd=0.5, color = 'black')
neg6.plots

neg6.plots_2 <- neg6.plots + scale_color_manual(values = c("gray", "magenta", "darkgray")) + 
  scale_fill_manual(values = c("gray", "magenta", "darkgray"))
neg6.plots_2

  ###plot everything together...
neg2_physio <- ggarrange(neg2.plots_2 + rremove("legend"))
neg6_physio <- ggarrange(neg6.plots_2 + rremove("legend"))
phys.plots <- grid.arrange(neg2_physio,
                           neg6_physio)
annotate_figure(phys.plots, left = text_grob("OSN Responses (dF/F)",
                                             rot = 90, vjust = 1),
                bottom = textGrob("Time (frame #)"))



#################### PLOT WT PEB-GAL4 AUC VALUES AS BOXPLOTS ########################################################################################################################
  ###plot AUC values for responses to 10^-2 ACV...
pebg4_AUC.data %>% 
  dplyr::filter(odor == "neg2") %>% 
  rstatix::group_by(glomerulus, treatment) %>% 
  ggplot(aes(x = glomerulus,
             y = measurement,
             fill = forcats::fct_inorder(treatment))) + 
  geom_boxplot(outlier.size = 0.75,
               outlier.colour = NULL)

  ###plot AUC values for responses to 10^-6 ACV...
pebg4_AUC.data %>% 
  dplyr::filter(odor == "neg6") %>% 
  rstatix::group_by(glomerulus, treatment) %>% 
  ggplot(aes(x = glomerulus,
             y = measurement,
             fill = forcats::fct_inorder(treatment))) + 
  geom_boxplot(outlier.size = 0.75,
               outlier.colour = NULL)


#################### CALCULATE MIP APPLICATION EFFECT SIZE ON PEB-GAL4 OSN RESPONSES ########################################################################################################################
  ###this example calculates effect size of MIP application treatment on DM2 responses to 10-2 ACV...
pebg4_AUC.data$treatment <- as.factor(pebg4_AUC.data$treatment)
dm2_neg2.effsize <- pebg4_AUC.data %>%
  dplyr::filter(odor == "neg2", glomerulus == "dm2") %>% 
  friedman_effsize(measurement ~ treatment |animal)


