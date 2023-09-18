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
packages <- c("bigmemory", "parallel", "parallelly", "Rcpp", "RcppParallel",
              "RcppProgress", "readxl", "tidyverse", "lubridate", "magrittr", 
              "knitr",  "zoo", "mice", "magick", "afex", "fractal", "foreach",
              "tidyquant", "rstatix", "ggstatsplot", "ggsci")
ipak(packages)
rm(packages, ipak)

#### Determine the number of LNs in R32F10-GAL4 and -LexA, and how many are MIPergic #######
  ### load the data...
drivr.cc <- read_excel("path/to/location/where/aggregated/cell/counts/are/located.xlsx")
  ### establish each class appropriately...
drivr.cc$animal <- as.factor(drivr.cc$animal)
drivr.cc$mipir_32g4 <- as.numeric(drivr.cc$mipir_32g4)
drivr.cc$totlns_32g4 <- as.numeric(drivr.cc$totlns_32g4)
drivr.cc$totmiplns <- as.numeric(drivr.cc$totmiplns)
  ### compute summary stats...
R32F10_G4.summary <- drivr.cc %>% 
  dplyr::select(animal, mipir_32g4,totlns_32g4,totmiplns) %>% 
  dplyr::mutate(mean_mipir = mean(mipir_32g4, na.rm = T),
                mean_lns = mean(totlns_32g4, na.rm = T),
                mean_totmip = mean(totmiplns, na.rm = T)) %>% 
  tidyr::pivot_longer(.,
                      cols = mipir_32g4:mean_totmip,
                      names_to = "category",
                      values_to = "measurement") %>% 
  dplyr::group_by(category) %>% 
  

ave_mipir <- R32F10_G4.summary %>% 
  summarise(mean_mipir = mean(mipir_32g4, na.rm = T), n = n())
ave_lns <- R32F10_G4.summary %>% 
  summarise(mean_lns = mean(totlns_32g4, na.rm = T), n = n())
ave_mip <- R32F10_G4.summary %>% 
  summarise(mean_totmip = mean(totmiplns, na.rm = T), n = n())

remove(ave_mipir,ave_lns,ave_mip,R32F10.percentMIP,R32F10_G4.summary,drivr.cc, R32F10_g4.percMIP)

##### Determine if MIPergic LNs preferentially innervate glomeruli of a particular valence #####
  ### load the data...
Innervation_by_Valence <- read_excel("path/to/location/of/excel/file/with/innervation/frequency/and/putative/glomerulus/valence/is/located.xlsx")

normality <- Innervation_by_Valence %>% 
  group_by(valence) %>% 
  shapiro_test(innervation_freq)
data.frame(normality) 

Innervation_by_Valence %>% 
  group_by(valence) %>% 
  identify_outliers(innervation_freq)

  #TEST
Innervation_by_Valence$valence <- as.factor(Innervation_by_Valence$valence)
t.test(innervation_freq ~ valence, 
       alternative = "two.sided", 
       paired = FALSE, 
       data = Innervation_by_Valence) # Welch correction is default, unless you include "var.equal = FALSE"

  #LET'S TRY ANOTHER TEST...
ggstatsplot::ggbetweenstats(
  data = Innervation_by_Valence,
  x = valence,
  y = innervation_freq,
  type = "parametric",
  p.adjust.method = "holm",
  xlab = "Hedonic Valence",
  ylab = "MIP LN Clone Innervation Freq. (%)",
  title = "Freq. an Individual MIP LN Innervates a Given Glomerulus based on the Glomerulus' Valence",
  pairwise.display = "s",
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
  outlier.label = glomerulus
)

#BOTH TEST1 & 2 GIVE US p = 0.991! :)
remove(Innervation_by_Valence, normality)



#### Determine if MIP LNs preferentially innervate glomeruli that respond to certain types of odors #######
  ### load the data...
Innervation_by_FuncGroup <- read_excel("path/to/location/of/excel/file/with/innervation/frequency/and/the/functional/group/for/the/best/ligand/for/the/glomerulus'/odorant/receptor/is/located.xlsx")


Innervation_by_FuncGroup$funcgroup <- as.factor(Innervation_by_FuncGroup$funcgroup)
normality <- Innervation_by_FuncGroup %>% 
  group_by(funcgroup) %>% 
  shapiro_test(innervation_freq)
data.frame(normality)

Innervation_by_FuncGroup %>% 
  group_by(funcgroup) %>% 
  identify_outliers(innervation_freq)

#TEST
ggstatsplot::ggbetweenstats(
  data = Innervation_by_FuncGroup,
  x = funcgroup,
  y = innervation_freq,
  type = "nonparametric",
  p.adjust.method = "holm",
  xlab = "Odorant Functional Group",
  ylab = "MIP LN Clone Innervation Freq.",
  title = "Freq. an Individual MIP LN Innervates a Given Glomerulus based on the Glomerulus' Odor-Tuning",
  pairwise.display = "all",
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
  outlier.label = glomerulus
)

remove(normality, Innervation_by_FuncGroup)

######### Determine if SPRT2AG4 expression in antennae & maxillary palps differs across sex/mating status #########################################
  ###load the data...
SPR_T2A_G4_cc <- read_excel("path/to/location/where/aggregated/cell/counts/are/located.xlsx")

  ###separate the antennae and maxillary palp cell counts...
spr.ant <- SPR_T2A_G4_cc %>% 
  dplyr::filter(`cell type` == "antennae")

spr.palp <- SPR_T2A_G4_cc %>% 
  dplyr::filter(`cell type` == "maxpalp")

  ### ANTENNAE: 
spr.ant$sex_status <- as.factor(spr.ant$sex_status)
normality <- spr.ant %>% 
  dplyr::group_by(sex_status)
normality.2 <- shapiro_test(normality$value) # had to do it in multiple steps because RStudio was being finnicky...
data.frame(normality.2) 

spr.ant %>% 
  group_by(sex_status) %>% 
  identify_outliers(value) 

welch <- spr.ant %>% 
  welch_anova_test(value ~ sex_status)
welch

  ### PALP: 
spr.palp$sex_status <- as.factor(spr.palp$sex_status)
normality <- spr.palp %>% 
  dplyr::group_by(sex_status)
normality.2 <- shapiro_test(normality$value) # had to do it in multiple steps because RStudio was being finnicky...
data.frame(normality.2) 

spr.palp %>% 
  group_by(sex_status) %>% 
  identify_outliers(value)

res.kruskal <- spr.palp %>% 
  kruskal_test(value ~ sex_status)
res.kruskal

remove(normality,normality.2,spr.ant,spr.palp,SPR_T2A_G4_cc)


####### Determine whether SPRG4VP expression in ventral glutamatergic LNs differs across sex/mating status #################################################################################
  ###load the data...
SPR_G4VP_cc <- read_excel("path/to/location/where/aggregated/cell/counts/are/located.xlsx")

  ###select for the relevant data...
spr.glut <- SPR_G4VP_cc %>% 
  dplyr::filter(`cell type` == "glutLN") %>% 
  dplyr::select(sex_status, value)

spr.glut$sex_status <- as.factor(spr.glut$sex_status)
normality <- spr.glut %>% 
  dplyr::group_by(sex_status)
normality.2 <- shapiro_test(normality$value) # had to do it in multiple steps because RStudio was being finnicky...
data.frame(normality.2)

ggstatsplot::ggbetweenstats(
  data = spr.glut,
  x = sex_status,
  y = value,
  type = "nonparametric",
  p.adjust.method = "bonferroni",
  xlab = "Sex & Mating Status",
  ylab = "GFP-positive cells",
  title = "Number of SPRG4VP+GlutLN cells across Sexes/Mating Status",
  pairwise.display = "all",
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = FALSE
)

remove(normality,normality.2,spr.glut,SPR_G4VP_countsforR)