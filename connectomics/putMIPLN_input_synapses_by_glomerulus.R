#################### LOAD PACKAGES ##########################################################################
library(googlesheets4) # to get around a weird conflict between 'googlesheets4' and 'hemibrainr', load 'googlesheets4' prior to loading 'hemibrainr'...
gs4_auth()
library(googledrive)

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

############ DETERMINE WHICH GLOMERULI A GIVEN putMIP LN *INPUT* SYNAPSE IS LOCATED  ##########################################
  ###fetch the neurons of interest...
miplns = c("1639886198", "1640555311", "1670916819", "1671257931",
           "1701947337", "1762704980", "1857799548", "1858171556",
           "1946178096", "2041621893", "2105086391", "2135408892",
           "5813039309", "5813132949")
miplns.neurons = neuprint_read_neurons(bodyids = miplns,
                               heal.threshold = 20e3)

  ###subset each putMIP LNs' connectors from the neurons we just read in...
mipln1.conns = miplns.neurons[["1639886198"]][["connectors"]]
mipln2.conns = miplns.neurons[["1640555311"]][["connectors"]]
mipln3.conns = miplns.neurons[["1670916819"]][["connectors"]]
mipln4.conns = miplns.neurons[["1671257931"]][["connectors"]]
mipln5.conns = miplns.neurons[["1701947337"]][["connectors"]]
mipln6.conns = miplns.neurons[["1762704980"]][["connectors"]]
mipln7.conns = miplns.neurons[["1857799548"]][["connectors"]]
mipln8.conns = miplns.neurons[["1858171556"]][["connectors"]]
mipln9.conns = miplns.neurons[["1946178096"]][["connectors"]]
mipln10.conns = miplns.neurons[["2041621893"]][["connectors"]]
mipln11.conns = miplns.neurons[["2105086391"]][["connectors"]]
mipln12.conns = miplns.neurons[["2135408892"]][["connectors"]]
mipln13.conns = miplns.neurons[["5813039309"]][["connectors"]]
mipln14.conns = miplns.neurons[["5813132949"]][["connectors"]]

  ###now, let's get some antennal lobe (AL) glomeruli...
  ###the Cambridge group have made a wonderful AL mesh based on the AL PN dendrites in the hemibrain, so let's use that...
D <- subset(hemibrain_al.surf,"D")
DA1 <- subset(hemibrain_al.surf, "DA1")
DA2 <- subset(hemibrain_al.surf, "DA2")
DA3 <- subset(hemibrain_al.surf, "DA3")
DA4l <- subset(hemibrain_al.surf,"DA4l")
DA4m <- subset(hemibrain_al.surf,"DA4m")
DC1 <- subset(hemibrain_al.surf, "DC1")
DC2 <- subset(hemibrain_al.surf, "DC2")
DC3 <- subset(hemibrain_al.surf, "DC3")
DC4 <- subset(hemibrain_al.surf, "DC4")
DL1 <- subset(hemibrain_al.surf, "DL1")
DL2d <- subset(hemibrain_al.surf,"DL2d")
DL2v <- subset(hemibrain_al.surf,"DL2v")
DL3 <- subset(hemibrain_al.surf, "DL3")
DL4 <- subset(hemibrain_al.surf, "DL4")
DL5 <- subset(hemibrain_al.surf, "DL5")
DM1 <- subset(hemibrain_al.surf, "DM1")
DM2 <- subset(hemibrain_al.surf, "DM2")
DM3 <- subset(hemibrain_al.surf, "DM3")
DM4 <- subset(hemibrain_al.surf, "DM4")
DM5 <- subset(hemibrain_al.surf, "DM5")
DM6 <- subset(hemibrain_al.surf, "DM6")
DP1l <- subset(hemibrain_al.surf,"DP1l")
DP1m <- subset(hemibrain_al.surf,"DP1m")
V <- subset(hemibrain_al.surf, "V")
VA1d <- subset(hemibrain_al.surf,"VA1d")
VA1v <- subset(hemibrain_al.surf,"VA1v")
VA2 <- subset(hemibrain_al.surf, "VA2")
VA3 <- subset(hemibrain_al.surf, "VA3")
VA4 <- subset(hemibrain_al.surf, "VA4")
VA5 <- subset(hemibrain_al.surf, "VA5")
VA6 <- subset(hemibrain_al.surf, "VA6")
VA7l <- subset(hemibrain_al.surf,"VA7l")
VA7m <- subset(hemibrain_al.surf,"VA7m")
VC1 <- subset(hemibrain_al.surf, "VC1")
VC2 <- subset(hemibrain_al.surf, "VC2")
VC3 <- subset(hemibrain_al.surf, "VC3")
VC4 <- subset(hemibrain_al.surf, "VC4")
VC5 <- subset(hemibrain_al.surf, "VC5")
VL1 <- subset(hemibrain_al.surf, "VL1")
VL2a <- subset(hemibrain_al.surf, "VL2a")
VL2p <- subset(hemibrain_al.surf, "VL2p")
VM1 <- subset(hemibrain_al.surf, "VM1")
VM2 <- subset(hemibrain_al.surf, "VM2")
VM3 <- subset(hemibrain_al.surf, "VM3")
VM4 <- subset(hemibrain_al.surf, "VM4")
VM5d <- subset(hemibrain_al.surf, "VM5d")
VM5v <- subset(hemibrain_al.surf, "VM5v")
VM6 <- subset(hemibrain_al.surf, "VM6")
VM7d <- subset(hemibrain_al.surf, "VM7d")
VM7v <- subset(hemibrain_al.surf, "VM7v")
VP1d <- subset(hemibrain_al.surf, "VP1d")
VP1l <- subset(hemibrain_al.surf, "VP1l")
VP1m <- subset(hemibrain_al.surf, "VP1m")
VP2 <- subset(hemibrain_al.surf, "VP2")
VP3 <- subset(hemibrain_al.surf, "VP3")
VP5 <- subset(hemibrain_al.surf, "VP5")
VP5 <- subset(hemibrain_al.surf, "VP5")

  ###now, you can select only those synapses within the given glomerulus being tested...
mipln1.conns$inD = select(mipln1.conns, x:z) %>%
  pointsinside(surf = D)
mipln1.conns %>% 
  filter(inD) -> 
  mipln1.conns.D

mipln1.conns$inDA1 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DA1)
mipln1.conns %>% 
  filter(inDA1) -> 
  mipln1.conns.DA1

mipln1.conns$inDA2 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DA2)
mipln1.conns %>% 
  filter(inDA2) -> 
  mipln1.conns.DA2

mipln1.conns$inDA3 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DA3)
mipln1.conns %>% 
  filter(inDA3) -> 
  mipln1.conns.DA3

mipln1.conns$inDA4l = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DA4l)
mipln1.conns %>% 
  filter(inDA4l) -> 
  mipln1.conns.DA4l

mipln1.conns$inDA4m = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DA4m)
mipln1.conns %>% 
  filter(inDA4m) -> 
  mipln1.conns.DA4m

mipln1.conns$inDC1 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DC1)
mipln1.conns %>% 
  filter(inDC1) -> 
  mipln1.conns.DC1

mipln1.conns$inDC2 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DC2)
mipln1.conns %>% 
  filter(inDC2) -> 
  mipln1.conns.DC2

mipln1.conns$inDC3 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DC3)
mipln1.conns %>% 
  filter(inDC3) -> 
  mipln1.conns.DC3

mipln1.conns$inDC4 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DC4)
mipln1.conns %>% 
  filter(inDC4) -> 
  mipln1.conns.DC4

mipln1.conns$inDL1 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DL1)
mipln1.conns %>% 
  filter(inDL1) -> 
  mipln1.conns.DL1

mipln1.conns$inDL2d = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DL2d)
mipln1.conns %>% 
  filter(inDL2d) -> 
  mipln1.conns.DL2d

mipln1.conns$inDL2v = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DL2v)
mipln1.conns %>% 
  filter(inDL2v) -> 
  mipln1.conns.DL2v

mipln1.conns$inDL3 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DL3)
mipln1.conns %>% 
  filter(inDL3) -> 
  mipln1.conns.DL3

mipln1.conns$inDL4 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DL4)
mipln1.conns %>% 
  filter(inDL4) -> 
  mipln1.conns.DL4

mipln1.conns$inDL5 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DL5)
mipln1.conns %>% 
  filter(inDL5) -> 
  mipln1.conns.DL5

mipln1.conns$inDM1 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DM1)
mipln1.conns %>% 
  filter(inDM1) -> 
  mipln1.conns.DM1

mipln1.conns$inDM2 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DM2)
mipln1.conns %>% 
  filter(inDM2) -> 
  mipln1.conns.DM2

mipln1.conns$inDM3 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DM3)
mipln1.conns %>% 
  filter(inDM3) -> 
  mipln1.conns.DM3

mipln1.conns$inDM4 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DM4)
mipln1.conns %>% 
  filter(inDM4) -> 
  mipln1.conns.DM4

mipln1.conns$inDM5 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DM5)
mipln1.conns %>% 
  filter(inDM5) -> 
  mipln1.conns.DM5

mipln1.conns$inDM6 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DM6)
mipln1.conns %>% 
  filter(inDM6) -> 
  mipln1.conns.DM6

mipln1.conns$inDP1l = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DP1l)
mipln1.conns %>% 
  filter(inDP1l) -> 
  mipln1.conns.DP1l

mipln1.conns$inDP1m = select(mipln1.conns, x:z) %>%
  pointsinside(surf = DP1m)
mipln1.conns %>% 
  filter(inDP1m) -> 
  mipln1.conns.DP1m

mipln1.conns$inV = select(mipln1.conns, x:z) %>%
  pointsinside(surf = V)
mipln1.conns %>% 
  filter(inV) -> 
  mipln1.conns.V

mipln1.conns$inVA1d = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA1d)
mipln1.conns %>% 
  filter(inVA1d) -> 
  mipln1.conns.VA1d

mipln1.conns$inVA1v = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA1v)
mipln1.conns %>% 
  filter(inVA1v) -> 
  mipln1.conns.VA1v

mipln1.conns$inVA2 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA2)
mipln1.conns %>% 
  filter(inVA2) -> 
  mipln1.conns.VA2

mipln1.conns$inVA3 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA3)
mipln1.conns %>% 
  filter(inVA3) -> 
  mipln1.conns.VA3

mipln1.conns$inVA4 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA4)
mipln1.conns %>% 
  filter(inVA4) -> 
  mipln1.conns.VA4

mipln1.conns$inVA5 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA5)
mipln1.conns %>% 
  filter(inVA5) -> 
  mipln1.conns.VA5

mipln1.conns$inVA6 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA6)
mipln1.conns %>% 
  filter(inVA6) -> 
  mipln1.conns.VA6

mipln1.conns$inVA7l = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA7l)
mipln1.conns %>% 
  filter(inVA7l) -> 
  mipln1.conns.VA7l

mipln1.conns$inVA7m = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VA7m)
mipln1.conns %>% 
  filter(inVA7m) -> 
  mipln1.conns.VA7m

mipln1.conns$inVC1 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VC1)
mipln1.conns %>% 
  filter(inVC1) -> 
  mipln1.conns.VC1

mipln1.conns$inVC2 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VC2)
mipln1.conns %>% 
  filter(inVC2) -> 
  mipln1.conns.VC2

mipln1.conns$inVC3 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VC3)
mipln1.conns %>% 
  filter(inVC3) -> 
  mipln1.conns.VC3

mipln1.conns$inVC4 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VC4)
mipln1.conns %>% 
  filter(inVC4) -> 
  mipln1.conns.VC4

mipln1.conns$inVC5 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VC5)
mipln1.conns %>% 
  filter(inVC5) -> 
  mipln1.conns.VC5

mipln1.conns$inVL1 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VL1)
mipln1.conns %>% 
  filter(inVL1) -> 
  mipln1.conns.VL1

mipln1.conns$inVL2a = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VL2a)
mipln1.conns %>% 
  filter(inVL2a) -> 
  mipln1.conns.VL2a

mipln1.conns$inVL2p = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VL2p)
mipln1.conns %>% 
  filter(inVL2p) -> 
  mipln1.conns.VL2p

mipln1.conns$inVM1 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM1)
mipln1.conns %>% 
  filter(inVM1) -> 
  mipln1.conns.VM1

mipln1.conns$inVM2 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM2)
mipln1.conns %>% 
  filter(inVM2) -> 
  mipln1.conns.VM2

mipln1.conns$inVM3 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM3)
mipln1.conns %>% 
  filter(inVM3) -> 
  mipln1.conns.VM3

mipln1.conns$inVM4 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM4)
mipln1.conns %>% 
  filter(inVM4) -> 
  mipln1.conns.VM4

mipln1.conns$inVM5d = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM5d)
mipln1.conns %>% 
  filter(inVM5d) -> 
  mipln1.conns.VM5d

mipln1.conns$inVM5v = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM5v)
mipln1.conns %>% 
  filter(inVM5v) -> 
  mipln1.conns.VM5v

mipln1.conns$inVM6 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM6)
mipln1.conns %>% 
  filter(inVM6) -> 
  mipln1.conns.VM6

mipln1.conns$inVM7d = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM7d)
mipln1.conns %>% 
  filter(inVM7d) -> 
  mipln1.conns.VM7d

mipln1.conns$inVM7v = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VM7v)
mipln1.conns %>% 
  filter(inVM7v) -> 
  mipln1.conns.VM7v

mipln1.conns$inVP1d = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VP1d)
mipln1.conns %>% 
  filter(inVP1d) -> 
  mipln1.conns.VP1d

mipln1.conns$inVP1l = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VP1l)
mipln1.conns %>% 
  filter(inVP1l) -> 
  mipln1.conns.VP1l

mipln1.conns$inVP1m = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VP1m)
mipln1.conns %>% 
  filter(inVP1m) -> 
  mipln1.conns.VP1m

mipln1.conns$inVP2 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VP2)
mipln1.conns %>% 
  filter(inVP2) -> 
  mipln1.conns.VP2

mipln1.conns$inVP3 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VP3)
mipln1.conns %>% 
  filter(inVP3) -> 
  mipln1.conns.VP3

mipln1.conns$inVP5 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VP5)
mipln1.conns %>% 
  filter(inVP5) -> 
  mipln1.conns.VP5

mipln1.conns$inVP5 = select(mipln1.conns, x:z) %>%
  pointsinside(surf = VP5)
mipln1.conns %>% 
  filter(inVP5) -> 
  mipln1.conns.VP5


### 5) NOW, WHETHER A POINT FALLS WITHIN A GIVEN GLOMERULUS SHOULD NOW BE INCLUDED IN THE ORIGINAL
###     ".conn" DATAFRAME FOR THE GIVEN putMIPLN YOU JUST ANALYZED (YAY!). 
###     THAT MEANS WE CAN CLEAR OUT EACH GLOMERULUS' INDIVIDUAL DATAFRAME FROM THE ENVIRONMENT...
remove(mipln1.conns.D,mipln1.conns.DA1,mipln1.conns.DA2,mipln1.conns.DA3,mipln1.conns.DA4l,mipln1.conns.DA4m,mipln1.conns.DC1,mipln1.conns.DC2,mipln1.conns.DC3,mipln1.conns.DC4,mipln1.conns.DL1,mipln1.conns.DL2d,mipln1.conns.DL2v,mipln1.conns.DL3,mipln1.conns.DL4,mipln1.conns.DL5,mipln1.conns.DM1,mipln1.conns.DM2,mipln1.conns.DM3,mipln1.conns.DM4,mipln1.conns.DM5,mipln1.conns.DM6,mipln1.conns.DP1l,mipln1.conns.DP1m,mipln1.conns.V,mipln1.conns.VA1d,mipln1.conns.VA1v,mipln1.conns.VA2,mipln1.conns.VA3,mipln1.conns.VA4,mipln1.conns.VA5,mipln1.conns.VA6,mipln1.conns.VA7l,mipln1.conns.VA7m,mipln1.conns.VC1,mipln1.conns.VC2,mipln1.conns.VC3,mipln1.conns.VC4,mipln1.conns.VC5,mipln1.conns.VL1,mipln1.conns.VL2a,mipln1.conns.VL2p,mipln1.conns.VM1,mipln1.conns.VM2,mipln1.conns.VM3,mipln1.conns.VM4,mipln1.conns.VM5d,mipln1.conns.VM5v,mipln1.conns.VM6,mipln1.conns.VM7d,mipln1.conns.VM7v,mipln1.conns.VP1d,mipln1.conns.VP1l,mipln1.conns.VP1m,mipln1.conns.VP2,mipln1.conns.VP3,mipln1.conns.VP5,mipln1.conns.VP5)


  ### 6) I have included the 'screeningdf' with this repo so we can identify what identified synaptic 
  ###     partner constitutes each putMIP LN's intra-glomerular point. You can load it now...
  ### NOTE: I got the list of 'modneurons' from the full list of neurons that connect with each putMIP LN
  ### (i.e., see "IDENTIFY NEURAL INPUTS TO putMIP LNs (prepost = 'PRE')_Demographics Analysis" above!)
screeningdf <- readxl::read_excel("path/to/where/the/relevant/file/is/located/screeningdf.xlsx")


  ### 7) Filter the putMIP LN connectors for only the neurons in the "screeningdf" dataframe
  ###       that provide synaptic input to the given putMIP LN being tested...
  ###     FROM 'neuprint_get_synapses {neuprintr}': 
  ###       prepost = 0 a downstream or output partner, postsynaptic to the query neuron
  ###       prepost = 1 an upstream or input partner, presynaptic to the query neuron
modneurons.Dfilter = mipln1.conns %>% 
  select(partner, prepost, inD) %>% 
  filter(prepost > 0, inD == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.Dfilter = mipln1.conns %>% 
  select(partner, prepost, inD) %>% 
  filter(prepost > 0, inD == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.Dfilter = mipln1.conns %>% 
  select(partner, prepost, inD) %>% 
  filter(prepost > 0, inD == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.Dfilter = mipln1.conns %>% 
  select(partner, prepost, inD) %>% 
  filter(prepost > 0, inD == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.Dfilter = mipln1.conns %>% 
  select(partner, prepost, inD) %>% 
  filter(prepost > 0, inD == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.Dfilter = mipln1.conns %>% 
  select(partner, prepost, inD) %>% 
  filter(prepost > 0, inD == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.Dfilter = mipln1.conns %>% 
  select(partner, prepost, inD) %>% 
  filter(prepost > 0, inD == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DA1filter = mipln1.conns %>% 
  select(partner, prepost, inDA1) %>% 
  filter(prepost > 0, inDA1 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DA1filter = mipln1.conns %>% 
  select(partner, prepost, inDA1) %>% 
  filter(prepost > 0, inDA1 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DA1filter = mipln1.conns %>% 
  select(partner, prepost, inDA1) %>% 
  filter(prepost > 0, inDA1 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DA1filter = mipln1.conns %>% 
  select(partner, prepost, inDA1) %>% 
  filter(prepost > 0, inDA1 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DA1filter = mipln1.conns %>% 
  select(partner, prepost, inDA1) %>% 
  filter(prepost > 0, inDA1 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DA1filter = mipln1.conns %>% 
  select(partner, prepost, inDA1) %>% 
  filter(prepost > 0, inDA1 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DA1filter = mipln1.conns %>% 
  select(partner, prepost, inDA1) %>% 
  filter(prepost > 0, inDA1 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DA2filter = mipln1.conns %>% 
  select(partner, prepost, inDA2) %>% 
  filter(prepost > 0, inDA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DA2filter = mipln1.conns %>% 
  select(partner, prepost, inDA2) %>% 
  filter(prepost > 0, inDA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DA2filter = mipln1.conns %>% 
  select(partner, prepost, inDA2) %>% 
  filter(prepost > 0, inDA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DA2filter = mipln1.conns %>% 
  select(partner, prepost, inDA2) %>% 
  filter(prepost > 0, inDA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DA2filter = mipln1.conns %>% 
  select(partner, prepost, inDA2) %>% 
  filter(prepost > 0, inDA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DA2filter = mipln1.conns %>% 
  select(partner, prepost, inDA2) %>% 
  filter(prepost > 0, inDA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DA2filter = mipln1.conns %>% 
  select(partner, prepost, inDA2) %>% 
  filter(prepost > 0, inDA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DA3filter = mipln1.conns %>% 
  select(partner, prepost, inDA3) %>% 
  filter(prepost > 0, inDA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DA3filter = mipln1.conns %>% 
  select(partner, prepost, inDA3) %>% 
  filter(prepost > 0, inDA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DA3filter = mipln1.conns %>% 
  select(partner, prepost, inDA3) %>% 
  filter(prepost > 0, inDA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DA3filter = mipln1.conns %>% 
  select(partner, prepost, inDA3) %>% 
  filter(prepost > 0, inDA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DA3filter = mipln1.conns %>% 
  select(partner, prepost, inDA3) %>% 
  filter(prepost > 0, inDA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DA3filter = mipln1.conns %>% 
  select(partner, prepost, inDA3) %>% 
  filter(prepost > 0, inDA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DA3filter = mipln1.conns %>% 
  select(partner, prepost, inDA3) %>% 
  filter(prepost > 0, inDA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DA4lfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4l) %>% 
  filter(prepost > 0, inDA4l == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DA4lfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4l) %>% 
  filter(prepost > 0, inDA4l == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DA4lfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4l) %>% 
  filter(prepost > 0, inDA4l == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DA4lfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4l) %>% 
  filter(prepost > 0, inDA4l == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DA4lfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4l) %>% 
  filter(prepost > 0, inDA4l == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DA4lfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4l) %>% 
  filter(prepost > 0, inDA4l == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DA4lfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4l) %>% 
  filter(prepost > 0, inDA4l == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DA4mfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4m) %>% 
  filter(prepost > 0, inDA4m == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DA4mfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4m) %>% 
  filter(prepost > 0, inDA4m == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DA4mfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4m) %>% 
  filter(prepost > 0, inDA4m == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DA4mfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4m) %>% 
  filter(prepost > 0, inDA4m == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DA4mfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4m) %>% 
  filter(prepost > 0, inDA4m == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DA4mfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4m) %>% 
  filter(prepost > 0, inDA4m == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DA4mfilter = mipln1.conns %>% 
  select(partner, prepost, inDA4m) %>% 
  filter(prepost > 0, inDA4m == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DC1filter = mipln1.conns %>% 
  select(partner, prepost, inDC1) %>% 
  filter(prepost > 0, inDC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DC1filter = mipln1.conns %>% 
  select(partner, prepost, inDC1) %>% 
  filter(prepost > 0, inDC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DC1filter = mipln1.conns %>% 
  select(partner, prepost, inDC1) %>% 
  filter(prepost > 0, inDC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DC1filter = mipln1.conns %>% 
  select(partner, prepost, inDC1) %>% 
  filter(prepost > 0, inDC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DC1filter = mipln1.conns %>% 
  select(partner, prepost, inDC1) %>% 
  filter(prepost > 0, inDC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DC1filter = mipln1.conns %>% 
  select(partner, prepost, inDC1) %>% 
  filter(prepost > 0, inDC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DC1filter = mipln1.conns %>% 
  select(partner, prepost, inDC1) %>% 
  filter(prepost > 0, inDC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DC2filter = mipln1.conns %>% 
  select(partner, prepost, inDC2) %>% 
  filter(prepost > 0, inDC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DC2filter = mipln1.conns %>% 
  select(partner, prepost, inDC2) %>% 
  filter(prepost > 0, inDC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DC2filter = mipln1.conns %>% 
  select(partner, prepost, inDC2) %>% 
  filter(prepost > 0, inDC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DC2filter = mipln1.conns %>% 
  select(partner, prepost, inDC2) %>% 
  filter(prepost > 0, inDC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DC2filter = mipln1.conns %>% 
  select(partner, prepost, inDC2) %>% 
  filter(prepost > 0, inDC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DC2filter = mipln1.conns %>% 
  select(partner, prepost, inDC2) %>% 
  filter(prepost > 0, inDC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DC2filter = mipln1.conns %>% 
  select(partner, prepost, inDC2) %>% 
  filter(prepost > 0, inDC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DC3filter = mipln1.conns %>% 
  select(partner, prepost, inDC3) %>% 
  filter(prepost > 0, inDC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DC3filter = mipln1.conns %>% 
  select(partner, prepost, inDC3) %>% 
  filter(prepost > 0, inDC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DC3filter = mipln1.conns %>% 
  select(partner, prepost, inDC3) %>% 
  filter(prepost > 0, inDC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DC3filter = mipln1.conns %>% 
  select(partner, prepost, inDC3) %>% 
  filter(prepost > 0, inDC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DC3filter = mipln1.conns %>% 
  select(partner, prepost, inDC3) %>% 
  filter(prepost > 0, inDC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DC3filter = mipln1.conns %>% 
  select(partner, prepost, inDC3) %>% 
  filter(prepost > 0, inDC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DC3filter = mipln1.conns %>% 
  select(partner, prepost, inDC3) %>% 
  filter(prepost > 0, inDC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DC4filter = mipln1.conns %>% 
  select(partner, prepost, inDC4) %>% 
  filter(prepost > 0, inDC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DC4filter = mipln1.conns %>% 
  select(partner, prepost, inDC4) %>% 
  filter(prepost > 0, inDC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DC4filter = mipln1.conns %>% 
  select(partner, prepost, inDC4) %>% 
  filter(prepost > 0, inDC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DC4filter = mipln1.conns %>% 
  select(partner, prepost, inDC4) %>% 
  filter(prepost > 0, inDC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DC4filter = mipln1.conns %>% 
  select(partner, prepost, inDC4) %>% 
  filter(prepost > 0, inDC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DC4filter = mipln1.conns %>% 
  select(partner, prepost, inDC4) %>% 
  filter(prepost > 0, inDC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DC4filter = mipln1.conns %>% 
  select(partner, prepost, inDC4) %>% 
  filter(prepost > 0, inDC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DL1filter = mipln1.conns %>% 
  select(partner, prepost, inDL1) %>% 
  filter(prepost > 0, inDL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DL1filter = mipln1.conns %>% 
  select(partner, prepost, inDL1) %>% 
  filter(prepost > 0, inDL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DL1filter = mipln1.conns %>% 
  select(partner, prepost, inDL1) %>% 
  filter(prepost > 0, inDL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DL1filter = mipln1.conns %>% 
  select(partner, prepost, inDL1) %>% 
  filter(prepost > 0, inDL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DL1filter = mipln1.conns %>% 
  select(partner, prepost, inDL1) %>% 
  filter(prepost > 0, inDL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DL1filter = mipln1.conns %>% 
  select(partner, prepost, inDL1) %>% 
  filter(prepost > 0, inDL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DL1filter = mipln1.conns %>% 
  select(partner, prepost, inDL1) %>% 
  filter(prepost > 0, inDL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DL2dfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2d) %>% 
  filter(prepost > 0, inDL2d == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DL2dfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2d) %>% 
  filter(prepost > 0, inDL2d == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DL2dfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2d) %>% 
  filter(prepost > 0, inDL2d == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DL2dfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2d) %>% 
  filter(prepost > 0, inDL2d == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DL2dfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2d) %>% 
  filter(prepost > 0, inDL2d == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DL2dfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2d) %>% 
  filter(prepost > 0, inDL2d == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DL2dfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2d) %>% 
  filter(prepost > 0, inDL2d == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DL2vfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2v) %>% 
  filter(prepost > 0, inDL2v == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DL2vfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2v) %>% 
  filter(prepost > 0, inDL2v == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DL2vfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2v) %>% 
  filter(prepost > 0, inDL2v == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DL2vfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2v) %>% 
  filter(prepost > 0, inDL2v == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DL2vfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2v) %>% 
  filter(prepost > 0, inDL2v == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DL2vfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2v) %>% 
  filter(prepost > 0, inDL2v == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DL2vfilter = mipln1.conns %>% 
  select(partner, prepost, inDL2v) %>% 
  filter(prepost > 0, inDL2v == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DL3filter = mipln1.conns %>% 
  select(partner, prepost, inDL3) %>% 
  filter(prepost > 0, inDL3 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DL3filter = mipln1.conns %>% 
  select(partner, prepost, inDL3) %>% 
  filter(prepost > 0, inDL3 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DL3filter = mipln1.conns %>% 
  select(partner, prepost, inDL3) %>% 
  filter(prepost > 0, inDL3 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DL3filter = mipln1.conns %>% 
  select(partner, prepost, inDL3) %>% 
  filter(prepost > 0, inDL3 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DL3filter = mipln1.conns %>% 
  select(partner, prepost, inDL3) %>% 
  filter(prepost > 0, inDL3 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DL3filter = mipln1.conns %>% 
  select(partner, prepost, inDL3) %>% 
  filter(prepost > 0, inDL3 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DL3filter = mipln1.conns %>% 
  select(partner, prepost, inDL3) %>% 
  filter(prepost > 0, inDL3 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DL4filter = mipln1.conns %>% 
  select(partner, prepost, inDL4) %>% 
  filter(prepost > 0, inDL4 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DL4filter = mipln1.conns %>% 
  select(partner, prepost, inDL4) %>% 
  filter(prepost > 0, inDL4 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DL4filter = mipln1.conns %>% 
  select(partner, prepost, inDL4) %>% 
  filter(prepost > 0, inDL4 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DL4filter = mipln1.conns %>% 
  select(partner, prepost, inDL4) %>% 
  filter(prepost > 0, inDL4 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DL4filter = mipln1.conns %>% 
  select(partner, prepost, inDL4) %>% 
  filter(prepost > 0, inDL4 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DL4filter = mipln1.conns %>% 
  select(partner, prepost, inDL4) %>% 
  filter(prepost > 0, inDL4 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DL4filter = mipln1.conns %>% 
  select(partner, prepost, inDL4) %>% 
  filter(prepost > 0, inDL4 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DL5filter = mipln1.conns %>% 
  select(partner, prepost, inDL5) %>% 
  filter(prepost > 0, inDL5 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DL5filter = mipln1.conns %>% 
  select(partner, prepost, inDL5) %>% 
  filter(prepost > 0, inDL5 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DL5filter = mipln1.conns %>% 
  select(partner, prepost, inDL5) %>% 
  filter(prepost > 0, inDL5 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DL5filter = mipln1.conns %>% 
  select(partner, prepost, inDL5) %>% 
  filter(prepost > 0, inDL5 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DL5filter = mipln1.conns %>% 
  select(partner, prepost, inDL5) %>% 
  filter(prepost > 0, inDL5 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DL5filter = mipln1.conns %>% 
  select(partner, prepost, inDL5) %>% 
  filter(prepost > 0, inDL5 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DL5filter = mipln1.conns %>% 
  select(partner, prepost, inDL5) %>% 
  filter(prepost > 0, inDL5 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DM1filter = mipln1.conns %>% 
  select(partner, prepost, inDM1) %>% 
  filter(prepost > 0, inDM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DM1filter = mipln1.conns %>% 
  select(partner, prepost, inDM1) %>% 
  filter(prepost > 0, inDM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DM1filter = mipln1.conns %>% 
  select(partner, prepost, inDM1) %>% 
  filter(prepost > 0, inDM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DM1filter = mipln1.conns %>% 
  select(partner, prepost, inDM1) %>% 
  filter(prepost > 0, inDM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DM1filter = mipln1.conns %>% 
  select(partner, prepost, inDM1) %>% 
  filter(prepost > 0, inDM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DM1filter = mipln1.conns %>% 
  select(partner, prepost, inDM1) %>% 
  filter(prepost > 0, inDM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DM1filter = mipln1.conns %>% 
  select(partner, prepost, inDM1) %>% 
  filter(prepost > 0, inDM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DM2filter = mipln1.conns %>% 
  select(partner, prepost, inDM2) %>% 
  filter(prepost > 0, inDM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DM2filter = mipln1.conns %>% 
  select(partner, prepost, inDM2) %>% 
  filter(prepost > 0, inDM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DM2filter = mipln1.conns %>% 
  select(partner, prepost, inDM2) %>% 
  filter(prepost > 0, inDM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DM2filter = mipln1.conns %>% 
  select(partner, prepost, inDM2) %>% 
  filter(prepost > 0, inDM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DM2filter = mipln1.conns %>% 
  select(partner, prepost, inDM2) %>% 
  filter(prepost > 0, inDM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DM2filter = mipln1.conns %>% 
  select(partner, prepost, inDM2) %>% 
  filter(prepost > 0, inDM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DM2filter = mipln1.conns %>% 
  select(partner, prepost, inDM2) %>% 
  filter(prepost > 0, inDM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DM3filter = mipln1.conns %>% 
  select(partner, prepost, inDM3) %>% 
  filter(prepost > 0, inDM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DM3filter = mipln1.conns %>% 
  select(partner, prepost, inDM3) %>% 
  filter(prepost > 0, inDM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DM3filter = mipln1.conns %>% 
  select(partner, prepost, inDM3) %>% 
  filter(prepost > 0, inDM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DM3filter = mipln1.conns %>% 
  select(partner, prepost, inDM3) %>% 
  filter(prepost > 0, inDM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DM3filter = mipln1.conns %>% 
  select(partner, prepost, inDM3) %>% 
  filter(prepost > 0, inDM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DM3filter = mipln1.conns %>% 
  select(partner, prepost, inDM3) %>% 
  filter(prepost > 0, inDM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DM3filter = mipln1.conns %>% 
  select(partner, prepost, inDM3) %>% 
  filter(prepost > 0, inDM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DM4filter = mipln1.conns %>% 
  select(partner, prepost, inDM4) %>% 
  filter(prepost > 0, inDM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DM4filter = mipln1.conns %>% 
  select(partner, prepost, inDM4) %>% 
  filter(prepost > 0, inDM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DM4filter = mipln1.conns %>% 
  select(partner, prepost, inDM4) %>% 
  filter(prepost > 0, inDM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DM4filter = mipln1.conns %>% 
  select(partner, prepost, inDM4) %>% 
  filter(prepost > 0, inDM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DM4filter = mipln1.conns %>% 
  select(partner, prepost, inDM4) %>% 
  filter(prepost > 0, inDM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DM4filter = mipln1.conns %>% 
  select(partner, prepost, inDM4) %>% 
  filter(prepost > 0, inDM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DM4filter = mipln1.conns %>% 
  select(partner, prepost, inDM4) %>% 
  filter(prepost > 0, inDM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DM5filter = mipln1.conns %>% 
  select(partner, prepost, inDM5) %>% 
  filter(prepost > 0, inDM5 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DM5filter = mipln1.conns %>% 
  select(partner, prepost, inDM5) %>% 
  filter(prepost > 0, inDM5 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DM5filter = mipln1.conns %>% 
  select(partner, prepost, inDM5) %>% 
  filter(prepost > 0, inDM5 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DM5filter = mipln1.conns %>% 
  select(partner, prepost, inDM5) %>% 
  filter(prepost > 0, inDM5 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DM5filter = mipln1.conns %>% 
  select(partner, prepost, inDM5) %>% 
  filter(prepost > 0, inDM5 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DM5filter = mipln1.conns %>% 
  select(partner, prepost, inDM5) %>% 
  filter(prepost > 0, inDM5 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DM5filter = mipln1.conns %>% 
  select(partner, prepost, inDM5) %>% 
  filter(prepost > 0, inDM5 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DM6filter = mipln1.conns %>% 
  select(partner, prepost, inDM6) %>% 
  filter(prepost > 0, inDM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DM6filter = mipln1.conns %>% 
  select(partner, prepost, inDM6) %>% 
  filter(prepost > 0, inDM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DM6filter = mipln1.conns %>% 
  select(partner, prepost, inDM6) %>% 
  filter(prepost > 0, inDM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DM6filter = mipln1.conns %>% 
  select(partner, prepost, inDM6) %>% 
  filter(prepost > 0, inDM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DM6filter = mipln1.conns %>% 
  select(partner, prepost, inDM6) %>% 
  filter(prepost > 0, inDM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DM6filter = mipln1.conns %>% 
  select(partner, prepost, inDM6) %>% 
  filter(prepost > 0, inDM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DM6filter = mipln1.conns %>% 
  select(partner, prepost, inDM6) %>% 
  filter(prepost > 0, inDM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1l) %>% 
  filter(prepost > 0, inDP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1l) %>% 
  filter(prepost > 0, inDP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1l) %>% 
  filter(prepost > 0, inDP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1l) %>% 
  filter(prepost > 0, inDP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1l) %>% 
  filter(prepost > 0, inDP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1l) %>% 
  filter(prepost > 0, inDP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1l) %>% 
  filter(prepost > 0, inDP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.DP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1m) %>% 
  filter(prepost > 0, inDP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.DP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1m) %>% 
  filter(prepost > 0, inDP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.DP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1m) %>% 
  filter(prepost > 0, inDP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.DP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1m) %>% 
  filter(prepost > 0, inDP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.DP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1m) %>% 
  filter(prepost > 0, inDP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.DP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1m) %>% 
  filter(prepost > 0, inDP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.DP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inDP1m) %>% 
  filter(prepost > 0, inDP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.Vfilter = mipln1.conns %>% 
  select(partner, prepost, inV) %>% 
  filter(prepost > 0, inV == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.Vfilter = mipln1.conns %>% 
  select(partner, prepost, inV) %>% 
  filter(prepost > 0, inV == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.Vfilter = mipln1.conns %>% 
  select(partner, prepost, inV) %>% 
  filter(prepost > 0, inV == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.Vfilter = mipln1.conns %>% 
  select(partner, prepost, inV) %>% 
  filter(prepost > 0, inV == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.Vfilter = mipln1.conns %>% 
  select(partner, prepost, inV) %>% 
  filter(prepost > 0, inV == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.Vfilter = mipln1.conns %>% 
  select(partner, prepost, inV) %>% 
  filter(prepost > 0, inV == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.Vfilter = mipln1.conns %>% 
  select(partner, prepost, inV) %>% 
  filter(prepost > 0, inV == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1d) %>% 
  filter(prepost > 0, inVA1d == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1d) %>% 
  filter(prepost > 0, inVA1d == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1d) %>% 
  filter(prepost > 0, inVA1d == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1d) %>% 
  filter(prepost > 0, inVA1d == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1d) %>% 
  filter(prepost > 0, inVA1d == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1d) %>% 
  filter(prepost > 0, inVA1d == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1d) %>% 
  filter(prepost > 0, inVA1d == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA1vfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1v) %>% 
  filter(prepost > 0, inVA1v == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA1vfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1v) %>% 
  filter(prepost > 0, inVA1v == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA1vfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1v) %>% 
  filter(prepost > 0, inVA1v == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA1vfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1v) %>% 
  filter(prepost > 0, inVA1v == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA1vfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1v) %>% 
  filter(prepost > 0, inVA1v == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA1vfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1v) %>% 
  filter(prepost > 0, inVA1v == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA1vfilter = mipln1.conns %>% 
  select(partner, prepost, inVA1v) %>% 
  filter(prepost > 0, inVA1v == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA2filter = mipln1.conns %>% 
  select(partner, prepost, inVA2) %>% 
  filter(prepost > 0, inVA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA2filter = mipln1.conns %>% 
  select(partner, prepost, inVA2) %>% 
  filter(prepost > 0, inVA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA2filter = mipln1.conns %>% 
  select(partner, prepost, inVA2) %>% 
  filter(prepost > 0, inVA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA2filter = mipln1.conns %>% 
  select(partner, prepost, inVA2) %>% 
  filter(prepost > 0, inVA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA2filter = mipln1.conns %>% 
  select(partner, prepost, inVA2) %>% 
  filter(prepost > 0, inVA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA2filter = mipln1.conns %>% 
  select(partner, prepost, inVA2) %>% 
  filter(prepost > 0, inVA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA2filter = mipln1.conns %>% 
  select(partner, prepost, inVA2) %>% 
  filter(prepost > 0, inVA2 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA3filter = mipln1.conns %>% 
  select(partner, prepost, inVA3) %>% 
  filter(prepost > 0, inVA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA3filter = mipln1.conns %>% 
  select(partner, prepost, inVA3) %>% 
  filter(prepost > 0, inVA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA3filter = mipln1.conns %>% 
  select(partner, prepost, inVA3) %>% 
  filter(prepost > 0, inVA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA3filter = mipln1.conns %>% 
  select(partner, prepost, inVA3) %>% 
  filter(prepost > 0, inVA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA3filter = mipln1.conns %>% 
  select(partner, prepost, inVA3) %>% 
  filter(prepost > 0, inVA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA3filter = mipln1.conns %>% 
  select(partner, prepost, inVA3) %>% 
  filter(prepost > 0, inVA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA3filter = mipln1.conns %>% 
  select(partner, prepost, inVA3) %>% 
  filter(prepost > 0, inVA3 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA4filter = mipln1.conns %>% 
  select(partner, prepost, inVA4) %>% 
  filter(prepost > 0, inVA4 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA4filter = mipln1.conns %>% 
  select(partner, prepost, inVA4) %>% 
  filter(prepost > 0, inVA4 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA4filter = mipln1.conns %>% 
  select(partner, prepost, inVA4) %>% 
  filter(prepost > 0, inVA4 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA4filter = mipln1.conns %>% 
  select(partner, prepost, inVA4) %>% 
  filter(prepost > 0, inVA4 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA4filter = mipln1.conns %>% 
  select(partner, prepost, inVA4) %>% 
  filter(prepost > 0, inVA4 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA4filter = mipln1.conns %>% 
  select(partner, prepost, inVA4) %>% 
  filter(prepost > 0, inVA4 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA4filter = mipln1.conns %>% 
  select(partner, prepost, inVA4) %>% 
  filter(prepost > 0, inVA4 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA5filter = mipln1.conns %>% 
  select(partner, prepost, inVA5) %>% 
  filter(prepost > 0, inVA5 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA5filter = mipln1.conns %>% 
  select(partner, prepost, inVA5) %>% 
  filter(prepost > 0, inVA5 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA5filter = mipln1.conns %>% 
  select(partner, prepost, inVA5) %>% 
  filter(prepost > 0, inVA5 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA5filter = mipln1.conns %>% 
  select(partner, prepost, inVA5) %>% 
  filter(prepost > 0, inVA5 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA5filter = mipln1.conns %>% 
  select(partner, prepost, inVA5) %>% 
  filter(prepost > 0, inVA5 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA5filter = mipln1.conns %>% 
  select(partner, prepost, inVA5) %>% 
  filter(prepost > 0, inVA5 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA5filter = mipln1.conns %>% 
  select(partner, prepost, inVA5) %>% 
  filter(prepost > 0, inVA5 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA6filter = mipln1.conns %>% 
  select(partner, prepost, inVA6) %>% 
  filter(prepost > 0, inVA6 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA6filter = mipln1.conns %>% 
  select(partner, prepost, inVA6) %>% 
  filter(prepost > 0, inVA6 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA6filter = mipln1.conns %>% 
  select(partner, prepost, inVA6) %>% 
  filter(prepost > 0, inVA6 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA6filter = mipln1.conns %>% 
  select(partner, prepost, inVA6) %>% 
  filter(prepost > 0, inVA6 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA6filter = mipln1.conns %>% 
  select(partner, prepost, inVA6) %>% 
  filter(prepost > 0, inVA6 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA6filter = mipln1.conns %>% 
  select(partner, prepost, inVA6) %>% 
  filter(prepost > 0, inVA6 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA6filter = mipln1.conns %>% 
  select(partner, prepost, inVA6) %>% 
  filter(prepost > 0, inVA6 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA7lfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7l) %>% 
  filter(prepost > 0, inVA7l == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA7lfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7l) %>% 
  filter(prepost > 0, inVA7l == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA7lfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7l) %>% 
  filter(prepost > 0, inVA7l == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA7lfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7l) %>% 
  filter(prepost > 0, inVA7l == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA7lfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7l) %>% 
  filter(prepost > 0, inVA7l == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA7lfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7l) %>% 
  filter(prepost > 0, inVA7l == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA7lfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7l) %>% 
  filter(prepost > 0, inVA7l == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VA7mfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7m) %>% 
  filter(prepost > 0, inVA7m == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VA7mfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7m) %>% 
  filter(prepost > 0, inVA7m == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VA7mfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7m) %>% 
  filter(prepost > 0, inVA7m == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VA7mfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7m) %>% 
  filter(prepost > 0, inVA7m == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VA7mfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7m) %>% 
  filter(prepost > 0, inVA7m == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VA7mfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7m) %>% 
  filter(prepost > 0, inVA7m == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VA7mfilter = mipln1.conns %>% 
  select(partner, prepost, inVA7m) %>% 
  filter(prepost > 0, inVA7m == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VC1filter = mipln1.conns %>% 
  select(partner, prepost, inVC1) %>% 
  filter(prepost > 0, inVC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VC1filter = mipln1.conns %>% 
  select(partner, prepost, inVC1) %>% 
  filter(prepost > 0, inVC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VC1filter = mipln1.conns %>% 
  select(partner, prepost, inVC1) %>% 
  filter(prepost > 0, inVC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VC1filter = mipln1.conns %>% 
  select(partner, prepost, inVC1) %>% 
  filter(prepost > 0, inVC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VC1filter = mipln1.conns %>% 
  select(partner, prepost, inVC1) %>% 
  filter(prepost > 0, inVC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VC1filter = mipln1.conns %>% 
  select(partner, prepost, inVC1) %>% 
  filter(prepost > 0, inVC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VC1filter = mipln1.conns %>% 
  select(partner, prepost, inVC1) %>% 
  filter(prepost > 0, inVC1 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VC2filter = mipln1.conns %>% 
  select(partner, prepost, inVC2) %>% 
  filter(prepost > 0, inVC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VC2filter = mipln1.conns %>% 
  select(partner, prepost, inVC2) %>% 
  filter(prepost > 0, inVC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VC2filter = mipln1.conns %>% 
  select(partner, prepost, inVC2) %>% 
  filter(prepost > 0, inVC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VC2filter = mipln1.conns %>% 
  select(partner, prepost, inVC2) %>% 
  filter(prepost > 0, inVC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VC2filter = mipln1.conns %>% 
  select(partner, prepost, inVC2) %>% 
  filter(prepost > 0, inVC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VC2filter = mipln1.conns %>% 
  select(partner, prepost, inVC2) %>% 
  filter(prepost > 0, inVC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VC2filter = mipln1.conns %>% 
  select(partner, prepost, inVC2) %>% 
  filter(prepost > 0, inVC2 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VC3filter = mipln1.conns %>% 
  select(partner, prepost, inVC3) %>% 
  filter(prepost > 0, inVC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VC3filter = mipln1.conns %>% 
  select(partner, prepost, inVC3) %>% 
  filter(prepost > 0, inVC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VC3filter = mipln1.conns %>% 
  select(partner, prepost, inVC3) %>% 
  filter(prepost > 0, inVC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VC3filter = mipln1.conns %>% 
  select(partner, prepost, inVC3) %>% 
  filter(prepost > 0, inVC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VC3filter = mipln1.conns %>% 
  select(partner, prepost, inVC3) %>% 
  filter(prepost > 0, inVC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VC3filter = mipln1.conns %>% 
  select(partner, prepost, inVC3) %>% 
  filter(prepost > 0, inVC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VC3filter = mipln1.conns %>% 
  select(partner, prepost, inVC3) %>% 
  filter(prepost > 0, inVC3 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VC4filter = mipln1.conns %>% 
  select(partner, prepost, inVC4) %>% 
  filter(prepost > 0, inVC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VC4filter = mipln1.conns %>% 
  select(partner, prepost, inVC4) %>% 
  filter(prepost > 0, inVC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VC4filter = mipln1.conns %>% 
  select(partner, prepost, inVC4) %>% 
  filter(prepost > 0, inVC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VC4filter = mipln1.conns %>% 
  select(partner, prepost, inVC4) %>% 
  filter(prepost > 0, inVC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VC4filter = mipln1.conns %>% 
  select(partner, prepost, inVC4) %>% 
  filter(prepost > 0, inVC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VC4filter = mipln1.conns %>% 
  select(partner, prepost, inVC4) %>% 
  filter(prepost > 0, inVC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VC4filter = mipln1.conns %>% 
  select(partner, prepost, inVC4) %>% 
  filter(prepost > 0, inVC4 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VC5filter = mipln1.conns %>% 
  select(partner, prepost, inVC5) %>% 
  filter(prepost > 0, inVC5 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VC5filter = mipln1.conns %>% 
  select(partner, prepost, inVC5) %>% 
  filter(prepost > 0, inVC5 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VC5filter = mipln1.conns %>% 
  select(partner, prepost, inVC5) %>% 
  filter(prepost > 0, inVC5 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VC5filter = mipln1.conns %>% 
  select(partner, prepost, inVC5) %>% 
  filter(prepost > 0, inVC5 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VC5filter = mipln1.conns %>% 
  select(partner, prepost, inVC5) %>% 
  filter(prepost > 0, inVC5 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VC5filter = mipln1.conns %>% 
  select(partner, prepost, inVC5) %>% 
  filter(prepost > 0, inVC5 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VC5filter = mipln1.conns %>% 
  select(partner, prepost, inVC5) %>% 
  filter(prepost > 0, inVC5 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VL1filter = mipln1.conns %>% 
  select(partner, prepost, inVL1) %>% 
  filter(prepost > 0, inVL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VL1filter = mipln1.conns %>% 
  select(partner, prepost, inVL1) %>% 
  filter(prepost > 0, inVL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VL1filter = mipln1.conns %>% 
  select(partner, prepost, inVL1) %>% 
  filter(prepost > 0, inVL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VL1filter = mipln1.conns %>% 
  select(partner, prepost, inVL1) %>% 
  filter(prepost > 0, inVL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VL1filter = mipln1.conns %>% 
  select(partner, prepost, inVL1) %>% 
  filter(prepost > 0, inVL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VL1filter = mipln1.conns %>% 
  select(partner, prepost, inVL1) %>% 
  filter(prepost > 0, inVL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VL1filter = mipln1.conns %>% 
  select(partner, prepost, inVL1) %>% 
  filter(prepost > 0, inVL1 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VL2afilter = mipln1.conns %>% 
  select(partner, prepost, inVL2a) %>% 
  filter(prepost > 0, inVL2a == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VL2afilter = mipln1.conns %>% 
  select(partner, prepost, inVL2a) %>% 
  filter(prepost > 0, inVL2a == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VL2afilter = mipln1.conns %>% 
  select(partner, prepost, inVL2a) %>% 
  filter(prepost > 0, inVL2a == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VL2afilter = mipln1.conns %>% 
  select(partner, prepost, inVL2a) %>% 
  filter(prepost > 0, inVL2a == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VL2afilter = mipln1.conns %>% 
  select(partner, prepost, inVL2a) %>% 
  filter(prepost > 0, inVL2a == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VL2afilter = mipln1.conns %>% 
  select(partner, prepost, inVL2a) %>% 
  filter(prepost > 0, inVL2a == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VL2afilter = mipln1.conns %>% 
  select(partner, prepost, inVL2a) %>% 
  filter(prepost > 0, inVL2a == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VL2pfilter = mipln1.conns %>% 
  select(partner, prepost, inVL2p) %>% 
  filter(prepost > 0, inVL2p == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VL2pfilter = mipln1.conns %>% 
  select(partner, prepost, inVL2p) %>% 
  filter(prepost > 0, inVL2p == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VL2pfilter = mipln1.conns %>% 
  select(partner, prepost, inVL2p) %>% 
  filter(prepost > 0, inVL2p == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VL2pfilter = mipln1.conns %>% 
  select(partner, prepost, inVL2p) %>% 
  filter(prepost > 0, inVL2p == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VL2pfilter = mipln1.conns %>% 
  select(partner, prepost, inVL2p) %>% 
  filter(prepost > 0, inVL2p == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VL2pfilter = mipln1.conns %>% 
  select(partner, prepost, inVL2p) %>% 
  filter(prepost > 0, inVL2p == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VL2pfilter = mipln1.conns %>% 
  select(partner, prepost, inVL2p) %>% 
  filter(prepost > 0, inVL2p == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM1filter = mipln1.conns %>% 
  select(partner, prepost, inVM1) %>% 
  filter(prepost > 0, inVM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM1filter = mipln1.conns %>% 
  select(partner, prepost, inVM1) %>% 
  filter(prepost > 0, inVM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM1filter = mipln1.conns %>% 
  select(partner, prepost, inVM1) %>% 
  filter(prepost > 0, inVM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM1filter = mipln1.conns %>% 
  select(partner, prepost, inVM1) %>% 
  filter(prepost > 0, inVM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM1filter = mipln1.conns %>% 
  select(partner, prepost, inVM1) %>% 
  filter(prepost > 0, inVM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM1filter = mipln1.conns %>% 
  select(partner, prepost, inVM1) %>% 
  filter(prepost > 0, inVM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM1filter = mipln1.conns %>% 
  select(partner, prepost, inVM1) %>% 
  filter(prepost > 0, inVM1 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM2filter = mipln1.conns %>% 
  select(partner, prepost, inVM2) %>% 
  filter(prepost > 0, inVM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM2filter = mipln1.conns %>% 
  select(partner, prepost, inVM2) %>% 
  filter(prepost > 0, inVM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM2filter = mipln1.conns %>% 
  select(partner, prepost, inVM2) %>% 
  filter(prepost > 0, inVM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM2filter = mipln1.conns %>% 
  select(partner, prepost, inVM2) %>% 
  filter(prepost > 0, inVM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM2filter = mipln1.conns %>% 
  select(partner, prepost, inVM2) %>% 
  filter(prepost > 0, inVM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM2filter = mipln1.conns %>% 
  select(partner, prepost, inVM2) %>% 
  filter(prepost > 0, inVM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM2filter = mipln1.conns %>% 
  select(partner, prepost, inVM2) %>% 
  filter(prepost > 0, inVM2 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM3filter = mipln1.conns %>% 
  select(partner, prepost, inVM3) %>% 
  filter(prepost > 0, inVM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM3filter = mipln1.conns %>% 
  select(partner, prepost, inVM3) %>% 
  filter(prepost > 0, inVM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM3filter = mipln1.conns %>% 
  select(partner, prepost, inVM3) %>% 
  filter(prepost > 0, inVM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM3filter = mipln1.conns %>% 
  select(partner, prepost, inVM3) %>% 
  filter(prepost > 0, inVM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM3filter = mipln1.conns %>% 
  select(partner, prepost, inVM3) %>% 
  filter(prepost > 0, inVM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM3filter = mipln1.conns %>% 
  select(partner, prepost, inVM3) %>% 
  filter(prepost > 0, inVM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM3filter = mipln1.conns %>% 
  select(partner, prepost, inVM3) %>% 
  filter(prepost > 0, inVM3 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM4filter = mipln1.conns %>% 
  select(partner, prepost, inVM4) %>% 
  filter(prepost > 0, inVM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM4filter = mipln1.conns %>% 
  select(partner, prepost, inVM4) %>% 
  filter(prepost > 0, inVM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM4filter = mipln1.conns %>% 
  select(partner, prepost, inVM4) %>% 
  filter(prepost > 0, inVM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM4filter = mipln1.conns %>% 
  select(partner, prepost, inVM4) %>% 
  filter(prepost > 0, inVM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM4filter = mipln1.conns %>% 
  select(partner, prepost, inVM4) %>% 
  filter(prepost > 0, inVM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM4filter = mipln1.conns %>% 
  select(partner, prepost, inVM4) %>% 
  filter(prepost > 0, inVM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM4filter = mipln1.conns %>% 
  select(partner, prepost, inVM4) %>% 
  filter(prepost > 0, inVM4 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM5dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5d) %>% 
  filter(prepost > 0, inVM5d == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM5dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5d) %>% 
  filter(prepost > 0, inVM5d == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM5dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5d) %>% 
  filter(prepost > 0, inVM5d == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM5dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5d) %>% 
  filter(prepost > 0, inVM5d == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM5dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5d) %>% 
  filter(prepost > 0, inVM5d == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM5dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5d) %>% 
  filter(prepost > 0, inVM5d == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM5dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5d) %>% 
  filter(prepost > 0, inVM5d == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM5vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5v) %>% 
  filter(prepost > 0, inVM5v == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM5vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5v) %>% 
  filter(prepost > 0, inVM5v == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM5vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5v) %>% 
  filter(prepost > 0, inVM5v == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM5vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5v) %>% 
  filter(prepost > 0, inVM5v == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM5vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5v) %>% 
  filter(prepost > 0, inVM5v == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM5vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5v) %>% 
  filter(prepost > 0, inVM5v == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM5vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM5v) %>% 
  filter(prepost > 0, inVM5v == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM6filter = mipln1.conns %>% 
  select(partner, prepost, inVM6) %>% 
  filter(prepost > 0, inVM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM6filter = mipln1.conns %>% 
  select(partner, prepost, inVM6) %>% 
  filter(prepost > 0, inVM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM6filter = mipln1.conns %>% 
  select(partner, prepost, inVM6) %>% 
  filter(prepost > 0, inVM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM6filter = mipln1.conns %>% 
  select(partner, prepost, inVM6) %>% 
  filter(prepost > 0, inVM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM6filter = mipln1.conns %>% 
  select(partner, prepost, inVM6) %>% 
  filter(prepost > 0, inVM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM6filter = mipln1.conns %>% 
  select(partner, prepost, inVM6) %>% 
  filter(prepost > 0, inVM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM6filter = mipln1.conns %>% 
  select(partner, prepost, inVM6) %>% 
  filter(prepost > 0, inVM6 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM7dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7d) %>% 
  filter(prepost > 0, inVM7d == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM7dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7d) %>% 
  filter(prepost > 0, inVM7d == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM7dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7d) %>% 
  filter(prepost > 0, inVM7d == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM7dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7d) %>% 
  filter(prepost > 0, inVM7d == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM7dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7d) %>% 
  filter(prepost > 0, inVM7d == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM7dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7d) %>% 
  filter(prepost > 0, inVM7d == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM7dfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7d) %>% 
  filter(prepost > 0, inVM7d == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VM7vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7v) %>% 
  filter(prepost > 0, inVM7v == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VM7vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7v) %>% 
  filter(prepost > 0, inVM7v == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VM7vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7v) %>% 
  filter(prepost > 0, inVM7v == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VM7vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7v) %>% 
  filter(prepost > 0, inVM7v == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VM7vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7v) %>% 
  filter(prepost > 0, inVM7v == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VM7vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7v) %>% 
  filter(prepost > 0, inVM7v == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VM7vfilter = mipln1.conns %>% 
  select(partner, prepost, inVM7v) %>% 
  filter(prepost > 0, inVM7v == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VP1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1d) %>% 
  filter(prepost > 0, inVP1d == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VP1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1d) %>% 
  filter(prepost > 0, inVP1d == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VP1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1d) %>% 
  filter(prepost > 0, inVP1d == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VP1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1d) %>% 
  filter(prepost > 0, inVP1d == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VP1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1d) %>% 
  filter(prepost > 0, inVP1d == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VP1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1d) %>% 
  filter(prepost > 0, inVP1d == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VP1dfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1d) %>% 
  filter(prepost > 0, inVP1d == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1l) %>% 
  filter(prepost > 0, inVP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1l) %>% 
  filter(prepost > 0, inVP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1l) %>% 
  filter(prepost > 0, inVP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1l) %>% 
  filter(prepost > 0, inVP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1l) %>% 
  filter(prepost > 0, inVP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1l) %>% 
  filter(prepost > 0, inVP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VP1lfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1l) %>% 
  filter(prepost > 0, inVP1l == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1m) %>% 
  filter(prepost > 0, inVP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1m) %>% 
  filter(prepost > 0, inVP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1m) %>% 
  filter(prepost > 0, inVP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1m) %>% 
  filter(prepost > 0, inVP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1m) %>% 
  filter(prepost > 0, inVP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1m) %>% 
  filter(prepost > 0, inVP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VP1mfilter = mipln1.conns %>% 
  select(partner, prepost, inVP1m) %>% 
  filter(prepost > 0, inVP1m == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VP2filter = mipln1.conns %>% 
  select(partner, prepost, inVP2) %>% 
  filter(prepost > 0, inVP2 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VP2filter = mipln1.conns %>% 
  select(partner, prepost, inVP2) %>% 
  filter(prepost > 0, inVP2 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VP2filter = mipln1.conns %>% 
  select(partner, prepost, inVP2) %>% 
  filter(prepost > 0, inVP2 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VP2filter = mipln1.conns %>% 
  select(partner, prepost, inVP2) %>% 
  filter(prepost > 0, inVP2 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VP2filter = mipln1.conns %>% 
  select(partner, prepost, inVP2) %>% 
  filter(prepost > 0, inVP2 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VP2filter = mipln1.conns %>% 
  select(partner, prepost, inVP2) %>% 
  filter(prepost > 0, inVP2 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VP2filter = mipln1.conns %>% 
  select(partner, prepost, inVP2) %>% 
  filter(prepost > 0, inVP2 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VP3filter = mipln1.conns %>% 
  select(partner, prepost, inVP3) %>% 
  filter(prepost > 0, inVP3 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VP3filter = mipln1.conns %>% 
  select(partner, prepost, inVP3) %>% 
  filter(prepost > 0, inVP3 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VP3filter = mipln1.conns %>% 
  select(partner, prepost, inVP3) %>% 
  filter(prepost > 0, inVP3 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VP3filter = mipln1.conns %>% 
  select(partner, prepost, inVP3) %>% 
  filter(prepost > 0, inVP3 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VP3filter = mipln1.conns %>% 
  select(partner, prepost, inVP3) %>% 
  filter(prepost > 0, inVP3 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VP3filter = mipln1.conns %>% 
  select(partner, prepost, inVP3) %>% 
  filter(prepost > 0, inVP3 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VP3filter = mipln1.conns %>% 
  select(partner, prepost, inVP3) %>% 
  filter(prepost > 0, inVP3 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


modneurons.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`modneurons`)
miplns.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`putMIPLNs`)
otherlns.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`otherlns`)
pns.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`pns`)
thehygro.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`thehygro`)
orns.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`orns`)
other_unknown.VP5filter = mipln1.conns %>% 
  select(partner, prepost, inVP5) %>% 
  filter(prepost > 0, inVP5 == TRUE) %>% 
  filter(partner %in% screeningdf$`other_unknown`)


  ### 8) Sum up the filtered results for each glomerulus...
resultsD = data.frame(
  "putMIP LNs" = count(miplns.Dfilter),
  "Modulatory Neurons" = count(modneurons.Dfilter),
  "Other LNs" = count(otherlns.Dfilter),
  "PNs" = count(pns.Dfilter),
  "ThermoHygro" = count(thehygro.Dfilter),
  "ORNs" = count(orns.Dfilter),
  "Other_Unknown" = count(other_unknown.Dfilter)
)

resultsDA1 = data.frame(
  "putMIP LNs" = count(miplns.DA1filter),
  "Modulatory Neurons" = count(modneurons.DA1filter),
  "Other LNs" = count(otherlns.DA1filter),
  "PNs" = count(pns.DA1filter),
  "ThermoHygro" = count(thehygro.DA1filter),
  "ORNs" = count(orns.DA1filter),
  "Other_Unknown" = count(other_unknown.DA1filter)
)

resultsDA2 = data.frame(
  "putMIP LNs" = count(miplns.DA2filter),
  "Modulatory Neurons" = count(modneurons.DA2filter),
  "Other LNs" = count(otherlns.DA2filter),
  "PNs" = count(pns.DA2filter),
  "ThermoHygro" = count(thehygro.DA2filter),
  "ORNs" = count(orns.DA2filter),
  "Other_Unknown" = count(other_unknown.DA2filter)
)

resultsDA3 = data.frame(
  "putMIP LNs" = count(miplns.DA3filter),
  "Modulatory Neurons" = count(modneurons.DA3filter),
  "Other LNs" = count(otherlns.DA3filter),
  "PNs" = count(pns.DA3filter),
  "ThermoHygro" = count(thehygro.DA3filter),
  "ORNs" = count(orns.DA3filter),
  "Other_Unknown" = count(other_unknown.DA3filter)
)

resultsDA4l = data.frame(
  "putMIP LNs" = count(miplns.DA4lfilter),
  "Modulatory Neurons" = count(modneurons.DA4lfilter),
  "Other LNs" = count(otherlns.DA4lfilter),
  "PNs" = count(pns.DA4lfilter),
  "ThermoHygro" = count(thehygro.DA4lfilter),
  "ORNs" = count(orns.DA4lfilter),
  "Other_Unknown" = count(other_unknown.DA4lfilter)
)

resultsDA4m = data.frame(
  "putMIP LNs" = count(miplns.DA4mfilter),
  "Modulatory Neurons" = count(modneurons.DA4mfilter),
  "Other LNs" = count(otherlns.DA4mfilter),
  "PNs" = count(pns.DA4mfilter),
  "ThermoHygro" = count(thehygro.DA4mfilter),
  "ORNs" = count(orns.DA4mfilter),
  "Other_Unknown" = count(other_unknown.DA4mfilter)
)

resultsDC1 = data.frame(
  "putMIP LNs" = count(miplns.DC1filter),
  "Modulatory Neurons" = count(modneurons.DC1filter),
  "Other LNs" = count(otherlns.DC1filter),
  "PNs" = count(pns.DC1filter),
  "ThermoHygro" = count(thehygro.DC1filter),
  "ORNs" = count(orns.DC1filter),
  "Other_Unknown" = count(other_unknown.DC1filter)
)

resultsDC2 = data.frame(
  "putMIP LNs" = count(miplns.DC2filter),
  "Modulatory Neurons" = count(modneurons.DC2filter),
  "Other LNs" = count(otherlns.DC2filter),
  "PNs" = count(pns.DC2filter),
  "ThermoHygro" = count(thehygro.DC2filter),
  "ORNs" = count(orns.DC2filter),
  "Other_Unknown" = count(other_unknown.DC2filter)
)

resultsDC3 = data.frame(
  "putMIP LNs" = count(miplns.DC3filter),
  "Modulatory Neurons" = count(modneurons.DC3filter),
  "Other LNs" = count(otherlns.DC3filter),
  "PNs" = count(pns.DC3filter),
  "ThermoHygro" = count(thehygro.DC3filter),
  "ORNs" = count(orns.DC3filter),
  "Other_Unknown" = count(other_unknown.DC3filter)
)

resultsDC4 = data.frame(
  "putMIP LNs" = count(miplns.DC4filter),
  "Modulatory Neurons" = count(modneurons.DC4filter),
  "Other LNs" = count(otherlns.DC4filter),
  "PNs" = count(pns.DC4filter),
  "ThermoHygro" = count(thehygro.DC4filter),
  "ORNs" = count(orns.DC4filter),
  "Other_Unknown" = count(other_unknown.DC4filter)
)

resultsDL1 = data.frame(
  "putMIP LNs" = count(miplns.DL1filter),
  "Modulatory Neurons" = count(modneurons.DL1filter),
  "Other LNs" = count(otherlns.DL1filter),
  "PNs" = count(pns.DL1filter),
  "ThermoHygro" = count(thehygro.DL1filter),
  "ORNs" = count(orns.DL1filter),
  "Other_Unknown" = count(other_unknown.DL1filter)
)

resultsDL2d = data.frame(
  "putMIP LNs" = count(miplns.DL2dfilter),
  "Modulatory Neurons" = count(modneurons.DL2dfilter),
  "Other LNs" = count(otherlns.DL2dfilter),
  "PNs" = count(pns.DL2dfilter),
  "ThermoHygro" = count(thehygro.DL2dfilter),
  "ORNs" = count(orns.DL2dfilter),
  "Other_Unknown" = count(other_unknown.DL2dfilter)
)

resultsDL2v = data.frame(
  "putMIP LNs" = count(miplns.DL2vfilter),
  "Modulatory Neurons" = count(modneurons.DL2vfilter),
  "Other LNs" = count(otherlns.DL2vfilter),
  "PNs" = count(pns.DL2vfilter),
  "ThermoHygro" = count(thehygro.DL2vfilter),
  "ORNs" = count(orns.DL2vfilter),
  "Other_Unknown" = count(other_unknown.DL2vfilter)
)

resultsDL3 = data.frame(
  "putMIP LNs" = count(miplns.DL3filter),
  "Modulatory Neurons" = count(modneurons.DL3filter),
  "Other LNs" = count(otherlns.DL3filter),
  "PNs" = count(pns.DL3filter),
  "ThermoHygro" = count(thehygro.DL3filter),
  "ORNs" = count(orns.DL3filter),
  "Other_Unknown" = count(other_unknown.DL3filter)
)

resultsDL4 = data.frame(
  "putMIP LNs" = count(miplns.DL4filter),
  "Modulatory Neurons" = count(modneurons.DL4filter),
  "Other LNs" = count(otherlns.DL4filter),
  "PNs" = count(pns.DL4filter),
  "ThermoHygro" = count(thehygro.DL4filter),
  "ORNs" = count(orns.DL4filter),
  "Other_Unknown" = count(other_unknown.DL4filter)
)

resultsDL5 = data.frame(
  "putMIP LNs" = count(miplns.DL5filter),
  "Modulatory Neurons" = count(modneurons.DL5filter),
  "Other LNs" = count(otherlns.DL5filter),
  "PNs" = count(pns.DL5filter),
  "ThermoHygro" = count(thehygro.DL5filter),
  "ORNs" = count(orns.DL5filter),
  "Other_Unknown" = count(other_unknown.DL5filter)
)

resultsDM1 = data.frame(
  "putMIP LNs" = count(miplns.DM1filter),
  "Modulatory Neurons" = count(modneurons.DM1filter),
  "Other LNs" = count(otherlns.DM1filter),
  "PNs" = count(pns.DM1filter),
  "ThermoHygro" = count(thehygro.DM1filter),
  "ORNs" = count(orns.DM1filter),
  "Other_Unknown" = count(other_unknown.DM1filter)
)

resultsDM2 = data.frame(
  "putMIP LNs" = count(miplns.DM2filter),
  "Modulatory Neurons" = count(modneurons.DM2filter),
  "Other LNs" = count(otherlns.DM2filter),
  "PNs" = count(pns.DM2filter),
  "ThermoHygro" = count(thehygro.DM2filter),
  "ORNs" = count(orns.DM2filter),
  "Other_Unknown" = count(other_unknown.DM2filter)
)

resultsDM3 = data.frame(
  "putMIP LNs" = count(miplns.DM3filter),
  "Modulatory Neurons" = count(modneurons.DM3filter),
  "Other LNs" = count(otherlns.DM3filter),
  "PNs" = count(pns.DM3filter),
  "ThermoHygro" = count(thehygro.DM3filter),
  "ORNs" = count(orns.DM3filter),
  "Other_Unknown" = count(other_unknown.DM3filter)
)

resultsDM4 = data.frame(
  "putMIP LNs" = count(miplns.DM4filter),
  "Modulatory Neurons" = count(modneurons.DM4filter),
  "Other LNs" = count(otherlns.DM4filter),
  "PNs" = count(pns.DM4filter),
  "ThermoHygro" = count(thehygro.DM4filter),
  "ORNs" = count(orns.DM4filter),
  "Other_Unknown" = count(other_unknown.DM4filter)
)

resultsDM5 = data.frame(
  "putMIP LNs" = count(miplns.DM5filter),
  "Modulatory Neurons" = count(modneurons.DM5filter),
  "Other LNs" = count(otherlns.DM5filter),
  "PNs" = count(pns.DM5filter),
  "ThermoHygro" = count(thehygro.DM5filter),
  "ORNs" = count(orns.DM5filter),
  "Other_Unknown" = count(other_unknown.DM5filter)
)

resultsDM6 = data.frame(
  "putMIP LNs" = count(miplns.DM6filter),
  "Modulatory Neurons" = count(modneurons.DM6filter),
  "Other LNs" = count(otherlns.DM6filter),
  "PNs" = count(pns.DM6filter),
  "ThermoHygro" = count(thehygro.DM6filter),
  "ORNs" = count(orns.DM6filter),
  "Other_Unknown" = count(other_unknown.DM6filter)
)

resultsDP1l = data.frame(
  "putMIP LNs" = count(miplns.DP1lfilter),
  "Modulatory Neurons" = count(modneurons.DP1lfilter),
  "Other LNs" = count(otherlns.DP1lfilter),
  "PNs" = count(pns.DP1lfilter),
  "ThermoHygro" = count(thehygro.DP1lfilter),
  "ORNs" = count(orns.DP1lfilter),
  "Other_Unknown" = count(other_unknown.DP1lfilter)
)

resultsDP1m = data.frame(
  "putMIP LNs" = count(miplns.DP1mfilter),
  "Modulatory Neurons" = count(modneurons.DP1mfilter),
  "Other LNs" = count(otherlns.DP1mfilter),
  "PNs" = count(pns.DP1mfilter),
  "ThermoHygro" = count(thehygro.DP1mfilter),
  "ORNs" = count(orns.DP1mfilter),
  "Other_Unknown" = count(other_unknown.DP1mfilter)
)

resultsV = data.frame(
  "putMIP LNs" = count(miplns.Vfilter),
  "Modulatory Neurons" = count(modneurons.Vfilter),
  "Other LNs" = count(otherlns.Vfilter),
  "PNs" = count(pns.Vfilter),
  "ThermoHygro" = count(thehygro.Vfilter),
  "ORNs" = count(orns.Vfilter),
  "Other_Unknown" = count(other_unknown.Vfilter)
)

resultsVA1d = data.frame(
  "putMIP LNs" = count(miplns.VA1dfilter),
  "Modulatory Neurons" = count(modneurons.VA1dfilter),
  "Other LNs" = count(otherlns.VA1dfilter),
  "PNs" = count(pns.VA1dfilter),
  "ThermoHygro" = count(thehygro.VA1dfilter),
  "ORNs" = count(orns.VA1dfilter),
  "Other_Unknown" = count(other_unknown.VA1dfilter)
)

resultsVA1v = data.frame(
  "putMIP LNs" = count(miplns.VA1vfilter),
  "Modulatory Neurons" = count(modneurons.VA1vfilter),
  "Other LNs" = count(otherlns.VA1vfilter),
  "PNs" = count(pns.VA1vfilter),
  "ThermoHygro" = count(thehygro.VA1vfilter),
  "ORNs" = count(orns.VA1vfilter),
  "Other_Unknown" = count(other_unknown.VA1vfilter)
)

resultsVA2 = data.frame(
  "putMIP LNs" = count(miplns.VA2filter),
  "Modulatory Neurons" = count(modneurons.VA2filter),
  "Other LNs" = count(otherlns.VA2filter),
  "PNs" = count(pns.VA2filter),
  "ThermoHygro" = count(thehygro.VA2filter),
  "ORNs" = count(orns.VA2filter),
  "Other_Unknown" = count(other_unknown.VA2filter)
)

resultsVA3 = data.frame(
  "putMIP LNs" = count(miplns.VA3filter),
  "Modulatory Neurons" = count(modneurons.VA3filter),
  "Other LNs" = count(otherlns.VA3filter),
  "PNs" = count(pns.VA3filter),
  "ThermoHygro" = count(thehygro.VA3filter),
  "ORNs" = count(orns.VA3filter),
  "Other_Unknown" = count(other_unknown.VA3filter)
)

resultsVA4 = data.frame(
  "putMIP LNs" = count(miplns.VA4filter),
  "Modulatory Neurons" = count(modneurons.VA4filter),
  "Other LNs" = count(otherlns.VA4filter),
  "PNs" = count(pns.VA4filter),
  "ThermoHygro" = count(thehygro.VA4filter),
  "ORNs" = count(orns.VA4filter),
  "Other_Unknown" = count(other_unknown.VA4filter)
)

resultsVA5 = data.frame(
  "putMIP LNs" = count(miplns.VA5filter),
  "Modulatory Neurons" = count(modneurons.VA5filter),
  "Other LNs" = count(otherlns.VA5filter),
  "PNs" = count(pns.VA5filter),
  "ThermoHygro" = count(thehygro.VA5filter),
  "ORNs" = count(orns.VA5filter),
  "Other_Unknown" = count(other_unknown.VA5filter)
)

resultsVA6 = data.frame(
  "putMIP LNs" = count(miplns.VA6filter),
  "Modulatory Neurons" = count(modneurons.VA6filter),
  "Other LNs" = count(otherlns.VA6filter),
  "PNs" = count(pns.VA6filter),
  "ThermoHygro" = count(thehygro.VA6filter),
  "ORNs" = count(orns.VA6filter),
  "Other_Unknown" = count(other_unknown.VA6filter)
)

resultsVA7l = data.frame(
  "putMIP LNs" = count(miplns.VA7lfilter),
  "Modulatory Neurons" = count(modneurons.VA7lfilter),
  "Other LNs" = count(otherlns.VA7lfilter),
  "PNs" = count(pns.VA7lfilter),
  "ThermoHygro" = count(thehygro.VA7lfilter),
  "ORNs" = count(orns.VA7lfilter),
  "Other_Unknown" = count(other_unknown.VA7lfilter)
)

resultsVA7m = data.frame(
  "putMIP LNs" = count(miplns.VA7mfilter),
  "Modulatory Neurons" = count(modneurons.VA7mfilter),
  "Other LNs" = count(otherlns.VA7mfilter),
  "PNs" = count(pns.VA7mfilter),
  "ThermoHygro" = count(thehygro.VA7mfilter),
  "ORNs" = count(orns.VA7mfilter),
  "Other_Unknown" = count(other_unknown.VA7mfilter)
)

resultsVC1 = data.frame(
  "putMIP LNs" = count(miplns.VC1filter),
  "Modulatory Neurons" = count(modneurons.VC1filter),
  "Other LNs" = count(otherlns.VC1filter),
  "PNs" = count(pns.VC1filter),
  "ThermoHygro" = count(thehygro.VC1filter),
  "ORNs" = count(orns.VC1filter),
  "Other_Unknown" = count(other_unknown.VC1filter)
)

resultsVC2 = data.frame(
  "putMIP LNs" = count(miplns.VC2filter),
  "Modulatory Neurons" = count(modneurons.VC2filter),
  "Other LNs" = count(otherlns.VC2filter),
  "PNs" = count(pns.VC2filter),
  "ThermoHygro" = count(thehygro.VC2filter),
  "ORNs" = count(orns.VC2filter),
  "Other_Unknown" = count(other_unknown.VC2filter)
)

resultsVC3 = data.frame(
  "putMIP LNs" = count(miplns.VC3filter),
  "Modulatory Neurons" = count(modneurons.VC3filter),
  "Other LNs" = count(otherlns.VC3filter),
  "PNs" = count(pns.VC3filter),
  "ThermoHygro" = count(thehygro.VC3filter),
  "ORNs" = count(orns.VC3filter),
  "Other_Unknown" = count(other_unknown.VC3filter)
)

resultsVC4 = data.frame(
  "putMIP LNs" = count(miplns.VC4filter),
  "Modulatory Neurons" = count(modneurons.VC4filter),
  "Other LNs" = count(otherlns.VC4filter),
  "PNs" = count(pns.VC4filter),
  "ThermoHygro" = count(thehygro.VC4filter),
  "ORNs" = count(orns.VC4filter),
  "Other_Unknown" = count(other_unknown.VC4filter)
)

resultsVC5 = data.frame(
  "putMIP LNs" = count(miplns.VC5filter),
  "Modulatory Neurons" = count(modneurons.VC5filter),
  "Other LNs" = count(otherlns.VC5filter),
  "PNs" = count(pns.VC5filter),
  "ThermoHygro" = count(thehygro.VC5filter),
  "ORNs" = count(orns.VC5filter),
  "Other_Unknown" = count(other_unknown.VC5filter)
)

resultsVL1 = data.frame(
  "putMIP LNs" = count(miplns.VL1filter),
  "Modulatory Neurons" = count(modneurons.VL1filter),
  "Other LNs" = count(otherlns.VL1filter),
  "PNs" = count(pns.VL1filter),
  "ThermoHygro" = count(thehygro.VL1filter),
  "ORNs" = count(orns.VL1filter),
  "Other_Unknown" = count(other_unknown.VL1filter)
)

resultsVL2a = data.frame(
  "putMIP LNs" = count(miplns.VL2afilter),
  "Modulatory Neurons" = count(modneurons.VL2afilter),
  "Other LNs" = count(otherlns.VL2afilter),
  "PNs" = count(pns.VL2afilter),
  "ThermoHygro" = count(thehygro.VL2afilter),
  "ORNs" = count(orns.VL2afilter),
  "Other_Unknown" = count(other_unknown.VL2afilter)
)

resultsVL2p = data.frame(
  "putMIP LNs" = count(miplns.VL2pfilter),
  "Modulatory Neurons" = count(modneurons.VL2pfilter),
  "Other LNs" = count(otherlns.VL2pfilter),
  "PNs" = count(pns.VL2pfilter),
  "ThermoHygro" = count(thehygro.VL2pfilter),
  "ORNs" = count(orns.VL2pfilter),
  "Other_Unknown" = count(other_unknown.VL2pfilter)
)

resultsVM1 = data.frame(
  "putMIP LNs" = count(miplns.VM1filter),
  "Modulatory Neurons" = count(modneurons.VM1filter),
  "Other LNs" = count(otherlns.VM1filter),
  "PNs" = count(pns.VM1filter),
  "ThermoHygro" = count(thehygro.VM1filter),
  "ORNs" = count(orns.VM1filter),
  "Other_Unknown" = count(other_unknown.VM1filter)
)

resultsVM2 = data.frame(
  "putMIP LNs" = count(miplns.VM2filter),
  "Modulatory Neurons" = count(modneurons.VM2filter),
  "Other LNs" = count(otherlns.VM2filter),
  "PNs" = count(pns.VM2filter),
  "ThermoHygro" = count(thehygro.VM2filter),
  "ORNs" = count(orns.VM2filter),
  "Other_Unknown" = count(other_unknown.VM2filter)
)

resultsVM3 = data.frame(
  "putMIP LNs" = count(miplns.VM3filter),
  "Modulatory Neurons" = count(modneurons.VM3filter),
  "Other LNs" = count(otherlns.VM3filter),
  "PNs" = count(pns.VM3filter),
  "ThermoHygro" = count(thehygro.VM3filter),
  "ORNs" = count(orns.VM3filter),
  "Other_Unknown" = count(other_unknown.VM3filter)
)

resultsVM4 = data.frame(
  "putMIP LNs" = count(miplns.VM4filter),
  "Modulatory Neurons" = count(modneurons.VM4filter),
  "Other LNs" = count(otherlns.VM4filter),
  "PNs" = count(pns.VM4filter),
  "ThermoHygro" = count(thehygro.VM4filter),
  "ORNs" = count(orns.VM4filter),
  "Other_Unknown" = count(other_unknown.VM4filter)
)

resultsVM5d = data.frame(
  "putMIP LNs" = count(miplns.VM5dfilter),
  "Modulatory Neurons" = count(modneurons.VM5dfilter),
  "Other LNs" = count(otherlns.VM5dfilter),
  "PNs" = count(pns.VM5dfilter),
  "ThermoHygro" = count(thehygro.VM5dfilter),
  "ORNs" = count(orns.VM5dfilter),
  "Other_Unknown" = count(other_unknown.VM5dfilter)
)

resultsVM5v = data.frame(
  "putMIP LNs" = count(miplns.VM5vfilter),
  "Modulatory Neurons" = count(modneurons.VM5vfilter),
  "Other LNs" = count(otherlns.VM5vfilter),
  "PNs" = count(pns.VM5vfilter),
  "ThermoHygro" = count(thehygro.VM5vfilter),
  "ORNs" = count(orns.VM5vfilter),
  "Other_Unknown" = count(other_unknown.VM5vfilter)
)

resultsVM6 = data.frame(
  "putMIP LNs" = count(miplns.VM6filter),
  "Modulatory Neurons" = count(modneurons.VM6filter),
  "Other LNs" = count(otherlns.VM6filter),
  "PNs" = count(pns.VM6filter),
  "ThermoHygro" = count(thehygro.VM6filter),
  "ORNs" = count(orns.VM6filter),
  "Other_Unknown" = count(other_unknown.VM6filter)
)

resultsVM7d = data.frame(
  "putMIP LNs" = count(miplns.VM7dfilter),
  "Modulatory Neurons" = count(modneurons.VM7dfilter),
  "Other LNs" = count(otherlns.VM7dfilter),
  "PNs" = count(pns.VM7dfilter),
  "ThermoHygro" = count(thehygro.VM7dfilter),
  "ORNs" = count(orns.VM7dfilter),
  "Other_Unknown" = count(other_unknown.VM7dfilter)
)

resultsVM7v = data.frame(
  "putMIP LNs" = count(miplns.VM7vfilter),
  "Modulatory Neurons" = count(modneurons.VM7vfilter),
  "Other LNs" = count(otherlns.VM7vfilter),
  "PNs" = count(pns.VM7vfilter),
  "ThermoHygro" = count(thehygro.VM7vfilter),
  "ORNs" = count(orns.VM7vfilter),
  "Other_Unknown" = count(other_unknown.VM7vfilter)
)

resultsVP1d = data.frame(
  "putMIP LNs" = count(miplns.VP1dfilter),
  "Modulatory Neurons" = count(modneurons.VP1dfilter),
  "Other LNs" = count(otherlns.VP1dfilter),
  "PNs" = count(pns.VP1dfilter),
  "ThermoHygro" = count(thehygro.VP1dfilter),
  "ORNs" = count(orns.VP1dfilter),
  "Other_Unknown" = count(other_unknown.VP1dfilter)
)

resultsVP1l = data.frame(
  "putMIP LNs" = count(miplns.VP1lfilter),
  "Modulatory Neurons" = count(modneurons.VP1lfilter),
  "Other LNs" = count(otherlns.VP1lfilter),
  "PNs" = count(pns.VP1lfilter),
  "ThermoHygro" = count(thehygro.VP1lfilter),
  "ORNs" = count(orns.VP1lfilter),
  "Other_Unknown" = count(other_unknown.VP1lfilter)
)

resultsVP1m = data.frame(
  "putMIP LNs" = count(miplns.VP1mfilter),
  "Modulatory Neurons" = count(modneurons.VP1mfilter),
  "Other LNs" = count(otherlns.VP1mfilter),
  "PNs" = count(pns.VP1mfilter),
  "ThermoHygro" = count(thehygro.VP1mfilter),
  "ORNs" = count(orns.VP1mfilter),
  "Other_Unknown" = count(other_unknown.VP1mfilter)
)

resultsVP2 = data.frame(
  "putMIP LNs" = count(miplns.VP2filter),
  "Modulatory Neurons" = count(modneurons.VP2filter),
  "Other LNs" = count(otherlns.VP2filter),
  "PNs" = count(pns.VP2filter),
  "ThermoHygro" = count(thehygro.VP2filter),
  "ORNs" = count(orns.VP2filter),
  "Other_Unknown" = count(other_unknown.VP2filter)
)

resultsVP3 = data.frame(
  "putMIP LNs" = count(miplns.VP3filter),
  "Modulatory Neurons" = count(modneurons.VP3filter),
  "Other LNs" = count(otherlns.VP3filter),
  "PNs" = count(pns.VP3filter),
  "ThermoHygro" = count(thehygro.VP3filter),
  "ORNs" = count(orns.VP3filter),
  "Other_Unknown" = count(other_unknown.VP3filter)
)

resultsVP5 = data.frame(
  "putMIP LNs" = count(miplns.VP5filter),
  "Modulatory Neurons" = count(modneurons.VP5filter),
  "Other LNs" = count(otherlns.VP5filter),
  "PNs" = count(pns.VP5filter),
  "ThermoHygro" = count(thehygro.VP5filter),
  "ORNs" = count(orns.VP5filter),
  "Other_Unknown" = count(other_unknown.VP5filter)
)

resultsVP5 = data.frame(
  "putMIP LNs" = count(miplns.VP5filter),
  "Modulatory Neurons" = count(modneurons.VP5filter),
  "Other LNs" = count(otherlns.VP5filter),
  "PNs" = count(pns.VP5filter),
  "ThermoHygro" = count(thehygro.VP5filter),
  "ORNs" = count(orns.VP5filter),
  "Other_Unknown" = count(other_unknown.VP5filter)
)

  ### 9) Create a new dataframe with all of the summed results from each glomerulus...
OVERALLRESULTS <- data.frame(
  resultsD,
  resultsDA1,
  resultsDA2,
  resultsDA3,
  resultsDA4l,
  resultsDA4m,
  resultsDC1,
  resultsDC2,
  resultsDC3,
  resultsDC4,
  resultsDL1,
  resultsDL2d,
  resultsDL2v,
  resultsDL3,
  resultsDL4,
  resultsDL5,
  resultsDM1,
  resultsDM2,
  resultsDM3,
  resultsDM4,
  resultsDM5,
  resultsDM6,
  resultsDP1l,
  resultsDP1m,
  resultsV,
  resultsVA1d,
  resultsVA1v,
  resultsVA2,
  resultsVA3,
  resultsVA4,
  resultsVA5,
  resultsVA6,
  resultsVA7l,
  resultsVA7m,
  resultsVC1,
  resultsVC2,
  resultsVC3,
  resultsVC4,
  resultsVC5,
  resultsVL1,
  resultsVL2a,
  resultsVL2p,
  resultsVM1,
  resultsVM2,
  resultsVM3,
  resultsVM4,
  resultsVM5d,
  resultsVM5v,
  resultsVM6,
  resultsVM7d,
  resultsVM7v,
  resultsVP1d,
  resultsVP1l,
  resultsVP1m,
  resultsVP2,
  resultsVP3,
  resultsVP5,
  resultsVP5
)


  ###give each column of the new dataframe a name...
names(OVERALLRESULTS) <- c(
  'D.miplns','D.mod','D.otherlns','D.pns','D.thehygro','D.orns','D.unkother',
  'DA1.miplns','DA1.mod','DA1.otherlns','DA1.pns','DA1.thehygro','DA1.orns','DA1.unkother',
  'DA2.miplns','DA2.mod','DA2.otherlns','DA2.pns','DA2.thehygro','DA2.orns','DA2.unkother',
  'DA3.miplns','DA3.mod','DA3.otherlns','DA3.pns','DA3.thehygro','DA3.orns','DA3.unkother',
  'DA4l.miplns','DA4l.mod','DA4l.otherlns','DA4l.pns','DA4l.thehygro','DA4l.orns','DA4l.unkother',
  'DA4m.miplns','DA4m.mod','DA4m.otherlns','DA4m.pns','DA4m.thehygro','DA4m.orns','DA4m.unkother',
  'DC1.miplns','DC1.mod','DC1.otherlns','DC1.pns','DC1.thehygro','DC1.orns','DC1.unkother',
  'DC2.miplns','DC2.mod','DC2.otherlns','DC2.pns','DC2.thehygro','DC2.orns','DC2.unkother',
  'DC3.miplns','DC3.mod','DC3.otherlns','DC3.pns','DC3.thehygro','DC3.orns','DC3.unkother',
  'DC4.miplns','DC4.mod','DC4.otherlns','DC4.pns','DC4.thehygro','DC4.orns','DC4.unkother',
  'DL1.miplns','DL1.mod','DL1.otherlns','DL1.pns','DL1.thehygro','DL1.orns','DL1.unkother',
  'DL2d.miplns','DL2d.mod','DL2d.otherlns','DL2d.pns','DL2d.thehygro','DL2d.orns','DL2d.unkother',
  'DL2v.miplns','DL2v.mod','DL2v.otherlns','DL2v.pns','DL2v.thehygro','DL2v.orns','DL2v.unkother',
  'DL3.miplns','DL3.mod','DL3.otherlns','DL3.pns','DL3.thehygro','DL3.orns','DL3.unkother',
  'DL4.miplns','DL4.mod','DL4.otherlns','DL4.pns','DL4.thehygro','DL4.orns','DL4.unkother',
  'DL5.miplns','DL5.mod','DL5.otherlns','DL5.pns','DL5.thehygro','DL5.orns','DL5.unkother',
  'DM1.miplns','DM1.mod','DM1.otherlns','DM1.pns','DM1.thehygro','DM1.orns','DM1.unkother',
  'DM2.miplns','DM2.mod','DM2.otherlns','DM2.pns','DM2.thehygro','DM2.orns','DM2.unkother',
  'DM3.miplns','DM3.mod','DM3.otherlns','DM3.pns','DM3.thehygro','DM3.orns','DM3.unkother',
  'DM4.miplns','DM4.mod','DM4.otherlns','DM4.pns','DM4.thehygro','DM4.orns','DM4.unkother',
  'DM5.miplns','DM5.mod','DM5.otherlns','DM5.pns','DM5.thehygro','DM5.orns','DM5.unkother',
  'DM6.miplns','DM6.mod','DM6.otherlns','DM6.pns','DM6.thehygro','DM6.orns','DM6.unkother',
  'DP1l.miplns','DP1l.mod','DP1l.otherlns','DP1l.pns','DP1l.thehygro','DP1l.orns','DP1l.unkother',
  'DP1m.miplns','DP1m.mod','DP1m.otherlns','DP1m.pns','DP1m.thehygro','DP1m.orns','DP1m.unkother',
  'V.miplns','V.mod','V.otherlns','V.pns','V.thehygro','V.orns','V.unkother',
  'VA1d.miplns','VA1d.mod','VA1d.otherlns','VA1d.pns','VA1d.thehygro','VA1d.orns','VA1d.unkother',
  'VA1v.miplns','VA1v.mod','VA1v.otherlns','VA1v.pns','VA1v.thehygro','VA1v.orns','VA1v.unkother',
  'VA2.miplns','VA2.mod','VA2.otherlns','VA2.pns','VA2.thehygro','VA2.orns','VA2.unkother',
  'VA3.miplns','VA3.mod','VA3.otherlns','VA3.pns','VA3.thehygro','VA3.orns','VA3.unkother',
  'VA4.miplns','VA4.mod','VA4.otherlns','VA4.pns','VA4.thehygro','VA4.orns','VA4.unkother',
  'VA5.miplns','VA5.mod','VA5.otherlns','VA5.pns','VA5.thehygro','VA5.orns','VA5.unkother',
  'VA6.miplns','VA6.mod','VA6.otherlns','VA6.pns','VA6.thehygro','VA6.orns','VA6.unkother',
  'VA7l.miplns','VA7l.mod','VA7l.otherlns','VA7l.pns','VA7l.thehygro','VA7l.orns','VA7l.unkother',
  'VA7m.miplns','VA7m.mod','VA7m.otherlns','VA7m.pns','VA7m.thehygro','VA7m.orns','VA7m.unkother',
  'VC1.miplns','VC1.mod','VC1.otherlns','VC1.pns','VC1.thehygro','VC1.orns','VC1.unkother',
  'VC2.miplns','VC2.mod','VC2.otherlns','VC2.pns','VC2.thehygro','VC2.orns','VC2.unkother',
  'VC3.miplns','VC3.mod','VC3.otherlns','VC3.pns','VC3.thehygro','VC3.orns','VC3.unkother',
  'VC4.miplns','VC4.mod','VC4.otherlns','VC4.pns','VC4.thehygro','VC4.orns','VC4.unkother',
  'VC5.miplns','VC5.mod','VC5.otherlns','VC5.pns','VC5.thehygro','VC5.orns','VC5.unkother',
  'VL1.miplns','VL1.mod','VL1.otherlns','VL1.pns','VL1.thehygro','VL1.orns','VL1.unkother',
  'VL2a.miplns','VL2a.mod','VL2a.otherlns','VL2a.pns','VL2a.thehygro','VL2a.orns','VL2a.unkother',
  'VL2p.miplns','VL2p.mod','VL2p.otherlns','VL2p.pns','VL2p.thehygro','VL2p.orns','VL2p.unkother',
  'VM1.miplns','VM1.mod','VM1.otherlns','VM1.pns','VM1.thehygro','VM1.orns','VM1.unkother',
  'VM2.miplns','VM2.mod','VM2.otherlns','VM2.pns','VM2.thehygro','VM2.orns','VM2.unkother',
  'VM3.miplns','VM3.mod','VM3.otherlns','VM3.pns','VM3.thehygro','VM3.orns','VM3.unkother',
  'VM4.miplns','VM4.mod','VM4.otherlns','VM4.pns','VM4.thehygro','VM4.orns','VM4.unkother',
  'VM5d.miplns','VM5d.mod','VM5d.otherlns','VM5d.pns','VM5d.thehygro','VM5d.orns','VM5d.unkother',
  'VM5v.miplns','VM5v.mod','VM5v.otherlns','VM5v.pns','VM5v.thehygro','VM5v.orns','VM5v.unkother',
  'VM6.miplns','VM6.mod','VM6.otherlns','VM6.pns','VM6.thehygro','VM6.orns','VM6.unkother',
  'VM7d.miplns','VM7d.mod','VM7d.otherlns','VM7d.pns','VM7d.thehygro','VM7d.orns','VM7d.unkother',
  'VM7v.miplns','VM7v.mod','VM7v.otherlns','VM7v.pns','VM7v.thehygro','VM7v.orns','VM7v.unkother',
  'VP1d.miplns','VP1d.mod','VP1d.otherlns','VP1d.pns','VP1d.thehygro','VP1d.orns','VP1d.unkother',
  'VP1l.miplns','VP1l.mod','VP1l.otherlns','VP1l.pns','VP1l.thehygro','VP1l.orns','VP1l.unkother',
  'VP1m.miplns','VP1m.mod','VP1m.otherlns','VP1m.pns','VP1m.thehygro','VP1m.orns','VP1m.unkother',
  'VP2.miplns','VP2.mod','VP2.otherlns','VP2.pns','VP2.thehygro','VP2.orns','VP2.unkother',
  'VP3.miplns','VP3.mod','VP3.otherlns','VP3.pns','VP3.thehygro','VP3.orns','VP3.unkother',
  'VP5.miplns','VP5.mod','VP5.otherlns','VP5.pns','VP5.thehygro','VP5.orns','VP5.unkother',
  'VP5.miplns','VP5.mod','VP5.otherlns','VP5.pns','VP5.thehygro','VP5.orns','VP5.unkother')


  ###write the results into a spreadsheet and save for later...
write.csv(OVERALLRESULTS, file = "inputs_usingSchlegeletalPNmeshes.csv")

  ###clean up the global environment by removing the individual results dataframes...
remove(resultsD,resultsDA1,resultsDA2,resultsDA3,resultsDA4l,resultsDA4m,resultsDC1,resultsDC2,resultsDC3,resultsDC4,resultsDL1,resultsDL2d,resultsDL2v,resultsDL3,resultsDL4,resultsDL5,resultsDM1,resultsDM2,resultsDM3,resultsDM4,resultsDM5,resultsDM6,resultsDP1l,resultsDP1m,resultsV,resultsVA1d,resultsVA1v,resultsVA2,resultsVA3,resultsVA4,resultsVA5,resultsVA6,resultsVA7l,resultsVA7m,resultsVC1,resultsVC2,resultsVC3,resultsVC4,resultsVC5,resultsVL1,resultsVL2a,resultsVL2p,resultsVM1,resultsVM2,resultsVM3,resultsVM4,resultsVM5d,resultsVM5v,resultsVM6,resultsVM7d,resultsVM7v,resultsVP1d,resultsVP1l,resultsVP1m,resultsVP2,resultsVP3,resultsVP5,resultsVP5,OVERALLRESULTS)

  ###...and, the individual dataframes filtered for each AL glomerulus...
remove(miplns.Dfilter,modneurons.Dfilter,otherlns.Dfilter,pns.Dfilter,thehygro.Dfilter,orns.Dfilter,miplns.DA1filter,modneurons.DA1filter,otherlns.DA1filter,pns.DA1filter,thehygro.DA1filter,orns.DA1filter,miplns.DA2filter,modneurons.DA2filter,otherlns.DA2filter,pns.DA2filter,thehygro.DA2filter,orns.DA2filter,miplns.DA3filter,modneurons.DA3filter,otherlns.DA3filter,pns.DA3filter,thehygro.DA3filter,orns.DA3filter,miplns.DA4lfilter,modneurons.DA4lfilter,otherlns.DA4lfilter,pns.DA4lfilter,thehygro.DA4lfilter,orns.DA4lfilter,miplns.DA4mfilter,modneurons.DA4mfilter,otherlns.DA4mfilter,pns.DA4mfilter,thehygro.DA4mfilter,orns.DA4mfilter,miplns.DC1filter,modneurons.DC1filter,otherlns.DC1filter,pns.DC1filter,thehygro.DC1filter,orns.DC1filter,miplns.DC2filter,modneurons.DC2filter,otherlns.DC2filter,pns.DC2filter,thehygro.DC2filter,orns.DC2filter,miplns.DC3filter,modneurons.DC3filter,otherlns.DC3filter,pns.DC3filter,thehygro.DC3filter,orns.DC3filter,miplns.DC4filter,modneurons.DC4filter,otherlns.DC4filter,pns.DC4filter,thehygro.DC4filter,orns.DC4filter,miplns.DL1filter,modneurons.DL1filter,otherlns.DL1filter,pns.DL1filter,thehygro.DL1filter,orns.DL1filter,miplns.DL2dfilter,modneurons.DL2dfilter,otherlns.DL2dfilter,pns.DL2dfilter,thehygro.DL2dfilter,orns.DL2dfilter,miplns.DL2vfilter,modneurons.DL2vfilter,otherlns.DL2vfilter,pns.DL2vfilter,thehygro.DL2vfilter,orns.DL2vfilter,miplns.DL3filter,modneurons.DL3filter,otherlns.DL3filter,pns.DL3filter,thehygro.DL3filter,orns.DL3filter,miplns.DL4filter,modneurons.DL4filter,otherlns.DL4filter,pns.DL4filter,thehygro.DL4filter,orns.DL4filter,miplns.DL5filter,modneurons.DL5filter,otherlns.DL5filter,pns.DL5filter,thehygro.DL5filter,orns.DL5filter,miplns.DM1filter,modneurons.DM1filter,otherlns.DM1filter,pns.DM1filter,thehygro.DM1filter,orns.DM1filter,miplns.DM2filter,modneurons.DM2filter,otherlns.DM2filter,pns.DM2filter,thehygro.DM2filter,orns.DM2filter,miplns.DM3filter,modneurons.DM3filter,otherlns.DM3filter,pns.DM3filter,thehygro.DM3filter,orns.DM3filter,miplns.DM4filter,modneurons.DM4filter,otherlns.DM4filter,pns.DM4filter,thehygro.DM4filter,orns.DM4filter,miplns.DM5filter,modneurons.DM5filter,otherlns.DM5filter,pns.DM5filter,thehygro.DM5filter,orns.DM5filter,miplns.DM6filter,modneurons.DM6filter,otherlns.DM6filter,pns.DM6filter,thehygro.DM6filter,orns.DM6filter,miplns.DP1lfilter,modneurons.DP1lfilter,otherlns.DP1lfilter,pns.DP1lfilter,thehygro.DP1lfilter,orns.DP1lfilter,miplns.DP1mfilter,modneurons.DP1mfilter,otherlns.DP1mfilter,pns.DP1mfilter,thehygro.DP1mfilter,orns.DP1mfilter,miplns.Vfilter,modneurons.Vfilter,otherlns.Vfilter,pns.Vfilter,thehygro.Vfilter,orns.Vfilter,miplns.VA1dfilter,modneurons.VA1dfilter,otherlns.VA1dfilter,pns.VA1dfilter,thehygro.VA1dfilter,orns.VA1dfilter,miplns.VA1vfilter,modneurons.VA1vfilter,otherlns.VA1vfilter,pns.VA1vfilter,thehygro.VA1vfilter,orns.VA1vfilter,miplns.VA2filter,modneurons.VA2filter,otherlns.VA2filter,pns.VA2filter,thehygro.VA2filter,orns.VA2filter,miplns.VA3filter,modneurons.VA3filter,otherlns.VA3filter,pns.VA3filter,thehygro.VA3filter,orns.VA3filter,miplns.VA4filter,modneurons.VA4filter,otherlns.VA4filter,pns.VA4filter,thehygro.VA4filter,orns.VA4filter,miplns.VA5filter,modneurons.VA5filter,otherlns.VA5filter,pns.VA5filter,thehygro.VA5filter,orns.VA5filter,miplns.VA6filter,modneurons.VA6filter,otherlns.VA6filter,pns.VA6filter,thehygro.VA6filter,orns.VA6filter,miplns.VA7lfilter,modneurons.VA7lfilter,otherlns.VA7lfilter,pns.VA7lfilter,thehygro.VA7lfilter,orns.VA7lfilter,miplns.VA7mfilter,modneurons.VA7mfilter,otherlns.VA7mfilter,pns.VA7mfilter,thehygro.VA7mfilter,orns.VA7mfilter,miplns.VC1filter,modneurons.VC1filter,otherlns.VC1filter,pns.VC1filter,thehygro.VC1filter,orns.VC1filter,miplns.VC2filter,modneurons.VC2filter,otherlns.VC2filter,pns.VC2filter,thehygro.VC2filter,orns.VC2filter,miplns.VC3filter,modneurons.VC3filter,otherlns.VC3filter,pns.VC3filter,thehygro.VC3filter,orns.VC3filter,miplns.VC4filter,modneurons.VC4filter,otherlns.VC4filter,pns.VC4filter,thehygro.VC4filter,orns.VC4filter,miplns.VC5filter,modneurons.VC5filter,otherlns.VC5filter,pns.VC5filter,thehygro.VC5filter,orns.VC5filter,miplns.VL1filter,modneurons.VL1filter,otherlns.VL1filter,pns.VL1filter,thehygro.VL1filter,orns.VL1filter,miplns.VL2afilter,modneurons.VL2afilter,otherlns.VL2afilter,pns.VL2afilter,thehygro.VL2afilter,orns.VL2afilter,miplns.VL2pfilter,modneurons.VL2pfilter,otherlns.VL2pfilter,pns.VL2pfilter,thehygro.VL2pfilter,orns.VL2pfilter,miplns.VM1filter,modneurons.VM1filter,otherlns.VM1filter,pns.VM1filter,thehygro.VM1filter,orns.VM1filter,miplns.VM2filter,modneurons.VM2filter,otherlns.VM2filter,pns.VM2filter,thehygro.VM2filter,orns.VM2filter,miplns.VM3filter,modneurons.VM3filter,otherlns.VM3filter,pns.VM3filter,thehygro.VM3filter,orns.VM3filter,miplns.VM4filter,modneurons.VM4filter,otherlns.VM4filter,pns.VM4filter,thehygro.VM4filter,orns.VM4filter,miplns.VM5dfilter,modneurons.VM5dfilter,otherlns.VM5dfilter,pns.VM5dfilter,thehygro.VM5dfilter,orns.VM5dfilter,miplns.VM5vfilter,modneurons.VM5vfilter,otherlns.VM5vfilter,pns.VM5vfilter,thehygro.VM5vfilter,orns.VM5vfilter,miplns.VM6filter,modneurons.VM6filter,otherlns.VM6filter,pns.VM6filter,thehygro.VM6filter,orns.VM6filter,miplns.VM7dfilter,modneurons.VM7dfilter,otherlns.VM7dfilter,pns.VM7dfilter,thehygro.VM7dfilter,orns.VM7dfilter,miplns.VM7vfilter,modneurons.VM7vfilter,otherlns.VM7vfilter,pns.VM7vfilter,thehygro.VM7vfilter,orns.VM7vfilter,miplns.VP1dfilter,modneurons.VP1dfilter,otherlns.VP1dfilter,pns.VP1dfilter,thehygro.VP1dfilter,orns.VP1dfilter,miplns.VP1lfilter,modneurons.VP1lfilter,otherlns.VP1lfilter,pns.VP1lfilter,thehygro.VP1lfilter,orns.VP1lfilter,miplns.VP1mfilter,modneurons.VP1mfilter,otherlns.VP1mfilter,pns.VP1mfilter,thehygro.VP1mfilter,orns.VP1mfilter,miplns.VP2filter,modneurons.VP2filter,otherlns.VP2filter,pns.VP2filter,thehygro.VP2filter,orns.VP2filter,miplns.VP3filter,modneurons.VP3filter,otherlns.VP3filter,pns.VP3filter,thehygro.VP3filter,orns.VP3filter,miplns.VP4filter,modneurons.VP4filter,otherlns.VP4filter,pns.VP4filter,thehygro.VP4filter,orns.VP4filter,miplns.VP5filter,modneurons.VP5filter,otherlns.VP5filter,pns.VP5filter,thehygro.VP5filter,orns.VP5filter)

remove(other_unknown.DA1filter,other_unknown.DA2filter,other_unknown.DA3filter, other_unknown.DA4lfilter,other_unknown.DA4mfilter,other_unknown.DC1filter,other_unknown.DC2filter,other_unknown.DC3filter,other_unknown.DC4filter, other_unknown.Dfilter,other_unknown.DL1filter,other_unknown.DL2dfilter,other_unknown.DL2vfilter,other_unknown.DL3filter,other_unknown.DL4filter,other_unknown.DL5filter,other_unknown.DM1filter,other_unknown.DM2filter,other_unknown.DM3filter,other_unknown.DM4filter,other_unknown.DM5filter,other_unknown.DM6filter,other_unknown.DP1lfilter,other_unknown.DP1mfilter,other_unknown.VA1dfilter,other_unknown.VA1vfilter,other_unknown.VA2filter,other_unknown.VA3filter,other_unknown.VA4filter,other_unknown.VA5filter,other_unknown.VA6filter,other_unknown.VA7lfilter,other_unknown.VA7mfilter,other_unknown.VC1filter,other_unknown.VC2filter,other_unknown.VC3filter,other_unknown.VC4filter,other_unknown.VC5filter,other_unknown.Vfilter,other_unknown.VL1filter,other_unknown.VL2afilter,other_unknown.VL2pfilter,other_unknown.VM1filter,other_unknown.VM2filter,other_unknown.VM3filter,other_unknown.VM4filter,other_unknown.VM5dfilter,other_unknown.VM5vfilter,other_unknown.VM6filter,other_unknown.VM7dfilter,other_unknown.VM7vfilter,other_unknown.VP1dfilter,other_unknown.VP1lfilter,other_unknown.VP1mfilter,other_unknown.VP2filter,other_unknown.VP3filter,other_unknown.VP4filter,other_unknown.VP5filter)


  ###...and, the overall results (of course, AFTER you save it for later reference)...
remove(MIPLN1.OVERALLRESULTS)
remove(MIPLN2.OVERALLRESULTS)
remove(MIPLN3.OVERALLRESULTS)
remove(MIPLN4.OVERALLRESULTS)
remove(MIPLN5.OVERALLRESULTS)
remove(MIPLN6.OVERALLRESULTS)
remove(MIPLN7.OVERALLRESULTS)
remove(MIPLN8.OVERALLRESULTS)
remove(MIPLN9.OVERALLRESULTS)
remove(MIPLN10.OVERALLRESULTS)
remove(MIPLN11.OVERALLRESULTS)
remove(MIPLN12.OVERALLRESULTS)
remove(MIPLN13.OVERALLRESULTS)
remove(MIPLN14.OVERALLRESULTS)


  ### ***YOU SHOULD DELETE & RE-DO THE putMIP LN CONNECTOR DATAFRAMES BEFORE DOING THE OUTPUT SYNAPSES!!!***
remove(mipln1.conns,mipln10.conns,mipln11.conns,mipln12.conns,mipln13.conns,mipln14.conns,mipln2.conns,mipln3.conns,mipln4.conns,mipln5.conns,mipln6.conns,mipln7.conns,mipln8.conns,mipln9.conns)
