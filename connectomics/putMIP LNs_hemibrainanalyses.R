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


################### COLLECT putMIP LN METADATA ###############################
miplns = c("1639886198", "1640555311", "1670916819", "1671257931",
           "1701947337", "1762704980", "1857799548", "1858171556",
           "1946178096", "2041621893", "2105086391", "2135408892",
           "5813039309", "5813132949")
metadata <- neuprint_get_meta(bodyids = miplns)

  #if you so desire, then let's save this info for later...
write.csv(metadata, file = "putMIPLNs_metadata.csv")


########### CALCULATE NBLAST SCORE FOR putMIP LNs - WITH SPECIAL INTEREST IN R32F10-GAL4 SCORE ##############################
  #read in gmrdps file for NBLASTING against...
gmrdps=read.neuronlistfh('http://flyemdev.mrc-lmb.cam.ac.uk/flybrain/si/nblast/gmrdps/gmrdps.rds')
gmrdps[1:3] #just to check the first 3 neurons...

  #read in and transform hemibrain skeleton into FCWB brain space...
miplns.hb.raw = neuprint_read_neurons(bodyids = miplns)
miplns.hb.fcwb=xform_brain(miplns.hb.raw/125, sample = "JRCFIB2018F", reference = FCWB)
miplns.hb.dps = dotprops(miplns.hb.fcwb)

  #check skeleton to make sure everything looks good...
nclear3d()
plot3d(miplns.hb.fcwb[[1]], WithNodes=F, col='green')
plot3d(miplns.hb.dps[[1]], col='blue')
plot3d(FCWB)
nview3d()

  #NBLAST against all GMR lines...
options(timeout = max(2000, getOption("timeout")))
mipln1.nb <- nblast(
  miplns.hb.dps[[1]],
  target = gmrdps,
  smat = smat_alpha.fcwb,
  version = 2,
  normalised = TRUE,
  UseAlpha = TRUE)

  #save for later...
mipln1_nbvalues <- tibble::enframe(mipln1.nb)
write.xlsx2(mipln1_nbvalues, "putMIPLN1_NBLASTscores.xlsx")


mipln2.nb <- nblast(
  miplns.hb.dps[[2]],
  target = gmrdps,
  smat = smat_alpha.fcwb,
  version = 2,
  normalised = TRUE,
  UseAlpha = TRUE)

mipln2_nbvalues <- tibble::enframe(mipln2.nb)
write.xlsx2(mipln2_nbvalues, "putMIPLN2_NBLASTscores.xlsx")


mipln3.nb <- nblast(
  miplns.hb.dps[[3]],
  target = gmrdps,
  smat = smat_alpha.fcwb,
  version = 2,
  normalised = TRUE,
  UseAlpha = TRUE)

mipln3_nbvalues <- tibble::enframe(mipln3.nb)
write.xlsx2(mipln3_nbvalues, "putMIPLN3_NBLASTscores.xlsx")
#~~~~~~~ YOU GET THE IDEA.... ~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# REPEAT THE ABOVE EXAMPLE FOR THE OTHER 11 AL LNs, BY SWAPPING THE NUMBER WITHIN THE       #
# DOUBLE-BRACKETS (i.e., the next one would be written as "miplns.hb.dps[[4]]", and so on). #
# - OR, YOU COULD TRY WRITING A CUSTOM FUNCTION THAT YOU CAN USE TO PERFORM THE SAME STEPS  #
# FOR ALL 14 putMIP LNs IN ONE GO :)                                                        #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

######## GET ALL THE PUTMIP LN DATA  ###############################
  #if you just want as much data as you can get, then why not....
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1639886198',
  1639886198)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1640555311',
  1640555311)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1670916819',
  1670916819)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1671257931',
  1671257931)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1701947337',
  1701947337)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1762704980',
  1762704980)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1857799548',
  1857799548)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1858171556',
  1858171556)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/1946178096',
  1946178096)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/2041621893',
  2041621893)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/2105086391',
  2105086391)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/2135408892',
  2135408892)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/5813039309',
  5813039309)
neuprint_dump(
  dir = '/Users/YOURCOMPUTERSNAME/Desktop/putMIP LNs Metadata/5813132949',
  5813132949)

########### GET PUTMIP LN SUMMARY DATA  ###############################
  #summarize all putMIP LNs (i.e. total cable length, connectors, etc.)
putmiplns = neuprint_read_skeletons(bodyids = miplns)
mipln.sumdata = summary(putmiplns)
  #save for later....
write.xlsx2(mipln.sumdata, file = "putMIPLN Summarized info.xlsx")

######## GET ADJACENCY TABLE FOR EACH putMIP LN #######################################
  #get adjacency table...
putMIPLN.adj = neuprint_get_adjacency_matrix(bodyids = miplns)
  #save for later...
write.xlsx2(putMIPLN.adj, file = "AdjacencyTable.xlsx")
remove(putMIPLN.adj)

########### GET FLOW CENTRALITY AND SEGREGATION INDICES #######################################
  #get Flow Centrality and Segregation Indices - using recommended parameters for Hemibrain neurons!!
putmiplns.flow = flow_centrality(putmiplns,
                               mode = "centrifugal",
                               split = "distance")
  #plot the split to check it
nat::nopen3d()
nlscan_split(putmiplns.flow, WithConnectors = TRUE) 
  ### if a new 3D viewer window doesn't come up,
  ###  then roll RGL package back to the previous version!
nclear3d()

  ## Export values from these analyses...
column_names <- c("primary.branch.point", "AD.segregation.index", "max.flow.centrality", "split")
putmiplns.flowcent <- do.call(
  full_join,
  lapply(1:14, function(i) {
    df <- putmiplns.flow[[i]][column_names]
    names(df) <- column_names
    return(df)
  })
)

  ###check it out, & if it looks good then...save for later...
View(putmiplns.flowcent)
write.csv(putmiplns.flowcent, file = "FlowCentrality_putMIPLNs.csv")

remove(df,df_list,putmiplns.flowcent,a,b,c,d,e,f,g,h,i,j,k,l,m,n,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,a3,b3,c3,d3,e3,f3,g3,h3,i3,j3,k3,l3,m3,n3,a4,b4,c4,d4,e4,f4,g4,h4,i4,j4,k4,l4,m4,n4,putmipLN1.flow, putmipLN2.flow, putmipLN3.flow, putmipLN4.flow, putmipLN5.flow, putmipLN6.flow, putmipLN7.flow, putmipLN8.flow, putmipLN9.flow, putmipLN10.flow,putmipLN11.flow,putmipLN12.flow,putmipLN13.flow,putmipLN14.flow)

######## IDENTIFY NEURAL INPUTS TO putMIP LNs (prepost = 'PRE')_Demographics Analysis ##########################################
putMIPLNs_SimpConnect.inputs <- neuprint_simple_connectivity(
  miplns,
  prepost = c("PRE")
)
  #check out what neurons provide the most input to all putMIP LNs...
head(sort(table(putMIPLNs_SimpConnect.inputs$type), decreasing = T))
write.csv(putMIPLNs_SimpConnect.inputs, file = "inputs_putMIPLNs_simpconnect.csv")


########### IDENTIFY NEURONS THAT RECEIVE OUTPUT FROM putMIP LNs (prepost = 'POST')_Demographics Analysis  ##########################################
putMIPLNs_SimpConnect.outputs <- neuprint_simple_connectivity(
  miplns,
  prepost = c("POST")
)
  #if you'd like, check out what neurons provide the most input to all putMIP LNs...
  #head(sort(table(putMIPLNs_SimpConnect.outputs$type), decreasing = T))
write.csv(putMIPLNs_SimpConnect.outputs, file = "outputs_putMIPLNs_simpconnect.csv")

remove(putMIPLNs_SimpConnect.inputs, putMIPLNs_SimpConnect.outputs)


######## PLOT GENERAL CONNECTIVITY DEMOGRAPHICS ##########################################
  ###load concatenated inputs data...
pre_hb1p2_MIPLNs <- read_excel("path/to/location/where/excel/file/with/aggregated/input/counts/for/each/putMIP/LN/presynaptic/partner/is.xlsx")
inputdgraph_miplns <- data.frame(pre_hb1p2_MIPLNs)

inputdgraph_miplns$cell <- factor(inputdgraph_miplns$cell,
                                  levels = c("putmipln1","putmipln2","putmipln3","putmipln4","putmipln5",
                                             "putmipln6","putmipln7","putmipln8","putmipln9","putmipln10",
                                             "putmipln11","putmipln12","putmipln13","putmipln14"))
  ###create the plot...
ggplot(inputdgraph_miplns, aes(fill=category, y=percentage, x=cell)) + 
  geom_bar(position = "stack", stat = "identity")

  ###load concatenated outputs data...
post_hb1p2_MIPLNs <- read_excel("path/to/location/where/excel/file/with/aggregated/input/counts/for/each/putMIP/LN/presynaptic/partner/is.xlsx")
outputdgraph_miplns <- data.frame(post_hb1p2_MIPLNs)

outputdgraph_miplns$cell <- factor(outputdgraph_miplns$cell,
                                  levels = c("putmipln1","putmipln2","putmipln3","putmipln4","putmipln5",
                                             "putmipln6","putmipln7","putmipln8","putmipln9","putmipln10",
                                             "putmipln11","putmipln12","putmipln13","putmipln14"))
  ###create the plot...
ggplot(outputdgraph_miplns, aes(fill=category, y=percentage, x=cell)) + 
  geom_bar(position = "stack", stat = "identity")

remove(outputdgraph_miplns, inputdgraph_miplns, pre_hb1p2_MIPLNs, post_hb1p2_MIPLNs)

######### GET putMIP LN META INFORMATION (OVERALL # OF AXONS ("PRE"), DENDRITES ("POST"), INCOMING CONNECTIONS ("UPSTREAM"), & OUTGOING CONNECTIONS ("DOWNSTREAM")) ####################################################################
putmipln.info <- hemibrainr::alln.info %>% 
  dplyr::filter(bodyid %in% miplns)

############### FOR OVERALL PRE/POST/UP/DOWNSTREAM NUMBERS FOR EACH putMIP LN...  ##########################
mipln.info = hemibrainr::alln.info %>% 
  filter(bodyid %in% miplns) %>% 
  select(pre, post, upstream, downstream) %>% 
  write.csv(file = "TotalPrePostUpDownstream_putMIPLNs.csv")

### NOW, YOU CAN JUST USE THE NUMBERS FROM THE PREVIOUS ANALYSIS OF WHICH SYNAPSES WERE IN WHICH GLOMERULUS
###   TO ESTABLISH THE NUMBER OF PRESYNAPTIC & POSTSYNAPTIC SITES WITHIN A GIVEN GLOMERULUS!
###   THAT IS TO SAY, JUST ADD UP THE NUMBERS FOR EACH NEURON TYPE ACROSS EACH GLOMERULUS!

############### GET putMIP LN COMMON CONNECTIVITY ######################################################################################
comconnect <- neuprint_common_connectivity(
  bodyids = miplns,
  prepost = "POST")
# plot(t(comconnect))
# head(cbind(t(comconnect), sum=colSums(comconnect)))
write.csv(comconnect, file = "CommonConnect_OUTPUTS_putMIPLNs.csv")

comconnect <- neuprint_common_connectivity(
  bodyids = miplns,
  prepost = "PRE")
plot(t(comconnect))
head(cbind(t(comconnect), sum=colSums(comconnect)))
write.csv(comconnect, file = "CommonConnect_INPUTS_putMIPLNs.csv")