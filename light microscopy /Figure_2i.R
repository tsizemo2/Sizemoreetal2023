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
pkgs <- c("readxl", "tidyverse", "Hmisc", "corrplot", "RColorBrewer")
ipak(pkgs)
rm(pkgs, ipak)


############## LOAD DATA #############################################
InnervationData <- read_excel("path/to/location/of/excel/file/with/thebinaryglomerulusinnervationdataforindividualMIPergicLNs.xlsx")

############## DATA WRANGLING #############################################
    # YOU HAVE TO GET RID OF THE "REFERENCE" COLUMN IN THE INNERVATION
    # DATAFRAME BEFORE YOU CAN RUN A CORRELATION ANALYSIS ON IT!
d2 <- InnervationData %>% 
  select(D:VM7)

############## BINARY GLOMERULUS CO-INNERVATION CORRELATION ANALYSIS - CALCULATE PEARSON'S CORRELATION COEFFICIENT #############################################
    # Run the correlation analysis! 
    # For innervation data, using either "everything" 
    # or "pairwise.complete.obs" give the same results!
d2.cor <- cor(d2, use = "pairwise.complete.obs", method = "pearson")

  # Check that everything looks good!
head(round(d2.cor,2)) # the "2" is so the "round" function rounds to 2 decimals! 
  
  # If everything looks good, then write it out to a .csv file for later reference!
write.csv(d2.cor, file = "GlomPairInnervationCorrelations.csv")

############## BINARY GLOMERULUS CO-INNERVATION CORRELATION ANALYSIS - RESOLVE WHICH CORRELATIONS ARE STATISTICALLY SIGNIFICANT #############################################
d2.cor.significance <- rcorr(as.matrix(d2), type = "pearson")
round(d2.cor.significance$P, 3)
round(d2.cor.significance$r, 2)

  # If nothing looks "off" then write these significance values out to a .csv file...
pairwisecorr_sig <- as.data.frame(d2.cor.significance$P)
  
  # SAVE...
write.csv(pairwisecorr_sig, file = "pairinnervcorr_significancevalues.csv")

############## PLOTTING THE PAIRWISE CORRELATION VALUES #############################################
# mycols = c("#D100FA", "#", "#000000")
# names(mycols) = c("odorant", "tastant", "combo")

corrplot::corrplot(d2.cor, method = "color",
         order = "hclust", hclust.method = "ward.D2",
         tl.col = "black", tl.cex = 0.5, tl.srt = 45,
         col = colorRampPalette(c("#D100FA", "#FFFFFF", "#000000"))(500))

# clear env
remove(d2,d2.cor,d2.cor.significance,InnervationData,pairwisecorr_sig)

           