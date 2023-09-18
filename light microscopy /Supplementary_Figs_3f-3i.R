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
packages <- c("readxl", "tidyverse", "ggplot2", "ggsci")
ipak(packages)
rm(packages, ipak)


#################### LOAD DATA ##########################################################################
WuTangClan <- read_excel("location/to/aggregated/excel/file/for/puncta/density/data.xlsx")

#################### CALCULATE THE LINEAR MODEL AND PLOT THE DATA ##########################################################################
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplot(WuTangClan, aes(x = GFP_mean, y = sytGFP_mean)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "black")
#`geom_smooth()` using formula 'y ~ x'


ggplotRegression(lm(sytGFP_mean ~ GFP_mean, data = WuTangClan))
