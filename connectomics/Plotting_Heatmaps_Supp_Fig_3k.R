#this script was used to plot generally every heatmap found in the original report.
# in this example, I am plotting putative MIPergic LN (putMIP LN-to-putMIP LN) inter-connectivity by 
# each AL glomerulus.

########### LOAD DATA #############################################
putMIPLN_to_putMIPLNbyGlom <- read_excel("path/to/your/data/file.xlsx")

########### PLOT #############################################
ggplot(putMIPLN_to_putMIPLNbyGlom, aes(x = forcats::fct_inorder(mipln), y = glomeruli, fill = percent_input)) + 
  geom_tile(aes(fill = percent_input)) +
  scale_y_discrete(limits=rev) +
  scale_fill_continuous(low = "#FFFFFF", high = "#000000",
                        name = "% input") + 
  labs(x = "putMIP LN #", y = "Glomerulus")
  

