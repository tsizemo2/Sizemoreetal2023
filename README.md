This repository contains code used to analyze and plot the data in: [Heterogeneous Receptor Expression Underlies Non-uniform Peptidergic Modulation of Olfaction in Drosophila](https://www.nature.com/articles/s41467-023-41012-3) by Tyler R. Sizemore, Julius Jonaitis, and Andrew M. Dacks. 


To view the original confocal scans for all light microscopy images in the main and supplementary figures, please see [the associated Zenodo repository here](https://zenodo.org/record/8127341).  


Technique-specific scripts are segregated into their own respective folders (i.e., connectomics, light microscopy, physiology). Within these folders, you will generally find scripts specific to a given figure from the manuscript. 


I STRONGLY urge those users new to connectomics (and, the analyses therein) to work their way through the well-documented/explained vignettes associated with the packages we used during the course of our study. These packages include, but are not limited to: [natverse](https://github.com/natverse/natverse), [flycircuit](https://github.com/natverse/flycircuit), [nat.nblast](https://github.com/natverse/nat.nblast), [neuprintr](https://github.com/natverse/neuprintr), and [hemibrainr](https://github.com/natverse/hemibrainr). That said, the scripts provided in this repository will allow any user - regardless of their familiarity with the aforementioned packages - to recreate the data described in our publication.    


*Two disclaimers: (1) The connectomics scripts were generally written ~2021, and many of the operations performed are likely either obsolete, or have been rolled into newer functions included in the most recent version(s) of the various connectomics packages listed above; and, (2) If you are well-versed in the R coding language, please forgive how tedious some of these scripts can be. My goal here is to provide well-notated scripts to get any user from point A-to-point B, while serving the additional purpose of helping users with very-limited coding experience understand what each step along the way is doing. Ultimately, I'd love for this to end up being the "jumping off point" for users to delve deeper into coding - as it was for me :)*

