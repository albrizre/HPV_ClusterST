R code for the paper *A spatio-temporal clustering model to describe human papillomavirus vaccination coverage in the Valencian Community, Spain*, by Mónica López-Lacort, Ana Corberán-Vallet, Álvaro Briz-Redón, Francisco J. Santonja Gómez, Cintia Muñoz-Quiles, and Alejandro Orrico-Sánchez.

The files *SCluster_spacetime_12_13.R* and *SCluster_spacetime_14.R* contain the NIMBLE codes that allow implementing the spatio-temporal model proposed in the paper with the *nimble* R package. Both models are essentially the same, but the one in *SCluster_spacetime_12_13.R* only includes 3 parametric trends, instead of 4 (the random walk trend is not considered for the 12-year-old girls cohort studied in the paper).
