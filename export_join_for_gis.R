#### Join count data w/ centerpoint for grid ####



# read in data
counts <- read.csv("C:/clam_suck/data/species_counts_modified.csv")
lengths_conv <- read.csv("lengths_conv.csv") 
stations <- read.csv("C:/clam_suck/data/clamsuck_dat_2015and2016.csv")
stations <- stations[-c(77,75,78,73,74,61,62,63,64,82,65,66,79,70,69,68,67,60,71,72,83),]

x <-merge(counts,stations, by.x = "Station", by.y = "Shelllfish")
x <- x[,-c(9:14)]
x <- x[,-12]

# write.csv(x, file = "species_counts_for_gis.csv")

y <- merge(lengths_conv, stations, by.x = "Station", by.y = "Shelllfish")
y <- y[,c(1,10,17,18,19,49)]
write.csv(y, file = "lengths_for_hist.csv")




# write.csv(z, file = "lengths_for_boxplot.csv")
