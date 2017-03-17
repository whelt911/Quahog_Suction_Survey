####CLEAN AND MERGE DATA####

# by: Will Helt
# date modified: 1/27/17

# import packages

library('reshape2')
library('plyr')

# set working directory
setwd("C:/clam_suck/Rwd")

# read in data
stations <- read.csv("C:/clam_suck/data/stations2016.csv")
lengths <- read.csv("C:/clam_suck/data/lengths2016.csv")
counts <- read.csv("C:/clam_suck/data/counts2016.csv")

####Prepare data for Eric's SHPfile####
# reference counts df by station ID to GPS to stations df
# convert counts$Species column to wide format where each species has its own column
## this will be easier to visualize in ArcMap
# save file as .csv for import into ArcMap

counts <- counts[,-12]

counts <- dcast(counts, S_Date + Station + Pond ~ Species, value.var = "CountOfLength")
dat <- merge(stations, counts, by = c("Station","Pond"))

# give colnames underscores
dat <- rename(dat, c("False Quahaug"="False_Quahaug","Razor Clam"="Razor_Clam","Soft Shell (Steamer)"="Steamer",
              "Stout Razor"="Stout_Razor"))

# convert NA in counts to 0
for (i in 6:11){dat[,i][is.na(dat[,i])] <- 0}


# Save file
write.csv(dat, file = "clamsuck_dat_2016.csv")
