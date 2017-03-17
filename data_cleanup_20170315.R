#### Import and Clean Clam Suck data ####
# by: Will Helt
# date modified: 3/15/17

# Steps
# 1. Import joined dataset of stations from ArcMap
# 2. Populate 2015 station names (convert from old names to new grids)
# 3. Merge station df with lengths
# 4. Derive counts per species per station per effort

# import packages
library('reshape2')
library('plyr')

# read in data
stations <- read.csv("C:/clam_suck/data/clamsuck_dat_2015and2016.csv")
lengths <- read.csv("C:/clam_suck/data/lengths_2015and2016.csv")

# Subset only stations from 2015
stations_2015 <- subset(stations, Station == " ")

stations <- stations[,-c(1:6,10)]

# Create conversion df with old and new station names from 2015
## Wrinkle:  The old and new grids do not line up, so must merge with lat & long as well
station_conversion <- stations_2015[,8:11]
colnames(station_conversion) <- c("Long","Lat","Old_Station","New_Station")

# Rename Lat and Long to proper position in stations
colnames(stations)[8:9] <- c("Long","Lat")
lengths_conv <- merge(lengths, station_conversion, by.x = c("OLD.GridStatSample"), 
               by.y = c("Old_Station"),
               all.x = T)

# Loop that merges 2015 and 2016 stations into one column
lengths_conv$New_Station <- as.character(lengths_conv$New_Station)

lengths_conv$Station[lengths_conv$Station == 0] <- NA
lengths_conv$Station[lengths_conv$Station == ""] <- NA
lengths_conv$Station <- as.character(lengths_conv$Station)

for (i in 1:nrow(lengths_conv)) {
  if (nchar(lengths_conv$New_Station[i]) < 3) {
    lengths_conv$New_Station[i] <- lengths_conv$Station[i]
  }
}
lengths_conv$Station <- factor(lengths_conv$Station)

lengths_conv <- lengths_conv[,-c(7:10,17)]
colnames(lengths_conv)[20] <- "Station"

# remove all non-shellfish species
lengths_conv <- subset(lengths_conv, Species == "Suction_Quahaug" | Species == "Rake_Quahaug" |
                         Species == "Razor clam" | Species == "Razor Clam" | Species == "Soft Shell (Steamer)" |
                         Species == "Stout Razor" | Species == "Surf Clam" | Species == "False Quahaug")

# Create a replicate column in lengths_conv to show multiple samplings of one station
replicate <- count(lengths_conv, vars = c("S_Date","Year","Time","Long","Lat.y","Station"))
replicate$Station <- factor(replicate$Station)

# Remove NA's
replicate<-subset(replicate,!(is.na(replicate["Station"])))
unique_stations <- unique(replicate$Station)
# working <- subset(replicate, Station == NA)
working <- subset(replicate, is.na(replicate$Station))


# add in replicate number column
for (j in 1:length(unique_stations)) {
  a <- subset(replicate, Station == unique_stations[j])
  ct <- seq_len(nrow(a))
  a <- cbind(a,ct)
  working <- rbind(working, a)
}
working <- working[,-7]
colnames(working)[7] <- "Replicate"

# merge replicate with lengths_conv
lengths_conv <- merge(lengths_conv, working, by = c("S_Date","Year","Time","Long","Lat.y","Station"))


# merge by station to capture location data
lengths_merge <- merge(lengths_conv, stations, by.x = "Station", by.y = "Shelllfish", all.x = TRUE)

# Save as .csv file
write.csv(lengths_conv, file = "lengths_conv.csv")



#### Count Data (account for duplicated stations) ####
count_by_spec <- aggregate(CountOfLength ~ Species+Station+Replicate, data = lengths_conv, FUN = sum)
mean_by_spec <- aggregate(CountOfLength ~ Species+Station, data = count_by_spec, FUN = mean)


# Consolidate both "Razor clam" and "Razor Clam"
for (i in 1:nrow(mean_by_spec)) {
  if (mean_by_spec$Species[i] == "Razor clam") {
    mean_by_spec$Species[i] <- "Razor Clam"
  }
}
mean_by_spec <- aggregate(CountOfLength~Species+Station, data = mean_by_spec, FUN = sum)
# Wide format
for (i in 1:nrow(count_by_spec)) {
  if (count_by_spec$Species[i] == "Razor clam") {
    count_by_spec$Species[i] <- "Razor Clam"
  }
}

counts2 <- dcast(count_by_spec, Station+Replicate ~ Species, value.var = "CountOfLength")
for (i in 3:9){counts2[,i][is.na(counts2[,i])] <- 0}




counts <- dcast(mean_by_spec, Station~ Species, value.var = "CountOfLength")
counts <- rename(counts, c("False Quahaug"="False_Quahaug",
                           "Razor Clam"="Razor_Clam",
                           "Soft Shell (Steamer)"="Steamer",
                           "Stout Razor"="Stout_Razor",
                           "Surf Clam" = "Surf_Clam"))

# convert NA in counts to 0
for (i in 2:8){counts[,i][is.na(counts[,i])] <- 0}

# Save counts file
write.csv(counts2, file = "species_counts.csv")

# MERGE COUNT DATA WITH LOCATION DATA
stations_for_merge <- stations[,-c(2,3)]
stations_for_merge <- unique(stations_for_merge)
counts_merge <- merge(counts, stations_for_merge, by.x = "Station", by.y = "Shelllfish")
