#### Kruskal-Wallis & boxplot of fished vs. unfished Quahog Lengths ####

# load packages
library(plyr)
library(car)

# Read in data
quahog_lengths <- read.csv("lengths_for_hist.csv")

# subset only quonnie data
quon <- subset(quahog_lengths, Pond.x == "QUON")

# subset only suction quahogs
quon <- subset(quon, Species == "Suction_Quahaug")

# clean up
quon_quahog <- quon[ rep( seq(dim(quon)[1]), quon$CountOfLength),]
quon_quahog <- quon_quahog[,c(2,5,7)]
colnames(quon_quahog)[3] <- "Harvest"

# add legal/sub-legal column
quon_quahog$Size <- ""

for (i in 1:nrow(quon_quahog)) {
  ifelse(quon_quahog$Length[i] > 25.4,
         quon_quahog$Size[i] <- "Legal",
         quon_quahog$Size[i] <- "Sub-legal")
}


# Count number of legal and sublegal per station
quon_count <- count(quon_quahog, vars = c("Station","Harvest","Size"))

quon_count <- dcast(quon_count, Station+Harvest~Size, value.var = "freq")
for (i in 3:4){quon_count[,i][is.na(quon_count[,i])] <- 0}

#### LENGTH -- Statistical Analysis ####

boxplot(Length ~ Harvest, data = quon_quahog)

# Levene's Test
leveneTest(Length ~ Harvest, data = quon_quahog)

## Not significant (p = .3381) --> K-W

# Kruskal-Wallis
kruskal.test(Length ~ Harvest, data = quon_quahog)

# means are significant (p < 0.0001)


##################################################################################

#### Fished vs. Unfished Quahog Densities ####

lengths2 <- read.csv("C:/clam_suck/data/lengths_2015and2016.csv")
stations <- read.csv("C:/clam_suck/data/clamsuck_dat_2015and2016.csv")
a <- subset(lengths2, Species == "Suction_Quahaug")
a <- a[,c(1,2,4,5,6,8,19,20)]
a <- a[ rep( seq(dim(a)[1]), a$CountOfLength),]

a <- count(a, vars = c("S_Date","Year","Time","Pond","Station","OLD.GridStatSample"))

b <- stations[,c(6,10,28)]
b <- unique(b)

c <- merge(a,b, by.x = "Station", by.y = "Station", all = F)
d <- merge(a,b, by.x = "OLD.GridStatSample", by.y = "OLD_StatGr", all = F)

c <- c[,-c(6,8)]
d <- d[,-c(6,8)]

colnames(d)[1] <- "Station"

c$Station <- as.character(c$Station)
c <- rbind(c,d)

quon_dens <- subset(c, Pond == "QUON")
colnames(quon_dens)[c(6,7)] <- c("Density","Harvest")

#### DENSITY -- Statistical Analysis ####
boxplot(Density ~ Harvest, data = quon_dens)

# Levene's Test
leveneTest(Density ~ Harvest, data = quon_dens)

## Not significant (p = .5258) --> K-W

# Kruskal-Wallis
kruskal.test(Density ~ Harvest, data = quon_dens)

## Not Significant (p = 0.1592)