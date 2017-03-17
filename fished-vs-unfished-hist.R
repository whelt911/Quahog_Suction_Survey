####Histogram for Length Dist. of Quahaugs in Fished vs. Unfished####

library(reshape2)

# read in data
lengths <- read.csv("lengths_for_hist.csv")

# Subset only suction quahogs
quahog_lengths <- subset(lengths, Species == "Suction_Quahaug")

quahog_lengths <- quahog_lengths[,c(3:7)]

quon <- subset(quahog_lengths, Pond.x == "QUON")
quon_quahog <- quon[ rep( seq(dim(quon)[1]), quon$CountOfLength),]

quon_quahog <- quon_quahog[,c(3,5)]
colnames(quon_quahog)[2] <- "Harvest"

pj_quahog <- subset(quahog_lengths, Pond.x == "PJ")
pj_quahog <- pj_quahog[ rep( seq(dim(pj_quahog)[1]), pj_quahog$CountOfLength),]

pj_quahog <- pj_quahog[,c(3,5)]
colnames(pj_quahog)[2] <- "Harvest"


#### Individual Histograms for Quonnie-Fished, Quonnie-Unfished, and Pt. Judith
quon_quahog_fished <- subset(quon_quahog, Harvest == "Reduced Harvest")
hist(quon_quahog_fished$Length, col = "grey", main = "",
     xlim = c(0,70), breaks = seq(0,70,5), xlab = "Length (mm)")
box()
abline(v = 25, col = "red")

quon_quahog_unfished <- subset(quon_quahog, Harvest == "Shellfish Harvest Prohibited")
hist(quon_quahog_unfished$Length, col = "grey", main = "",
     xlim = c(0,70), breaks = seq(0,70,5), xlab = "Length (mm)")
box()
abline(v = 25, col = "red")

hist(pj_quahog$Length, col = "grey", main = "",
     xlim = c(0,70), breaks = seq(0,70,5), xlab = "Length (mm)", ylim = c(0,20))
box()
abline(v = 25, col = "red")

#### Quahog Overlayed Plot Histogram ####
hist(subset(quon_quahog, Harvest == "Reduced Harvest")$Length, col=rgb(1,0,0,0.5),
     xlim = c(0,70), breaks = seq(0,70,5), ylim = c(0,.1), prob = TRUE,
     main= NULL, xlab="Length (mm)", ylab= "Proportion at Length")
hist(subset(quon_quahog, Harvest == "Shellfish Harvest Prohibited")$Length, col=rgb(0,0,1,0.5),
     breaks = seq(0,70,5), add=T, prob = TRUE)
abline(v = 25, col = "red")
box()
legend("topright", c(paste('Reduced Harvest','(n=',as.character(
  length(subset(quon_quahog, Harvest == "Reduced Harvest")$Length)),')'),
  paste('Harvest Prohibited','(n=',as.character(
  length(subset(quon_quahog, Harvest == "Shellfish Harvest Prohibited")$Length)),')')),
  fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))


# save length of quahog file
write.csv(quahog_lengths, file = "suction_quahog_lengths.csv")


####Histogram for steamers for Quonnie Pond ####
steamer <- subset(lengths, Species == "Soft Shell (Steamer)")

steamer <- steamer[,c(3:7)]

steamer_quon <- subset(steamer, Pond.x == "QUON")
steamer_quon <- steamer_quon[ rep( seq(dim(steamer_quon)[1]), steamer_quon$CountOfLength),]

steamer_quon <- steamer_quon[,3]

hist(steamer_quon, xlim = c(0,100), ylim = c(0,15), breaks = seq(0,90,5), main= NULL, 
     xlab="Length (mm)", ylab= "Proportion at Length", col = "grey")
abline(v = 50, col = "red")



#### Histogram for Stout Razor across Quonnie Fvs.U & PJ

stout_razor <- subset(lengths, Species == "Stout Razor")

stout_razor <- stout_razor[,c(3:7)]

stout_razor <- stout_razor[ rep( seq(dim(stout_razor)[1]), stout_razor$CountOfLength),]

#subset by pond
stout_razor_quon <- subset(stout_razor, Pond.x == "QUON") 
stout_razor_pj <- subset(stout_razor, Pond.x == "PJ")


# PJ Histogram

hist(stout_razor_pj$Length, xlim = c(0,120),ylim = c(0,30), breaks = seq(0,110,5), main= NULL, 
     xlab="Length (mm)", ylab= "Proportion at Length", col = "grey")
# abline(v = 50, col = "red") # is there minimum limit for stout razor?

# Quonnie Histogram
## Combined
hist(stout_razor_quon$Length, xlim = c(0,100), ylim = c(0,100), breaks = seq(0,90,5), main= NULL, 
     xlab="Length (mm)", ylab= "Proportion at Length", col = "grey")

## Parsed with fished vs unfished
colnames(stout_razor_quon)[5] <- "Harvest"

hist(subset(stout_razor_quon, Harvest == "Reduced Harvest")$Length, col=rgb(1,0,0,0.5),
     xlim = c(0,100), breaks = seq(0,100,5), ylim = c(0,.2), prob = TRUE, add = F,
     main= NULL, xlab="Length (mm)", ylab= "Proportion at Length")
hist(subset(stout_razor_quon, Harvest == "Shellfish Harvest Prohibited")$Length, col=rgb(0,0,1,0.5),
     breaks = seq(0,100,5), add=T, prob = TRUE)
abline(v = 25.4, col = "red")
box()
legend("topright", c(paste('Reduced Harvest','(n=',as.character(
  length(subset(stout_razor_quon, Harvest == "Reduced Harvest")$Length)),')'),
  paste('Harvest Prohibited','(n=',as.character(
    length(subset(stout_razor_quon, Harvest == "Shellfish Harvest Prohibited")$Length)),')')),
  fill = c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))