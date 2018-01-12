### This script will approximate the relative mmr and average them among different equal sized teams
remove(list=ls())

library(combinat)

# TODO: replace this with the section header of the players relative rank
# NOTE: use . instead of spaces as fields cannot recognize spaces
sect.tier <- "what.is.your.tier"
sect.div <- "what.div"
sect.name <- "summoner.name"

# TODO: update this to be the correct directory
dataDir <- "L:\\Equal Partitioning\\data\\"
fileName <- "data.csv"
data <- read.table(paste(dataDir, fileName, sep = ""), as.is = TRUE, sep = ",", header = TRUE)

# converts the name to be usable by this code
colnames(data)[which(names(data) == sect.tier)] <- "tier"
colnames(data)[which(names(data) == sect.div)] <- "div"
colnames(data)[which(names(data) == sect.name)] <- "Name"

### approximates the mmr of each player
mmr.tier <- list(c("bronze", "unranked", "silver", "gold", "platinum", "diamond"),
                 c(830L, 1300L, 1180L, 1530L, 1880L, 2230L))
mmr.div <- 69L

# replace the div for unranked players automatically to 5
data$div[which(is.na(data$div))] <- 5

# sort the mmr ranks descending
data$mmr <- unlist(mmr.tier[2])[match(data$tier, unlist(mmr.tier[1]))] + ((5 - data$div) * mmr.div)
data <- data[order(-data$mmr),]

remove(mmr.tier)
remove(mmr.div)

# calculates average mmr
avg.mmr <- sum(data$mmr) / nrow(data)

# uses a np complete algorithm to approximate best solution
lowest.score <- 99999999
optimal.order <- NA

# TODO: choose whether you want to use the optimal solution or the pseudo-optimal
use.opt <- FALSE


### PSEUDO OPTIMAL SOLUTION - use this unless the data set is small enough
# in almost every situation, this is preferred
if (!use.opt) {
  # TODO: total amount of tries
  # 10,000 > about < 5 seconds for set of size 10
  # 100,000 > about < 1 minute for set of size 10 (probably use this)
  # 1,000,000 > about < 5 minutes for set of size 10 (not recommended)
  total.tries <- 100000
  # Randomized solution
  for (idx.tries in 1:total.tries) {
    random.order <- data.frame(
      Name = data$Name,
      mmr = data$mmr,
      stringsAsFactors = FALSE
    )
    random.order <- random.order[sample(nrow(random.order)),]
    
    # total squared average from the actual mean
    tsa <- 0
    
    for (idx.team in 0:(nrow(random.order) / 5 - 1)) {
      
      team.average <- sum(random.order$mmr[(idx.team + 1):(idx.team + 5)]) / 5
      tsa <- tsa + (abs(team.average - avg.mmr))^2
    }
    
    if (tsa < lowest.score) {
      lowest.score <- tsa
      optimal.order <- random.order
    }
  }
}

### OPTIMAL SOLUTION - consider scrapping because it is too powerful
# factorial time algorithms are infinitesmally inefficient
if (use.opt) {
  # the two permutations of mmr and names
  perm.name <- permn(data$Name)
  perm.mmr <- permn(data$mmr)
  
  ### subsets into each group
  for (i in 1:length(perm.name)) {
    perm.order <- data.frame(
      Name = perm.name[i],
      mmr = perm.mmr[i],
      stringsAsFactors = FALSE
    )
    
    # total squared average from the actual mean
    tsa <- 0
    
    for (idx.team in 0:(nrow(perm.order) / 5 - 1)) {
      
      team.average <- sum(perm.order$mmr[(idx.team + 1):(idx.team + 5)]) / 5
      tsa <- tsa + (abs(team.average - avg.mmr))^2
    }
    
    if (tsa < lowest.score) {
      lowest.score <- tsa
      optimal.order <- perm.order
    }
  }
}


teamNums <- list()
### sets the team number for each group of five
for (idx.team in 0:(nrow(optimal.order) / 5 - 1)) {
  
  for (idx.ind in 1:5) {
    teamNums[idx.team * 5 + idx.ind] <- idx.team + 1
  }
}

# sets the teamNumber to the individuals
optimal.order$teamNum <- unlist(teamNums)

write.csv(optimal.order, paste(dataDir, "Best Teams.csv", sep = ""), row.names = FALSE)

