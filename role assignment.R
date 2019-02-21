### This script will give teams the optimal roles so the most amount of people their optimal roles
remove(list=ls())

library(combinat)

# TODO: replace this with the section header of the relative roles' names
# NOTE: use . instead of spaces as fields cannot recognize spaces
sect.top <- "top"
sect.jng <- "jungle"
sect.mid <- "middle"
sect.adc <- "bottom"
sect.sup <- "support"
sect.name <- "summoner.name"

# TODO: update this to be the correct directory
dataDir <- "F:\\Team-Balancing\\data\\"
# TODO: input the Best Teams file crated from balance.R
bestTeams.name <- "Best Teams.csv"
# TODO: input the role preferences file
rolePref.name <- "data.csv"

# TODO: use check.names = FALSE if quote="" is still needed
# bestTeams should be in format from balance.R script
bestTeams.data <- read.table(paste(dataDir, bestTeams.name, sep = ""),
                             as.is = TRUE, sep = ",", header = TRUE, check.names = FALSE)
rolePref.data <- read.table(paste(dataDir, rolePref.name, sep = ""),
                            as.is = TRUE, sep = ",", quote="", header = TRUE)

colnames(rolePref.data)[which(names(rolePref.data) == sect.name)] <- "name"
colnames(rolePref.data)[which(names(rolePref.data) == sect.top)] <- "top"
colnames(rolePref.data)[which(names(rolePref.data) == sect.jng)] <- "jng"
colnames(rolePref.data)[which(names(rolePref.data) == sect.mid)] <- "mid"
colnames(rolePref.data)[which(names(rolePref.data) == sect.adc)] <- "adc"
colnames(rolePref.data)[which(names(rolePref.data) == sect.sup)] <- "sup"

if (nrow(bestTeams.data) != nrow(rolePref.data)) {
  stop("The datasets have a mismatch in player count.")
}

if (nrow(rolePref.data) %% 5 != 0) {
  stop("The amount of players is not divisible by 5.")
}

# assume summoner names are unique
# order the sets by name
rolePref.data <- rolePref.data[order(rolePref.data$name),]
bestTeams.data <- bestTeams.data[order(bestTeams.data$name),]

# add the team numbers into the players
rolePref.data$teamNum <- bestTeams.data$teamNum

# add the team mmr for the players
rolePref.data$teamMmr <- bestTeams.data$teamMmr

# sort rolePref.data by team numbers
rolePref.data <- rolePref.data[order(rolePref.data$teamNum),]

teamOrderList <- list()

### sets the optimal team organization for each group of five
for (idx.team in 0:(nrow(rolePref.data) / 5 - 1)) {

  #teamList <- rolePref.data[1:5,]
  teamList <- rolePref.data[(idx.team * 5 + 1):(idx.team * 5 + 5),]
  
  permNames <- permn(teamList$name)
  
  bestScore <- 99999
  bestOrder <- c()
  
  for (iter in permNames) {
    
    curScore <- 0
    
    # switches it so more medium high values instead of most high values
    # uses squared average to weight down the 1s and 2s more
    for (idx in 1:5) {
      curScore <- curScore +
        switch(
          idx,
          6 - teamList[which(teamList$name == iter[idx]),]$top,
          6 - teamList[which(teamList$name == iter[idx]),]$jng,
          6 - teamList[which(teamList$name == iter[idx]),]$mid,
          6 - teamList[which(teamList$name == iter[idx]),]$adc,
          6 - teamList[which(teamList$name == iter[idx]),]$sup
        ) ^ 2
    }
    
    if (curScore < bestScore) {
      bestScore <- curScore
      bestOrder <- iter
    }
  }
  
  for (idx in 1:5) {
    teamOrderList[idx.team * 5 + idx] <- bestOrder[idx]
  }
}

# wow works really well
rolePref.data <- rolePref.data[match(teamOrderList, rolePref.data$name),]

teamRoles <- list()
# given just this, the order is sorted for top jng mid adc sup
### sets the team number for each group of five
for (idx.team in 0:(nrow(rolePref.data) / 5 - 1)) {
  
  for (idx.ind in 1:5) {
    teamRoles[idx.team * 5 + idx.ind] <- switch(idx.ind,
                                                "top",
                                                "jungle",
                                                "mid",
                                                "adc",
                                                "support")
  }
}

# sets indicators for the actual role based on order
rolePref.data$role <- unlist(teamRoles)


write.csv(rolePref.data, paste(dataDir, "Final Teams.csv", sep = ""), row.names = FALSE)