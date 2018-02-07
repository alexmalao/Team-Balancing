### ALEX: this code will first calculate the MMR of each player and then will calculate the MMR of each
# team and give it to you. If you want to manually choose the teams, this code will be of use.
remove(list=ls())

library(combinat)

# TODO: replace this with the section header of the players relative rank
# NOTE: use . instead of spaces as fields cannot recognize spaces
sect.tier <- "what.is.your.tier"
sect.div <- "what.div"
sect.name <- "summoner.name"
sect.team <- "team.name"

# TODO: update this to be the correct directory
dataDir <- "L:\\Equal Partitioning\\data\\"
fileName <- "data.csv"
data <- read.table(paste(dataDir, fileName, sep = ""), as.is = TRUE, sep = ",", quote="", header = TRUE)

# converts the name to be usable by this code
colnames(data)[which(names(data) == sect.tier)] <- "tier"
colnames(data)[which(names(data) == sect.div)] <- "div"
colnames(data)[which(names(data) == sect.name)] <- "name"
if (sect.team %in% colnames(data)) {
  colnames(data)[which(names(data) == sect.team)] <- "team"
  data$team.elo <- NA
}

### approximates the mmr of each player
mmr.tier <- list(c("bronze", "unranked", "silver", "gold", "platinum", "diamond"),
                 c(830L, 1300L, 1180L, 1530L, 1880L, 2230L))
mmr.div <- 69L

# replace the div for unranked players automatically to 5
data$div[which(is.na(data$div))] <- 5

# sort the mmr ranks descending
data$mmr <- unlist(mmr.tier[2])[match(data$tier, unlist(mmr.tier[1]))] + ((5 - data$div) * mmr.div)
data <- data[order(data$team),]

remove(mmr.tier)
remove(mmr.div)

# calculates average mmr
avg.mmr <- sum(data$mmr) / nrow(data)

print(paste("This is the average team mmr: ", avg.mmr, sep=""))

mmr.points <- 0

team.list <- unique(data$team)
# this code will calculate the average mmr of each team
# NOTE: this code will still work if teams are not made
if ("team" %in% colnames(data)) {
  
  for (team.name in team.list) {
    if (is.na(team.name)) {
      next
    }
    
    cur.team <- data[which(data$team == team.name),]
    
    team.mmr <- sum(cur.team$mmr) / nrow(cur.team)
    cur.team$team.elo <- team.mmr
    
    for (idx in 1:nrow(data)) {
      if (is.na(data$team[idx])) {
        break
      }
      if (data$team[idx] == team.name) {
        data$team.elo[idx] <- team.mmr
      }
    }
    
    mmr.points <- (abs(team.mmr - avg.mmr)) ^ 2
  }
  
  # the best teams will have the lowest mmr points
  # TODO: compare this to the Lowest Total Squared Average from balance.R
  print(paste("This is the total mmr points for these teams: ", mmr.points, sep=""))
}

write.csv(data, paste(dataDir, "Team Elo.csv", sep = ""), row.names = FALSE)