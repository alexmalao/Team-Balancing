### This script will approximate the relative mmr and average them among different equal sized teams
remove(list=ls())

library(combinat)

# TODO: replace this with the section header of the players relative rank
# NOTE: use . instead of spaces as fields cannot recognize spaces
sect.tier <- "what.is.your.tier"
sect.div <- "what.div"
sect.name <- "summoner.name"

# TODO: update this to be the correct directory
dataDir <- "D:\\cs4500\\Team-Balancing\\data\\"
fileName <- "data.csv"
data <- read.table(paste(dataDir, fileName, sep = ""), as.is = TRUE, sep = ",", quote="", header = TRUE)

# converts the name to be usable by this code
colnames(data)[which(names(data) == sect.tier)] <- "tier"
colnames(data)[which(names(data) == sect.div)] <- "div"
colnames(data)[which(names(data) == sect.name)] <- "name"

### approximates the mmr of each player
mmr.tier <- list(c("bronze", "unranked", "silver", "gold", "platinum", "diamond"),
                 c(470.0, 830.0, 1300.0, 1180.0, 1530.0, 1880.0, 2230.0))
mmr.div <- 87.5

# replace the div for unranked players automatically to 4
data$div[which(is.na(data$div))] <- 4

# sort the mmr ranks descending
data$mmr <- unlist(mmr.tier[2])[match(data$tier, unlist(mmr.tier[1]))] + ((4 - data$div) * mmr.div)
data <- data[order(-data$mmr),]

remove(mmr.tier)
remove(mmr.div)

# calculates average mmr
avg.mmr <- sum(data$mmr) / nrow(data)

# uses a np complete algorithm to approximate best solution
lowest.score <- 99999999
optimal.order <- NA

# TODO: choose whether you want to use the optimal solution or the pseudo-optimal
# optimal solution will take years past a set of size 10.
use.opt <- FALSE
use.duo <- TRUE

if (nrow(data) %% 5 != 0) {
  stop("Stop, you have violated the law! Pay the court a fine or serve a sentence. Your teams are now forfeit.")
}


# takes two ints and a data frame and swaps the rows
swap.rows <- function(val1, val2, df) {
  row1 <- df[val1,]
  
  df.copy <- data.frame(df)
  df.copy[val1,] <- df[val2,]
  df.copy[val2,] <- row1
  return(df.copy)
}

# finds the position of the duo player
# set isLeft based on the duo player
duo.placement <- function(duo.number, isLeft = TRUE) {
  total.teams <- nrow(data) / 5
  
  if (isLeft) {
    start.idx <- 1
    spot.idx <- 1
    while (duo.number > 1) {
      if (spot.idx + 5 > nrow(data)) {
        if (start.idx + 2 > 5) {
          stop("Too many duo queues! Please remove some duos.")
        }
        else {
          start.idx <- start.idx + 2
          spot.idx <- start.idx
        }
      }
      else {
        spot.idx <- spot.idx + 5
      }
      duo.number <- duo.number - 1
    }
    return(spot.idx)
  }
  else {
    start.idx <- 2
    spot.idx <- 2
    while (duo.number > 1) {
      if (spot.idx + 5 > nrow(data)) {
        if (start.idx + 2 > 5) {
          stop("Too many duo queues! Please remove some duos.")
        }
        else {
          start.idx <- start.idx + 2
          spot.idx <- start.idx
        }
      }
      else {
        spot.idx <- spot.idx + 5
      }
      duo.number <- duo.number - 1
    }
    return(spot.idx)
  }
}


# Grabs all the duo pairs into a list of pairs
duo.queue1 <- list()
duo.queue2 <- list()

for (idx in 1:nrow(data)) {
  if (data$duo[[idx]] != "") {
    
    if(!(data$duo[[idx]] %in% duo.queue1
       || data$duo[[idx]] %in% duo.queue2)) {
      duo.queue1[[length(duo.queue1) + 1]] <- data$name[[idx]]
      duo.queue2[[length(duo.queue2) + 1]] <- data$duo[[idx]]
    }
  }
}


### PSEUDO OPTIMAL SOLUTION - use this unless the data set is small enough
# in almost every situation, this is preferred
if (!use.opt) {
  # TODO: total amount of tries
  # 10,000 > about < 5 seconds for set of size 10
  # 100,000 > about < 1 minute for set of size 10 (recommended)
  # 1,000,000 > about < 5 minutes for set of size 10
  total.tries <- 30000
  total.swaps <- 100000
  unswappable <- list()
  bonus.elo <- rep(0L, nrow(data) / 5)
  if (use.duo) {
    for (idx in 1:length(duo.queue1)) {
      unswappable[[length(unswappable) + 1]] <- duo.placement(idx, TRUE)
      unswappable[[length(unswappable) + 1]] <- duo.placement(idx, FALSE)
      bonus.elo[idx %% length(bonus.elo)] <- 69L + bonus.elo[idx %% length(bonus.elo)]
    }
  }
  
  # Randomized solution
  for (idx.tries in 1:total.tries) {
    random.order <- data.frame(
      name = data$name,
      mmr = data$mmr,
      stringsAsFactors = FALSE
    )
    random.order <- random.order[sample(nrow(random.order)),]
    
    if (idx.tries %% 10000 == 0) {
      print(paste("random ordering: ", idx.tries))
    }
    
    # total squared average from the actual mean
    tsa <- 0
    
    # adds the duo queue players to their team positions
    if (use.duo) {
    for (idx in 1:length(duo.queue1)) {
      left.summoner <- which(random.order$name %in% duo.queue1[[idx]])
      right.summoner <- which(random.order$name %in% duo.queue2[[idx]])
      
      random.order <- swap.rows(duo.placement(idx, TRUE), left.summoner, random.order)
      random.order <- swap.rows(duo.placement(idx, FALSE), right.summoner, random.order)
    }
    }
    
    # calculates teams average and adds the squared difference to total squared average
    for (idx.team in 0:(nrow(random.order) / 5 - 1)) {
      idx.offset <- idx.team * 5
      team.average <- sum(random.order$mmr[(idx.offset + 1):(idx.offset + 5)]) / 5 + bonus.elo[[idx.team + 1]]
      tsa <- tsa + as.integer((team.average - avg.mmr)^2)
    }
    
    if (tsa < lowest.score) {
      print(paste("lowest score dropped from ", lowest.score, " to ", tsa))
      lowest.score <- tsa
      optimal.order <- random.order
    }
  }
  
  swappable <- list()
  for (idx in 1:nrow(data)) {
    if (!(idx %in% unswappable)) {
      swappable[[length(swappable) + 1]] <- idx
    }
  }
  
  # loop to calculate more effective pairs
  for (idx.tries in 1:total.swaps) {
    
    if (idx.tries %% 10000 == 0) {
      print(paste("random swapping: ", idx.tries))
    }
    
    int.swap <- sample.int(length(swappable), 2)
    swap1 <- int.swap[[1]]
    swap2 <- int.swap[[2]]
    random.order <- swap.rows(swappable[[swap1]], swappable[[swap2]], optimal.order)
    
    # total squared average from the actual mean
    tsa <- 0
    
    # calculates teams average and adds the squared difference to total squared average
    for (idx.team in 0:(nrow(random.order) / 5 - 1)) {
      idx.offset <- idx.team * 5
      team.average <- sum(random.order$mmr[(idx.offset + 1):(idx.offset + 5)]) / 5 + bonus.elo[[idx.team + 1]]
      tsa <- tsa + as.integer((team.average - avg.mmr)^2)
    }
    
    if (tsa < lowest.score) {
      print(paste("lowest score swapped from ", lowest.score, " to ", tsa))
      lowest.score <- tsa
      optimal.order <- random.order
    }
  }
}


### OPTIMAL SOLUTION - consider scrapping because it is too powerful
# factorial time algorithms are infinitesmally inefficient
if (use.opt) {
  # the two permutations of mmr and names
  perm.name <- permn(data$name)
  perm.mmr <- permn(data$mmr)
  
  ### subsets into each group
  for (i in 1:length(perm.name)) {
    perm.order <- data.frame(
      name = perm.name[i],
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
teamNums.mmr <- list()
### sets the team number for each group of five
for (idx.team in 0:(nrow(optimal.order) / 5 - 1)) {
  
  team.mmr <- 0
  for (idx.ind in 1:5) {
    teamNums[idx.team * 5 + idx.ind] <- idx.team + 1
    team.mmr <- team.mmr + optimal.order$mmr[idx.team * 5 + idx.ind]
  }
  team.mmr <- team.mmr / 5
  
  for (idx.ind in 1:5) {
    teamNums.mmr[idx.team * 5 + idx.ind] <- team.mmr
  }
}

print(paste("Average MMR: ", avg.mmr, sep=""))
print(paste("Lowest Total Squared Average: ", lowest.score, sep=""))

# sets the teamNumber to the individuals
optimal.order$teamNum <- unlist(teamNums)
optimal.order$teamMmr <- unlist(teamNums.mmr)

write.csv(optimal.order, paste(dataDir, "Best Teams.csv", sep = ""), row.names = FALSE)

