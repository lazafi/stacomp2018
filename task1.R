matches <- read.csv("data/WorldCupMatches.csv",
                    colClasses = c("factor",       #year
                                   "character",         #Datetime
                                   "factor",       #stage
                                   "factor",      #stadium
                                   "factor",      #city 
                                   "factor",      #home name
                                   "integer",     #home goals
                                   "integer",     #away goals
                                   "factor",      #away name
                                   "character",   #win cond
                                   "integer",     #attendance
                                   "integer",    # half home goals
                                   "integer",     # half away goals
                                   "factor",     # referee
                                   "factor",    #ass 1
                                   "factor",   # ass2
                                   "integer",   # round id
                                   "integer",    # match id
                                   "factor",    #home team
                                   "factor"     #away team
                    )) 
head(matches)
str(matches)
summary(matches)
#table(matches)

players <- read.csv("data/WorldCupPlayers.csv") 
head(players)
str(players)
summary(players)
#table(players)

ranking <- read.csv("data/fifa_ranking.csv") 
head(ranking)
str(ranking)
summary(ranking)
#table(ranking)

cups <- read.csv("data/WorldCups.csv") 
head(cups)
str(cups)
summary(cups)
#table(cups)

countries <- read.csv("data/countries of the world.csv") 
head(countries)
str(countries)
summary(countries)
#table(cups)

