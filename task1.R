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

countries <- read.csv("data/countries of the world.csv", dec = ",")
# countries <- read.csv("data/countries of the world.csv", dec = ",", colClasses = c(
#   #Country                           : 
#     "Factor",
#   #Region
#   "Factor",
#   #Population
#   "int",
#   #Area..sq..mi..
#   "int",
#   #Pop..Density..per.sq..mi..
#   "numeric",
#   #Coastline..coast.area.ratio.
#   "numeric",
#   #Net.migration
#   "numeric",
#   #Infant.mortality..per.1000.births.
#   "numeric",
#   #GDP....per.capita.           
#   "int",
#   #Literacy....                      
#   "numeric",
#   #Phones..per.1000.                 
#   "numeric",
#   #Arable....                        
#   "numeric",
#   #Crops....                         
#   "numeric",
#   #Other....                         
#   "numeric",
#   #Climate                           
#   "Factor",
#   #Birthrate                         
#   "numeric",
#   #Deathrate                         
#   "numeric",
#   #Agriculture                       
#   "numeric",
#   #Industry                          
#   "numeric",
#   #Service                           
#   "numeric"
# )) 
head(countries)
str(countries)
summary(countries)
#library(aplpack) 
#faces(countries[3:20])
countries.reg <- countries[order(countries$Region),]
stars(countries.reg[3:20], labels=countries.reg$Country, len=0.7, cex=0.7,  key.loc=c(15,1.5), flip.labels=FALSE,  draw.segments = TRUE)
library(MASS)
parcoord(countries[3:20], col=countries$Climate)

#table(cups)

