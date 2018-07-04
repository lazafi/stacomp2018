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

library(dplyr)
library(MASS)
library(statisticalModeling)
library(rpart)
library(ggplot2)

matches <- read.csv("data/WorldCupMatches.csv") 
head(matches)
str(matches)
summary(matches)
#table(matches)

all <- c("Russia", "Portugal", "France", "Argentina", "Saudi Arabia", "Spain", "Australia", "Iceland", "Egypt", "Morocco", "Peru", "Croatia", "Uruguay", "Iran", "Denmark", "Nigeria", "Brazil", "Germany", "Belgium", "Poland", "Switzerland", "Sweden", "Panama", "Senegal", "Costa Rica", "Mexico", "Tunisia", "Colombia", "Serbia", "South Korea", "England", "Japan")

matches <- matches %>%
  mutate(
    y = Home.Team.Goals - Away.Team.Goals,
    h = (Home.Team.Goals > Away.Team.Goals),
    a = (Home.Team.Goals < Away.Team.Goals)
  )


ggplot(matches)  +
  geom_histogram(aes(Home.Team.Goals), col=1) +
  geom_histogram(aes(Away.Team.Goals), col=2)

filter(matches,
       (Home.Team.Name=="Germany" |
          Away.Team.Name == "Germany" )
)

filter(matches,
       (Home.Team.Name=="Brazil" |
         Away.Team.Name == "Brazil" )
       &
        (Home.Team.Name=="Germany" |
         Away.Team.Name == "Germany" )
         )

m.home <- glm(Home.Team.Goals ~ Home.Team.Name + Away.Team.Name, family="poisson", data=matches)
m.away <- glm(Away.Team.Goals ~ Home.Team.Name + Away.Team.Name, family="poisson", data=matches)
m.result <- glm(y ~ Home.Team.Name + Away.Team.Name, family="poisson", data=matches)

m.home.logit <- glm(h ~ Home.Team.Name + Away.Team.Name, family="quasibinomial", data=matches)

m.lm <- lm(y ~ Home.Team.Name + Away.Team.Name, data=matches)
m.rpart <- rpart(y ~ Home.Team.Name + Away.Team.Name, data=matches, , cp=0.02)


newdata1 <- data.frame(Away.Team.Name=c("Germany", "Brazil", "Belgium", "France", "Argentina"),
                      Home.Team.Name=rev(c("Germany", "Brazil", "Belgium", "France", "Argentina")))

newdata2 <- data.frame(Away.Team.Name=c("Germany"),
                       Home.Team.Name=c("Brazil"))

newdata2 <- data.frame(Away.Team.Name=all,
                       Home.Team.Name=rev(all))


summary(m.home)
predict(m.home, newdata=newdata2)

predict(m.away, newdata=newdata1)
predict(m.lm, newdata=newdata1)
predict(m.home.logit, newdata=newdata1)
plot(m.rpart)

X <-  rbind(
  c( 1, -1,  0,  0),
  c( 0,  0,  1, -1),
  c( 0, -1,  0,  1),
  c( 1,  0,  0, -1),
  c( 0,  1,  -1, 0)
)
y = c(2,1,-3,2,2)

ginv(X) %*% y

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

countries <- read.csv("/home/lazafi/labor/stc-2018/data/countries of the world.csv", dec = ",")
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

