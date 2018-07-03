
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

teams.all <- c("Russia", "Portugal", "France", "Argentina", "Saudi Arabia", "Spain", "Australia", "Iceland", "Egypt", "Morocco", "Peru", "Croatia", "Uruguay", "Iran", "Denmark", "Nigeria", "Brazil", "Germany", "Belgium", "Poland", "Switzerland", "Sweden", "Panama", "Senegal", "Costa Rica", "Mexico", "Tunisia", "Colombia", "Serbia", "Korea Republic", "England", "Japan")
teams.fav <- c("Russia", "Portugal", "France", "Argentina")


matches <- matches %>%
  filter(
      Year >= 1970
      )

matches.a <- matches %>%
  transmute(
    home.team = Home.Team.Name,
    home.goals = Home.Team.Goals,
    away.team = Away.Team.Name,
    away.goals = Away.Team.Goals
  )

matches.b <- matches %>%
  transmute(
    away.team = Home.Team.Name,
    away.goals = Home.Team.Goals,
    home.team = Away.Team.Name,
    home.goals = Away.Team.Goals
  )

rest <- expand.grid(away.team = c("Panama", "Peru", "Iceland"),
            away.goals =  0,
            home.team = c("Panama", "Peru", "Iceland"),
            home.goals = 0
            )

# peru.vs.panama <- data.frame(    
#   away.team = factor("Panama"),
#   away.goals =  0,
#   home.team =  factor("Peru"),
#   home.goals = 0
# )
# 
# panama.vs.peru <- data.frame(    
#   away.team = factor("Peru"),
#   away.goals =  0,
#   home.team =  factor("Panama"),
#   home.goals = 0
# )

matches <- rbind(matches.a, matches.b, rest)

ggplot(matches)  +
  geom_histogram(aes(home.goals), col=1) +
  geom_histogram(aes(away.goals), col=2)

#filter(matches,
#       Home.Team.Name=="Iceland"
#)

#filter(matches,
#       (Home.Team.Name=="Brazil" |
#         Away.Team.Name == "Brazil" )
#       &
#        (Home.Team.Name=="Germany" |
#         Away.Team.Name == "Germany" )
#         )


m.home <- glm(home.goals ~ home.team + away.team, family="poisson", data=matches)
m.away <- glm(away.goals ~ home.team + away.team, family="poisson", data=matches)

#m.home.logit <- glm(h ~ Home.Team.Name + Away.Team.Name, family="quasibinomial", data=matches)

#m.lm <- lm(y ~ Home.Team.Name + Away.Team.Name, data=matches)
#m.rpart <- rpart(y ~ Home.Team.Name + Away.Team.Name, data=matches, , cp=0.02)


newdata1 <- data.frame(away.team=c("Germany", "Brazil", "Belgium", "France", "Argentina"),
                       home.team=rev(c("Germany", "Brazil", "Belgium", "France", "Argentina")))

newdata2 <- data.frame(away.team=c("Germany"),
                       home.team=c("Brazil"))

newdata2 <- data.frame(away.team=all,
                       home.team=rev(all))

expand.grid(away.team=c("Germany", "Brazil", "Belgium", "France", "Argentina"),
            home.team=c("Germany", "Brazil", "Belgium", "France", "Argentina"))

summary(m.home)

testdata <- expand.grid(away.team=teams.fav,home.team=teams.fav)

prediction <- predict(m.home, newdata=testdata)

cbind(testdata, prediction)




# i <- 1
# for (x in testdata) {
#   print(x)
#   print(prediction[i])
#   i <- i+1
# }

# X <-  rbind(
#   c( 1, -1,  0,  0),
#   c( 0,  0,  1, -1),
#   c( 0, -1,  0,  1),
#   c( 1,  0,  0, -1),
#   c( 0,  1,  -1, 0)
# )
# y = c(2,1,-3,2,2)
# 
# ginv(X) %*% y
