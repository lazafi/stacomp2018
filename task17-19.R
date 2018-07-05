
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
      Away.Team.Name %in% teams.all | Home.Team.Name %in% teams.all,
      Year >= 1990,
      Win.conditions != ""
      
      )

some.team <- matches %>%
  filter(Home.Team.Name=="Italy")

some.home.hist <- some.team %>%
  group_by(Home.Team.Goals) %>%
  summarize(density=n()/nrow(.)) %>%
  mutate(goals = Home.Team.Goals)
  
some.home.hist$pois <- dpois(some.home.hist$goals, mean(some.team$Home.Team.Goals))

ggplot(some.home.hist)  +
  geom_col(aes(x=goals,y=density, fill=as.factor("real"))) +
  geom_line(aes(x=goals,y=pois, fill=as.factor("poisson")), color="#f8766d", size=2) +
  scale_fill_manual(values=c("#f8766d","#04bfc4"), name="Verteilung") 

some.away.hist <- some.team %>%
  group_by(Away.Team.Goals) %>%
  summarize(density=n()/nrow(.)) %>%
  mutate(goals = Away.Team.Goals)

some.away.hist$pois <- dpois(some.away.hist$goals, mean(some.team$Away.Team.Goals))

ggplot(some.away.hist)  +
  geom_col(aes(x=goals,y=density, fill=as.factor("real"))) +
  geom_line(aes(x=goals,y=pois, fill=as.factor("poisson")), color="#f8766d", size=2) +
  scale_fill_manual(values=c("#f8766d","#04bfc4"), name="Verteilung") 

## ALL home goals distribution (offensive)
t.offensive <- matches %>%
  group_by(Home.Team.Goals) %>%
  summarize(density=n()/nrow(.)) %>%
  mutate(goals = Home.Team.Goals)

t.offensive$pois <- dpois(t.offensive$goals, mean(matches$Home.Team.Goals))

ggplot(t.offensive)  +
  geom_col(aes(x=goals,y=density, fill=as.factor("real"))) +
  geom_line(aes(x=goals,y=pois, fill=as.factor("poisson")), color="#f8766d", size=2) +
  scale_fill_manual(values=c("#f8766d","#04bfc4"), name="Verteilung") +
  scale_x_continuous(breaks=c(0,2,4,6,8,10))


## ALL away goals distribution (deffensive)
t.defensive <- matches %>%
  group_by(Away.Team.Goals) %>%
  summarize(density=n()/nrow(.)) %>%
  mutate(goals = Away.Team.Goals)

t.defensive$pois <- dpois(t.defensive$goals, mean(matches$Away.Team.Goals))

ggplot(t.defensive)  +
  geom_col(aes(x=goals,y=density, fill=as.factor("real"))) +
  geom_line(aes(x=goals,y=pois, fill=as.factor("poisson")), color="#f8766d", size=2)  +
  scale_fill_manual(values=c("#f8766d","#04bfc4"), name="Verteilung") 

#training the model

matches.a <- matches %>%
  transmute(
    team = Home.Team.Name,
    goals = Home.Team.Goals,
    opponent = Away.Team.Name,
  )
matches.b <- matches %>%
  transmute(
    team = Away.Team.Name,
    goals = Away.Team.Goals,
    opponent = Home.Team.Name,
  )

rest <- expand.grid(team = c("Panama", "Peru", "Iceland"),
                    opponent = c("Panama", "Peru", "Iceland"),
                    goals = 0
)

model.poisson <- rbind(matches.a, matches.b, rest) %>%
  glm(data=., goals ~ team + opponent, family="poisson")

summary(model.poisson)


testdata <-data.frame(team=c("Italy", "Russia"),opponent=c("Russia", "Italy"))

#prediction <- predict(m.home, newdata=testdata)

testdata$prediction <- predict(model.poisson, newdata=testdata, type="response")

testdata



predict.wm <- function(model, home.team, away.team) {
  #model=model.poisson
  #home.team="Russia"
  #away.team="Italy"
    input <- data.frame(team=c(home.team, away.team),opponent=c(away.team, home.team))
    input$prediction <- predict(model, newdata=input, type="response")
    
    m <- dpois(0:10, input[1,]$prediction) %o%  dpois(0:10, input[2,]$prediction)
    
    r <- c(
      '1' = sum(m[lower.tri(m)]),  
      X = sum(diag(m)),
      '2' = sum(m[upper.tri(m)])
    )
    #max(home.prob, draw.prob, away.prob)
    #colname(max(r))
    names(which.max(r))
}

predict.matrix <- function(teams, model) {
  #teams <- teams.all
  #model <- model.poisson
  m <- matrix(0, nrow = length(teams), ncol = length(teams), dimnames = list(teams, teams))
  diag(m) <- NA

  for(j in 1:ncol(m)){
    for(i in 1:nrow(m)){
      if (i != j) {
        m[i,j] = predict.wm(model, colnames(m)[i], colnames(m)[j])
      }
    }
  } 
  m
}

evaluate.wm <- function(prediction, results) {
     results <- results %>%
     mutate(
       accurate = if (!is.na(result)) {result == prediction[home.team][away.team]} else { NA }
     )
   sum(results$accurate, na.rm=TRUE)/sum( !is.na( results$result ) ) 
}

fifa.results <- read.csv("data/fifa-world-cup-2018-RussianStandardTime-Results.csv", stringsAsFactors=TRUE, strip.white=TRUE, na.strings = "") %>%
  transmute(
    home.team= Home.Team,
    away.team=Away.Team,
    result = res
  )

prediction <- predict.matrix(teams.all, model.poisson)

evaluate.wm(prediction, fifa.results)

predict.wm(model.poisson, "Russia", "Italy")



