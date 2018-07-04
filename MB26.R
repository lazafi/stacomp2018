install.packages("tidyverse")
library(tidyverse)
library(data.table)

read_csv("countries of the world")


# Socio Economic Data
countrofworld = read_csv("countries of the world.csv")


# Ranking Data
fifa_rank = read_csv("fifa_ranking.csv")


WorldCup = read_csv("WorldCups.csv")
WorldCupPlay = read_csv("WorldCupPlayers.csv")

# Historical DAta ?
WorldCupMatch = read_csv("WorldCupMatches.csv")





# Task 1: Get familiar with the data using several functions.

str(WorldCupMatch)
head(WorldCupMatch)
glimpse(WorldCupMatch)
table(WorldCupMatch)
summary(WorldCupMatch)

# Historic WC Data contains relevant information regarding soccer games from World Cups beginning at 1930 and ending at 2014
# The entries after 2014 are probobly used for 2018 but are currently empty - they display NA

# Task 2: Write a function that generates the result of the game (W, D, L) from the vectors. How would you incorporate the information in the variable Win.condition?


Result_Home_Team = ifelse(WorldCupMatch[,7] > WorldCupMatch[,8], "W", ifelse(WorldCupMatch[,7] == WorldCupMatch[,8], "D", "L"))



#Get column with Results for the home team into the big Matrix - History of World Cups

Hist_Data = cbind(WorldCupMatch, Result_Home_Team)

colnames(Hist_Data)[21]="Result for Home Team"




#Task 3: Explore the data set using summary statistics and illustrations.


##How often has every nation participated
### NarrowData

###### !!!Complete!!!


Hist1 = Hist_Data %>%
  group_by(`Home Team Name`) %>%
  select(`Year`, `Home Team Name`)

Histunique = unique(Hist1)

###Gives a tibble with the frequency of participation of every country
###### !!!Complete!!!

count(Histunique)


### Visualization - Nation that participated most times
##### !!!Complete!!!

library(ggplot2)

Numberofappearance = count(Histunique)
Numberofappearance = as.data.frame(Numberofappearance)
colnames(Numberofappearance) = c("Team", "nAppearance")
Numberofappearance = na.omit(Numberofappearance)

Numberofappearance %>% 
  arrange(desc(nAppearance)) %>% 
  subset(nAppearance > 9) %>%
  ggplot(aes(x = Team, y = nAppearance)) +
  geom_bar(stat = "identity")







## Create a table with the number of wins/draws/losses of every nation

HistWDL = Hist_Data %>%
  select(`Year`, `Home Team Name`, `Home Team Goals`, `Away Team Name`, `Away Team Goals`) %>%
  group_by(`Home Team Name`)

HistWDLdf = as.data.frame(HistWDL)
colnames(Result_Home_Team) = c("Result")
HistWDL2 = cbind(HistWDL, Result_Home_Team)







## How many Goals were scored on average by every nation?
## Data Table - Home Team Matrix + Away Team Matrix and before group by
#### !Complete!



Home1 = Hist_Data %>%
  select(`Home Team Name`, `Home Team Goals`)

Away1 = Hist_Data %>%
  select(`Away Team Name`, `Away Team Goals`)

colnames(Home1) = c("Team", "Goals")
colnames(Away1) = c("Team", "Goals")

HomeAway = rbind(Home1, Away1)

HomeAway1 = HomeAway %>%
  group_by(`Team`)

HomeAway1 %>%
  group_by(`Team`) %>%
  summarise(mean = mean(`Goals`))




  


# Task 4 - Do you think this data set can be used to build a predictor function?
## Since this dataset contains result from countries over a long period of time one can investigate trends regarding the strength of any Team. However Teams are changing and their trainers
## are changing. After a few years the whole Team is completely new and therefore it is possible to get entirely different results. So maybe an overall trend can be observed but to predict 
## it would be much more accurate to use latest results. Germany is a good example. Being the World Champion before this WC they struggled to maintain good performance e.g. against Austria. 
## In this WC they didn´t make it to the next round and are on the last position in that group. From Historic WC Data this wouldn´t have been possible to predict and many ML algorithms predicted
## Germany to be champion this year.

##### !!!Complete!!!






# Task 8 - Have a look on the following website https://www.transfermarkt.com/weltmeisterschaft-2018/startseite/pokalwettbewerb/WM18. Where can you find detailed information from the past world cups?

## In the section "History" and "Statistics" can be found lots of information regarding participation, goals, Market value, age....
######!!!Complete !!!!


# Task 9 - (voluntary) Browse the vignette of the following package:
##### !!!!Complete!!!!
library(rvest)


# Task 10 - (voluntary) How can you scrape the relevant information of the following websites (in the order of the vector participants)?

## Getting links in a vector


links.transfermarkt <- c(
  "https://www.transfermarkt.com/argentinien/startseite/verein/3437",
  "https://www.transfermarkt.com/australien/startseite/verein/3433",
  "https://www.transfermarkt.com/belgien/startseite/verein/3382",
  "https://www.transfermarkt.com/brasilien/startseite/verein/3439",
  "https://www.transfermarkt.com/kolumbien/startseite/verein/3816",
  "https://www.transfermarkt.com/costa-rica/startseite/verein/8497",
  "https://www.transfermarkt.com/kroatien/startseite/verein/3556",
  "https://www.transfermarkt.com/danemark/startseite/verein/3436",
  "https://www.transfermarkt.com/agypten/startseite/verein/3672",
  "https://www.transfermarkt.com/england/startseite/verein/3299",
  "https://www.transfermarkt.com/frankreich/startseite/verein/3377",
  "https://www.transfermarkt.com/deutschland/startseite/verein/3262",
  "https://www.transfermarkt.com/island/startseite/verein/3574",
  "https://www.transfermarkt.com/iran/startseite/verein/3582",
  "https://www.transfermarkt.com/japan/startseite/verein/3435",
  "https://www.transfermarkt.com/sudkorea/startseite/verein/3589",
  "https://www.transfermarkt.com/mexiko/startseite/verein/6303",
  "https://www.transfermarkt.com/marokko/startseite/verein/3575",
  "https://www.transfermarkt.com/nigeria/startseite/verein/3444",
  "https://www.transfermarkt.com/panama/startseite/verein/3577",
  "https://www.transfermarkt.com/peru/startseite/verein/3584",
  "https://www.transfermarkt.com/polen/startseite/verein/3442",
  "https://www.transfermarkt.com/portugal/startseite/verein/3300",
  "https://www.transfermarkt.com/russland/startseite/verein/3448",
  "https://www.transfermarkt.com/saudi-arabien/startseite/verein/3807",
  "https://www.transfermarkt.com/senegal/startseite/verein/3499",
  "https://www.transfermarkt.com/serbien/startseite/verein/3438",
  "https://www.transfermarkt.com/spanien/startseite/verein/3375",
  "https://www.transfermarkt.com/schweden/startseite/verein/3557",
  "https://www.transfermarkt.com/schweiz/startseite/verein/3384",
  "https://www.transfermarkt.com/tunesien/startseite/verein/3670",
  "https://www.transfermarkt.com/uruguay/startseite/verein/3449"
)


######Nur zum Abschauen

url = 'https://www.conference-service.com/conferences/nanotechnology.html'
webpage = read_html(url)

# Getting information - Using Google Chrome Selector Gadget to get CSS Names

Name_data_html = html_nodes(webpage, '.conflist_title')
Name_data = html_text(Name_data_html)

Link_data_html = html_nodes(webpage, '.external_link')
Link = html_text(Link_data_html)

Dates_data_html = html_nodes(webpage, '.dates_location')
Dates_data = html_text(Dates_data_html)




webpage = read_html(links.transfermarkt)


####Test ob es damit geht

extract_node <- function(node){
  # function that accepts a css selector or xpath to extract
  # text from an html node with
  super_node_read %>% 
    html_nodes(node) %>% 
    html_text()
}





Transferlist1 <- lapply(links.transfermarkt, function(i) {
  webpage <- read_html(i)
  draft_table <- html_nodes(webpage, 'td')
  draft <- html_table(draft_table)[[i]]
})






url = "https://www.transfermarkt.com/uruguay/startseite/verein/3449"



## Getting Webpage list

for(i in 1:32) {
  webpage = c()
  webpage[i] = read_html(links.transfermarkt[i])
}

for(i in 1:32) {
  NodesName = list()
  Countrytransfer = list()
  NodesName[i] = html_nodes(webpage[1], 'b')
  html_children() %>%
  html_text()
  
}


for(i in 1:32) {
  NodesName = list()
  Countrytransfer = list()
  NodesName[1] = html_nodes(webpage[i], 'b')
  Countrytransfer[i] = html_text(NodesName[i])
  
  



Dates_data_html = html_nodes(webpage, '.dates_location')
Dates_data = html_text(Dates_data_html)


# Evaluation

# Task 13 - Implement the evaluation metric. This is a function with two 
# arguments: the prediction matrix $R$ and a data structure with the true 
# results $R_{true}$. Note, that this data structure should not be a matrix 
# since two teams may compete twice against each other during the course of
# a world cup tournament. What data structure would you suggest?

## Fit a model
#### !!!Complete !!!!

TrainingssetHist = HistWDL[sample(nrow(HistWDL), 4200), ]
TestsetHist = HistWDL[sample(nrow(HistWDL), 100), ]

TestsetHist = na.omit(TestsetHist)
TrainingssetHist = na.omit(TrainingssetHist)

Hist_Model = glm(`Home Team Goals` ~ `Home Team Name` + `Away Team Name` + `Away Team Goals`, data = TrainingssetHist)

summary(Hist_Model)

TestsetHist$PredictHist = predict(Hist_Model, newdata = TestsetHist, droplevels = TRUE)


## Visualization of Model
##### !!!Complete!!!!

ggplot(data = TestsetHist, aes(x = `Home Team Goals`, y = PredictHist))+
  geom_point()+
  geom_abline(color = "blue")

## Evaluation
##### !!!Complete!!!
fitModel = lm(TestsetHist$`Home Team Goals` ~ TestsetHist$PredictHist)

summary(fitModel)


# 
