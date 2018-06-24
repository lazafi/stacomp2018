library(ggplot2)
library(gridExtra)

countries <- read.csv("data/countries of the world.csv", dec = ",", strip.white=TRUE)
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
#countries$Country
summary(countries)
#library(aplpack) 
#faces(countries[3:20])
countries <- countries %>%
  arrange(Region)  %>%
  mutate(
    key = gsub("&", "and", trimws(Country))
  )

countries.reg <- countries[order(countries$Region),]
stars(countries.reg[3:20], labels=countries.reg$Country, len=0.7, cex=0.7,  key.loc=c(15,1.5), flip.labels=FALSE,  draw.segments = TRUE)
library(MASS)
parcoord(countries[3:20], col=countries$Climate)

#load ranking data
ranking <- read.csv("data/fifa_ranking.csv") 
#convert to date
ranking <- ranking %>%
  mutate(
    rank_date = as.Date(rank_date)
#    country_full = as.character(country_full)
  )
#ranking$rank_date <- as.Date(ranking$rank_date)
#calculate median rank
# TODO find better metrics of rank
med_rank_by_country <- ranking %>%
  group_by(country_full) %>%
  summarise(
    med_rank = median(rank)
  ) %>%
  arrange(med_rank)  %>%
  transmute(country_full =country_full,
            med_rank = med_rank,
            key =  gsub("&", "and", trimws(as.character(country_full)))
            )

str(med_rank_by_country)
str(countries)
country_rank <- merge(med_rank_by_country, countries , by="key", all=TRUE)

#par(mfrow = c(5,5))
#dev.off()
plots <- list()
for (col in names(country_rank)) {
    print(col)
  p <- ggplot(country_rank, aes_string(x=col, y="med_rank")) 
  p <- p  + geom_point()
  p <- p  + geom_smooth(method = loess)
  plots <- c(plots, p)
  #print(p)
}
#grid.arrange(grobs = list(plots))
#arrangeGrob(plots)
#TODO: display on one page

#scatterplotMatrix(county_rank)



