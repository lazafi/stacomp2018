library(ggplot2)
library(gridExtra)
library(GGally)

countries <- read.csv("data/countries of the world.csv", dec = ",", strip.white=TRUE) %>%
  arrange(Region)  %>%
  mutate(
    key = gsub("&", "and", trimws(Country)),
  )

head(countries)
str(countries)
summary(countries)

library(aplpack) 
faces(countries[3:10])

# stars(countries.reg[3:20], labels=countries.reg$Country, len=0.7, cex=0.7,  key.loc=c(15,1.5), flip.labels=FALSE,  draw.segments = TRUE)
# library(MASS)
# parcoord(countries[3:20], col=countries$Climate)


#calculate median rank
# TODO find better metrics of rank
med_rank_by_country <- read.csv("data/fifa_ranking.csv")  %>%
  group_by(country_full) %>%
  summarise(
    med_rank = median(rank),
    mean_rank = mean(rank)
  ) %>%
  arrange(med_rank)  %>%
  transmute(
    country_full =country_full,
            med_rank = med_rank,
            mean_rank = mean_rank,
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

ggbpairs(country_rank[,sapply(country_rank, is.numeric)])

#grid.arrange(grobs = list(plots))
#arrangeGrob(plots)
#TODO: display on one page

#scatterplotMatrix(county_rank)



