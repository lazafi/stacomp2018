
# 3.1 load data into workspace
  WC.hist <- read.csv("/home/Fabian2/Desktop/WCBC/WorldCupMatches.csv", stringsAsFactors = FALSE) # load data
  
  # 3.2 basic formatting
  WC.hist <- WC.hist[1:852,] # delete empty rows
  WC.hist$Home.Team.Name <- gsub("rn\">", "", WC.hist$Home.Team.Name)
  WC.hist$Home.Team.Name[grepl("Ivoire", WC.hist$Home.Team.Name)] <- "Ivory Coast"
  WC.hist$Away.Team.Name <- gsub("rn\">", "", WC.hist$Away.Team.Name)
  WC.hist$Away.Team.Name[grepl("Ivoire", WC.hist$Away.Team.Name)] <- "Ivory Coast"
  WC.hist$Datetime <- lubridate::dmy_hm(WC.hist$Datetime)
  
  # map country name to unique identifier
  init.maps <- unique(cbind(c(WC.hist$Home.Team.Name, WC.hist$Away.Team.Name), c(WC.hist$Home.Team.Initials, WC.hist$Away.Team.Initials)))
  init.maps <- init.maps[order(init.maps[,1]),]
  init.maps <- rbind(init.maps, c("Iceland", "ISL"))
  init.maps <- rbind(init.maps, c("Panama", "PAN"))

# 5.1 scrape the links of the team websites

  links.lst <- c( "https://www.transfermarkt.com/weltmeisterschaft-2018/teilnehmer/pokalwettbewerb/WM18/saison_id/2017",
                  "https://www.transfermarkt.com/weltmeisterschaft-2014/teilnehmer/pokalwettbewerb/WM14/saison_id/2013",
                  "https://www.transfermarkt.com/weltmeisterschaft-2010/teilnehmer/pokalwettbewerb/WM10/saison_id/2009",
                  "https://www.transfermarkt.com/weltmeisterschaft-2006/teilnehmer/pokalwettbewerb/WM06/saison_id/2005",
                  "https://www.transfermarkt.com/weltmeisterschaft-2002/teilnehmer/pokalwettbewerb/WM02/saison_id/2001")
  
  names(links.lst) <- c("2018", "2014", "2010", "2006", "2002")
  
  team.info <- data.frame("Year" = numeric(),
                          "Team" = character(), 
                          "Name" = character(), 
                          "Number" = numeric(), 
                          "Position" = character(), 
                          "Born" = as.Date(x=rep("01/01/0", length = 0), format="%d/%m/%Y"), 
                          "Age" = numeric(), 
                          "Height" = numeric(), 
                          "Foot" = character(), 
                          "Caps" = numeric(), 
                          "Goals" = numeric(), 
                          "Debut" = as.Date(x=rep("01/01/0", length = 0), format="%d/%m/%Y"), 
                          "Market Value" = numeric(),
                          stringsAsFactors = FALSE)
  
  
  for (k in 1:length(links.lst)) {
    # scrape the links of the team websites
    links.site <- read_html(links.lst[k])
    links.names <- links.site %>% html_nodes(".links") %>% html_children() %>% html_text()
    links <- links.site %>% html_nodes(".links") %>% html_children() %>% html_attr("href")
    links <- paste0("https://www.transfermarkt.de", links)
    links <- gsub("startseite", "kader", links)
    links <- paste0(links, "/saison_id/",as.numeric(names(links.lst)[k])-1,"/plus/1")
    links.names <- str_replace(links.names, "Bosnia-Herzegovina", "Bosnia and Herzegovina")
    links.names <- str_replace(links.names, "South Korea", "Korea Republic")
    links.names <- str_replace(links.names, "United States", "USA" )
    links.names <- str_replace(links.names, "North Korea", "Korea DPR")
    links.names <- str_replace(links.names, "China", "China PR")
    
    #print(all(links.names %in% init.maps[,1]))
    
    ord <- match(links.names, init.maps[,1])
    links.init <- init.maps[ord,2]
    
    if (names(links.lst)[k]=="2006") { links <- links[-33]}
    
    for (j in 1:length(links)) {
      print(links[j])
      
      ws <- read_html(links[j])
      
      ws %>% 
        html_nodes(".items") %>% 
        html_children() %>% 
        magrittr::extract(2) %>% 
        html_children() -> elements
      
      n.team <- length(elements)
      team.info.sub <- data.frame("Year" = numeric(length = n.team),
                                  "Team" = character(length = n.team), 
                                  "Name" = character(length = n.team), 
                                  "Number" = numeric(length = n.team), 
                                  "Position" = character(length = n.team), 
                                  "Born" = as.Date(x=rep("01/01/0", length = n.team), format="%d/%m/%Y"), 
                                  "Age" = numeric(length = n.team), 
                                  "Height" = numeric(length = n.team), 
                                  "Foot" = character(length = n.team), 
                                  "Caps" = numeric(length = n.team), 
                                  "Goals" = numeric(length = n.team), 
                                  "Debut" = as.Date(x=rep("01/01/0", length = n.team), format="%d/%m/%Y"), 
                                  "Market Value" = numeric(length = n.team),
                                  stringsAsFactors = FALSE)
      
      for (i in 1:length(elements)) {
        
        team.info.sub[i,1] <- as.numeric(names(links.lst)[k]) # year
        team.info.sub[i,2] <- links.init[j] # team
        team.info.sub[i,3] <- html_text(html_children(html_nodes(html_children(elements[i])[2], "td")[2])[1]) # player
        team.info.sub[i,4] <- as.numeric(html_text(html_children(html_children(elements[i])[1]))) # number
        team.info.sub[i,5] <- html_text(html_nodes(elements[i], "td")[5]) # position
        team.info.sub[i,6] <- dmy(strsplit(html_text(html_children(elements[i])[3]), " ")[[1]][1]) # born
        team.info.sub[i,7] <- as.numeric(gsub("\\)","", gsub("\\(", "", strsplit(html_text(html_children(elements[i])[3]), " ")[[1]][2]))) # age
        team.info.sub[i,8] <- as.numeric(gsub(",", ".", gsub(" m", "", html_text(html_children(elements[i])[5])))) # heigth
        team.info.sub[i,9] <- html_text(html_children(elements[i])[6]) # foot
        
        if (names(links.lst[k]) == "2018") {
          team.info.sub[i,10] <- as.numeric(ifelse(html_text(html_children(elements[i])[7]) == "-", 0, html_text(html_children(elements[i])[7]))) # caps
          team.info.sub[i,11] <- as.numeric(ifelse(html_text(html_children(elements[i])[8]) == "-", 0, html_text(html_children(elements[i])[8]))) # goals
          team.info.sub[i,12] <- dmy(ifelse(html_text(html_children(elements[i])[9]) == "-", NA, html_text(html_children(elements[i])[9]))) # debut
        } else {
          team.info.sub[i,10] <- NA # caps
          team.info.sub[i,11] <- NA # goals
          team.info.sub[i,12] <- NA # debut
        }
        
        # market value
        mv.tmp <- html_text(html_children(elements[i])[10])
        
        if (grepl("Mio", mv.tmp)) {
          team.info.sub[i,13] <- as.numeric(gsub(",", ".", strsplit(mv.tmp, " ")[[1]][1])) * 1000000
        } else if(grepl("Tsd", mv.tmp)) {
          team.info.sub[i,13] <- as.numeric(gsub(",", ".", strsplit(mv.tmp, " ")[[1]][1])) * 1000
        }
        
      }
      
      team.info <- rbind(team.info, team.info.sub)
    }
    
  }
  
