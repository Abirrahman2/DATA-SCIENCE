library(rvest)
library(dplyr)
mydata<-data.frame()
for(pageNumber in seq(from =1 ,to =20))
{
  link<-paste0("https://www.transfermarkt.com/spieler-statistik/wertvollstespieler/marktwertetop?ajax=yw1&altersklasse=alle&ausrichtung=alle&jahrgang=0&kontinent_id=0&land_id=0&page=",pageNumber,"&plus=1&spielerposition_id=alle")
  page<-read_html(link)
  playerNames <- page %>% html_nodes(".inline-table td.hauptlink") %>%  html_text()
  
  playerPosition<-page %>% html_nodes(".inline-table tr+ tr td") %>% html_text()
  
  playerAge<-page %>% html_nodes("td:nth-child(3)") %>% html_text()
  
  totalMatchPlayed<- page %>% html_nodes(".hauptlink+ .zentriert") %>% html_text()
  
  totalGoalScored<- page %>% html_nodes("td:nth-child(8)") %>% html_text()
  
  totalAssist<- page %>% html_nodes("td:nth-child(10)") %>% html_text()
  
  yellowCard<-page %>% html_nodes("td:nth-child(11)") %>% html_text()
  
  redCard<- page %>% html_nodes("td:nth-child(13)") %>% html_text()
  
  currentMarketValue<- page %>% html_nodes(".rechts.hauptlink") %>% html_text()
  mydata<-rbind(mydata,data.frame(playerNames,playerPosition,playerAge,totalMatchPlayed,totalGoalScored,totalAssist,yellowCard,redCard,currentMarketValue,stringsAsFactors = FALSE))
}
names(mydata)<-c(c("PLAYER NAME","PLAYER POSITION", "PLAYER AGE","MATCH PLAYED","GOAL SCORED","ASSIST","YELLOW CARD","RED CARD","PRESENT MARKET VALUE(EURO)"))

write.csv(mydata, file = "E:/Group_06.csv", fileEncoding = "UTF-8")
