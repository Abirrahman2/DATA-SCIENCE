library(rvest)
library(dplyr)


link <- "https://www.transfermarkt.com/spieler-statistik/wertvollstespieler/marktwertetop?land_id=0&ausrichtung=alle&spielerposition_id=alle&altersklasse=alle&jahrgang=0&kontinent_id=0&plus=1"
page <- read_html(link)


playerNames <- page %>% html_nodes(".inline-table td.hauptlink") %>%  html_text()

playerPosition<-page %>% html_nodes(".inline-table tr+ tr td") %>% html_text()

playerAge<-page %>% html_nodes("td:nth-child(3)") %>% html_text()

totalMatchPlayed<- page %>% html_nodes(".hauptlink+ .zentriert") %>% html_text()

totalGoalScored<- page %>% html_nodes("td:nth-child(8)") %>% html_text()

totalAssist<- page %>% html_nodes("td:nth-child(10)") %>% html_text()

yellowCard<-page %>% html_nodes("td:nth-child(11)") %>% html_text()

redCard<- page %>% html_nodes("td:nth-child(13)") %>% html_text()

currentMarketValue<- page %>% html_nodes(".rechts.hauptlink") %>% html_text()

mydata<-data.frame(playerNames,playerPosition,playerAge,totalMatchPlayed,totalGoalScored,totalAssist,yellowCard,redCard,currentMarketValue,stringsAsFactors = FALSE)
write.csv(mydata, file = "E:/playerTransferFour.csv", fileEncoding = "UTF-8",quote = TRUE)



