library(rvest)
library(dplyr)

#url <- "https://www.fambe.sites.be.ch/angebote?offerType=institution&institutionType=1&limit=1000&layout=list"
url <- "C:/Users/Hackstutz/Documents/familienportal/Angebote - Familienportal des Kanton Bern.html" # via Browser speichern
raw <- read_html(url)

container <- html_nodes(raw, ".offerContainer")
namen <- container %>% html_nodes("h3") %>% html_text
zip <- container %>% html_nodes(".contactBlock--zip") %>% html_text
betreuungsart <- container %>% html_nodes("div.offerPropertyElement:nth-of-type(1) span") %>% html_text
#alterskategorie <- container %>% html_nodes("div.offerPropertyElement:nth-of-type(3)") %>% html_text
#baby <- as.numeric(grepl("Baby", alterskategorie))
#vorschulkinder <- as.numeric(grepl("Vorschulkinder", alterskategorie))
#kindergartenschulkinder <- as.numeric(grepl("Kindergarten- und Schulkinder", alterskategorie))
#schulkinderjugendliche <- as.numeric(grepl("Schulkinder und Jugendliche", alterskategorie))
library(stringr)
zip <- gemeindename <- word(zip, 1)
df <- data.frame(namen, zip, betreuungsart)

# plz to bfs nummer
library(readxl)
mysheet <- read_excel("C:/Users/Hackstutz/Documents/familienportal/be-b-00.04-osv-01.xls", sheet = "PLZ4 -> GDE - NPA4 -> COM", range = "A11:E4945")
mysheet <- mysheet %>% group_by(PLZ4) %>% filter(`%_IN_GDE` == max(`%_IN_GDE`))

df$bfs <- mysheet$GDENR[match(df$zip, mysheet$PLZ4)]

write.csv(df, "C:/Users/Hackstutz/Documents/familienportal/familienportal.csv")
