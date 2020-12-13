# Hack4SocialGood - Inequalities and Poverty Challenge ####
# Version c.f. file info.
####

# Libraries ####

# s <- summary
# af <- as.factor

library(data.table)
library(dplyr)
library(huxtable)

###

# municipalities_Bern ####

municipalities_Bern <- fread("inequalities-master/data/municipalities_Bern.csv",
                             data.table = T)
municipalities_Bern[,
                    V1 := NULL, ]


# municipalities_Switzerland <- fread("inequalities-master/archive/municipalities_Switzerland.csv",
#                                     data.table = T)
# municipalities_Switzerland[,
#                            V1 := NULL, ]

###

# InstitutionenAngebote ####

InstitutionenAngebote <- readxl::read_xlsx("inequalities-master/data/InstitutionenAngebote Arbeitsintegration KtBE_final.xlsx",
                                           skip = 6)

InstitutionenAngebote <- as.data.table(InstitutionenAngebote)

InstitutionenAngebote[,
                      ":=" ('1...1' = NULL,
                            '1...2' = NULL), ]

InstitutionenAngebote <- t(InstitutionenAngebote)

write.csv(InstitutionenAngebote, "inequalities-master/data/InstitutionenAngebote_interim0.csv")

InstitutionenAngebote <- fread("inequalities-master/data/InstitutionenAngebote_interim0.csv",
                               # encoding = "UTF-8",
                               skip = 1,
                               header = T,
                               data.table = T)

InstitutionenAngebote <- InstitutionenAngebote[c(2:nrow(InstitutionenAngebote)), ]

InstitutionenAngebote[,
                      ":=" (Adresse = NULL,
                            Telefon = NULL,
                            Webseite = NULL,
                            'E-Mail' = NULL,
                            Kontaktperson = NULL,
                            Zertifizierung = NULL,
                            V13 = NULL,
                            V14 = NULL,
                            'Kontakt Angebot' = NULL,
                            'Kontaktperson Vorname Name' = NULL,
                            Standort = NULL
                            # ,
                            # Telefon = NULL,
                            # 'E-Mail' = NULL
                            ), ]

InstitutionenAngebote[,
                      ":=" (Telefon = NULL,
                            'E-Mail' = NULL), ]


names(InstitutionenAngebote) <- gsub("ä",
                                     "ae",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("ö",
                                     "oe",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("Ö",
                                     "Oe",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("ü",
                                     "ue",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("\\*",
                                     "_",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("-",
                                     "",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("/",
                                     "_",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub(" ",
                                     "_",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("\\(",
                                     "",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub(")",
                                     "",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub(",",
                                     "",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("\\.",
                                     "",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("\\+",
                                     "_plus",
                                     names(InstitutionenAngebote))
  
names(InstitutionenAngebote) <- gsub(">",
                                     "_mehr_als",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("<",
                                     "_weniger_als",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("%",
                                     "_Prozent",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("___",
                                     "_",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("\"\"",
                                     "",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("Insititution",
                                     "Institution",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote) <- gsub("…",
                                     "",
                                     names(InstitutionenAngebote))

names(InstitutionenAngebote)[10:13] <- paste0(names(InstitutionenAngebote)[9], "_", names(InstitutionenAngebote)[10:13])

InstitutionenAngebote[,
                      ":=" (V20 = NULL,
                            V21 = NULL,
                            Integrationsziel = NULL), ]

names(InstitutionenAngebote)[14:54] <- paste0(names(InstitutionenAngebote)[13], "_", names(InstitutionenAngebote)[14:54])

InstitutionenAngebote[,
                      ":=" (V27 = NULL,
                            V28 = NULL,
                            Angebotstyp = NULL), ]

names(InstitutionenAngebote)[55:88] <- paste0(names(InstitutionenAngebote)[54], "_", names(InstitutionenAngebote)[55:88])

InstitutionenAngebote[,
                      ":=" (V71 = NULL,
                            V72 = NULL,
                            Institutionsinterne_Arbeits_und_Einsatzgebiete_Programme = NULL), ]

names(InstitutionenAngebote)[89:92] <- paste0(names(InstitutionenAngebote)[88], "_", names(InstitutionenAngebote)[89:92])

InstitutionenAngebote[,
                      ":=" (V108 = NULL,
                            V109 = NULL,
                            Einsatzdauer = NULL), ]

names(InstitutionenAngebote)[93:96] <- paste0(names(InstitutionenAngebote)[92], "_", names(InstitutionenAngebote)[93:96])

InstitutionenAngebote[,
                      ":=" (V115 = NULL,
                            V116 = NULL,
                            Pensum = NULL), ]

names(InstitutionenAngebote)[97:101] <- paste0(names(InstitutionenAngebote)[96], "_", names(InstitutionenAngebote)[97:101])

InstitutionenAngebote[,
                      ":=" (V122 = NULL,
                            V123 = NULL,
                            Anreisedistanz = NULL), ]

names(InstitutionenAngebote)[102:104] <- paste0(names(InstitutionenAngebote)[101], "_", names(InstitutionenAngebote)[102:104])

InstitutionenAngebote[,
                      ":=" (V130 = NULL,
                            V131 = NULL,
                            Angebotsbindung = NULL), ]

names(InstitutionenAngebote)[105:111] <- paste0(names(InstitutionenAngebote)[104], "_", names(InstitutionenAngebote)[105:111])

InstitutionenAngebote[,
                      ":=" (V136 = NULL,
                            V137 = NULL,
                            Kosten = NULL), ]

names(InstitutionenAngebote)[112:130] <- paste0(names(InstitutionenAngebote)[111], "_", names(InstitutionenAngebote)[112:130])

InstitutionenAngebote[,
                      ":=" (V146 = NULL,
                            V147 = NULL,
                            Finanzierung = NULL), ]

names(InstitutionenAngebote)[131:143] <- paste0(names(InstitutionenAngebote)[130], "_", names(InstitutionenAngebote)[131:143])

InstitutionenAngebote[,
                      ":=" (V168 = NULL,
                            V169 = NULL,
                            Zuweisende_vermittlende_Organisation = NULL), ]

names(InstitutionenAngebote)[144:146] <- paste0(names(InstitutionenAngebote)[143], "_", names(InstitutionenAngebote)[144:146])


InstitutionenAngebote[,
                      ":=" (V184 = NULL,
                            V185 = NULL,
                            Auslastung_Kontingente_Groesse = NULL), ]

names(InstitutionenAngebote)[147:165] <- paste0(names(InstitutionenAngebote)[146], "_", names(InstitutionenAngebote)[147:165])

InstitutionenAngebote[,
                      ":=" (V190 = NULL,
                            V191 = NULL,
                            Institutionsprofil = NULL), ]

names(InstitutionenAngebote)[167:212] <- paste0(names(InstitutionenAngebote)[166], "_", names(InstitutionenAngebote)[167:212])

InstitutionenAngebote[,
                      ":=" (V212 = NULL,
                            V213 = NULL,
                            V214 = NULL,
                            Spezialisierungen_Zielgruppe_soziale_Problemlagen = NULL), ]

names(InstitutionenAngebote)[212:240] <- paste0(names(InstitutionenAngebote)[211], "_", names(InstitutionenAngebote)[212:240])

InstitutionenAngebote[,
                      ":=" (V262 = NULL,
                            V263 = NULL,
                            Eintrittsbedingungen = NULL), ]

names(InstitutionenAngebote)[241:263] <- paste0(names(InstitutionenAngebote)[240], "_", names(InstitutionenAngebote)[241:263])

InstitutionenAngebote[,
                      ":=" (V294 = NULL,
                            V295 = NULL,
                            Qualifizierung_der_Mitarbeitenden_der_Institution = NULL), ]

InstitutionenAngebote[,
                      ":=" (Anreisedistanz_Nahe_Umkreis_max_30_km_zu_Wohnort = NULL,
                            Anreisedistanz_Mittel_Umkreis_max_50_km_zu_Wohnort = NULL,
                            Anreisedistanz_Weit_Umkreis_mind_50_km_zu_Wohnort = NULL), ]

InstitutionenAngebote[,
                      ":=" (Institutionsprofil_Zugang = NULL), ]

###

# Link municipalities_Bern InstitutionenAngebote ####

InstitutionenAngebote[,
                      gmdename := Ort, ]

InstitutionenAngebote$gmdename <- gsub("([0-9]{4}) ([A-Za-z]*)",
                                       "\\2",
                                       InstitutionenAngebote$gmdename)

InstitutionenAngebote$gmdename <- gsub("([A-Za-z]*) ([0-9]*)",
                                       "\\1",
                                       InstitutionenAngebote$gmdename)

InstitutionenAngebote$gmdename[340] <- "Biel/Bienne"

InstitutionenAngebote$gmdename[27] <- "Bern"

InstitutionenAngebote$gmdename[52] <- "Biel/Bienne"

InstitutionenAngebote$gmdename[137] <- "Kirchlindach"

InstitutionenAngebote$gmdename <- gsub("Biel-Bienne",
                                       "Biel/Bienne",
                                       InstitutionenAngebote$gmdename)

InstitutionenAngebote$gmdename <- gsub("Biel/Bienne",
                                       "Bienne",
                                       InstitutionenAngebote$gmdename)

InstitutionenAngebote$gmdename <- gsub("Biel",
                                       "Bienne",
                                       InstitutionenAngebote$gmdename)

InstitutionenAngebote$gmdename <- gsub("Teuffenthal",
                                       "Teuffenthal(BE)",
                                       InstitutionenAngebote$gmdename)

municipalities_Bern$gmdename <- gsub(" ",
                                     "",
                                     municipalities_Bern$gmdename)

municipalities_Bern$gmdename <- gsub("Biel/Bienne",
                                     "Bienne",
                                     municipalities_Bern$gmdename)

setkey(municipalities_Bern,
       gmdename)

setkey(InstitutionenAngebote,
       gmdename)

Hack4SocialGood_data <- InstitutionenAngebote[municipalities_Bern]

rm(InstitutionenAngebote,
   municipalities_Bern)

###

# Arbeitslosenquoten_Bern_2019 ####

Arbeitslosenquoten_Bern_2019 <- readxl::read_xlsx("inequalities-master/data/WIDA_ARBEIT-ERWERB_Arbeitslose_und_Arbeitslosenquote_nach_Gemeinde_DE.xlsx",
                                                  sheet = 2,
                                                  skip = 5)

Arbeitslosenquoten_Bern_2019 <- Arbeitslosenquoten_Bern_2019[c(1:347), ]

name_vector <- c("Arbeitslose",
                 "Arbeitslosenquote",
                 "Stellensuchende")

names(Arbeitslosenquoten_Bern_2019) <- c("bfsid",
                                         "Gemeinde",
                                         paste0("Januar_", name_vector, "_2019"),
                                         paste0("Februar_", name_vector, "_2019"),
                                         paste0("Maerz", name_vector, "_2019"),
                                         paste0("April_", name_vector, "_2019"),
                                         paste0("Mai_", name_vector, "_2019"),
                                         paste0("Juni_", name_vector, "_2019"),
                                         paste0("Juli_", name_vector, "_2019"),
                                         paste0("August_", name_vector, "_2019"),
                                         paste0("September_", name_vector, "_2019"),
                                         paste0("Oktober_", name_vector, "_2019"),
                                         paste0("November_", name_vector, "_2019"),
                                         paste0("Dezember_", name_vector, "_2019"),
                                         paste0("Jahresdurchschnitt_", name_vector, "_2019"))

Arbeitslosenquoten_Bern_2019 <- Arbeitslosenquoten_Bern_2019[c(2:347), ]

rm(name_vector)

###

# Link Arbeitslosenquoten_Bern_2019 Hack4SocialGood_data ####

Arbeitslosenquoten_Bern_2019 <- as.data.table(Arbeitslosenquoten_Bern_2019)

Arbeitslosenquoten_Bern_2019[,
                             bfsid := as.numeric(bfsid), ]

setkey(Arbeitslosenquoten_Bern_2019,
       bfsid)

setkey(Hack4SocialGood_data,
       bfsid)

Hack4SocialGood_data <- Hack4SocialGood_data[Arbeitslosenquoten_Bern_2019]

rm(Arbeitslosenquoten_Bern_2019)

###

# Altersgruppen_Kinder ####

Altersgruppen_Kinder <- readxl::read_xlsx("inequalities-master/data/px-x-0102010000_103_20201212-142826.xlsx",
                           skip = 6)

Altersgruppen_Kinder <- Altersgruppen_Kinder[c(1:710), ]

Altersgruppen_Kinder <- as.data.table(Altersgruppen_Kinder)

Altersgruppen_Kinder[,
      ":=" ('...1' = NULL,
            '...2' = NULL,
            '...4' = NULL,
            '2' = NULL), ]

names(Altersgruppen_Kinder) <- c("bfsid",
                  "Nichtstaendige_Wohnbevoelkerung",
                  "Personen_Altersgruppe_0_4_2019",
                  "Personen_Altersgruppe_5_9_2019")

a <- Altersgruppen_Kinder$bfsid[seq(1, 710, 2)]

a <- a[sort(rep(1:355, 2))]

Altersgruppen_Kinder[,
      bfsid := a, ]

Altersgruppen_Kinder <- Altersgruppen_Kinder[,
               lapply(.SD,
                      sum),
               .SDcols = c('Personen_Altersgruppe_0_4_2019',
                           'Personen_Altersgruppe_5_9_2019'),
               .(bfsid)]

Altersgruppen_Kinder$bfsid <- gsub("([0-9]{1})([0-9]{3})",
                  "\\2",
                  Altersgruppen_Kinder$bfsid)

Altersgruppen_Kinder$bfsid <- gsub("([0-9]{5})",
                  "99999",
                  Altersgruppen_Kinder$bfsid)

Altersgruppen_Kinder <- Altersgruppen_Kinder[bfsid < 99999, ]

rm(a)

###

# Link Altersgruppen_Kinder Hack4SocialGood_data ####

Altersgruppen_Kinder[,
                     bfsid := as.numeric(bfsid), ]

setkey(Altersgruppen_Kinder,
       bfsid)

setkey(Hack4SocialGood_data,
       bfsid)

Hack4SocialGood_data <- Hack4SocialGood_data[Altersgruppen_Kinder]

write.csv(Hack4SocialGood_data, "inequalities-master/data/Hack4SocialGood_data.csv")

rm(Altersgruppen_Kinder)

###

# Hack4SocialGood_data_collapsed ####

# Hack4SocialGood_data <- fread("inequalities-master/data/Hack4SocialGood_data.csv",
#                               data.table = T)
# 
# Hack4SocialGood_data[,
#                      V1 := NULL, ]

Hack4SocialGood_data[,
                     Anzahl_Angebote_Arbeitsintegration := .N,
                     .(bfsid)]

Hack4SocialGood_data[,
                     Anzahl_Angebote_Arbeitsintegration := ifelse(is.na(Name_Institution),
                                                                  0,
                                                                  Anzahl_Angebote_Arbeitsintegration), ]

Hack4SocialGood_data[,
                     ":=" (Zweckverband = NULL,
                           Kosten_Zusatzkosten_fuer_ergaenzende_Dienstleistungen_ExtraCoaching_etc = NULL,
                           Kosten_Angebotspauschale = NULL,
                           Angebotsbindung_Zweckverband = NULL,
                           Angebotsbindung_Leistungsvertraege = NULL,
                           Finanzierung_Aufsichtsbehoerde = NULL,
                           Institutionsprofil_Methodik = NULL,
                           Institutionsprofil_Haltungen_Menschenbild = NULL,
                           Institutionsprofil_ethischmoralische_Prinzipien = NULL,
                           Eintrittsbedingungen_unter_25_Jahren = NULL), ]

name_vector <- c(names(Hack4SocialGood_data)[6:246],
                 names(Hack4SocialGood_data)[249:268],
                 names(Hack4SocialGood_data)[273:306],
                 names(Hack4SocialGood_data)[662:726],
                 names(Hack4SocialGood_data)[728:766],
                 "Personen_Altersgruppe_0_4_2019",
                 "Personen_Altersgruppe_5_9_2019",
                 "Anzahl_Angebote_Arbeitsintegration")

ja <- function(x){
   ifelse(x %in% "ja",
          1,
          x)
}

nein <- function(x){
   ifelse(x %in% "nein",
          0,
          x)
}

fragezeichen <- function(x){
   ifelse(x %in% "?",
          NA,
          x)
}

make_numeric <- function(x){
   as.numeric(x)
}

Hack4SocialGood_data_collapsed <- Hack4SocialGood_data[,
                                                       lapply(.SD,
                                                              ja),
                                                       .SDcols = c(name_vector), ]

Hack4SocialGood_data_collapsed <- Hack4SocialGood_data_collapsed[,
                                                                 lapply(.SD,
                                                                        nein),
                                                                 .SDcols = c(name_vector), ]

Hack4SocialGood_data_collapsed <- Hack4SocialGood_data_collapsed[,
                                                                 lapply(.SD,
                                                                        fragezeichen),
                                                                 .SDcols = c(name_vector), ]

Hack4SocialGood_data_collapsed <- Hack4SocialGood_data_collapsed[,
                                                                 lapply(.SD,
                                                                        make_numeric),
                                                                 .SDcols = c(name_vector), ]

Hack4SocialGood_data_collapsed <- cbind(bfsid = Hack4SocialGood_data$bfsid,
                                        Hack4SocialGood_data_collapsed)

mittelwert <- function(x){
   mean(x,
        na.rm = T)
}

Hack4SocialGood_data_collapsed <- Hack4SocialGood_data_collapsed[,
                                                                 lapply(.SD,
                                                                        mittelwert),
                                                                 .SDcols = c(name_vector),
                                                                 .(bfsid)]

maximum <- function(x){
   max(x,
       na.rm = T)
}

Gemeindeinfo <- Hack4SocialGood_data[,
                                     lapply(.SD,
                                            maximum),
                                     .SDcols = c('Gemeinde',
                                                 'kantone',
                                                 'grossregionen',
                                                 'sprachregionen',
                                                 'stadt_land'),
                                     .(bfsid)]

setkey(Gemeindeinfo,
       bfsid)

setkey(Hack4SocialGood_data_collapsed,
       bfsid)

Hack4SocialGood_data_collapsed <- Gemeindeinfo[Hack4SocialGood_data_collapsed]

rm(name_vector,
   fragezeichen,
   ja,
   make_numeric,
   nein,
   Gemeindeinfo,
   maximum)


# Familienportal ####

familienportal <- fread("inequalities-master/data/familienportal.csv",
                        data.table = T)

familienportal[,
               ":=" (V1 = NULL,
                     namen = NULL,
                     zip = NULL,
                     bfsid = bfs,
                     bfs = NULL), ]

Angebote_Kita <- familienportal[betreuungsart %in% "Kita", ]

Angebote_Tagesschule <- familienportal[betreuungsart %in% "Tagesschule", ]

Angebote_Kita <- Angebote_Kita[,
                               Anzahl_Angebote_Kita := .N,
                               .(bfsid)]

Angebote_Kita <- Angebote_Kita[,
                               lapply(.SD,
                                      mittelwert),
                               .SDcols = c('Anzahl_Angebote_Kita'),
                               .(bfsid)]

Angebote_Tagesschule <- Angebote_Tagesschule[,
                                             Anzahl_Angebote_Tagesschule := .N,
                                             .(bfsid)]

Angebote_Tagesschule <- Angebote_Tagesschule[,
                                             lapply(.SD,
                                                    mittelwert),
                                             .SDcols = c('Anzahl_Angebote_Tagesschule'),
                                             .(bfsid)]

rm(familienportal,
   mittelwert)

# Link Familienportal Hack4SocialGood_data_collapsed ####

setkey(Hack4SocialGood_data_collapsed,
       bfsid)

setkey(Angebote_Kita,
       bfsid)

Hack4SocialGood_data_collapsed <- Angebote_Kita[Hack4SocialGood_data_collapsed]

Hack4SocialGood_data_collapsed[,
                               Anzahl_Angebote_Kita := ifelse(is.na(Anzahl_Angebote_Kita),
                                                              0,
                                                              Anzahl_Angebote_Kita), ]

setkey(Hack4SocialGood_data_collapsed,
       bfsid)

setkey(Angebote_Tagesschule,
       bfsid)

Hack4SocialGood_data_collapsed <- Angebote_Tagesschule[Hack4SocialGood_data_collapsed]

Hack4SocialGood_data_collapsed[,
                               Anzahl_Angebote_Tagesschule := ifelse(is.na(Anzahl_Angebote_Tagesschule),
                                                                     0,
                                                                     Anzahl_Angebote_Tagesschule), ]

write.csv(Hack4SocialGood_data_collapsed, "inequalities-master/data/Hack4SocialGood_data_collapsed.csv")

rm(Angebote_Kita,
   Angebote_Tagesschule)

# Logit ####

Hack4SocialGood_data_collapsed[,
                               dummy_Arbeitsintegration := ifelse(Anzahl_Angebote_Arbeitsintegration > 0,
                                                                  1,
                                                                  0), ]

logit.1 <- glm(dummy_Arbeitsintegration ~ sozsich_sh,
               data = Hack4SocialGood_data_collapsed)

Hack4SocialGood_data_collapsed[,
                               p.score_Anzahl_Angebote_Arbeitsintegration := predict(logit.1, type = "response"), ]

Hack4SocialGood_data_collapsed[,
                               dummy_Kita := ifelse(Anzahl_Angebote_Kita > 0,
                                                    1,
                                                    0), ]

logit.2 <- glm(dummy_Kita ~ sozsich_sh,
               data = Hack4SocialGood_data_collapsed)

Hack4SocialGood_data_collapsed[,
                               p.score_Anzahl_Angebote_Kita := predict(logit.2, type = "response"), ]

Hack4SocialGood_data_collapsed[,
                               dummy_Tagesschule := ifelse(Anzahl_Angebote_Tagesschule > 0,
                                                           1,
                                                           0), ]

logit.3 <- glm(dummy_Tagesschule ~ sozsich_sh,
               data = Hack4SocialGood_data_collapsed)

Hack4SocialGood_data_collapsed[,
                               p.score_Anzahl_Angebote_Tagesschule := predict(logit.3, type = "response"), ]

a <- huxtable::huxreg("Arbeitsintegration" = logit.1,
                      "Kita" = logit.2,
                      "Tagesschule" = logit.3,
                      statistics = c("N" = "nobs",
                                     "logLik" = "logLik",
                                     "AIC" = "AIC"),
                      note = "*** p < 0.001; ** p < 0.01; * p < 0.05.")

a[4, 1] <- "Sozialhilfequote"

a <- a %>%
   set_bold(1, 2:4, T) %>%
   set_caption("Logit-Modelle für Gemeinden im Kanton Bern")

quick_html(a, file = "inequalities-master/data/logit.html")
quick_docx(a, file = "inequalities-master/data/logit.docx")

rm(logit.1,
   logit.2,
   logit.3,
   a)
