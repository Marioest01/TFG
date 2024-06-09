library(haven)
library(dplyr)
library(gt)
library(gmodels)
library(knitr)
library(ggplot2)
library(visreg)

#HIPÒTESIS 1 PART 2
#CARREGUEM DADES
dta1<-read_sav("C:/Users/mario/Downloads/Microdades_anonimitzades_1071.sav")
dta2<-read_sav("C:/Users/mario/Downloads/dades2.sav")

#SELECCIONEM VARIABLES D'INTERES
dta1 <- dta1 %>%
  select(EFICACIA_EXT_1,EFICACIA_EXT_2, EFICACIA_INT_1, EFICACIA_INT_2, SIT_ECO_PERSONAL, SATIS_DEMOCRACIA, INTERES_POL_PUBLICS, SIT_LAB, SEXE, EDAT, ESTUDIS_1_15, INT_PARLAMENT_VOT)

dta2 <- dta2 %>%
  select(ANY,EFICACIA_EXT_1,EFICACIA_EXT_2, EFICACIA_INT_1, EFICACIA_INT_2, SIT_ECO_PERSONAL, SATIS_DEMOCRACIA, INTERES_POL_PUBLICS, SIT_LAB, SEXE, EDAT, ESTUDIS_1_15)

#FILTREM PER ANYS
dta2_2014 <- dta2 %>%
  filter(ANY == 2014)

dta2_2015 <- dta2 %>%
  filter(ANY == 2015)

dta2_2016 <- dta2 %>%
  filter(ANY == 2016)

dta2_2017 <- dta2 %>%
  filter(ANY == 2017)

dta2_2018 <- dta2 %>%
  filter(ANY == 2018)

dta2_2019 <- dta2 %>%
  filter(ANY == 2019)

dta2_2020 <- dta2 %>%
  filter(ANY == 2020)

dta2_2021 <- dta2 %>%
  filter(ANY == 2021)

dta2_2022 <- dta2 %>%
  filter(ANY == 2022)

dta2_2023 <- dta2 %>%
  filter(ANY == 2023)

#FILTREM PER OBSERVACIONS AMB DADES PER EFICACIA_. L'any 2014 hi ha molts casos incomplets al total de les variables.
dta2_2014 <- dta2_2014 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2015 <- dta2_2015 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2016 <- dta2_2016 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2017 <- dta2_2017 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2018 <- dta2_2018 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2019 <- dta2_2019 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2020 <- dta2_2020 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2021 <- dta2_2021 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2022 <- dta2_2022 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

dta2_2023 <- dta2_2023 %>%
  filter(!is.na(EFICACIA_EXT_1) | !is.na(EFICACIA_EXT_2) | !is.na(EFICACIA_INT_1) | !is.na(EFICACIA_INT_2))

#FEM UNA FUNCIO AMB LA FUNCIO QUE NECESSITEM
add_desafeccio <- function(df) {
  df %>%
    mutate(DESAFECCIO = ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, "Desafectes",
                               ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 2 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, "Parcialment desafectes",
                                      ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 2, "Parcialment desafectes",
                                             ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 1 & EFICACIA_INT_2 == 1, "Parcialment desafectes",
                                                    ifelse(EFICACIA_EXT_1 == 1 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, "Parcialment desafectes", 
                                                           "No desafectes"))))))
}

add_generacions <- function(df) {
  df %>%
    mutate(EDAT_R = case_when(
      EDAT < 26 ~ "Generció Z",
      EDAT >= 26 & EDAT < 43 ~ "Millenials",
      EDAT >= 43 & EDAT < 59 ~ "Generació X",
      EDAT >= 59 & EDAT < 78 ~ "Baby-boomers",
      EDAT >= 78 ~ "Generació silenciosa"
    ))
}

add_taules <- function(df) {
  df %>%
    prop.table(table(DESAFECCIO, EDAT_R), margin=2)*100
}
#AFEGIM LA FUNCIO DESAFECCIO A CADA DF
dta2_2014 <- dta2_2014 %>%add_desafeccio()
dta2_2015 <- dta2_2015 %>%add_desafeccio()
dta2_2016 <- dta2_2016 %>%add_desafeccio()
dta2_2017 <- dta2_2017 %>%add_desafeccio()
dta2_2018 <- dta2_2018 %>%add_desafeccio()
dta2_2019 <- dta2_2019 %>%add_desafeccio()
dta2_2020 <- dta2_2020 %>%add_desafeccio()
dta2_2021 <- dta2_2021 %>%add_desafeccio()
dta2_2022 <- dta2_2022 %>%add_desafeccio()
dta2_2023 <- dta2_2023 %>%add_desafeccio()

#AFEGIM LA FUNCIO GENERACIONS A CADA DF
dta2_2014 <- dta2_2014 %>%add_generacions()
dta2_2015 <- dta2_2015 %>%add_generacions()
dta2_2016 <- dta2_2016 %>%add_generacions()
dta2_2017 <- dta2_2017 %>%add_generacions()
dta2_2018 <- dta2_2018 %>%add_generacions()
dta2_2019 <- dta2_2019 %>%add_generacions()
dta2_2020 <- dta2_2020 %>%add_generacions()
dta2_2021 <- dta2_2021 %>%add_generacions()
dta2_2022 <- dta2_2022 %>%add_generacions()
dta2_2023 <- dta2_2023 %>%add_generacions()

dta2_2014 <- subset(dta2_2014, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2015 <- subset(dta2_2015, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2016 <- subset(dta2_2016, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2017 <- subset(dta2_2017, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2018 <- subset(dta2_2018, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2019 <- subset(dta2_2019, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2020 <- subset(dta2_2020, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2021 <- subset(dta2_2021, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2022 <- subset(dta2_2022, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta2_2023 <- subset(dta2_2023, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))

t_14<-prop.table(table(dta2_2014$DESAFECCIO,dta2_2014$EDAT_R), margin=2)*100
t_23<-prop.table(table(dta2_2023$DESAFECCIO,dta2_2023$EDAT_R), margin=2)*100
t_23_14 <- t_23 - t_14

kable(t_23_14, digits = 2, caption = "Difference between tables")
kable(t_14, digits = 2, caption = "T14")
kable(t_23, digits = 2, caption = "T23")


#HIPÒTESIS 2
dta1<-read_sav("C:/Users/mario/Downloads/Microdades_anonimitzades_1071.sav")

dta1 <- dta1 %>%
  select(EFICACIA_EXT_1,EFICACIA_EXT_2, EFICACIA_INT_1, EFICACIA_INT_2, SIT_ECO_PERSONAL, SATIS_DEMOCRACIA, INTERES_POL_PUBLICS, SIT_LAB, SEXE, EDAT, ESTUDIS_1_15, INT_PARLAMENT_VOT)

dta1 <- subset(dta1, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta1 <- subset(dta1, !(INT_PARLAMENT_VOT %in% c(99, 98, 97, 96, 94, 93, 80, 19)))

dta1 <- dta1 %>%
  mutate(DESAFECCIO = ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, "Desafectes",
                             ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 2 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, "Parcialment desafectes",
                                    ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 2, "Parcialment desafectes",
                                           ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 1 & EFICACIA_INT_2 == 1, "Parcialment desafectes",
                                                  ifelse(EFICACIA_EXT_1 == 1 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, "Parcialment desafectes", 
                                                         "No desafectes"))))))

ggplot(data.frame(dta1$DESAFECCIO), aes(x=dta1$DESAFECCIO)) +
  geom_bar()

dta1 <- dta1 %>%
  mutate(EDAT_R = case_when(
    EDAT < 26 ~ "Generció Z",
    EDAT >= 26 & EDAT < 43 ~ "Millenials",
    EDAT >= 43 & EDAT < 59 ~ "Generació X",
    EDAT >= 59 & EDAT < 78 ~ "Baby-boomers",
    EDAT >= 78 ~ "Generació silenciosa"
  ))

dta1 <- dta1 %>%
  mutate(VOT = ifelse(INT_PARLAMENT_VOT == 1, "PP",
                      ifelse(INT_PARLAMENT_VOT == 3, "ERC",
                             ifelse(INT_PARLAMENT_VOT == 4, "PSC",
                                    ifelse(INT_PARLAMENT_VOT == 6, "Cs",
                                           ifelse(INT_PARLAMENT_VOT == 10, "CUP",
                                                  ifelse(INT_PARLAMENT_VOT == 18, "SUMAR",
                                                         ifelse(INT_PARLAMENT_VOT == 21, "JUNTS",
                                                                ifelse(INT_PARLAMENT_VOT == 23, "VOX", "Altres o NS/NC")))))))))
#VOT PP
dta1$VOT_PP <- NA
dta1$VOT_PP[dta1$INT_PARLAMENT_VOT == 1]<-1
dta1$VOT_PP[dta1$INT_PARLAMENT_VOT %in% c(18, 23, 6, 10, 4, 20, 21, 3)] <- 0

re1<-glm(VOT_PP ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re1

#VOT ERC
dta1$VOT_ERC <- NA
dta1$VOT_ERC[dta1$INT_PARLAMENT_VOT == 3]<-1
dta1$VOT_ERC[dta1$INT_PARLAMENT_VOT %in% c(18, 23, 6, 10, 4, 20, 21, 1)] <- 0

re2<-glm(VOT_ERC ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re2

#VOT PSC
dta1$VOT_PSC <- NA
dta1$VOT_PSC[dta1$INT_PARLAMENT_VOT == 4]<-1
dta1$VOT_PSC[dta1$INT_PARLAMENT_VOT %in% c(18, 23, 6, 10, 1, 20, 21, 3)] <- 0

re3<-glm(VOT_PSC ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re3

#VOT Cs
dta1$VOT_CS <- NA
dta1$VOT_CS[dta1$INT_PARLAMENT_VOT == 6]<-1
dta1$VOT_CS[dta1$INT_PARLAMENT_VOT %in% c(18, 23, 1, 10, 4, 20, 21, 3)] <- 0

re4<-glm(VOT_CS ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re4

#VOT CUP
dta1$VOT_CUP <- NA
dta1$VOT_CUP[dta1$INT_PARLAMENT_VOT == 10]<-1
dta1$VOT_CUP[dta1$INT_PARLAMENT_VOT %in% c(18, 23, 6, 1, 4, 20, 21, 3)] <- 0

re5<-glm(VOT_CUP ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re5

#VOT SUMAR
dta1$VOT_SUMAR <- NA
dta1$VOT_SUMAR[dta1$INT_PARLAMENT_VOT == 18]<-1
dta1$VOT_SUMAR[dta1$INT_PARLAMENT_VOT %in% c(1, 23, 6, 10, 4, 20, 21, 3)] <- 0

re6<-glm(VOT_SUMAR ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re6

#VOT JUNTS/PDCAT
dta1$VOT_JUNTS <- NA
dta1$VOT_JUNTS[dta1$INT_PARLAMENT_VOT  %in% c(20, 21)] <- 1
dta1$VOT_JUNTS[dta1$INT_PARLAMENT_VOT %in% c(18, 23, 6, 10, 4, 1, 3)] <- 0

re7<-glm(VOT_JUNTS ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re7

#VOT VOX
dta1$VOT_VOX <- NA
dta1$VOT_VOX[dta1$INT_PARLAMENT_VOT == 23]<-1
dta1$VOT_VOX[dta1$INT_PARLAMENT_VOT %in% c(18, 1, 6, 10, 4, 20, 21, 3)] <- 0

re8<-glm(VOT_VOX ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re8

#VOT PARTITS TRADICIONALS
dta1$VOT_TRAD <- NA
dta1$VOT_TRAD[dta1$INT_PARLAMENT_VOT %in% c(1, 3, 4, 19, 20, 21)] <- 1
dta1$VOT_TRAD[dta1$INT_PARLAMENT_VOT %in% c(6,10, 18, 23)] <- 0

re9<-glm(VOT_TRAD ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re9

#VOT PARTITS NOUS
dta1$VOT_NOUS <- NA
dta1$VOT_NOUS[dta1$INT_PARLAMENT_VOT %in% c(1, 3, 4, 19, 20, 21)] <- 0
dta1$VOT_NOUS[dta1$INT_PARLAMENT_VOT %in% c(6, 10, 18, 23)] <- 1

re10<-glm(VOT_NOUS ~ EDAT + DESAFECCIO, data=dta1, family = "binomial") 
re10

#Per veure els gràfics dels diferents partits, cal anar canviant el "re" depenent del model que pertoqui

visreg(re10, "EDAT", 
       by = "DESAFECCIO",
       band = TRUE,    
       gg = TRUE,      
       overlay=TRUE,   
       scale="response") +
  labs(y = "Prob. de vot a partits nous", 
       x = "Edat dels votants")


#HIPÒTESIS 3

dta1<-read_sav("C:/Users/mario/Downloads/Microdades_anonimitzades_1071.sav")
dta1 <- dta1 %>%
  select(EFICACIA_EXT_1,EFICACIA_EXT_2, EFICACIA_INT_1, EFICACIA_INT_2, SIT_ECO_PERSONAL, SATIS_DEMOCRACIA, INTERES_POL_PUBLICS, SIT_LAB, SEXE, EDAT, ESTUDIS_1_15, INT_PARLAMENT_VOT, INGRESSOS_1_15, CONFI_POL_CAT, VAL_GOV_CAT,ACTITUD_PROTESTA)

dta1 <- subset(dta1, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))
dta21 <- subset(dta1, !(EFICACIA_EXT_1 %in% c(99, 98) | EFICACIA_EXT_2 %in% c(99, 98) | EFICACIA_INT_1 %in% c(99, 98) | EFICACIA_INT_2 %in% c(99, 98)))

dta1 <- dta1 %>%
  mutate(DESAFECCIO_1 = ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, 1,
                               ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 2 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, 0,
                                      ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 2,0,
                                             ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 1 & EFICACIA_INT_2 == 1, 0,
                                                    ifelse(EFICACIA_EXT_1 == 1 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, 0, 
                                                           0))))))

dta21 <- dta21 %>%
  mutate(DESAFECCIO_2 = ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, 1,
                               ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 2 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, 1,
                                      ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 2,1,
                                             ifelse(EFICACIA_EXT_1 == 2 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 1 & EFICACIA_INT_2 == 1, 1,
                                                    ifelse(EFICACIA_EXT_1 == 1 & EFICACIA_EXT_2 == 1 & EFICACIA_INT_1 == 2 & EFICACIA_INT_2 == 1, 1, 
                                                           0))))))

#AJUSTAR VARIABLES PER DTA1 & DTA22
dta1$ESTUDIS <- NA
dta1$ESTUDIS[dta1$ESTUDIS_1_15  %in% c(1,2,3,4)] <- "Fins Educació secundària"
dta1$ESTUDIS[dta1$ESTUDIS_1_15 %in% c(5,6,7)] <- "Batxillerat i Cicles formatius, mitjans i superiors"
dta1$ESTUDIS[dta1$ESTUDIS_1_15 %in% c(8,9,10,11,12,13,14,15)] <- "Estudis universitaris i superiors"

dta1$ESTUDIS <- as.factor (dta1$ESTUDIS)
dta1$ESTUDIS <- relevel(dta1$ESTUDIS, ref = "Fins Educació secundària")

dta1$SIT_ECO <- NA
dta1$SIT_ECO[dta1$SIT_ECO_PERSONAL == 1] <- "Situació econòmica personal millor que fa un any"
dta1$SIT_ECO[dta1$SIT_ECO_PERSONAL == 2] <- "Situació econòmica personal igual que fa un any"
dta1$SIT_ECO[dta1$SIT_ECO_PERSONAL == 3] <- "Situació econòmica personal pitjor que fa un any"

dta1$SIT_ECO <- as.factor (dta1$SIT_ECO)
dta1$SIT_ECO <- relevel(dta1$SIT_ECO, ref = "Situació econòmica personal millor que fa un any")

dta1$INTERES_POL <- NA
dta1$INTERES_POL[dta1$INTERES_POL_PUBLICS == 1] <- "Molt interés en la política"
dta1$INTERES_POL[dta1$INTERES_POL_PUBLICS == 2] <- "Bastant interés en la política"
dta1$INTERES_POL[dta1$INTERES_POL_PUBLICS == 3] <- "Poc interés en la política"
dta1$INTERES_POL[dta1$INTERES_POL_PUBLICS == 4] <- "Gens interés en la política"

dta1$INTERES_POL <- as.factor (dta1$INTERES_POL)
dta1$INTERES_POL <- relevel(dta1$INTERES_POL, ref = "Molt interés en la política")

dta1$DEMOCRACIA <- NA
dta1$DEMOCRACIA[dta1$SATIS_DEMOCRACIA == 1] <- "Molt satisfet amb la democràcia"
dta1$DEMOCRACIA[dta1$SATIS_DEMOCRACIA == 2] <- "Bastant satisfet amb la democràcia"
dta1$DEMOCRACIA[dta1$SATIS_DEMOCRACIA == 3] <- "Poc satisfet amb la democràcia"
dta1$DEMOCRACIA[dta1$SATIS_DEMOCRACIA == 4] <- "Gens satisfet amb la democràcia"

dta1$DEMOCRACIA <- as.factor (dta1$DEMOCRACIA)
dta1$DEMOCRACIA <- relevel(dta1$DEMOCRACIA, ref = "Molt satisfet amb la democràcia")

dta1$SEXE_recod <- NA
dta1$SEXE_recod[dta1$SEXE == 1] <- 1
dta1$SEXE_recod[dta1$SEXE == 2] <- 0

dta1$LAB <- NA
dta1$LAB[dta1$SIT_LAB == 1] <- "Treballa"
dta1$LAB[dta1$SIT_LAB  %in% c(2,3)] <- "No treballa"

dta1$PROTESTA <- NA
dta1$PROTESTA[dta1$ACTITUD_PROTESTA == 1] <- "Protesta eficaç"
dta1$PROTESTA[dta1$ACTITUD_PROTESTA == 2] <- "Protesta inutil"

dta1$PROTESTA <- as.factor (dta1$PROTESTA)
dta1$PROTESTA <- relevel(dta1$PROTESTA, ref = "Protesta eficaç")

dta1$INGRESSOS <- NA
dta1$INGRESSOS[dta1$INGRESSOS_1_15 %in% c(1,2,3,4,5)] <- "Ingressos baixos"
dta1$INGRESSOS[dta1$INGRESSOS_1_15 %in% c(6,7,8)] <- "Ingressos mitjans"
dta1$INGRESSOS[dta1$INGRESSOS_1_15 %in% c(9,10,11,12,13,14,15)] <- "Ingressos alts"

dta1$INGRESSOS <- as.factor (dta1$INGRESSOS)
dta1$INGRESSOS <- relevel(dta1$INGRESSOS, ref = "Ingressos baixos")

dta22 <- dta1 %>%
  select(ESTUDIS,SIT_ECO, INTERES_POL, DEMOCRACIA, SEXE_recod, EDAT, LAB, CONFI_POL_CAT, VAL_GOV_CAT, PROTESTA, DESAFECCIO_1)
dta22 <- na.omit(dta22)
dta22 <- subset(dta22, !(CONFI_POL_CAT %in% c(99, 98)))
dta22 <- subset(dta22, !(VAL_GOV_CAT %in% c(99, 98)))



#AJUSTAR VARIABLES PER dta21
dta21$ESTUDIS <- NA
dta21$ESTUDIS[dta1$ESTUDIS_1_15  %in% c(1,2,3,4)] <- "Fins Educació secundària"
dta21$ESTUDIS[dta1$ESTUDIS_1_15 %in% c(5,6,7)] <- "Batxillerat i Cicles formatius, mitjans i superiors"
dta21$ESTUDIS[dta1$ESTUDIS_1_15 %in% c(8,9,10,11,12,13,14,15)] <- "Estudis universitaris i superiors"

dta21$ESTUDIS <- as.factor (dta21$ESTUDIS)
dta21$ESTUDIS <- relevel(dta21$ESTUDIS, ref = "Fins Educació secundària")

dta21$SIT_ECO <- NA
dta21$SIT_ECO[dta1$SIT_ECO_PERSONAL == 1] <- "Situació econòmica personal millor que fa un any"
dta21$SIT_ECO[dta1$SIT_ECO_PERSONAL == 2] <- "Situació econòmica personal igual que fa un any"
dta21$SIT_ECO[dta1$SIT_ECO_PERSONAL == 3] <- "Situació econòmica personal pitjor que fa un any"

dta21$SIT_ECO <- as.factor (dta21$SIT_ECO)
dta21$SIT_ECO <- relevel(dta21$SIT_ECO, ref = "Situació econòmica personal millor que fa un any")

dta21$INTERES_POL <- NA
dta21$INTERES_POL[dta1$INTERES_POL_PUBLICS == 1] <- "Molt interés en la política"
dta21$INTERES_POL[dta1$INTERES_POL_PUBLICS == 2] <- "Bastant interés en la política"
dta21$INTERES_POL[dta1$INTERES_POL_PUBLICS == 3] <- "Poc interés en la política"
dta21$INTERES_POL[dta1$INTERES_POL_PUBLICS == 4] <- "Gens interés en la política"

dta21$INTERES_POL <- as.factor (dta21$INTERES_POL)
dta21$INTERES_POL <- relevel(dta21$INTERES_POL, ref = "Molt interés en la política")

dta21$DEMOCRACIA <- NA
dta21$DEMOCRACIA[dta1$SATIS_DEMOCRACIA == 1] <- "Molt satisfet amb la democràcia"
dta21$DEMOCRACIA[dta1$SATIS_DEMOCRACIA == 2] <- "Bastant satisfet amb la democràcia"
dta21$DEMOCRACIA[dta1$SATIS_DEMOCRACIA == 3] <- "Poc satisfet amb la democràcia"
dta21$DEMOCRACIA[dta1$SATIS_DEMOCRACIA == 4] <- "Gens satisfet amb la democràcia"

dta21$DEMOCRACIA <- as.factor (dta21$DEMOCRACIA)
dta21$DEMOCRACIA <- relevel(dta21$DEMOCRACIA, ref = "Molt satisfet amb la democràcia")

dta21$SEXE_recod <- NA
dta21$SEXE_recod[dta1$SEXE == 1] <- 1
dta21$SEXE_recod[dta1$SEXE == 2] <- 0

dta21$LAB <- NA
dta21$LAB[dta1$SIT_LAB == 1] <- "Treballa"
dta21$LAB[dta1$SIT_LAB  %in% c(2,3)] <- "No treballa"

dta21$PROTESTA <- NA
dta21$PROTESTA[dta1$ACTITUD_PROTESTA == 1] <- "Protesta eficaç"
dta21$PROTESTA[dta1$ACTITUD_PROTESTA == 2] <- "Protesta inutil"

dta21$PROTESTA <- as.factor (dta21$PROTESTA)
dta21$PROTESTA <- relevel(dta21$PROTESTA, ref = "Protesta eficaç")

dta21$INGRESSOS <- NA
dta21$INGRESSOS[dta1$INGRESSOS_1_15 %in% c(1,2,3,4,5)] <- "Ingressos baixos"
dta21$INGRESSOS[dta1$INGRESSOS_1_15 %in% c(6,7,8)] <- "Ingressos mitjans"
dta21$INGRESSOS[dta1$INGRESSOS_1_15 %in% c(9,10,11,12,13,14,15)] <- "Ingressos alts"

dta21$INGRESSOS <- as.factor (dta21$INGRESSOS)
dta21$INGRESSOS <- relevel(dta21$INGRESSOS, ref = "Ingressos baixos")

dta21 <- dta21 %>%
  select(ESTUDIS,SIT_ECO, INTERES_POL, DEMOCRACIA, SEXE_recod, EDAT, LAB, CONFI_POL_CAT, VAL_GOV_CAT, PROTESTA, DESAFECCIO_2)
dta21 <- na.omit(dta21)
dta21 <- subset(dta21, !(CONFI_POL_CAT %in% c(99, 98)))
dta21 <- subset(dta21, !(VAL_GOV_CAT %in% c(99, 98)))

dta22 <- subset(dta22, !(CONFI_POL_CAT %in% c(99, 98)))
ggplot(data.frame(dta22$PROTESTA), aes(x=dta22$PROTESTA)) +
  geom_bar()
hist(dta22$VAL_GOV_CAT)

reg1<-lm(DESAFECCIO_1  ~ ESTUDIS + SIT_ECO + INTERES_POL + DEMOCRACIA + SEXE_recod + EDAT + LAB + VAL_GOV_CAT + CONFI_POL_CAT + PROTESTA , data = dta22)
summary(reg1)

reg2<-lm(DESAFECCIO_2  ~ ESTUDIS + SIT_ECO + INTERES_POL + DEMOCRACIA + SEXE_recod + EDAT + LAB + VAL_GOV_CAT + CONFI_POL_CAT + PROTESTA , data = dta21)
summary(reg2)

library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(
  reg1, reg2,
  string.pred = "Coefficient",
  string.ci = "Conf.Int (95%)",
  string.p = "P-value",
  p.style = "stars"
)

library(coefplot)
coefplot(reg1)
coefplot(reg2)


#HIPÒTESIS 1 PART 1

library(tidyverse)
library(dplyr)
library(haven)
dta2<-read_sav("C:/Users/mario/Downloads/Microdades anonimitzades fusio presencial.sav")

#EFICACIA EXTERNA 1
desafeccio_ext_1 <- dta2 %>% select(EFICACIA_EXT_1, ANY)
desafeccio_ext_1 <- desafeccio_ext_1 %>%
  filter(!is.na(EFICACIA_EXT_1))
desafeccio_ext_1 <- subset(desafeccio_ext_1, !(EFICACIA_EXT_1 %in% c(99, 98)))

df_percentage_1 <- desafeccio_ext_1 %>%
  group_by(ANY, EFICACIA_EXT_1) %>%
  summarise(count = n()) %>%
  mutate('Eficacia externa 1' = count / sum(count) * 100) %>%  
  filter(EFICACIA_EXT_1 == 2)

#EFICACIA EXTERNA 2
desafeccio_ext_2 <- dta2 %>% select(EFICACIA_EXT_2, ANY)
desafeccio_ext_2 <- desafeccio_ext_2 %>%
  filter(!is.na(EFICACIA_EXT_2))
desafeccio_ext_2 <- subset(desafeccio_ext_2, !(EFICACIA_EXT_2 %in% c(99, 98)))

df_percentage_2 <- desafeccio_ext_2 %>%
  group_by(ANY, EFICACIA_EXT_2) %>%
  summarise(count = n()) %>%
  mutate('Eficacia externa 2' = count / sum(count) * 100) %>%  
  filter(EFICACIA_EXT_2 == 1)

#EFICACIA INTERNA 1
desafeccio_int_1 <- dta2 %>% select(EFICACIA_INT_1, ANY)
desafeccio_int_1 <- desafeccio_int_1 %>%
  filter(!is.na(EFICACIA_INT_1))
desafeccio_int_1 <- subset(desafeccio_int_1, !(EFICACIA_INT_1 %in% c(99, 98)))

df_percentage_3 <- desafeccio_int_1 %>%
  group_by(ANY, EFICACIA_INT_1) %>%
  summarise(count = n()) %>%
  mutate('Eficacia interna 1' = count / sum(count) * 100) %>%  
  filter(EFICACIA_INT_1 == 2)

#EFICACIA INTERNA 2
desafeccio_int_2 <- dta2 %>% select(EFICACIA_INT_2, ANY)
desafeccio_int_2 <- desafeccio_int_2 %>%
  filter(!is.na(EFICACIA_INT_2))
desafeccio_int_2 <- subset(desafeccio_int_2, !(EFICACIA_INT_2 %in% c(99, 98)))

df_percentage_4 <- desafeccio_int_2 %>%
  group_by(ANY, EFICACIA_INT_2) %>%
  summarise(count = n()) %>%
  mutate('Eficacia interna 2' = count / sum(count) * 100) %>%  
  filter(EFICACIA_INT_2 == 1)

#GRÀFICS

p1 <- ggplot(df_percentage_1, aes(x= ANY, y=`Eficacia externa 1`)) +
  geom_line( color="red", size=1, alpha=0.9, linetype=1) +
  ggtitle("Evolució de la desafecció, en sentit d'eficàcia externa")
p1 <- p1 + labs(subtitle = "Desafecció mesurada amb l'afirmació: Crec que els polítics tenen en compte el que pensa la gent")
p1

p2 <- ggplot(df_percentage_2, aes(x= ANY, y=`Eficacia externa 2`)) +
  geom_line( color="red", size=1, alpha=0.9, linetype=1) +
  ggtitle("Evolució de la desafecció, en sentit d'eficàcia externa")
p2 <- p2 + labs(subtitle = "Desafecció mesurada amb l'afirmació: Els polítics només busquen el benefici propi")
p2

p3 <- ggplot(df_percentage_3, aes(x= ANY, y=`Eficacia interna 1`)) +
  geom_line( color="red", size=1, alpha=0.9, linetype=1) +
  ggtitle("Evolució de la desafecció, en sentit d'eficàcia interna")
p3 <- p3 + labs(subtitle = "Desafecció mesurada amb l'afirmació: La gent del carrer pot influir en el que fan els polítics")
p3

p4 <- ggplot(df_percentage_4, aes(x= ANY, y=`Eficacia interna 2`)) +
  geom_line( color="red", size=1, alpha=0.9, linetype=1) +
  ggtitle("Evolució de la desafecció, en sentit d'eficàcia interna")
p4 <- p4 + labs(subtitle = "Desafecció mesurada amb l'afirmació: De vegades la política sembla tan complicada que se'm fa difícil entendre el que està passant")
p4
