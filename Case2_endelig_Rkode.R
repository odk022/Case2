## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE)
#knitr::opts_chunk$set(comment=NA)


## --------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(PxWebApiData)

## --------------------------------------------------------------------------------------------------------------------------------
webpage <- read_html("https://www.ssb.no/a/histstat/aarbok/ht-0901-bnp.html")
tabell <- html_table(html_nodes(webpage, "table")[[2]])


## --------------------------------------------------------------------------------------------------------------------------------
head(tabell)
tail(tabell)
str(tabell)
names(tabell)


## --------------------------------------------------------------------------------------------------------------------------------
tabell <- tabell %>% drop_na()


## --------------------------------------------------------------------------------------------------------------------------------
names(tabell) <- c("År", "BNP", "BNP_endring",
                   "BNP_percap", "BNP_percap_endring")

tabell <- as_tibble(tabell)

tabell


## --------------------------------------------------------------------------------------------------------------------------------
# dette er nødvendige operasjoner i forbindelse med datascraping
tabell <-
  tabell %>%  
  mutate(BNP=str_replace_all(BNP, " ", ""),# fjerner mellomrom
         BNP_endring=na_if(BNP_endring, ""),# tar vekk "" som erstattes med na
         BNP_percap_endring=na_if(BNP_percap_endring, ""), # tar vekk "" som erstattes med na
         BNP_endring=str_replace(BNP_endring, ",","."), # erstatter komma med punktum
         BNP_percap_endring=str_replace(BNP_percap_endring, ",",".")) %>% # # erstatter komma med punktum
  mutate_if(is.character, as.numeric) # alle tekst gjøres om til tall

tabell 

# Oppgave 1: Lag et plott med BNP per innbygger i perioden

tabell %>%
  ggplot(aes(x=År, y=BNP_percap)) +
  geom_line(color="dark blue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Bruttonasjonalprodukt - BNP \n (kr per innbygger)",
       x =" ",
       y = "Kr per innbygger") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Oppgave 2 Gitt disse dataene, er det en annen viktig variabel som vi kan beregne ut fra dem, i så fall hvilken?

# Jeg tror dette er data over befolkningstallet.
Nye_data<-
  tabell %>%
  select(År,BNP,BNP_endring,BNP_percap,BNP_percap_endring) %>% 
  mutate(Befolkning = BNP/BNP_percap)

Nye_data
tail(Nye_data)

# oppgave 3: 
# Denne tabellen inneholder årlige BNP data frem til 2011. I det forrige caset så vi på nyere månedlige 
# BNP tall. I denne oppgaven skal du spleise de to BNP seriene til en lang tabell, per år. Vi trenger ikke justere 
# BNP-tallene ettersom begge tabellene har 2005 som basisår.

# henter data fra 2012 - 2020 og lager nytt navn
tabell_2 <- ApiData("https://data.ssb.no/api/v0/no/table/09842/",
                    Tid = paste(2012:2020),
                    ContentsCode = "BNP")

tabell_2 # ser på tabellen, ser at det er to datasett
tabell_2 <- tabell_2[[1]] # velger sett 1
tabell_2 
str(tabell_2)
tabell_2 <- as_tibble(tabell_2)
tabell_2

# lager et nytt datasett med bare år og value
tabell_2 <-
  tabell_2 %>%
  select(år, value)
tabell_2

tabell_2 <- tabell_2 %>%
  mutate(år=parse_integer(år)) %>% 
  rename(År=år) %>% 
  rename(BNP_percap=value)
tabell_2

#henter tabell og lager nytt datasett med de samme variablene. Kaller datasettet med gamle data for tabell_gml
tabell
tabell_gml <-
  tabell %>% 
  select(År, BNP_percap)
tabell_gml

# Lager et nytt datasett som omfatter alle data fra 1865 til 2020 og kaller det tabell_total 
tabell_total<- bind_rows(tabell_gml, tabell_2)
tabell_total
tail(tabell_total)

#Lager plott med fullstendige data 
tabell_total %>%
  ggplot(aes(x=År, y=BNP_percap)) +
  geom_line(color="dark blue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Bruttonasjonalprodukt - BNP \n (kr per innbygger)",
       x =" ",
       y = "Kr per innbygger") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
