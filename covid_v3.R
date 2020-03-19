########################################################
# Elaborado por:
#    Cleiton Otavio da Exaltacao Rocha (cleitonrocha@sei.ba.gov.br)
#    Jonatas Silva do Espirito Santo (jonatassanto@sei.ba.gov.br)
########################################################

library(RCurl)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(lubridate)
library(tidyr)
library(ggthemes)
library(magrittr)


###### dataset do github
x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

covid <- read.csv(text = x)

##### ajustando dataset para um formato de dataframe mais adequado
covid_world <- covid %>% gather(key=data, value = total_casos, -Province.State, -Country.Region, -Lat, -Long,)


###### ajustando coluna data
covid_world$data <- sub(".", "", covid_world$data)

covid_world$data <- mdy(covid_world$data)

####### criando dataset agrupado por paises

covid_ajustado <- covid_world %>% 
                   group_by(Country.Region, data) %>% 
                    summarise(total_registros= sum(total_casos)) %>%
                     filter(total_registros!=0) %>% 
                      group_by(Country.Region) %>% 
                       mutate(dia=1:length(Country.Region))

######## filtrando paises

covid_filter <- covid_ajustado %>% filter(Country.Region %in% c("Brazil", "Italy", "China", "US"))
 
######## Ajustando nome dos países para lingua portuguesa 

covid_filter$Country.Region <- recode(covid_filter$Country.Region, Brazil = "Brasil",
                                      China = "China",
                                      Italy="Itália",
                                      US="EUA")

######## Ordenando variáveis  

covid_filter$Country.Region <- factor(covid_filter$Country.Region, levels = c("Brasil",	"China","EUA", "Itália"))

###### Deixando data automatica e ajustando para o formato brasileiro 

data_att <- format(as.Date(max(covid_world$data)),'%d/%m/%Y')

############# Grafico animado #################

ggplot(covid_filter, aes(dia, log(total_registros), group = Country.Region)) +
  geom_line(aes(color=Country.Region),size=1.5) +
  geom_segment(aes(xend = dia, yend = log(total_registros)), linetype =2, colour = "grey") +
  geom_point(size = 2) + 
  geom_text(aes(x = dia, label = Country.Region), hjust = -0.5, size=3, fontface='bold') + 
  transition_reveal(dia) + 
  coord_cartesian(clip = 'off') + 
  scale_y_continuous(name = 'log(Número de Casos)', limits = c(0, 12)) +
  scale_x_continuous(limits=c(0,max(covid_filter$dia)), 
                     breaks  = seq(0,max(covid_filter$dia), by = 5)) +
  theme_economist() +
  theme(legend.position="bottom",
        axis.title.x = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 12),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=12),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        plot.title = element_text(colour = "black", size = 16, hjust=0.5),
        plot.caption = element_text(colour = "black", size = 10),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=9))+
    labs(title = 'Casos do COVID-19 (em escala logarítmica)\n(Brasil, China, EUA e Itália)',
         x="Dia da epidemia", color="Países",
         caption = paste0("Fonte: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)\nElaborado por Jonatas Silva e Cleiton Rocha\nÚltima atualização: ",data_att)
         ) 
     





