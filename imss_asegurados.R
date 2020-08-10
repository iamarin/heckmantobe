# asegurados IMSS
# @elcontrafactual
## los acentos se omiten intencionalmente

# librerias
library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(readr)
library(janitor)
library(tidyr)

# PATH

# carga de datos
(asegurados <- read_excel(paste0(bases,"asegurados_mexicocomovamos_abr.xlsx"),col_types = c("guess", "guess", "guess", "guess", "guess", "guess")))

asegurados <- asegurados %>% 
  separate(`Año/Mes`,into = c ( 'Year','Month'),sep = '/', remove = T) %>% 
  clean_names() %>% 
  unite(Periodo,c('year','month'), sep = "-", remove = T)

colnames(asegurados)

asegurados %>% 
  glimpse()

asegurados_2018 <- asegurados %>% 
  filter(Periodo >= "2018-01")

asegurados[asegurados$Periodo == "2020-04",]

ggplot(asegurados_2018) +
  geom_line(aes(x = Periodo, y = tasa_anual_de_crecimiento), group = 1) + 
  labs(
      x = '',
      y = '',
      title = 'Asegurados al IMSS',
      subtitle = 'variación mensual anual',
      caption = "Fuente: Elaborado por @elvagodeldato con información de México,¿Cómo Vamos?"
      
    )+  theme(axis.text.x = element_text(angle = 90, hjust = 1,size =3)) + scale_y_continuous(labels = scales::percent)


as_mar <- asegurados %>% 
  separate(`Año/Mes`,into = c ( 'Year','Month'),sep = '/', remove = T) %>% 
  filter(Month=='04') %>% 
  clean_names() %>% 
  unite(Periodo,c('year','month'), sep = "-", remove = T)


as_mar %>% 
  select(Periodo,generacion_de_empleo_formal_mensual) %>% 
  ggplot() + geom_col(aes(x= Periodo, y = generacion_de_empleo_formal_mensual))

# Tasa de creciemiento
asegurados %>% 
  separate(`Año/Mes`,into = c ( 'Year','Month'),sep = '/', remove = T) %>% 
  clean_names() %>% 
  unite(Periodo,c('year','month'), sep = "-", remove = T) %>% 
  select(Periodo,empleos_formales_totales) %>% 
  mutate(
    g = empleos_formales_totales/lag(empleos_formales_totales) - 1
  )

# Promedio de asegurados trimestral
(asegurados %>% 
  separate(`Año/Mes`,into = c ( 'Year','Month'),sep = '/', remove = T) %>% 
  clean_names() %>% 
  unite(Periodo,c('year','month'), sep = "-", remove = T) %>% 
  select(Periodo,empleos_formales_totales) %>% 
  separate(Periodo,into = c ( 'Year','Month'),sep = '-', remove = T) %>% 
  mutate(Month = as.numeric(Month)) %>% 
  mutate(
    cg = case_when(Month %in% c(1,2,3) ~ "q1",Month %in% c(4,5,6) ~ ' q2',Month %in% c(7,8,9) ~'q3',Month %in% c(10,11,12)~'q4')
  ) %>% 
  mutate(cg = as.factor(cg)) %>% 
  group_by(Year,cg) %>% 
  summarise(
    prom = mean(empleos_formales_totales,na.rm = T)
  ))







asegurados %>% 
  ggplot() + geom_line(aes(x = `Año/Mes`, y = `Empleos formales totales`),group = 1)

asegurados$`Año/Mes` <- as.numeric(asegurados$`Año/Mes`)
asegurados %>% 
  ggplot()+ geom_line(aes(x = `Año/Mes`, y = `Empleos formales totales` ), group = 1)

(asegurados  %>% 
    mutate(
      Month = as.numeric(Month),
    ) %>% 
  mutate(
    q1 = if_else(Month %in% c(1,2,3),1,0),
    q2 = if_else(Month %in% c(4,5,6),1,0),
    q3 = if_else(Month %in% c(7,8,9),1,0),
    q4 = if_else(Month %in% c(10,11,12),1,0)
  ) %>% 
    ggplot() + geom_line(aes(x = Year,y =`Empleos formales totales`))) 

as.Date(asegurados$`Año/Mes`)
