#Instalar paquetes y llamar librerias
install.packages("lubridate")
install.packages("geomtextpath")
install.packages("hrbrthemes")
install.packages("patchwork")
remove.packages("sivirep")
install.packages("gridExtra")
if (!require("pak")) install.packages("pak")
pak::pak("epiverse-trace/sivirep")
library(sivirep)
library(dplyr)
library(tidyr)
library (lubridate)
library(purrr)
library(ggplot2)
library(geomtextpath)
library(hrbrthemes)
library(patchwork)
library(MASS)
library(gridExtra)

# Años a abarcar
years <- 2009:2023 

# Lista para almacenar los datos
data_list <- list()

# Bucle para importar datos de cada año
for (year in years) {
  dengue <- import_data_event(nombre_event = "dengue", year = year, ruta_dir = "data", cache = FALSE)
  data_list[[as.character(year)]] <- dengue
}

# Combinar todos los dataframes en uno solo
dengue <- do.call(rbind, data_list)

# Filtrar los datos para las ciudades
dengue_ciud <- dengue%>%
  filter(municipio_ocurrencia %in% c("BARRANQUILLA", "CARTAGENA", "SANTA MARTHA"))

# Agrupar por semana y municipio, y contar los casos
dengue_ciud_sem <- dengue_ciud %>%
  group_by(ano, semana, municipio_ocurrencia) %>%
  summarise(casos = n()) %>%
  arrange(ano, semana)

#Ciudades a columnas
dengue_casos <- dengue_ciud_sem %>%
  pivot_wider(names_from = municipio_ocurrencia, values_from = casos, values_fill = 0)

#Cargar datos ambientales
HR_SM=read.csv("data/HR STA MARTA/HRHG_CON@15015050.data", sep="|", dec = "." )
HR_CTG=read.csv("data/HR CARTAGENA/HRHG_CON@14015080.data", sep="|", dec = "." )
HR_BQ=read.csv("data/HR BARRANQUILLA/HRHG_CON@29045120.data", sep="|", dec = "." )
T_SM=read.csv("data/T STA MARTA/TSSM_MEDIA_D@15015050.data", sep="|", dec = "." )
T_CTG=read.csv("data/T CARTAGENA/TSSM_MEDIA_D@14015080.data", sep="|", dec = "." )
T_BQ=read.csv("data/T BARRANQUILLA/TSSM_MEDIA_D@29045120.data", sep="|", dec = "." )
P_SM=read.csv("data/P STA MARTA/PTPM_CON@15015050.data", sep="|", dec = "." )
P_CTG=read.csv("data/P CARTAGENA/PTPM_CON@14015080.data", sep="|", dec = "." )
P_BQ=read.csv("data/P BARRANQUILLA/PTPM_CON@29045120.data", sep="|", dec = "." )

#Establecer formato datos ambientales  
HR_SM$Fecha <- as.POSIXct(HR_SM$Fecha, format = "%Y-%m-%d %H:%M:%S")
HR_CTG$Fecha <- as.POSIXct(HR_CTG$Fecha, format = "%Y-%m-%d %H:%M:%S")
HR_BQ$Fecha <- as.POSIXct(HR_BQ$Fecha, format = "%Y-%m-%d %H:%M:%S")
T_SM$Fecha <- as.POSIXct(T_SM$Fecha, format = "%Y-%m-%d %H:%M:%S")
T_CTG$Fecha <- as.POSIXct(T_CTG$Fecha, format = "%Y-%m-%d %H:%M:%S")
T_BQ$Fecha <- as.POSIXct(T_BQ$Fecha, format = "%Y-%m-%d %H:%M:%S")
P_SM$Fecha <- as.POSIXct(P_SM$Fecha, format = "%Y-%m-%d %H:%M:%S")
P_CTG$Fecha <- as.POSIXct(P_CTG$Fecha, format = "%Y-%m-%d %H:%M:%S")
P_BQ$Fecha <- as.POSIXct(P_BQ$Fecha, format = "%Y-%m-%d %H:%M:%S")

#Crear columna de semana epidemiologica y año
HR_SM$Semana_Epidemiologica <- epiweek(HR_SM$Fecha)
HR_SM$Año <- year(HR_SM$Fecha)
HR_CTG$Semana_Epidemiologica <- epiweek(HR_CTG$Fecha)
HR_CTG$Año <- year(HR_CTG$Fecha)
HR_BQ$Semana_Epidemiologica <- epiweek(HR_BQ$Fecha)
HR_BQ$Año <- year(HR_BQ$Fecha)
T_SM$Semana_Epidemiologica <- epiweek(T_SM$Fecha)
T_SM$Año <- year(T_SM$Fecha)
T_CTG$Semana_Epidemiologica <- epiweek(T_CTG$Fecha)
T_CTG$Año <- year(T_CTG$Fecha)
T_BQ$Semana_Epidemiologica <- epiweek(T_BQ$Fecha)
T_BQ$Año <- year(T_BQ$Fecha)
P_SM$Semana_Epidemiologica <- epiweek(P_SM$Fecha)
P_SM$Año <- year(P_SM$Fecha)
P_CTG$Semana_Epidemiologica <- epiweek(P_CTG$Fecha)
P_CTG$Año <- year(P_CTG$Fecha)
P_BQ$Semana_Epidemiologica <- epiweek(P_BQ$Fecha)
P_BQ$Año <- year(P_BQ$Fecha)

#Formato semana y año
HR_BQ$Semana_Epidemiologica <- as.numeric(HR_BQ$Semana_Epidemiologica)
HR_CTG$Semana_Epidemiologica <- as.numeric(HR_CTG$Semana_Epidemiologica)
HR_SM$Semana_Epidemiologica <- as.numeric(HR_SM$Semana_Epidemiologica)
T_BQ$Semana_Epidemiologica <- as.numeric(T_BQ$Semana_Epidemiologica)
T_CTG$Semana_Epidemiologica <- as.numeric(T_CTG$Semana_Epidemiologica)
T_SM$Semana_Epidemiologica <- as.numeric(T_SM$Semana_Epidemiologica)
P_BQ$Semana_Epidemiologica <- as.numeric(P_BQ$Semana_Epidemiologica)
P_CTG$Semana_Epidemiologica <- as.numeric(P_CTG$Semana_Epidemiologica)
P_SM$Semana_Epidemiologica <- as.numeric(P_SM$Semana_Epidemiologica)

HR_BQ$Año <- as.numeric(HR_BQ$Año)
HR_CTG$Año <- as.numeric(HR_CTG$Año)
HR_SM$Año <- as.numeric(HR_SM$Año)
T_BQ$Año <- as.numeric(T_BQ$Año)
T_CTG$Año <- as.numeric(T_CTG$Año)
T_SM$Año <- as.numeric(T_SM$Año)
P_BQ$Año <- as.numeric(P_BQ$Año)
P_CTG$Año <- as.numeric(P_CTG$Año)
P_SM$Año <- as.numeric(P_SM$Año)

#Filtrar datos por años y corrección semanas epidemiológicas
HR_BQ <- HR_BQ %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

HR_BQ<- HR_BQ %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

HR_CTG <- HR_CTG %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

HR_CTG<- HR_CTG %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

HR_SM <- HR_SM %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

HR_SM<- HR_SM %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

T_BQ <- T_BQ %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

T_BQ<- T_BQ %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

T_CTG <- T_CTG %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

T_CTG<- T_CTG %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

T_SM <- T_SM %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

T_SM<- T_SM %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

P_BQ <- P_BQ %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

P_BQ<- P_BQ %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

P_CTG <- P_CTG %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

P_CTG <- P_CTG %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

P_SM <- P_SM %>%
  filter(between(Año, 2008, 2023) & (Año != 2008 | Semana_Epidemiologica >= 29))

P_SM <- P_SM %>%
  mutate(
    Fecha = as.Date(Fecha), 
    Semana_Epidemiologica = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01", "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30", "2012-12-31",
                           "2013-12-29", "2013-12-30", "2013-12-31",
                           "2017-12-31",
                           "2018-12-30", "2018-12-31",
                           "2019-12-29", "2019-12-30", "2019-12-31")) ~ 1,
      TRUE ~ epiweek (Fecha),
    ),
    
    Año = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ Año
    )
  )

#Promedio semanal 
HR_SM_sem <- HR_SM %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

HR_CTG_sem <- HR_CTG %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

HR_BQ_sem <- HR_BQ %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

T_SM_sem <- T_SM %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

T_CTG_sem <- T_CTG %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

T_BQ_sem <- T_BQ %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

P_SM_sem <- P_SM %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

P_CTG_sem <- P_CTG %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

P_BQ_sem <- P_BQ %>%
  group_by(Año, Semana_Epidemiologica) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

#Unir datos ambientales IDEAM
datos_amb <- list(HR_BQ_sem,HR_CTG_sem,HR_SM_sem, T_BQ_sem, T_CTG_sem, T_SM_sem, P_BQ_sem, P_CTG_sem, P_SM_sem)
datos_ideam<- reduce(datos_amb, full_join, by = c("Año", "Semana_Epidemiologica"))

#Renombrar las columnas y cambiar el formato de las columnas
colnames(dengue_casos)[colnames(dengue_casos) == "ano"] <- "Año"
colnames(dengue_casos)[colnames(dengue_casos) == "semana"] <- "Semana_Epidemiologica"
colnames(dengue_casos)[colnames(dengue_casos) == "BARRANQUILLA"] <- "Casos_Barranquilla"
colnames(dengue_casos)[colnames(dengue_casos) == "CARTAGENA"] <- "Casos_Cartagena"
colnames(dengue_casos)[colnames(dengue_casos) == "SANTA MARTHA"] <- "Casos_SantaMarta"

dengue_casos$Año <- as.numeric(dengue_casos$Año)
dengue_casos$Semana_Epidemiologica <- as.numeric(dengue_casos$Semana_Epidemiologica)

#Renombrar las columnas del archivo datos satelitales
colnames(data_clima_caribe_2008_2023)[colnames(data_clima_caribe_2008_2023) == "year"] <- "Año"
colnames(data_clima_caribe_2008_2023)[colnames(data_clima_caribe_2008_2023) == "epi_week"] <- "Semana_Epidemiologica"
data_clima_caribe_2008_2023$Semana_Epidemiologica <- as.numeric(data_clima_caribe_2008_2023$Semana_Epidemiologica)
data_clima_caribe_2008_2023=subset(data_clima_caribe_2008_2023,data_clima_caribe_2008_2023$Año!= 2024)

#Unir todos los datos
dengue_def <- dengue_casos %>%
  full_join(datos_ideam, by = c("Año", "Semana_Epidemiologica")) %>%
  full_join(data_clima_caribe_2008_2023, by = c("Año", "Semana_Epidemiologica"))
dengue_def <- arrange(dengue_def, Año, Semana_Epidemiologica)

#Renombrar columnas datos IDEAM
colnames(dengue_def)[colnames(dengue_def) == "Valor.x"] <- "HR_Barranquilla"
colnames(dengue_def)[colnames(dengue_def) == "Valor.y"] <- "HR_Cartagena"
colnames(dengue_def)[colnames(dengue_def) == "Valor.x.x"] <- "HR_SantaMarta"
colnames(dengue_def)[colnames(dengue_def) == "Valor.y.y"] <- "TM_Barranquilla"
colnames(dengue_def)[colnames(dengue_def) == "Valor.x.x.x"] <- "TM_Cartagena"
colnames(dengue_def)[colnames(dengue_def) == "Valor.y.y.y"] <- "TM_SantaMarta"
colnames(dengue_def)[colnames(dengue_def) == "Valor.x.x.x.x"] <- "PT_Barranquilla"
colnames(dengue_def)[colnames(dengue_def) == "Valor.y.y.y.y"] <- "PT_Cartagena"
colnames(dengue_def)[colnames(dengue_def) == "Valor"] <- "PT_SantaMarta"

#Descargar base
write.csv(dengue_def, "Datosdengueclima.csv")

#Distribución 
shapiro.test(dengue_def$Casos_Barranquilla) #No distribución normal
shapiro.test(dengue_def$Casos_Cartagena) #No distribución normal
shapiro.test(dengue_def$Casos_SantaMarta) #No distribución normal
shapiro.test(dengue_def$HR_Barranquilla) #No distribución normal
shapiro.test(dengue_def$HR_Cartagena) #No distribución normal
shapiro.test(dengue_def$HR_SantaMarta) #No distribución normal
shapiro.test(dengue_def$TM_Barranquilla) #No distribución normal
shapiro.test(dengue_def$TM_Cartagena) #No distribución normal
shapiro.test(dengue_def$TM_SantaMarta) #No distribución normal
shapiro.test(dengue_def$PT_Barranquilla) #No distribución normal
shapiro.test(dengue_def$PT_Cartagena) #No distribución normal
shapiro.test(dengue_def$PT_SantaMarta) #No distribución normal
shapiro.test(dengue_def$BAQ_avg_temperature) #Si distribución normal
shapiro.test(dengue_def$BAQ_max_temperature) #No distribución normal
shapiro.test(dengue_def$BAQ_min_temperature) #No distribución normal
shapiro.test(dengue_def$BAQ_avg_specific_humidity) #No distribución normal
shapiro.test(dengue_def$BAQ_avg_relative_humidity) #No distribución normal
shapiro.test(dengue_def$BAQ_total_precipitation) #No distribución normal

#CORRELACIONES SATELITALES
#Casos Barranquilla VS Temperatura media 
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, (dengue_def$BAQ_avg_temperature), method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_temperature, 24), method="spearman") #Spearman

#Casos Barranquilla VS Temperatura minima 
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, dengue_def$BAQ_min_temperature, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_min_temperature, 24), method="spearman") #Spearman

#Casos Barranquilla VS Temperatura maxima 
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, dengue_def$BAQ_max_temperature, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_max_temperature, 24), method="spearman") #Spearman

#Casos Barranquilla VS Precipitación
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, dengue_def$BAQ_total_precipitation, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,18), method="spearman") #Spearman
##LAG 20 
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_total_precipitation,24), method="spearman") #Spearman

#Casos Barranquilla VS Humedad Relativa
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, dengue_def$BAQ_avg_relative_humidity, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_relative_humidity,24), method="spearman") #Spearman

#Casos Barranquilla VS Humedad especifica
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, dengue_def$BAQ_avg_specific_humidity, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$BAQ_avg_specific_humidity,24), method="spearman") #Spearman


#Casos Cartagena VS Temperatura media 
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$CTG_avg_temperature, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_temperature,24), method="spearman") #Spearman

#Casos Cartagena VS. Temperatura minima 
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$CTG_min_temperature, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_min_temperature,24), method="spearman") #Spearman

#Casos Cartagena VS. Temperatura maxima 
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$CTG_max_temperature, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_max_temperature,24), method="spearman") #Spearman

#Casos Cartagena  VS. Precipitación
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$CTG_total_precipitation, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_total_precipitation,24), method="spearman") #Spearman

#Casos Cartagena VS. Humedad Relativa
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$CTG_avg_relative_humidity, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_relative_humidity,24), method="spearman") #Spearman

#Casos Cartagena VS. Humedad especifica
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$CTG_avg_specific_humidity, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$CTG_avg_specific_humidity,24), method="spearman") #Spearman


##Casos Santa Marta VS. Temperatura media 
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$SMR_avg_temperature, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_temperature,24), method="spearman") #Spearman

#Casos Santa Marta VS. Temperatura minima 
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$SMR_min_temperature, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_min_temperature,24), method="spearman") #Spearman

#Casos Santa Marta VS. Temperatura maxima 
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$SMR_max_temperature, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_max_temperature,24), method="spearman") #Spearman

##Casos Santa Marta  VS. Precipitación
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$SMR_total_precipitation, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_total_precipitation,24), method="spearman") #Spearman

##Casos Santa Marta VS. Humedad Relativa
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$SMR_avg_relative_humidity, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_relative_humidity,24), method="spearman") #Spearman

#Casos Santa Marta VS. Humedad especifica
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$SMR_avg_specific_humidity, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$SMR_avg_specific_humidity,24), method="spearman") #Spearman

#CORRELACIONES IDEAM

#Casos Barranquilla VS Temperatura media 
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, dengue_def$TM_Barranquilla, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$TM_Barranquilla,24), method="spearman") #Spearman

#Casos Barranquilla VS Precipitación
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, dengue_def$PT_Barranquilla, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,18), method="spearman") #Spearman
##LAG 20 
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$PT_Barranquilla,24), method="spearman") #Spearman

#Casos Barranquilla VS Humedad Relativa
##LAG 0
cor.test(dengue_def$Casos_Barranquilla, dengue_def$HR_Barranquilla, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Barranquilla, lag(dengue_def$HR_Barranquilla,24), method="spearman") #Spearman


#Casos Cartagena VS Temperatura media 
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$TM_Cartagena, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$TM_Cartagena,24), method="spearman") #Spearman

#Casos Cartagena  VS. Precipitación
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$PT_Cartagena, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$PT_Cartagena,24), method="spearman") #Spearman

#Casos Cartagena VS. Humedad Relativa
##LAG 0
cor.test(dengue_def$Casos_Cartagena, dengue_def$HR_Cartagena, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_Cartagena, lag(dengue_def$HR_Cartagena,24), method="spearman") #Spearman


##Casos Santa Marta VS. Temperatura media 
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$TM_SantaMarta, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$TM_SantaMarta,24), method="spearman") #Spearman

##Casos Santa Marta  VS. Precipitación
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$PT_SantaMarta, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$PT_SantaMarta, 24), method="spearman") #Spearman

##Casos Santa Marta VS. Humedad Relativa
##LAG 0
cor.test(dengue_def$Casos_SantaMarta, dengue_def$HR_SantaMarta, method="spearman") #Spearman
##LAG 2
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 2), method="spearman") #Spearman
##LAG 4
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 4), method="spearman") #Spearman
##LAG 8
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 8), method="spearman") #Spearman
##LAG 12
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 12), method="spearman") #Spearman
##LAG 14
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 14), method="spearman") #Spearman
##LAG 16
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 16), method="spearman") #Spearman
##LAG 18
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 18), method="spearman") #Spearman
##LAG 20
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 20), method="spearman") #Spearman
##LAG 22
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 22), method="spearman") #Spearman
##LAG 24
cor.test(dengue_def$Casos_SantaMarta, lag(dengue_def$HR_SantaMarta, 24), method="spearman") #Spearman


###Gráfico de dispersión discriminado por años
dengue_def$Año <- as.factor(dengue_def$Año)

grap_bq_tm_a <-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_avg_temperature, 20), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = 'none') + # remove legend
  labs(title = "Barranquilla",x = NULL, y = "Temperatura media") 

grap_ctg_tm_a <- ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_avg_temperature, 24), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = 'none') + # remove legend
  labs(title = "Cartagena",x = NULL, y = NULL) 

grap_smr_tm_a <- ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_avg_temperature, 16), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = 'none') + # remove legend
  labs(title = "Santa Marta",x = NULL, y = NULL) 

grap_bq_tmin_a <- ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_min_temperature, 24), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Temperatura mínima") # remove axis labels    

grap_ctg_tmin_a <-ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_min_temperature, 24), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_tmin_a <- ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_min_temperature, 16), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4)+
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_bq_tmax_a <-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_max_temperature, 20), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Temperatura máxima") # remove axis labels 

grap_ctg_tmax_a <-ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_max_temperature, 24), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_tmax_a <- ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_max_temperature, 16), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4)+
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_bq_tp_a <-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_total_precipitation,12), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Precipitación total") # remove axis labels     

grap_ctg_tp_a <- ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_total_precipitation, 14), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_tp_a <-ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_total_precipitation, 14), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4)+
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_bq_hr_a <-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_avg_relative_humidity, 12), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Humedad relativa") # remove axis labels    

grap_ctg_hr_a <-ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_avg_relative_humidity, 12), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_hr_a <-ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_avg_relative_humidity,14), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4)+
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_bq_he_a <-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_avg_specific_humidity,14), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Humedad específica") # remove axis labels 

grap_ctg_he_a <-ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_avg_specific_humidity,16), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) +
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_he_a <-ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_avg_specific_humidity, 18), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4)+
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

# Combinar los gráficos
layout <- matrix(c(1, 2, 3,
                   4, 5, 6,
                   7, 8, 9,
                   10, 11, 12,
                   13, 14, 15,
                   16, 17, 18), ncol = 3, byrow = TRUE)

grid.arrange(grap_bq_tm_a, grap_ctg_tm_a, grap_smr_tm_a, grap_bq_tmin_a, grap_ctg_tmin_a, grap_smr_tmin_a, grap_bq_tmax_a, grap_ctg_tmax_a, grap_smr_tmax_a, grap_bq_tp_a, grap_ctg_tp_a, grap_smr_tp_a, grap_bq_hr_a, grap_ctg_hr_a, grap_smr_hr_a, grap_bq_he_a, grap_ctg_he_a, grap_smr_he_a,
             bottom = "Casos de dengue", layout_matrix = layout) 


###Gráfico de dispersión sin discriminar
grap_bq_tm<-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_avg_temperature, 20))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = 'none') + # remove legend
  labs(title = "Barranquilla", x = NULL, y = "Temperatura media") 

grap_ctg_tm<- ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_avg_temperature, 24))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = 'none') + # remove legend
  labs(title = "Cartagena", x = NULL, y = NULL) 

grap_smr_tm<- ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_avg_temperature, 16))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = 'none') + # remove legend
  labs(title = "Santa Marta", x = NULL, y = NULL) 

grap_bq_tmin<- ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_min_temperature, 24))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Temperatura mínima") # remove axis labels 

grap_ctg_tmin<-ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_min_temperature, 24))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_tmin<- ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_min_temperature, 16))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_bq_tmax<-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_max_temperature, 20))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Temperatura máxima") # remove axis labels 

grap_ctg_tmax<-ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_max_temperature, 24))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_tmax<- ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_max_temperature, 16))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_bq_tp <-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_total_precipitation,12))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Precipitación total") # remove axis labels 

grap_ctg_tp <- ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_total_precipitation, 14))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_tp<-ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_total_precipitation, 14))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_bq_hr<-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_avg_relative_humidity, 12))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Humedad relativa") # remove axis labels    

grap_ctg_hr <-ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_avg_relative_humidity, 12))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_hr<-ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_avg_relative_humidity, 14))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_bq_he <-ggplot(dengue_def, aes(x = Casos_Barranquilla, y = lag(BAQ_avg_specific_humidity, 14))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = "Humedad específica") # remove axis labels 

grap_ctg_he<-ggplot(dengue_def, aes(x = Casos_Cartagena, y = lag(CTG_avg_specific_humidity, 16))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

grap_smr_he <-ggplot(dengue_def, aes(x = Casos_SantaMarta, y = lag(SMR_avg_specific_humidity, 18))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, fill = "white", color = "blue", size = 1, linewidth = 1) + # Curva general
  theme_bw() +
  guides(color = 'none') + # remove legend
  labs(x = NULL, y = NULL) # remove axis labels

# Combinar los gráficos
grid.arrange(grap_bq_tm, grap_ctg_tm, grap_smr_tm, 
             grap_bq_tmin, grap_ctg_tmin, grap_smr_tmin, 
             grap_bq_tmax, grap_ctg_tmax, grap_smr_tmax, 
             grap_bq_tp, grap_ctg_tp, grap_smr_tp, 
             grap_bq_hr, grap_ctg_hr, grap_smr_hr, 
             grap_bq_he, grap_ctg_he, grap_smr_he, 
             bottom = "Casos de dengue", layout_matrix = layout)

### Negative Binomial Models
# Modelo Barranquilla
nb_model_BQ  <- glm.nb(Casos_Barranquilla ~ lag(BAQ_avg_temperature, 20) + lag(BAQ_max_temperature, 20) + lag(BAQ_total_precipitation, 12) + lag(BAQ_avg_relative_humidity, 12) + lag(BAQ_avg_specific_humidity, 14),
               data = dengue_def)
summary(nb_model_BQ) #AIC:7252.8 


# Modelo Casos_Cartagena
nb_model_CTG <- glm.nb(Casos_Cartagena ~  lag(CTG_total_precipitation, 14)  + lag(CTG_avg_relative_humidity, 12),
                  data = dengue_def)
summary(nb_model_CTG) #AIC: 6448.5


# Modelo Santa Marta
nb_model_SMR  <- glm.nb(Casos_SantaMarta ~ lag(SMR_avg_temperature, 16) + lag(SMR_total_precipitation, 14) + lag(SMR_avg_specific_humidity, 18),
                  data = dengue_def)
summary(nb_model_SMR) #AIC: 5471.4

##Descriptivo
casosbq <- sum(dengue_def$Casos_Barranquilla, na.rm = TRUE)
print(casosbq)

casosctg <- sum(dengue_def$Casos_Cartagena, na.rm = TRUE)
print(casosctg)

casossmr <- sum(dengue_def$Casos_SantaMarta, na.rm = TRUE)
print(n=casossmr)

mean_casos_general <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  summarise(Casos_totales = sum(Casos_Barranquilla, Casos_Cartagena, Casos_SantaMarta, na.rm = TRUE)) %>%  # Sumar los casos totales
  pull(Casos_totales) %>%  # Extraer el valor de los casos totales
  mean(na.rm = TRUE)  # Calcular la media de los casos totales
print(mean_casos_general)

#Medias semanales de las variables ambientales
dengue_def <- dengue_def %>%
  mutate(Año = as.numeric(as.character(Año)))

#Barranquilla
mean_temp_bq <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_TM_Semanal_bq = mean(BAQ_avg_temperature, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_TM_bq = mean(Media_TM_Semanal_bq, na.rm = TRUE))    # Media de todas las semanas
print(mean_temp_bq)

mean_pt_bq <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_PT_Semanal_bq = sum(BAQ_total_precipitation, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_PT_bq = mean(Media_PT_Semanal_bq, na.rm = TRUE))    # Media de todas las semanas
print(mean_pt_bq)

mean_hr_bq <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_HR_Semanal_bq = mean(BAQ_avg_relative_humidity, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_HR_bq = mean(Media_HR_Semanal_bq, na.rm = TRUE))    # Media de todas las semanas
print(mean_hr_bq)

#Cartagena
mean_temp_ctg <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_TM_Semanal_ctg = mean(CTG_avg_temperature, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_TM_ctg = mean(Media_TM_Semanal_ctg, na.rm = TRUE))    # Media de todas las semanas
print(mean_temp_ctg)

mean_pt_ctg <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_PT_Semanal_ctg = sum(CTG_total_precipitation, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_PT_ctg = mean(Media_PT_Semanal_ctg, na.rm = TRUE))    # Media de todas las semanas
print(mean_pt_ctg)

mean_hr_ctg <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_HR_Semanal_ctg = mean(CTG_avg_relative_humidity, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_HR_ctg = mean(Media_HR_Semanal_ctg, na.rm = TRUE))    # Media de todas las semanas
print(mean_hr_ctg)

#Santa Marta
mean_temp_smr <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_TM_Semanal_smr = mean(SMR_avg_temperature, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_TM_smr = mean(Media_TM_Semanal_smr, na.rm = TRUE))    # Media de todas las semanas
print(mean_temp_smr)

mean_pt_smr <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_PT_Semanal_smr = sum(SMR_total_precipitation, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_PT_smr = mean(Media_PT_Semanal_smr, na.rm = TRUE))    # Media de todas las semanas
print(mean_pt_smr)

mean_hr_smr <- dengue_def %>%
  filter(Año >= 2009 & Año <= 2023) %>%  # Filtrar los años de interés
  group_by(Semana_Epidemiologica) %>%    # Agrupar por semana
  summarise(Media_HR_Semanal_smr = mean(SMR_avg_relative_humidity, na.rm = TRUE)) %>%  # Media semanal
  summarise(Media_Semanal_HR_smr = mean(Media_HR_Semanal_smr, na.rm = TRUE))    # Media de todas las semanas
print(mean_hr_smr)
