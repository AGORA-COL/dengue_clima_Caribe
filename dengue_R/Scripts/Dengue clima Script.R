#Instalar paquetes y llamar librerias
install.packages("lubridate")
remove.packages("sivirep")
if (!require("pak")) install.packages("pak")
pak::pak("epiverse-trace/sivirep")
library(sivirep)
library(dplyr)
library(tidyr)
library (lubridate)
library(purrr)
library(ggplot2)

# Definir los años que deseas abarcar
years <- 2009:2022 

# Inicializar una lista para almacenar los datos
data_list <- list()

# Usar un bucle para importar datos de cada año
for (year in years) {
  dengue <- import_data_event(nombre_event = "dengue", year = year)
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

# Ciudades a columnas
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

#Establecer formato fecha de .data   
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

#Formato datos ambientales
HR_BQ_sem$Semana_Epidemiologica <- as.numeric(HR_BQ_sem$Semana_Epidemiologica)
HR_CTG_sem$Semana_Epidemiologica <- as.numeric(HR_CTG_sem$Semana_Epidemiologica)
HR_SM_sem$Semana_Epidemiologica <- as.numeric(HR_SM_sem$Semana_Epidemiologica)
T_BQ_sem$Semana_Epidemiologica <- as.numeric(T_BQ_sem$Semana_Epidemiologica)
T_CTG_sem$Semana_Epidemiologica <- as.numeric(T_CTG_sem$Semana_Epidemiologica)
T_SM_sem$Semana_Epidemiologica <- as.numeric(T_SM_sem$Semana_Epidemiologica)
P_BQ_sem$Semana_Epidemiologica <- as.numeric(P_BQ_sem$Semana_Epidemiologica)
P_CTG_sem$Semana_Epidemiologica <- as.numeric(P_CTG_sem$Semana_Epidemiologica)
P_SM_sem$Semana_Epidemiologica <- as.numeric(P_SM_sem$Semana_Epidemiologica)

HR_BQ_sem$Año <- as.numeric(HR_BQ_sem$Año)
HR_CTG_sem$Año <- as.numeric(HR_CTG_sem$Año)
HR_SM_sem$Año <- as.numeric(HR_SM_sem$Año)
T_BQ_sem$Año <- as.numeric(T_BQ_sem$Año)
T_CTG_sem$Año <- as.numeric(T_CTG_sem$Año)
T_SM_sem$Año <- as.numeric(T_SM_sem$Año)
P_BQ_sem$Año <- as.numeric(P_BQ_sem$Año)
P_CTG_sem$Año <- as.numeric(P_CTG_sem$Año)
P_SM_sem$Año <- as.numeric(P_SM_sem$Año)

#Unir datos ambientales
datos_amb <- list(HR_BQ_sem,HR_CTG_sem,HR_SM_sem, T_BQ_sem, T_CTG_sem, T_SM_sem, P_BQ_sem, P_CTG_sem, P_SM_sem)
datos_ambientales<- reduce(datos_amb, full_join, by = c("Año", "Semana_Epidemiologica"))


#Renombrar las columnas y cambiar el formato de las columnas
colnames(dengue_casos)[colnames(dengue_casos) == "ano"] <- "Año"
colnames(dengue_casos)[colnames(dengue_casos) == "semana"] <- "Semana_Epidemiologica"

dengue_casos$Año <- as.numeric(dengue_casos$Año)
dengue_casos$Semana_Epidemiologica <- as.numeric(dengue_casos$Semana_Epidemiologica)

#Unir todos los datos
datos_final <- left_join(dengue_casos, datos_ambientales, by = c("Año", "Semana_Epidemiologica"))

#Datos finales base
colnames(datos_final)[colnames(datos_final) == "BARRANQUILLA"] <- "Casos_Barranquilla"
colnames(datos_final)[colnames(datos_final) == "CARTAGENA"] <- "Casos_Cartagena"
colnames(datos_final)[colnames(datos_final) == "SANTA MARTHA"] <- "Casos_SantaMarta"
colnames(datos_final)[colnames(datos_final) == "Valor.x"] <- "HR_Barranquilla"
colnames(datos_final)[colnames(datos_final) == "Valor.y"] <- "HR_Cartagena"
colnames(datos_final)[colnames(datos_final) == "Valor.x.x"] <- "HR_SantaMarta"
colnames(datos_final)[colnames(datos_final) == "Valor.y.y"] <- "T_Barranquilla"
colnames(datos_final)[colnames(datos_final) == "Valor.x.x.x"] <- "T_Cartagena"
colnames(datos_final)[colnames(datos_final) == "Valor.y.y.y"] <- "T_SantaMarta"
colnames(datos_final)[colnames(datos_final) == "Valor.x.x.x.x"] <- "P_Barranquilla"
colnames(datos_final)[colnames(datos_final) == "Valor.y.y.y.y"] <- "P_Cartagena"
colnames(datos_final)[colnames(datos_final) == "Valor"] <- "P_SantaMarta"

write.csv(datos_final, "Datosdenguef15.csv")

#Distribuciòn normal
shapiro.test(datos_final$Casos_Barranquilla) #No distribución normal
shapiro.test(datos_final$Casos_Barranquilla) #No distribución normal

##Casos Barranquilla: Casos VS. Temperatura media 
##LAG 0
cor.test(datos_final$Casos_Barranquilla, datos_final$T_Barranquilla, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_Barranquilla[-c(1:2)], head(datos_final$T_Barranquilla,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_Barranquilla[-c(1:4)], head(datos_final$T_Barranquilla,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_Barranquilla[-c(1:8)], head(datos_final$T_Barranquilla,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_Barranquilla[-c(1:12)], head(datos_final$T_Barranquilla,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_Barranquilla[-c(1:14)], head(datos_final$T_Barranquilla,-14), method="spearman") #Spearman
## LAG-18
cor.test(datos_final$Casos_Barranquilla[-c(1:18)], head(datos_final$T_Barranquilla,-18), method="spearman") #Spearman

#Gráfico lag 12
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_Barranquilla[-c(1:12)], y = head(datos_final$T_Barranquilla,-12), color = Año)) +
  geom_point() +
  geom_labelsmooth(aes(label = Año), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = 1, boxlinewidth = 0.4) + xlim(0,210)  + ylim(26,30)+
  theme_bw() + guides(color = 'none') # remove legend


ggplot(head(datos_final), aes(x = datos_final$Casos Barranquilla, y = datos_final$BAQ.Numero de casos, color = "red")) + 
  geom_line (aes(x = datos_final$Cartagena, y = datos_final$BAQ.Numero de casos, color = "blue")) + 
  geom_line (aes(x = datos_final$Santa Marta, y = datos_final$BAQ.Numero de casos, color = "green") +
               geom_point() +
               geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4) + xlim(0,210)  + ylim(26,30)+
               theme_bw() + guides(color = 'none') # remove legend

##Casos_Barranquilla: Casos VS. Precipitación
             
##LAG 0
cor.test(datos_final$Casos_Barranquilla, datos_final$P_Barranquilla, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_Barranquilla[-c(1:2)], head(datos_final$P_Barranquilla,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_Barranquilla[-c(1:4)], head(datos_final$P_Barranquilla,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_Barranquilla[-c(1:8)], head(datos_final$P_Barranquilla,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_Barranquilla[-c(1:12)], head(datos_final$P_Barranquilla,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_Barranquilla[-c(1:14)], head(datos_final$P_Barranquilla,-14), method="spearman") #Spearman
## LAG-18
cor.test(datos_final$Casos_Barranquilla[-c(1:18)], head(datos_final$P_Barranquilla,-18), method="spearman") #Spearman
             
#Gráfico lag 12
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_Barranquilla[-c(1:12)], y = head(datos_final$P_Barranquilla,-12), color = Año)) +
geom_point() +
geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4) + xlim(0,210) + ylim(0,1500)+
theme_bw() + guides(color = 'none') # remove legend
             
##Casos_Barranquilla: Casos VS. Humedad
##LAG 0
cor.test(datos_final$Casos_Barranquilla, datos_final$HR_Barranquilla, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_Barranquilla[-c(1:2)], head(datos_final$HR_Barranquilla,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_Barranquilla[-c(1:4)], head(datos_final$HR_Barranquilla,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_Barranquilla[-c(1:8)], head(datos_final$HR_Barranquilla,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_Barranquilla[-c(1:12)], head(datos_final$HR_Barranquilla,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_Barranquilla[-c(1:14)], head(datos_final$HR_Barranquilla,-14), method="spearman") #Spearman
## LAG-18
cor.test(datos_final$Casos_Barranquilla[-c(1:18)], head(datos_final$HR_Barranquilla,-18), method="spearman") #Spearman
             
#Gráfico lag 12
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_Barranquilla[-c(1:12)], y = head(datos_final$HR_Barranquilla,-12), color = Año)) +
geom_point() +
geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4) + xlim(0,210) + ylim(70,95)+
theme_bw() + guides(color = 'none') # remove legend
             
             
##Casos_Cartagena: Casos VS. Temperatura
##LAG 0
cor.test(datos_final$Casos_Cartagena, datos_final$T_Cartagena, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_Cartagena[-c(1:2)], head(datos_final$T_Cartagena,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_Cartagena[-c(1:4)], head(datos_final$T_Cartagena,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_Cartagena[-c(1:8)], head(datos_final$T_Cartagena,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_Cartagena[-c(1:12)], head(datos_final$T_Cartagena,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_Cartagena[-c(1:14)], head(datos_final$T_Cartagena,-14), method="spearman") #Spearman
## LAG-18
cor.test(datos_final$Casos_Cartagena[-c(1:18)], head(datos_final$T_Cartagena,-18), method="spearman") #Spearman
             
#Gráfico lag 12
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_Cartagena[-c(1:12)], y = head(datos_final$T_Casos_Cartagena,-12), color = Año)) +
geom_point() +
geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4) + xlim(0,210)  + ylim(26,30)+
theme_bw() + guides(color = 'none') # remove legend
             
##Casos_Cartagena: Casos VS. Precipitación
##LAG 0
cor.test(datos_final$Casos_Cartagena, datos_final$P_Cartagena, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_Cartagena[-c(1:2)], head(datos_final$P_Cartagena,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_Cartagena[-c(1:4)], head(datos_final$P_Cartagena,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_Cartagena[-c(1:8)], head(datos_final$P_Cartagena,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_Cartagena[-c(1:12)], head(datos_final$P_Cartagena,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_Cartagena[-c(1:14)], head(datos_final$P_Cartagena,-14), method="spearman") #Spearman
## LAG-18
cor.test(datos_final$Casos_Cartagena[-c(1:18)], head(datos_final$P_Cartagena,-18), method="spearman") #Spearman

#Gráfico lag 12
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_Cartagena[-c(1:12)], y = head(datos_final$P_Cartagena,-12), color = Año)) +
geom_point() +
geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4) + ylim(0,1500) + xlim(0,200)+
theme_bw() + guides(color = 'none') # remove legend
             
            
##Casos_Cartagena: Casos VS. Humedad
##LAG 0
cor.test(datos_final$Casos_Cartagena, datos_final$HR_Cartagena, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_Cartagena[-c(1:2)], head(datos_final$HR_Cartagena,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_Cartagena[-c(1:4)], head(datos_final$HR_Cartagena,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_Cartagena[-c(1:8)], head(datos_final$HR_Cartagena,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_Cartagena[-c(1:12)], head(datos_final$HR_Cartagena,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_Cartagena[-c(1:14)], head(datos_final$HR_Cartagena,-14), method="spearman") #Spearman
## LAG-18
cor.test(datos_final$Casos_Cartagena[-c(1:18)], head(datos_final$HR_Cartagena,-18), method="spearman") #Spearman
             
#Gráfico lag 12             
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_Cartagena[-c(1:12)], y = head(datos_final$HR_Cartagena,-12), color = Año)) +
geom_point() +
geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4)  + xlim(0,200)+ ylim(70,95)+
theme_bw() + guides(color = 'none') # remove legend
             
             
##Santa Marta: Casos VS. Temperatura
##LAG 0
cor.test(datos_final$Casos_SantaMarta, datos_final$T_SantaMarta, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_SantaMarta[-c(1:2)], head(datos_final$T_SantaMarta,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_SantaMarta[-c(1:4)], head(datos_final$T_SantaMarta,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_SantaMarta[-c(1:8)], head(datos_final$T_SantaMarta,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_SantaMarta[-c(1:12)], head(datos_final$T_SantaMarta,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_SantaMarta[-c(1:14)], head(datos_final$T_SantaMarta,-14), method="spearman") #Spearman
## LAG-18
cor.test(datos_final$Casos_SantaMarta[-c(1:18)], head(datos_final$T_SantaMarta,-18), method="spearman") #Spearman
             
#Gráfico lag 12             
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_SantaMarta[-c(1:12)], y = head(datos_final$T_SantaMarta,-12), color = Año)) +
geom_point() +
geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4) + xlim(0,70)  + ylim(26,30)+
theme_bw() + guides(color = 'none') # remove legend
             
             
##Santa Marta: Casos VS. Precipitación
##LAG 0
cor.test(datos_final$Casos_SantaMarta, datos_final$P_SantaMarta, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_SantaMarta[-c(1:2)], head(datos_final$P_SantaMarta,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_SantaMarta[-c(1:4)], head(datos_final$P_SantaMarta,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_SantaMarta[-c(1:8)], head(datos_final$P_SantaMarta,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_SantaMarta[-c(1:12)], head(datos_final$P_SantaMarta,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_SantaMarta[-c(1:14)], head(datos_final$P_SantaMarta,-14), method="spearman") #Spearman            
## LAG-18
cor.test(datos_final$Casos_SantaMarta[-c(1:18)], head(datos_final$P_SantaMarta,-18), method="spearman") #Spearman

#Gráfico lag 12
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_SantaMarta[-c(1:12)], y = head(datos_final$P_SantaMarta,-12), color = Año)) +
geom_point() +
geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4)  + ylim(0,1500)+ xlim(0,70)+
theme_bw() + guides(color = 'none') # remove legend
             
             
##Santa Marta: Casos VS. Humedad
##LAG 0
cor.test(datos_final$Casos_SantaMarta, datos_final$HR_SantaMarta, method="spearman") #Spearman
##LAG -2
cor.test(datos_final$Casos_SantaMarta[-c(1:2)], head(datos_final$HR_SantaMarta,-2), method="spearman") #Spearman
## LAG-4
cor.test(datos_final$Casos_SantaMarta[-c(1:4)], head(datos_final$HR_SantaMarta,-4), method="spearman") #Spearman
## LAG-8
cor.test(datos_final$Casos_SantaMarta[-c(1:8)], head(datos_final$HR_SantaMarta,-8), method="spearman") #Spearman
## LAG-12
cor.test(datos_final$Casos_SantaMarta[-c(1:12)], head(datos_final$HR_SantaMarta,-12), method="spearman") #Spearman
## LAG-14
cor.test(datos_final$Casos_SantaMarta[-c(1:14)], head(datos_final$HR_SantaMarta,-14), method="spearman") #Spearman    
## LAG-18
cor.test(datos_final$Casos_SantaMarta[-c(1:18)], head(datos_final$HR_SantaMarta,-18), method="spearman") #Spearman
             
#Gráfico lag 12           
ggplot(head(datos_final,-12), aes(x = datos_final$Casos_SantaMarta[-c(1:12)], y = head(datos_final$HR_SantaMarta,-12), color = Año)) +
               geom_point() +
               geom_labelsmooth(aes(label = Año), fill = "white",
                                method = "lm", formula = y ~ x,
                                size = 3, linewidth = 1, boxlinewidth = 0.4)  + ylim(70,95)+ xlim(0,70)+
               theme_bw() + guides(color = 'none') # remove legend
             
             
