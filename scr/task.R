#==============================================================================#
# Autor(es): Eduard Martinez
# Colaboradores: 
# Fecha creacion: 10/08/2019
# Fecha modificacion: 10/03/2021
# Version de R: 4.0.3.
#==============================================================================#

# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,lfe,plm,AER,margins,stargazer,outreg) # cargar y/o instalar paquetes a usar

# cargar bases de datos
covid = readRDS(file = 'data/output/Casos por UPZ.rds')

# generar variable de tratamiento
rstudioapi::viewer(url = 'lecture/pics/cuarentenas.jpg')
treated = c(2,3,4,5,6,14,18,19)
control = c(11,10,12)
covid = mutate(covid , treatment = ifelse(cod_localidad %in% treated,1,0),
                       borde =  ifelse(cod_localidad %in% c(control,treated),1,0))

# correr regresiones (cuarentena contra no cuarentena)
cat("casos_08 ~ casos_07 + treatment")
ols_1 = lm(casos_08 ~ casos_07 + treatment , data = covid)
ols_1
ols_1 %>% summary()

# correr regresion borde 
cat("casos_08 ~ casos_07 + treatment")
ols_2 = lm(casos_08 ~ casos_07 + treatment , data = covid , subset = borde==1)
ols_2
ols_2 %>% summary()

# revise los objetos de las regresiones

