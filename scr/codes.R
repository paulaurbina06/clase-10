#==============================================================================#
# Autor(es): Eduard Martinez
# Colaboradores: 
# Fecha creacion: 10/08/2019
# Fecha modificacion: 10/03/2021
# Version de R: 4.0.3.
#==============================================================================#

# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,lfe,plm,AER,margins,stargazer,outreg,broom,estimatr) # cargar y/o instalar paquetes a usar

#--------#
# 1. OLS #
#--------#

### 1.1. Cargar bases de datos
geih = readRDS(file = 'data/output/GEIH Junio 2019-2020.rds') %>%
       subset(f_trabajo==1 & is.na(inactivo)==T) 
geih = lapply(geih,function(x) x = ifelse(is.na(x)==T,0,x)) %>% data.frame()
geih = mutate(geih , fex_c_2011 = as.character(fex_c_2011) %>% gsub(',','.',.) %>% as.numeric()/12, p6020 = ifelse(p6020==1,1,0))

### 1.1.2. Regresion lineal
?lm
ols = lm(formula = ocupado ~ p6020 + p6040 + urbano + esc , data = geih) 
ols
ols %>% summary() 

### 1.1.3. Que contiene el objeto OLS
View(ols)
ols$call # modelo estimado
ols$coefficients #  un vector con los coeficientes del modelo 
ols$na.action # observaciones para las que hay NA 
summary(ols)$r.squared # R^2
summary(ols)$adj.r.squared # R^2 ajustado

ols$residuals # un vector con los residuales del modelo
summary(ols$residuals)
hist(ols$residuals)

### 1.1.4. Valores predichos
ols %>% predict()
covid$y_gorro = predict(object = ols,newdata=geih)

#---------------------------------------------------#
# 2. Maximum Likelihood (Logit, Probit, Count Data) #
#---------------------------------------------------#

### 2.1. Estimaciones
logit = glm(formula = modelo, subset = year == 2020, data = geih, family = binomial(link = "logit")) 
logit %>% summary()

probit = glm(modelo, subset = year == 2020, data = geih, family = binomial(link = "probit") , weights = fex_c_2011) 
probit %>% summary()

### 2.2. Varios modelos (2^k modelos)
indepent = c("p6020","p6040","esc","poly(esc,2)","urbano")
matriz = expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE)) 
all_models = apply(matriz, 1, function(x) as.formula(paste(c("ocupado ~ 1", indepent[x]),collapse=" + ")))

### 2.3. Estimaciones
all_results = lapply(all_models, function(x) glm(formula = x, subset=year==2020, data=geih, family=binomial(link="logit")))
all_results
glm(formula = ocupado ~ p6020 + p6040 + urbano + esc + as.factor(dpto), subset = year == 2020, data = geih, family = binomial(link = "logit")) %>% summary()

#-------------------#
# 3.posestimaciones #
#-------------------#

### 4.1. Funcion 'outreg'
ols %>% summary()
outreg(fitlist = ols, digits = 4L, alpha = c(0.1, 0.05, 0.01),bracket = c("se"), starred = c("coef"), 
       robust = FALSE, small = TRUE,constlast = FALSE, norepeat = TRUE,)

### 4.2. Varios modelos a la vez"
list_models = list(all_results[[1]],all_results[[2]],all_results[[3]],all_results[[4]])
outreg(list_models)
outreg(all_results)

### 4.2.1. Cambiar los nombres de los modelos"
r_outreg = outreg(setNames(list_models, c('Modelo 1', 'Modelo 2', 'Modelo 3','Modelo 4')))
r_outreg

### 4.3. Funcion stargazer
stargazer(all_results[[1]],all_results[[2]],all_results[[3]],all_results[[4]],
          dep.var.caption = '' , dep.var.labels=c('Cluster') ,
          model.names = F , column.labels = c('Modelo 1','Modelo 2','Modelo 3','Modelo 4') ,
          covariate.labels = c('Wind') , #keep = c('sum_wind'),
          type = "text" , digits=4 , title="Night Lights Estimation" ,
          out = 'views/Resultados 1.tex',keep.stat = c('n','rsq','adj.rsq'),
          add.lines=list(c("Year FE","Yes","Yes"),c("Pixel FE","Yes","Yes")))

outreg_model = rockchalk::outreg(all_results[[1]],title = "Night Lights Estimation", float = TRUE)
outreg_model
cat(outreg_model,file = 'views/Resultados 2.tex')

### 4.4. Coefplot()
source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")
logit = glm(modelo, subset = year == 2020, data = geih, family = binomial(link = "logit")) 
coefplot(logit)

#---------------------#
# 4. Datos panel y IV #
#---------------------#

### 3.1. Cargar bases de dato
data = gapminder

### 3.2. Dos caminos para estimar datos panel
model_panel = as.formula('lifeExp ~ gdpPercap')

### 3.2.1. plm()
plm_model = plm(formula = model_panel,data = data , model = 'within' , effect = 'twoways' , index = c('country','year')) 
plm_model %>% summary()

### 3.2.2. felm()
felm_model = felm(lifeExp ~ gdpPercap | country + year | 0 | country , data = data)
felm_model %>% summary()

