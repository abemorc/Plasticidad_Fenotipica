# library(mise)
library(here)
library(readr)
library(tidyverse)
library(lmerTest)
library(car)
library(emmeans)
library(nortest)
library(labelled)
library(dunn.test)


# desactivar notacion cientifica
options (scipen = 999)

# limpiar entorno de trabajo
# mise(vars = TRUE, figs = TRUE, console = TRUE)
# setwd("C:/R/rtest_n")

# importar datos
grow <- read.csv(here("Data/crecimiento_v2.2.csv"))
crecimiento_v2_2 <- read_csv(here("Data/crecimiento_v2.2.csv"))

# ver datos
View(crecimiento_v2_2)
View(grow)

# cargar variables de grow al entorno global
attach(grow)


# exploracion y visualizacion de los datos
ggplot(grow, aes(x=medicion2, y=lcu, shape=tratamiento)) + 
  geom_point() + 
  theme_bw()
ggplot(grow, aes(x=medicion2, y=lcu, color=tratamiento)) + 
  geom_point() + 
  theme_bw()


# Creo que quedarian mejor con unos boxplot 
ggplot(grow, aes(x=medicion2, y=lcu, color=tratamiento)) + 
  geom_boxplot()
ggplot(grow, aes(x=medicion2, y=lcu, shape=tratamiento)) + 
  geom_boxplot()


ggplot(grow, aes(x=tratamiento, y=lcu)) + 
  geom_boxplot() + 
  theme_bw()


# install.packages("nortest")       # Para probar normalidad




# añadir descripcion a las variables --------------------------------------

var_label(grow$lcu) <- "longitud del cuerpo de las larvas"

var_label(grow$lco) <- "longitud de la cola"
var_label(grow$lt) <- "longitud total"
var_label(grow$peso) <- "peso total de las larvas"
var_label(grow$tratamiento) <- "Las temperatura del agua"
var_label(grow$medicion2) <- "periodo del experimento"
var_label(grow$tank) <- "codigo del tanque de agua"
var_label(grow$clutch) <- "# de puesta"

# vemos que ya quedo bonito
str(grow)
View(grow)

# exploracion datos -------------------------------------------------------

grow %>% group_by(medicion) %>% 
  summarise(n = n())

grow %>% group_by(tank) %>% 
  summarise(n = n()) %>% 
  view()

grow %>% group_by(clutch) %>% 
  summarise(n = n())

grow %>% group_by(medicion2) %>% 
  summarise(n = n()) %>% 
  summarise(total = sum(n))


# veamos si las variables son normales

# prueba Lilliefors
lillie.test(lcu)
# Resultado: no normal

lillie.test(lco)
# no normal

lillie.test(lt)
# no normal

lillie.test(peso)
# no normal




# Ajuste de un modelo lineal con efectos aleatorios -----------------------


# unicamente es un ejemplo usando un model lineal -------------------------
ejem <- lm(lcu~medicion2)
summary(ejem)
plot(ejem)
##############################################################



# Empieza aqui:

# lo mismo que lo de abajo
# mod_lcu <- glmer(lcu~tratamiento+medicion2+(1|tank)+(1|clutch))

# primer modelo
mod_lcu <- lmer(lcu~tratamiento+medicion2+(1|tank)+(1|clutch))

# test accuracy
mod_lcu
summary(mod_lcu)

plot(mod_lcu)

Anova(mod_lcu)
anova(mod_lcu)


summary(anova(mod_lcu))

# revisar los residuales 
qqPlot(residuals(mod_lcu))

# parece que el modelo presenta un ajuste aceptable
#######


# un posible enfoque ------------------------------------------------------

modelo1 <- lmer(lcu~tratamiento +(1|tank))

# resultados del modelo
modelo1
summary(modelo1)
anova(modelo1)

qqPlot(residuals(modelo1))

# no sirvio

modelo2 <- lmer(lcu~tratamiento +(1|clutch))
modelo2
summary(modelo2)
anova(modelo2)

qqPlot(residuals(modelo2))

#  tampoco sirve
#  

modelo3 <- lmer(lcu~tratamiento+(1|tank)+(1|clutch))
modelo3
summary(modelo3)
anova(modelo3)

qqPlot(residuals(modelo3))

# tampoco
# 


modelo4 <- lmer(lcu~tratamiento+medicion2+(1|tank)+(1|clutch))
modelo4
summary(modelo4)
anova(modelo4)

qqPlot(residuals(modelo4))




modelo5 <- lmer(lcu~tratamiento+medicion2+(1|tank))
modelo5
summary(modelo5)
anova(modelo5)

qqPlot(residuals(modelo5))








# esta parte no le entiendo completamente 
# Probando otras interacciones --------------------------------------------

mod_lcu2 <- lmer(lcu~tratamiento*medicion2+(1|tank)+(1|clutch))
anova(mod_lcu,mod_lcu2)
summary(mod_lcu2)

# esto lo añadi para checar los residuales
qqPlot(residuals(mod_lcu2))



broom.mixed::tidy(mod_lcu2)


kruskal.test(lcu~tratamiento*medicion2)
kruskal.test(lcu~tratamiento)


dunn.test(lcu~tratamiento)
broom.mixed::tidy(mod_lcu2)


# esta parte que significa??
# 
emmeans(mod_lcu2,list(pairwise~tratamiento), adjust="tukey")
emmeans(mod_lcu2,list(pairwise~tratamiento*medicion2), adjust="tukey")
emmeans(mod_lcu2,list(pairwise~tratamiento), adjust="tukey")


# ggplot(grow, aes(x=medicion2, y=lcu, color=tratamiento)) + geom_point() + theme_bw()
# ggplot(grow, aes(x=tratamiento, y=lcu)) + geom_boxplot() + theme_bw()
# ggplot(grow, aes(x=medicion2, y=lcu, color=tratamiento)) + geom_point() + theme_bw()
# ggplot(grow, aes(x=tratamiento, y=lcu)) + geom_boxplot() + theme_bw()


Anova(mod_lcu2)
anova(mod_lcu2)


ggplot(grow, aes(x=tratamiento, y=lcu)) + 
  geom_boxplot(aes(fill=medicion2)) + 
  theme_bw()
ggplot(grow, aes(x=medicion2, y=lcu, color=tratamiento)) + 
  geom_point() + 
  theme_bw()
ggplot(grow, aes(x=tratamiento, y=lcu)) + 
  geom_boxplot() + 
  theme_bw()

mod_lcu <- lmer(lcu~tratamiento+medicion2+(1|tank)+(1|clutch))
mod_lcu2 <- lmer(lcu~tratamiento*medicion2+(1|tank)+(1|clutch))

anova(mod_lcu,mod_lcu2)
anova(mod_lcu2)
summary(mod_lcu2)
anova(mod_lcu2)
Anova(mod_lcu2)



mod_lco <- lmer(lco~tratamiento+medicion2+(1|tank)+(1|clutch))
mod_lco2 <- lmer(lco~tratamiento*medicion2+(1|tank)+(1|clutch))
anova(mod_lco,mod_lco2)
# mod_lco2 <- lmer(lco~tratamiento*medicion2+(1|tank)+(1|clutch))
# anova(mod_lco,mod_lco2)

Anova(mod_lco2)



ggplot(grow, aes(x=tratamiento, y=lcu)) + 
  geom_boxplot(aes(fill=medicion2)) + 
  theme_bw()
ggplot(grow, aes(x=tratamiento, y=lco)) + 
  geom_boxplot(aes(fill=medicion2)) + 
  theme_bw()




ggplot(grow, aes(x=tratamiento, y=lcu)) + 
  geom_boxplot(aes(fill=medicion2))
ggplot(grow, aes(x=tratamiento, y=lco)) + 
  geom_boxplot(aes(fill=medicion2))










# ejemplo -----------------------------------------------------------------

view(grow)

ggplot(data = grow, aes(x = medicion2, y = lcu))+
  geom_boxplot()

# modelo lineal simple
summary(lm(data = grow, formula = lcu~medicion2))

# modelo lineal con factor aleatorio
summary(lmer(data = grow, 
     formula = lcu~medicion2+(1|tank)))



