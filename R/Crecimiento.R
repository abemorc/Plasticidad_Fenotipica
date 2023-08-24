

# Analisis del efecto de la temperatura en la longitud del cuerpo -------
# Anfibios




# Descripcion del experimiento --------------------------------------------

# Con el fin de estudiar el efecto de la temperatura en el desarrollo de 
# anfibios, se realizo el siguiente experimiento:
# 
# En un espacio acondicionado lo mejor posible para este fin, se situaron 
# 36 tanques con agua, cada uno con un nivel de temperatura diferente 
# (tres posibles niveles de temperatura, es decir 12 tanques para cada nivel)
# acomodados de manera aleatoria
# Se obtuvieron un total de 144 renacuajos (estadio 27), cada uno proveniente
# de diferente puestas o clutches (tres puestas en total) y en cada tanque 
# se colocaron 4 renacuajos al azar y se registraron sus medidas de 
# longitud de cuerpo, cola, peso, estadio etc.
# Cada 4 dias se realizo una "medicion" posterior hasta el final del 
# experimento, dando un total de 8 mediciones mas la inicial (32 dias en
# total de duracion)
# 
# Los datos de este experimento se registraron en el archivo
# crecimiento_v2.2.csv
# 
# Nota:
# Aunque se busco la completa equidad entre cada individuo del experimento,
# es posible que en el espacio donde se realizo algunos 
# tanques experimentaran diferentes condiciones minimas, tales
# como luz solar, corriente de aire, humedad etc.
# Es importante mencionar esto debido a que se sabe que los anfibios
# (renacuajos) son mucho mas sensibles a este tipo de condiciones a 
# diferencia de otras especies.
# 
# #############


# Carga paquetes ----------------------------------------------------------

library(here)
library(readr)
library(ggpubr)
library(fitdistrplus)
library(lme4)
library(tidyverse)
library(visreg)
library(ggeffects)
library(car)

#######


# Importar y corregir datos -----------------------------------------------

dfcrecimiento <- read_csv(file = here("Data/crecimiento_v2.2.csv"), 
                          col_types = cols(
                            lcu = col_number(), 
                            fecha = col_date(format = "%d/%m/%Y")))
view(dfcrecimiento)
str(dfcrecimiento)
# vemos que hay algunas columnas que en realidad son variables categoricas 
# aunque esten como variables numericas

dfcrecimiento <- dfcrecimiento %>% 
  mutate(across(c(posicion, tank, tratamiento, poza, pool, puesta, clutch,
                  medicion, medicion2, estadio, id), .fns = as.factor))

#######



# Objetivo ----------------------------------------------------------------

# Para este analisis, nuestro objetivo es evaluar el efecto de los 
# diferentes tratamientos (temperaturas) en el crecimiento de los anfibios
# (longitud cuerpo, longitud cola, peso corporal, etc.)
# Por lo tanto las variables de mayor interes en este caso son:
# 
# lcu
# lco
# lt
# peso
# medicion2
# tank
# puesta
# fecha



# Analisis exploratorio ---------------------------------------------------



str(dfcrecimiento)
summary(dfcrecimiento)
view(dfcrecimiento)
max(dfcrecimiento$fecha)-min(dfcrecimiento$fecha)

# podemos ver que efectivamente son 144 individuos los que se observaron 
# y midieron durante el experimento, el cual tuvo duracion del 28 de 
# febrero del 2023 al 01 abril 2023 (32 dias)
# 
# 36 tanques de agua
# 3 tratamientos
# 3 puestas o clutches
# 15 niveles de estadio o crecimiento
# 9 fechas de medicion de datos, a intervalos de 4 dias incluyendo el 
#   momento inicial



# Individuos por tanque 
dfcrecimiento %>% 
  group_by(tank) %>% 
  summarise(n=n()) %>% 
  view()
# se observa que algunos tienen mas observaciones, lo que indica que 
# a lo largo del experimento fallecieron o alcanzaron el 
# estadio de adultos 

# progreso a traves del tiempo
individuos <- dfcrecimiento %>% 
  group_by(fecha) %>% 
  summarise(Renacuajos=n()) %>% 
  view()
# vemos que entre el dia 12 y 16 del experimento es cuando los primeros 
# individuos mueren o se dejan de registrar sus datos por alcanzar la adultez
 
individuos %>% ggplot(aes(fecha,Renacuajos))+
  geom_line()+
  geom_point()




dfcrecimiento %>% 
  group_by(fecha, tratamiento) %>% 
  summarise(Renacuajos=n()) %>% 
  ggplot(aes(fecha, Renacuajos,color=tratamiento))+
  geom_point()+
  geom_line()
# se puede observar como el numero de registros desciende primero del
# tratamiento 3, ya sea porque alcanzaron su fase adulta o porque
# murieron




# vamos a explorar la variable de interes lcu

df2 <- dfcrecimiento %>% 
  group_by(clutch, tank) %>% 
  summarise(mean=mean(lcu, na.rm=T))

ggplot(data = df2, aes(x = tank, y = mean)) + geom_point()

df2 %>%
  ggplot( aes(x=tank, y=mean, group=clutch, color=clutch)) +
  geom_line()
# existe un ligero patron en los tanques, teniendo varios picos(altos y bajos)
# en los mismos puntos, entre las tres puestas, lo que podria sugerir que 
# efectivamente como se mencionaba al inicio algunos tanques por la ubicacion
# de los mismos podiran haber estado sometidos al lo largo del experimento a
# unas condiciones diferentes (no todos los tanques tenian exactamente las 
# mismas condiciones, habian algunas mas favorables)
# IMPORTANTE**



dfcrecimiento %>% 
  group_by(medicion2, tratamiento) %>% 
  summarise(mean=mean(lcu, na.rm=T)) %>% 
  ggplot(aes(medicion2,mean, group=tratamiento, color=tratamiento))+
  geom_line()
# se observa claramente que a medida que pasa el tiempo existe un aumento 
# apresurado en la longitud del cuerpo para el tratamiento 3, haciendo
# que estos lleguen antes al estadio adulto


# boxplot longitud cuerpo
dfcrecimiento %>% 
  ggplot(aes(medicion2, lcu, color=tratamiento))+
  geom_boxplot()
# el tratamiento tres presenta  crecimiento mas rapido 
# respecto a los otros dos



# Distribucion de probabilidad --------------------------------------------

# Es importante determinar la distribucion para poder elegir
# el modelo adecuado que explique la relacion entre nuetras variables

# exploracion visual
dfcrecimiento %>% 
  ggplot(aes(x=lcu))+
  geom_histogram()


dfcrecimiento %>% 
  ggplot(aes(x=lcu))+
  geom_density()

plot(density(na.omit(dfcrecimiento$lcu)))
# no parece haber una clara distribucion conocida
# 

# graficas qqplot
# 
ggpubr::ggqqplot(dfcrecimiento$lcu)
qqnorm(dfcrecimiento$lcu)
# al menos la variable unicamente sola no indica tener una distribucion normal


ggplot(dfcrecimiento, aes(x = lcu)) +
  geom_density() + facet_wrap(tratamiento~medicion2)
# al ser pocas observaciones, no se ve una tendencia clara






# test formales de normalidad
# 
shapiro.test(dfcrecimiento$lcu)
# no es normal

ks.test(na.omit(dfcrecimiento$lcu), "pnorm")
# no es normal la variable sola



# Mas pruebas de distribucion ---------------------------------------------

# se puede usar un modelo lineal simple para determinar si una distribucion
# es adecuada para una variable en funcion de otra variable 

ml1 <- lm(formula = lcu~medicion2, data = dfcrecimiento)
summary(ml1)
qqPlot(residuals(ml1))
# En este caso vemos que una distribucion normal si es adecuada para estos 
# datos


# Pruebas respecto a otras distribuciones


# El paquete fitdistrplus nos puede ayudar como una guia para determinar 
# cual es una distribucion adecuada para nuestros datos, el problema es
# que solo compara respecto a las principales familias de 
# distribucion, pudiendo no ser lo mas exacto posible, sin embargo 
# como guia esta muy bien


descdist(c(na.omit(dfcrecimiento$lcu)), boot = 1000)
# nos indica que nuestra variable aleatoria tiene podria ser bien ajustada 
# respecto a una variable uniforme, normal o beta, (entre otras)
# sin embargo por la naturaleza del experimento y la forma en que fue 
# obtenido los datos, sabemos que no es uniforme ni beta

# probemos entonces las distribuciones para ver cual se ajusta mejor

d1 <- fitdist(c(na.omit(dfcrecimiento$lcu)), "norm")
plot(d1)

d2 <- fitdist(c(na.omit(dfcrecimiento$lcu)), "lnorm")
plot(d2)


d3 <- fitdist(c(na.omit(dfcrecimiento$lcu)), "gamma")
plot(d3)

# 
gofstat(list(d1,d2,d3))


# vemos que probablemente una distribucion normal se ajusta 
# bastante bien a nuestros datos



# Modelar el crecimiento  -------------------------------------------------

# Debido al diseño del experimento, se menciona que los tanques de agua
# no tienen exactamente las mismas condiciones, razon por la que se alteran 
# las condiciones del modelo lineal, es necesario ajustar para cada tanque de 
# agua su propio intercepto 
# Esto se podria lograr haciendo multiples regresiones para cada uno de 
# los tanques, pero esto tendria el problema de uqe para cada tanque tendriamos
# muy pocos datos en cada grupo.
# Otra solucion es crear un modelo mixto en donde se considere a cada tanque
# como un grupo que comparte cosas en comun con los demas grupos(las variables 
# explicativas) pero independientemente (cada uno con su propio intercepto)
# de esta forma se evalua cada tanque sin que interfiera los datos de los
# otros tanques pero conservando la informacion de las otras varibales que si 
# tienen en comun (temperatura y periodo de medicion)
# A esto se le llamara efecto aleatorio
# Lo mismo ocurre exactamente con la puesta, por lo que tenemos para este 
# experimento de forma teorica dos efectos aleatorios que estan **cruzados**,
# es decir cada tanque  tiene 3 posibles puestas, y cada puesta tiene 
# 36 posibles tanques.
# 
# Nos interesa saber como se comporta el crecimiento en funcion del tratamiento 
# y del tiempo, teniendo entonces estas dos como variables explicativas.
# 
# Con lo anterior el modelo teorico seria el siguiente.
# 

# modelo teorico 1 --------------------------------------------------------

mm1 <- lmer(lcu~tratamiento+medicion2+(1|tank)+(1|clutch), 
            data = dfcrecimiento, REML = T)

mm1
summary(mm1)

# vemos que la varianza del efecto aleatorio es muy pequeña. por lo que es 
# un indicio de que talvez en realidad no sean muy importantes estos efectos 
# aleatorios para el modelo, es decir, un modelo lineal lo podria explicar 
# casi igual.

plot(mm1)

Anova(mm1)
# Aqui observamos que tan confiables son nuestros coeficientes del modelo
# en ambos casos obtenemos que efectivamente son significantes y explican muy
# muy bien el crecimiento de las larvas.

qqPlot(residuals(mm1))
# vemos que el modelo se ajusta muy bien a los datos



# modelo teorico 2 --------------------------------------------------------

mm2 <- lmer(lcu~tratamiento+(1|tank)+(1|clutch), 
            data = dfcrecimiento, REML = T)

mm2
summary(mm1)
plot(mm2)
Anova(mm2)
qqPlot(residuals(mm2))
# vemos que el odelo unicamente en funcion del tiempo no es tan ajustado como
# el primero



# modelo teorico 3 --------------------------------------------------------

mm3 <- lmer(lcu~medicion2+(1|tank)+(1|clutch), 
            data = dfcrecimiento, REML = T)

mm3
summary(mm3)
plot(mm3)
Anova(mm3)
qqPlot(residuals(mm3))
# en este caso el modelo unicamente en funcion del tiempo es muy bueno, sin 
# embargo resulta obvio que el crecimiento aumenta con el tiempo, 
# por lo que talvez no aprta demasiada informacion,
# de igual manera vemos que practicamente los efectos aleatorios no aportan
# demasiada informacion al modelo.




# modelo 4 ----------------------------------------------------------------

mm4 <- lmer(lcu~tratamiento+medicion2+(1|tank), 
            data = dfcrecimiento, REML = T)

mm4
summary(mm4)
plot(mm4)
Anova(mm4)
qqPlot(residuals(mm2))
# Este no se ajusta a los datos



# modelo 5 ----------------------------------------------------------------


mm5 <- lmer(lcu~tratamiento+medicion2+(1|clutch), 
            data = dfcrecimiento, REML = T)

mm5
summary(mm5)
plot(mm5)
Anova(mm5)
qqPlot(residuals(mm5))
# presenta un ajuste mejor que el anterior, pero sin ser mejor que el primero
# teorico.
# 
# Con esto cubrimos las posibles combinaciones de las variables al construir 
# el modelo, claro esta sin tomar en cuenta posibles interacciones

# veamos el modelo lineal simple que deberia ser bueno

mlsimple <- lm(formula = lcu~tratamiento+medicion2, 
               data = dfcrecimiento)
summary(mlsimple)
AIC(mlsimple)
qqPlot(mlsimple)
# Efectivamente como lo sugieren los modelos anteriores un modelo lineal
# es suficiente para explicar de buena manera el crecimiento en funcion de
# la temperatura y del paso del tiempo(mediciones)



# comparar modelos --------------------------------------------------------

comparacion <- anova(mm1, mm2, mm3, mm4, mm5, mlsimple)
comparacion1 <- as.data.frame(comparacion)
comparacion1 %>% 
  arrange(AIC) %>% view()

# con esto vemos en orden descendente los modelos evaluados en su AIC, 
# Segun este criterio, los mas adecuados serias los modelos 1, 4, 3




# NOS QUEDAMOS ENTONCES  CON EL MODELO mm1



# Graficas ----------------------------------------------------------------

mm1

summary(mm1)
qqPlot(residuals(mm1))



# Efectos fijos
visreg(mm1, "tratamiento", type="contrast", ylab=expression(Delta*"lcu"), 
       points=list(col="#55555540", cex=0.25))
visreg(mm1, "medicion2", type="contrast", ylab=expression(Delta*"lcu"), 
       points=list(col="#55555540", cex=0.25))




# Efectos aleatorios
visreg(mm1, "medicion2", by="tank", re.form=~(1|tank)+(1|clutch), 
       ylab="#Longitud del cuerpo", layout=c(6,6))
visreg(mm1, "medicion2", by="clutch", re.form=~(1|tank)+(1|clutch),
       ylab="#Longitud del cuerpo")








# Graficos con ggeffects --------------------------------------------------

# Prediccion de valores
plot(ggeffect(mm1, terms = c("medicion2", "tratamiento")))+
  labs(x="Medición", y="Tratamiento")

# para cada mediciion o mejor dicho, periodo de tiempo, se puede  observar
# el valor estimado de la longitud del cuerpo de acuerdo a la temperatura




# Conclusion --------------------------------------------------------------


# Todos los modelos indican que los efectos aleatorios, tanque y puesta,
# no son de gran importancia para explicar el aumento en la longitud del
# cuerpo, incluso un modelo lineal sin considerar los efectos aleatorios 
# explica esto en funcion del tratamiento y medicion, es decir:
# UNICAMENTE LA TEMPERATURA Y EL PERIODO DE MEDICION EXPLICAN EL AUMENTO 
# EN EL CRECIMIENTO, INDEPENDIENTEMENTE DE LAS CARACTERISTICAS AMBIENTALES
# (TANQUE) O DE SUS CARACTERISTICAS GENETICAS (PUESTA)
# 


# -------------------------------------------------------------------------


