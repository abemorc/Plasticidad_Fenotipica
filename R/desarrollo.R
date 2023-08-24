
# Analisis del efecto de la temperatura en el desarollo de larvas



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

#######




# importar datos
dfdesarrollo <- read_csv(here("Data/desarrollo_v2.2.csv"), 
                         col_types = cols(
                           lcu = col_number(), 
                           fecha = col_date(format = "%d/%m/%Y")))

str(dfdesarrollo)
view(dfdesarrollo)



dfdesarrollo <- dfdesarrollo %>% 
  mutate(across(c(posicion, tank, tratamiento, pool, clutch,
                  id), .fns = as.factor))

#######




# Objetivo ----------------------------------------------------------------

# Evaluar el efecto de la temperatura en el desarrollo de laslarvas

# las variables a estudiar son:
# smetamor
# dmetamorf




# Analisis exploratorio --------------------------------------------------


# La mayor parte del analisis exploratorio de las variables se realiza
# en el sxript de CRECIMIENTO, dado que son los  mismo datos

view(dfdesarrollo)
summary(dfdesarrollo)


dfdesarrollo$smetamorf %>% 
  summary()
# vemos que el desarrollo se dio en un tiemp de entre  2 y 4 semanas

dfdesarrollo$dmetamorf %>% 
  summary()
# el tiempo en dias fue minimo 13 y maximo 38


dfdesarrollo %>% 
  group_by(tank) %>% 
  summarise(n=n())

# son las mismas variables
dfdesarrollo %>% 
  group_by(pool) %>% 
  summarise(n=n())
dfdesarrollo %>% 
  group_by(clutch) %>% 
  summarise(n=n())


dfdesarrollo %>% 
  ggplot(aes(x= tratamiento, y=dmetamorf)) + 
  geom_boxplot()
# visualmente se ve que a mayor temperatura, el tiempo de desarrollo
# es menor

# teoricamente deberia ser una distribucion exponencial de la variable tiempo


####################
# vamos a explorar la variable de interes dmetamorf

df3 <- dfdesarrollo %>% 
  group_by(clutch, tank) %>% 
  summarise(mean=mean(dmetamorf, na.rm=T))


df3 %>%
  ggplot( aes(x=tank, y=mean, group=clutch, color=clutch)) +
  geom_line()
# Se observa que el promedio de dias que tardan en alcanzar el ultimo
# estadio  es  constante a traves de los tanques
# es decir, los tanques estaban sometidos a diferentes condiciones
# y aun asi independientemente de la puesta, el conteo de dias es 
# similar  en las tres puestas






# Distribucion de probabilidad --------------------------------------------

# exploracion visual



hist(dfdesarrollo$dmetamorf, breaks = 10)
plot(density(dfdesarrollo$dmetamorf))

plot(dfdesarrollo$dmetamorf)

hist(dfdesarrollo$smetamorf)
plot(density(dfdesarrollo$smetamorf))


# graficas qqplot
# 
ggpubr::ggqqplot(dfdesarrollo$dmetamorf)
qqnorm(dfdesarrollo$dmetamorf)







# test formales de normalidad
# 
shapiro.test(dfdesarrollo$dmetamorf)
# no es normal

ks.test(na.omit(dfdesarrollo$dmetamorf), "pnorm")
# no es normal la variable sola



# veamos como se  comportan los  residuales en un modelo  asumiendo 
# la normalidad

modelo_lineal <- dfdesarrollo %>% 
  lm(formula=dmetamorf~tratamiento)

summary(modelo_lineal)
plot(modelo_lineal)


# parece que un modelo lineal explica relativamente bien la 
# relacion entre ambas varibles esto sin contar los efectos aleatorios




# otras pruebas de distribucion

descdist(dfdesarrollo$dmetamorf, boot = 150)


plotdist(dfdesarrollo$dmetamorf, demp = TRUE)
# vemos que los datos se podrian ajustar a una distribucion tanto uniforme 
# como normal, la distribucion beta no aplica en este caso y no es correcto 
# reescalar esta variable a un rango 0,1
# 


plot(fitdist(dfdesarrollo$dmetamorf, distr="unif", method="mme"))


# ver distribucion de poisson
plot(fitdist(dfdesarrollo$dmetamorf, distr="pois", method="mme"))


# probemos transformando la variable


logdias <- log(dfdesarrollo$dmetamorf)

descdist(logdias, boot = 150)


# 
# Distribucion normal
normal_test <- dfdesarrollo$dmetamorf %>% 
  fitdist(distr = "norm") 
plot(normal_test)


exp_test <- dfdesarrollo$dmetamorf %>% 
  fitdist(distr = "exp") 
plot(exp_test)

lnorm_test <- dfdesarrollo$dmetamorf %>% 
  fitdist(distr = "lnorm") 
plot(lnorm_test)

geom_test <- dfdesarrollo$dmetamorf %>% 
  fitdist(distr = "geom") 
plot(geom_test)

cauchy_test <- dfdesarrollo$dmetamorf %>% 
  fitdist(distr = "cauchy") 
plot(cauchy_test)


weibull_test <- dfdesarrollo$dmetamorf %>% 
  fitdist(distr = "weibull") 
plot(weibull_test)



# "comparacion"

gofstat(list(normal_test,weibull_test, cauchy_test, geom_test,lnorm_test,
             exp_test))


# La distribucion normal presenta un ajuste aceptable  basado en las 
# pruebas








# Modelado del desarrollo  -------------------------------------------------

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
# Nos interesa saber como se comporta el desarrollo en funcion del tratamiento 
# y del tiempo, teniendo entonces estas dos como variables explicativas.
# 
# Con lo anterior el modelo teorico seria el siguiente.
# 

# modelo teorico 1 --------------------------------------------------------


md1 <- lmer(data = dfdesarrollo, 
           formula = dmetamorf~tratamiento + (1|tank))

summary(md1)

# vemos que la varianza del efecto aleatorio no es muy grande por lo que es 
# un indicio de que talvez en realidad no sean muy importantes estos efectos 
# aleatorios para el modelo

plot(md1)

Anova(md1)
# Vemos que el valor del tratamiento si es significante (<0.05)
qqPlot(residuals(md1))
# vemos que el modelo se ajusta relativamente bien



# modelo teorico 2 --------------------------------------------------------

md2 <- lmer(data = dfdesarrollo, 
            formula = dmetamorf~tratamiento + (1|tank) + (1|clutch),
            REML = T)

summary(md2)

# vemos que la varianza del efecto aleatorio no es muy grande por lo que es 
# un indicio de que talvez en realidad no sean muy importantes estos efectos 
# aleatorios para el modelo

plot(md2)

Anova(md2)
# Vemos que el valor del tratamiento si es significante (<0.05)
qqPlot(residuals(md2))
# vemos que el modelo se ajusta relativamente bien







# modelo teorico 3 --------------------------------------------------------

# NOTA;;
# ESTA PARTE  AUN NO LA COMPLETO

# 
# md3 <- lmer(data = dfdesarrollo, 
#             formula = dmetamorf~tratamiento + (1|tank) + (1|clutch) + (1|id),
#             REML = T)
# 
# summary(md2)
# 
# # vemos que la varianza del efecto aleatorio no es muy grande por lo que es 
# # un indicio de que talvez en realidad no sean muy importantes estos efectos 
# # aleatorios para el modelo
# 
# plot(md2)
# 
# Anova(md2)
# # Vemos que el valor del tratamiento si es significante (<0.05)
# qqPlot(residuals(md2))
# # vemos que el modelo se ajusta relativamente bien
# 
# 
# 

# Modelo lineal -----------------------------------------------------------


md_lineal <- lm(dmetamorf~tratamiento, data = dfdesarrollo)
summary(md_lineal)
plot(md_lineal)





# Comparacion del modelo --------------------------------------------------



comparacion2 <- anova(md1, md2, md_lineal)
comparacion12 <- as.data.frame(comparacion2)
comparacion12 %>% 
  arrange(AIC) %>% view()




# Elmodelo md1, y md2 presentan practicamente el mismo aic







# Graficas ----------------------------------------------------------------

md1

summary(md1)
qqPlot(residuals(md1))



# Efectos fijos
visreg(md1, "tratamiento", type="contrast", ylab=expression(Delta*"dmetamor"), 
       points=list(col="#55555540", cex=0.25))




# Efectos aleatorios
visreg(mm1, "tratamiento", by="tank", re.form=~(1|tank)+(1|clutch), 
       ylab="Desarrollo")
visreg(mm1, "tratamiento", by="clutch", re.form=~(1|tank)+(1|clutch),
       ylab="Desarrollo")







# Graficos con ggeffects --------------------------------------------------

# Prediccion de valores
plot(ggeffect(md1, terms = c("tratamiento")))

# para cada tratamiento, se puede observar el tiempo estimado en dias para
# alcanzar  el  desarrrollo completo



# Conclusion --------------------------------------------------------------


# Todos los modelos indican que los efectos aleatorios, tanque y puesta,
# no son de gran importancia para explicar eL TIEMPO DE  DESARROLO incluso 
# un modelo lineal sin considerar los efectos aleatorios 
# explica esto en funcion del tratamiento, es decir:
# UNICAMENTE LA TEMPERATURA EXPLICA EL DESARROLLO APRESURADO DE LAS LARVAS

# -------------------------------------------------------------------------



# NOTA:
# PROBABLEMENTE ES NECCESARIO REVISAR NUEVAMENTE LA DISTRIBUCION DEL DESARROLLO
# YA QUE ESTA ES EN REALIDAD UNA VARIABLE DE TIEMPO, ES DECIR, EL NUMERO DE DIAS 
# EN ALCANZAR CIERTO ESTADO FISICO.

