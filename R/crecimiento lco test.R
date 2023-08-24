
# Distribucion de probabilidad --------------------------------------------

# Es importante determinar la distribucion para poder elegir
# el modelo adecuado que explique la relacion entre nuetras variables

# exploracion visual
dfcrecimiento %>% 
  ggplot(aes(x=lco))+
  geom_histogram()


dfcrecimiento %>% 
  ggplot(aes(x=lco))+
  geom_density()

plot(density(na.omit(dfcrecimiento$lco)))
# no parece haber una clara distribucion conocida
# 

# graficas qqplot
# 
ggpubr::ggqqplot(dfcrecimiento$lco)
qqnorm(dfcrecimiento$lco)
# al menos la variable unicamente sola no indica tener una distribucion normal


ggplot(dfcrecimiento, aes(x = lco)) +
  geom_density() + facet_wrap(tratamiento~medicion2)
# al ser pocas observaciones, no se ve una tendencia clara






# test formales de normalidad
# 
shapiro.test(dfcrecimiento$lco)
# no es normal

ks.test(na.omit(dfcrecimiento$lco), "pnorm")
# no es normal la variable sola



# Mas pruebas de distribucion ---------------------------------------------

# se puede usar un modelo lineal simple para determinar si una distribucion
# es adecuada para una variable en funcion de otra variable 

ml1 <- lm(formula = lco~medicion2, data = dfcrecimiento)
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


descdist(c(na.omit(dfcrecimiento$lco)), boot = 500)
# nos indica que nuestra variable aleatoria tiene podria ser bien ajustada 
# respecto a una variable uniforme, normal o beta, (entre otras)
# sin embargo por la naturaleza del experimento y la forma en que fue 
# obtenido los datos, sabemos que no es uniforme ni beta

# probemos entonces las distribuciones para ver cual se ajusta mejor

d1 <- fitdist(c(na.omit(dfcrecimiento$lco)), "norm")
plot(d1)

d2 <- fitdist(c(na.omit(dfcrecimiento$lco)), "lnorm")
plot(d2)


d3 <- fitdist(c(na.omit(dfcrecimiento$lco)), "gamma")
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

mm1 <- lmer(lco~tratamiento+medicion2+(1|tank)+(1|clutch), 
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

mm2 <- lmer(lco~tratamiento+(1|tank)+(1|clutch), 
            data = dfcrecimiento, REML = T)

mm2
summary(mm1)
plot(mm2)
Anova(mm2)
qqPlot(residuals(mm2))
# vemos que el odelo unicamente en funcion del tiempo no es tan ajustado como
# el primero



# modelo teorico 3 --------------------------------------------------------

mm3 <- lmer(lco~medicion2+(1|tank)+(1|clutch), 
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

mm4 <- lmer(lco~tratamiento+medicion2+(1|tank), 
            data = dfcrecimiento, REML = T)

mm4
summary(mm4)
plot(mm4)
Anova(mm4)
qqPlot(residuals(mm2))
# Este no se ajusta a los datos



# modelo 5 ----------------------------------------------------------------


mm5 <- lmer(lco~tratamiento+medicion2+(1|clutch), 
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

mlsimple <- lm(formula = lco~tratamiento+medicion2, 
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




# Graficas ----------------------------------------------------------------

summary(mm1)
qqPlot(residuals(mm1))




# Efectos fijos
visreg(mm1, "tratamiento", type="contrast", ylab=expression(Delta*"lco"), 
       points=list(col="#55555540", cex=0.25))
visreg(mm1, "medicion2", type="contrast", ylab=expression(Delta*"lco"), 
       points=list(col="#55555540", cex=0.25))




# Efectos aleatorios
visreg(mm1, "medicion2", by="tank", re.form=~(1|tank)+(1|clutch), 
       ylab="#Longitud del cuerpo", layout=c(6,6))
visreg(mm1, "medicion2", by="clutch", re.form=~(1|tank)+(1|clutch),
       ylab="#Longitud del cuerpo")



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


