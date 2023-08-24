
plot(fitdist(dfdesarrollo$dmetamorf, distr = "lnorm"))

plot(fitdist(dfdesarrollo$dmetamorf, distr = "geom"))

plot(fitdist(dfdesarrollo$dmetamorf, distr = "logis"))

plot(fitdist(dfdesarrollo$dmetamorf, distr = "cauchy"))

normal1 <- fitdist(dfdesarrollo$dmetamorf, "norm") 
plot(normal1)





# pruebas de interaacion variables ----------------------------------------



library(mise)
library(readr)
crecimiento_v2_2 <- read_csv("crecimiento_v2.2.csv")
View(crecimiento_v2_2)

mise(vars = TRUE, figs = TRUE, console = TRUE)
options (scipen = 999)
setwd("C:/R/rtest_n")
list.files()
grow <- read.csv("crecimiento_v2.2.csv")
attach(grow)
library(lmerTest)
library(car)

library(emmeans)
library(broom.mixed)
library(CRAN)
library(tidyverse)
###analisis de crecimiento
###largo del cuerpo
mod_lcu1 <- lmer(lcu~tratamiento+medicion2+(1|clutch:tank))   ###hipotesis nula
Anova(mod_lcu1)

summary(mod_lcu1)


mod_lcu <- lmer(lcu~tratamiento*medicion2+(1|clutch:tank))   ###interaccion
Anova(mod_lcu)
summary(mod_lcu)