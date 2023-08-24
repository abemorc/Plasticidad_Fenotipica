#TEORIA: https://youtu.be/cDksCOmGFio

#PAQUETES NECESARIOS:

install.packages("mise")
install.packages("ggplot2")
install.packages("fitdistrplus")
install.packages("glmmTMB")
install.packages("car")
install.packages("ggeffects")
install.packages("effects")

# DIRECTORIO DE TRABAJO:

#LABEC
setwd("C:/Users/labec/OneDrive - Instituto Politecnico Nacional/000_ECA/Hermes/ispic")
#OMEN
setwd("D:/01-DOCUMENTOS/0- CIIDIR/2-PhD/OneDrive - Instituto Politecnico Nacional/000_ECA/Hermes/ispic")
#IMac
setwd("/Users/usuario/Documents/ispic")


#Apagar la notación científica:
options (scipen = 999)

###     Limpiamos todo
library(mise)
mise(vars = TRUE, figs = TRUE, console = TRUE)


list.files()

ispic <- read.csv("ispic_data4.csv")
attach(ispic)

summary(ispic)


#######################################
####                               ####
####    GRAFICOS EXPLORATORIOS     ####
####                               ####
#######################################

library(ggplot2)

# Gráfico de densidades:

#Pérdida por sitios

ggplot(ispic, aes(x=perdida2, fill=sitio))+
  geom_density(alpha=0.3, colour="black")+
  labs(x="Pérdida hídrica", y="Densidad")+
  theme_bw()


#Perdida de Sitios*Refugios

sitio.refugios=paste(sitio,refugios2,sep=".")

ggplot(ispic, aes(x=perdida2, fill=sitio.refugios))+
  geom_density(alpha=0.3, colour="black")+
  labs(x="Pérdida hídrica", y="Densidad")+
  theme_bw()


# Boxplot

ggplot(ispic, aes(x=reorder(sitio.refugios,-perdida2, mean), y=perdida2, fill=sitio.refugios))+
  geom_boxplot(outlier.shape = 1)+
  scale_y_continuous(limits = quantile(perdida2, c(0,0.9)))+
  theme_bw()+
  labs(x="Sitios * Refugios", y="Pérdida hídrica")+
  theme(axis.text = element_text(angle = 90), legend.position = "none")

#############################
####                     ####
####    DISTRIBUCION     ####
####                     ####
#############################

#https://rpubs.com/aafernandez1976/fitdistrplus

library(fitdistrplus)


# Observar la distribución empirica:

plotdist(perdida2, histo = T, demp = T)


descdist(perdida2)

#Si el valor de skewness es diferente de cero indica que carece de simetría la distribución empírica,
#y la kurtosis cuantifica el peso de los extremos, ya que un valor de 3 es la kurtosis de una distribución normal.

# MODELAR VARIAS DISTRIBUCIONES Y COMPARARLAS

#NORMAL


norm_perdida <- fitdist(perdida2, "norm")
plot(norm_perdida)

#BETA

beta_perdida <-fitdist(perdida2, "beta")
plot(beta_perdida)

#WILBUR

weibull_perdida <- fitdist(perdida2, "weibull")
plot(weibull_perdida)

#CAUCHY 

cauchy_perdida <- fitdist(perdida2, "cauchy") 
plot(cauchy_perdida)

#GAMMA
gamma_perdida <- fitdist(perdida2, "gamma") 
plot(gamma_perdida)

#COMPARACION

cdfcomp(list(norm_perdida,beta_perdida,weibull_perdida,cauchy_perdida,gamma_perdida))

#Gráfico de quantiles

qqcomp(list(norm_perdida,beta_perdida,weibull_perdida,cauchy_perdida))


#Empleando AIC


gofstat(list(norm_perdida,beta_perdida,weibull_perdida, cauchy_perdida,gamma_perdida))


  
#############################
####                     ####
####       MODELADO      ####
####                     ####
#############################

# EFECTOS FIJOS: Sitio*Refugios
# EFECTOS ALEATORIOS: individuo2

# Los valores de pérdida hídrica de cada individuo están "ordenados" en el tiempo
# (no son independientes).


library(glmmTMB)
library(car)

mod1 <- glmmTMB(perdida2~sitio+(1|individuo2), family = beta_family(link = "logit"), data = ispic)
summary(mod1)

mod2<- glmmTMB(perdida2~refugios2+(1|individuo2)+(1|momento), family = beta_family(link = "logit"), data = ispic)
summary(mod2)

mod3 <- glmmTMB(perdida2~sitio+refugios2+(1|individuo2), family = beta_family(link = "logit"), data = ispic)
summary(mod3)

mod4 <- glmmTMB(perdida2~sitio*refugios2+sexo +(1|individuo2/tiempo), family = beta_family(link = "logit"), data = ispic)
summary(mod4)

mod5 <- glmmTMB(perdida2~sitio+refugios2+sexo+(1|individuo2/momento), family = beta_family(link = "logit"), data = ispic)
summary(mod5)

mod55 <- glmmTMB(perdida2~sitio+refugios2+sexo+(1|individuo2)+(1|momento), family = beta_family(link = "logit"), data = ispic)
summary(mod55)

mod555 <- glmmTMB(perdida2~sitio+refugios2+sexo+(1|individuo2), family = beta_family(link = "logit"), data = ispic)
summary(mod555)

mod6 <- glmmTMB(perdida2~sitio*sexo+(1|individuo2), family = beta_family(link = "logit"), data = ispic)
summary(mod6)

mod7 <- glmmTMB(perdida2~sitio+sexo+(1|individuo2/momento), family = beta_family(link = "logit"), data = ispic)
summary(mod7)

mod77 <- glmmTMB(perdida2~sitio+sexo+(1|individuo2)+(1|momento), family = beta_family(link = "logit"), data = ispic)
summary(mod77)

mod8 <- glmmTMB(perdida2~sitio+sexo+(1|individuo2), family = beta_family(link = "logit"), data = ispic)
summary(mod8)

mod9 <- glmmTMB(perdida2~refugios2*sexo+sitio+(1|individuo2), family = beta_family(link = "logit"), data = ispic)
summary(mod9)

mod10 <- glmmTMB(perdida2~refugios2*sexo+refugios2*sitio+sitio*sexo+sitio+sexo+refugios2+(1|individuo2)+(1|momento), family = beta_family(link = "logit"), data = ispic)
summary(mod10)

mod11 <- glmmTMB(perdida2~sitio+sexo+(1|individuo2)+(1|tiempo), family = beta_family(link = "logit"), data = ispic)
summary(mod11)

mod12 <- glmmTMB(perdida2~sitio+sexo+refugios2+(1|individuo2)+(1|tiempo), family = beta_family(link = "logit"), data = ispic)
summary(mod12)

mod13 <- glmmTMB(perdida3~sitio+sexo+(1|individuo2)+(1|tiempo), family = beta_family(link = "logit"), data = ispic)
summary(mod13)



# Comparar modelos: https://youtu.be/FPRRP3U7YIk

anova(mod1,mod2,mod3,mod4, mod5, mod6, mod7, mod8,mod55,mod555,mod77,mod9,mod10,mod11,mod12,mod13)

################

#############################
####                     ####
####  GRAFICOS FINALES   ####
####                     ####
#############################


#https://www.sscc.wisc.edu/sscc/pubs/Rmisc/margins.html

library(ggeffects)

graf_mod11 <- ggeffect(mod11, terms = c("sitio","sexo"))



plot(graf_mod11)+
  theme_bw()+
  labs(y="Pérdida hídrica", x="Sitio")+
  theme(plot.title = element_blank())

#gráfica para que incluya a los refugios 
graf_mod12 <- ggeffect(mod12, terms = c("refugios2","sexo","sitio"))


plot(graf_mod12)+
  theme_bw()+
  labs(y="Pérdida hídrica", x="Refugios")+
  theme(plot.title = element_blank())


###########


