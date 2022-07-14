################################# CABEÇALHO ####################################

#Desenvolvido por Carlos Magno de Jesus Amora
#Estudante de Economia da Uff
#O codigo foi desenvolvido para obter resultados para o TCC do mesmo

################################################################################
#Determine o espaço de trabalho especifico de sua maquina.
#Mantenha em mente que o arquico excel deverá está no espaço determinado 
#Exemplo : setwd("C:/Users/")

require(car)
require(readxl)
require(aTSA)
require(EnvStats)
require(tseries)
require(lmtest)
require(sandwich)
require(skedastic)
require(Hmisc)
require(ggplot2)
require(scales)
require(broom)
require(bayesforecast)
require(dynamac)


############################### CHAMANDO DADOS #################################

data <- read_xlsx('dados.xlsx')

data$log_gdp_pc <- log(data$gdp_percap)
data$log_gcf <- log(data$gcf)
data$ihk <- data$index_human_K

dados <- data

rownames(dados) <- dados$Data

################################################################################


###################################DF DIFERENÇA ##################################

diffdados = as.data.frame(diff(as.matrix(dados), lag = 1))

################################################################################


################################# UNIT ROOT TEST ###############################

aTSA::adf.test(dados$log_gdp_pc)# É Estacionario neste nivel
aTSA::adf.test(dados$ihk)
aTSA::adf.test(dados$democrat_index)
aTSA::adf.test(dados$log_gcf)# É Estacionario neste nivel
aTSA::adf.test(dados$tfp)
aTSA::adf.test(diffdados$ihk)# É Estacionario neste nivel
aTSA::adf.test(diffdados$democrat_index)# É Estacionario neste nivel
aTSA::adf.test(diffdados$tfp)# É Estacionario neste nivel
aTSA::adf.test(diffdados$log_gdp_pc)# É Estacionario neste nivel
aTSA::adf.test(diffdados$log_gcf)# É Estacionario neste nivel


aTSA::pp.test(dados$log_gdp_pc,type = 'Z_tau')
aTSA::pp.test(dados$ihk,type = 'Z_tau')
aTSA::pp.test(dados$democrat_index,type = 'Z_tau')
aTSA::pp.test(dados$tfp,type = 'Z_tau')
aTSA::pp.test(dados$log_gcf,type = 'Z_tau')
aTSA::pp.test(diffdados$ihk,type = 'Z_tau')# É Estacionario neste nivel
aTSA::pp.test(diffdados$democrat_index,type = 'Z_tau')# É Estacionario neste nivel
aTSA::pp.test(diffdados$log_gcf,type = 'Z_tau')# É Estacionario neste nivel
aTSA::pp.test(diffdados$log_gdp_pc,type = 'Z_tau')# É Estacionario neste nivel
aTSA::pp.test(diffdados$tfp,type = 'Z_tau')# É Estacionario neste nivel

################################################################################


#################################### MODELO ####################################
#Filtro para igualar tamanho das variaveis
datafr <- subset(dados , dados$Data > 1969)
difdata<- subset(diffdados, rownames(diffdados) > 1969)
##

modelo = lm(difdata$log_gdp_pc ~  difdata$log_gcf + difdata$tfp +
              difdata$ihk +  difdata$democrat_index )

summary(modelo)

##########################NORMALIDADE RESIDOS##############################

shapiro.test(modelo$residuals) ## 0.76 no p-valor logo temos normalidade nos residos do modelo
####################################

###########VIF PARA MULTICOLINEARIDADE############

vif(modelo)

######################MEDIA RESIDOS#####################################
mean(modelo$residuals)###8.115e-19 logo é proximo de zero.

############################ Correlograma ##################################

c <- difdata
c$Data<- NULL
c$index_human_K<-NULL
c$gcf<-NULL
c$gdp_percap<-NULL
cor(c,use = "complete.obs")


################################################################################

################### Breusch-Godfrey test (autocorrelação) ######################

bgtest(modelo, type = 'F')
#bgtest(modelo,type = 'F',order = 2)

################################################################################

######################### TESTE DE HETEROCEDASTICIDADE #########################

bptest(modelo)

################################################################################

######################### TESTE DE LINEARIDADE#############################

harvtest(modelo)

######################### CONSERTANDO MODELO COM HC ###########################

coeftest(modelo, vcov. = vcovHC.default(modelo,type = "HC1"))

################################# GRAFICOS #####################################

ggplot() + geom_line(data = dados, aes(Data,ihk),size=2) +
  ylab('Índice Capita Humano')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() + xlab("ANO") + 
  theme(axis.line = element_line(colour = "black"),text = element_text(size=20),
        panel.grid.major = element_line(colour="black"),
        panel.grid.minor = element_line(colour="black"),
        panel.border = element_blank(),
        panel.background = element_blank()) 

ggplot() + geom_line(data = dados, aes(Data,democrat_index),size=2) +
  ylab('Índice democratico')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() + xlab("ANO") +
  theme(axis.line = element_line(colour = "black"),text = element_text(size=20),
        panel.grid.major = element_line(colour="black"),
        panel.grid.minor = element_line(colour="black"),
        panel.border = element_blank(),
        panel.background = element_blank()) 


ggplot() + geom_line(data = dados, aes(Data,log_gcf),size=2) +
  ylab('Log Formação Bruta de capita')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() + xlab("ANO") +
  theme(axis.line = element_line(colour = "black"),text = element_text(size=20),
        panel.grid.major = element_line(colour="black"),
        panel.grid.minor = element_line(colour="black"),
        panel.border = element_blank(),
        panel.background = element_blank()) 


ggplot() + geom_line(data = dados, aes(Data,tfp),size=2) +
  ylab('Produtividade Total dos Fatores')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() + xlab("ANO") +
  theme(axis.line = element_line(colour = "black"),text = element_text(size=20),
        panel.grid.major = element_line(colour="black"),
        panel.grid.minor = element_line(colour="black"),
        panel.border = element_blank(),
        panel.background = element_blank()) 

ggplot() + geom_line(data = dados, aes(Data,log_gdp_pc),size=2) +
  ylab('Log PIB per Capita')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() + xlab("ANO") +
  theme(axis.line = element_line(colour = "black"),text = element_text(size=20),
        panel.grid.major = element_line(colour="black"),
        panel.grid.minor = element_line(colour="black"),
        panel.border = element_blank(),
        panel.background = element_blank()) 
