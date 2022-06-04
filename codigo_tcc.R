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
require(lmtest)
require(sandwich)
require(skedastic)
require(Hmisc)
require(ggplot2)
require(scales)
require(broom)


############################### CHAMANDO DADOS #################################

data <- read_xlsx('dados.xlsx')

data$log_gdp <- log(data$gdp_percapita)
data$log_gcf <- log(data$gcf)


dados <- data
dados$gdp = NULL
dados$gcf = NULL
rownames(dados) <- dados$Data



################################################################################


################################### DIFERENÇA ##################################

diffdados = as.data.frame(diff(as.matrix(dados), lag = 1))

################################################################################


################################# UNIT ROOT TEST ###############################

adf.test(dados$log_gdp)
adf.test(dados$index_human_K)
adf.test(dados$democrat_index)
adf.test(dados$log_gcf)
adf.test(dados$tfp)
adf.test(diffdados$index_human_K)
adf.test(diffdados$democrat_index)
adf.test(diffdados$tfp)


pp.test(dados$log_gdp,type = 'Z_tau')
pp.test(dados$index_human_K,type = 'Z_tau')
pp.test(dados$democrat_index,type = 'Z_tau')
pp.test(dados$tfp,type = 'Z_tau')
pp.test(dados$log_gcf,type = 'Z_tau')
pp.test(diffdados$index_human_K,type = 'Z_tau')
pp.test(diffdados$democrat_index,type = 'Z_tau')
pp.test(diffdados$log_gcf,type = 'Z_tau')
pp.test(diffdados$tfp,type = 'Z_tau')


kpss.test(dados$log_gcf)


################################################################################


#################################### MODELO ####################################

datafr <- subset(dados , dados$Data > 1969)
difdata<- subset(diffdados, rownames(diffdados) > 1969)

modelo = lm(datafr$log_gdp ~  Lag(datafr$log_gcf,shift = 1) + difdata$tfp + difdata$index_human_K +  difdata$democrat_index )

summary(modelo)

################################################################################


############################ Multicolinearidade teste ##############################

vif(modelo)

################################################################################


################### Breusch-Godfrey test (autocorrelação) ######################

bgtest(modelo, type = 'F')
#bgtest(modelo,type = 'F',order = 2)

################################################################################


######################### TESTE DE HETEROCEDASTICIDADE #########################

bptest(modelo)

################################################################################


######################### CONSERTANDO MODELO COM HAC ###########################

coeftest(modelo, vcov. = vcovHAC.default(modelo))

################################################################################


################################# GRAFICOS #####################################

ggplot() + geom_line(data = datafr, aes(Data,index_human_K)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

ggplot() + geom_line(data = datafr, aes(Data,democrat_index)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


ggplot() + geom_line(data = datafr, aes(Data,log_gcf)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


ggplot() + geom_line(data = datafr, aes(Data,tfp)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 



ggplot() + geom_line(data = datafr, aes(Data,log_gdp)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
