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

data$log_gdp_pc <- log(data$gdp_percap)
data$log_gcf <- log(data$gcf)
data$ihk <- data$index_human_K


dados <- data
dados$gdp_percap = NULL
dados$gcf = NULL
dados$index_human_K = NULL
dados$POP = NULL
rownames(dados) <- dados$Data


################################################################################


################################### DIFERENÇA ##################################

diffdados = as.data.frame(diff(as.matrix(dados), lag = 1))

################################################################################


################################# UNIT ROOT TEST ###############################

adf.test(dados$log_gdp_pc)# É Estacionario neste nivel
adf.test(dados$ihk)
adf.test(dados$democrat_index)
adf.test(dados$log_gcf)# É Estacionario neste nivel
adf.test(dados$tfp)
adf.test(diffdados$ihk)# É Estacionario neste nivel
adf.test(diffdados$democrat_index)# É Estacionario neste nivel
adf.test(diffdados$tfp)# É Estacionario neste nivel


pp.test(dados$log_gdp_pc,type = 'Z_tau')
pp.test(dados$ihk,type = 'Z_tau')
pp.test(dados$democrat_index,type = 'Z_tau')
pp.test(dados$tfp,type = 'Z_tau')
pp.test(dados$log_gcf,type = 'Z_tau')
pp.test(diffdados$ihk,type = 'Z_tau')# É Estacionario neste nivel
pp.test(diffdados$democrat_index,type = 'Z_tau')# É Estacionario neste nivel
pp.test(diffdados$log_gcf,type = 'Z_tau')# É Estacionario neste nivel
pp.test(diffdados$log_gdp_pc,type = 'Z_tau')# É Estacionario neste nivel
pp.test(diffdados$tfp,type = 'Z_tau')# É Estacionario neste nivel


kpss.test(dados$log_gcf)# É Estacionario neste nivel
kpss.test(dados$log_gdp_pc)# É Estacionario neste nivel

##Após Utilizarmos o Kpss Teste chegamos a conclusão de que a variavel "log_gcf"
## e "log_gdp_pc" são estacionarias em seu niveis original, já que a hipotes 
## alternativa (Não estacionaria) não é verdadeira. 


################################################################################


#################################### MODELO ####################################
#Filtro para igualar tamanho das variaveis
datafr <- subset(dados , dados$Data > 1969)
difdata<- subset(diffdados, rownames(diffdados) > 1969)
##

modelo = lm(datafr$log_gdp_pc ~  Lag(datafr$log_gcf,shift = 1) + difdata$tfp + difdata$ihk +  difdata$democrat_index )

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

ggplot() + geom_line(data = dados, aes(Data,ihk)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

ggplot() + geom_line(data = dados, aes(Data,democrat_index)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


ggplot() + geom_line(data = dados, aes(Data,log_gcf)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


ggplot() + geom_line(data = dados, aes(Data,tfp)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 



ggplot() + geom_line(data = dados, aes(Data,log_gdp_pc)) +
  ylab('')+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

