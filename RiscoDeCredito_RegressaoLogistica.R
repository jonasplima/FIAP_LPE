#### Instala/Carrega os packages que serao usados no script ####
# Instala pacote
install.packages("readxl")
install.packages("plotly") 
install.packages("fdth")

# Le pacote
library(readxl)
library(plotly)
library(fdth)

####################################################################################
#### Carrega arquivo inicial com os dados que serão utilizados no modelo ####

risco_credito <- read_excel("Dados/risco_credito.xlsx")

# Valida os dados do arquivo
View(risco_credito)

# Valida as variaveis
attach(risco_credito)
names(risco_credito)
str(risco_credito)

# Transformando variaveis
risco_credito$gender <- as.factor(risco_credito$gender)
risco_credito$marital <- as.factor(risco_credito$marital)
risco_credito$howpaid <- as.factor(risco_credito$howpaid)
risco_credito$mortgage <- as.factor(risco_credito$mortgage)
risco_credito$bad_debt <- as.factor(risco_credito$bad_debt)
risco_credito$goodrisk <- as.factor(risco_credito$goodrisk) 

####################################################################################
#### Inicia o processo de análise exploratória ####

#### Grafico de pizza para variável dependente ####
# Separa os dados
grafico_pie <- table(risco_credito$target_goodrisk)
# Tabula os dados
grafico_pie <-as.data.frame(grafico_pie)
grafico_pie
#Var1 Freq
#1    0 3313
#2    1 1607
# Efetua analise visual dos dados
grafico_pie2 <- plot_ly(grafico_pie,
                        labels = ~Var1,
                        values = ~Freq,
                        type = 'pie')%>%
                        layout(title = "Graficos Risco de Credito")
grafico_pie2

# A base possui um total de 4290 registros onde estão divididos 
# 3313, 67,3% como "arriscado" e 1607 ou 32,7% da base como
# "baixo risco"

#### Analise de distribuicao para variaveis indepentendes ####

par (mfrow=c(1,1))

# Para idade será necessário criar faixas de idade, para isso será usado a formula de sturges
plot(age, ylab="left",xlab="salary",col=c('red','darkgreen'))

# Utilizando a formula de Sturges foram localizados 
classe=fdt(x=age,  
           start=min(age),
           end=max(age),
           breaks="Sturges")


plot(as.factor(gender), left,ylab="left",xlab="number_project",col=c('red','darkgreen'))
plot(as.factor(marital), left,ylab="left",xlab="work_accident",col=c('red','darkgreen'))
plot(numkids, left,ylab="left",xlab="sales",col=c('red','darkgreen'))
plot(as.factor(numcards), left,ylab="left",xlab="promotion_last_5years",col=c('red','darkgreen'))
plot(as.factor(howpaid), left,ylab="left",xlab="time_spend_company",col=c('red','darkgreen'))




