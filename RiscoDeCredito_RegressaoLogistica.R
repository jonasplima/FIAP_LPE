#### Instala/Carrega os packages que serao usados no script ####
# Instala pacote
install.packages("readxl")
install.packages("plotly") 
install.packages("fdth")
install.packages("glm2") 
install.packages("rpart") 
install.packages("rpart.plot") 

# Le pacote
library(readxl)
library(plotly)
library(fdth)
library(glm2)
library(rpart) 
library(rpart.plot) 

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
risco_credito$numkids <- as.factor(risco_credito$numkids) 
risco_credito$numcards <- as.factor(risco_credito$numcards)  
risco_credito$storecar <- as.factor(risco_credito$storecar) 
risco_credito$loans <- as.factor(risco_credito$loans)   

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
plot_ly(x = risco_credito$age, type = "histogram")

# Utilizando a formula de Sturges para gerar 5 agrupamentos de idade
classe=fdt(x=risco_credito$age,
           k=5,
           breaks="Sturges") 

# Usando cut para criar a faixa de agrupamento de idade
risco_credito$faixa_idade <- cut(risco_credito$age
                                  , breaks = c(17.82,24.36,30.89,37.43,43.96,50.5)
                                  , right = T
                                  , labels = c("[17.82,24.36)", '[24.36,30.89)','[30.89,37.43)','[37.43,43.96)','[43.96,50.5)'))

plot_ly(x = risco_credito$faixa_idade, type = "histogram")
# Para salário também será necessário criar faixas 
plot_ly(x = risco_credito$income, type = "histogram")

# Utilizando a formula de Sturges para agrupamentos de salário
classe_salario=fdt(x=risco_credito$income, 
                   breaks="Sturges") 

# Usando cut para criar a faixa de agrupamento de idade
risco_credito$faixa_salario <- cut(risco_credito$income
                                 , breaks = c(14854.95,18118.414,21381.877,24645.341,27908.804,31172.268,34435.731,37699.195,40962.659,44226.122,47489.586,50753.049,54016.513,57279.976,60543.44)
                                 , right = T
                                 , labels = c("[14854.95,18118.414)", '[18118.414,21381.877)','[21381.877,24645.341)','[24645.341,27908.804)','[27908.804,31172.268)',
                                              "[31172.268,34435.731)", "[34435.731,37699.195)", "[37699.195,40962.659)", "[40962.659,44226.122)", "[44226.122,47489.586)", 
                                              "[47489.586,50753.049)", "[50753.049,54016.513)", "[54016.513,57279.976)", "[57279.976,60543.44)"))

plot_ly(x = risco_credito$faixa_salario, type = "histogram")
# Analisando a saída da distribuição de sexos, é possível ver que a base está bem distribuída entre homem e mulher, tendo praticamente a mesma distribuição entre ambos
plot(risco_credito$gender, ylab="left",xlab="Sexo",col=c('red','darkgreen'))
table(risco_credito$gender)
# Female   Male 
# 2478   2442

# Analisando a distribuição da variavel marital, vemos que a maior parte da base é de pessoas casadas (50,6%), logo em seguida vem solteiro (28%) e, por fim, o agrupamento de divorciado, separado e viuvo (21,4%)
marital_pie <- plot_ly(as.data.frame(table(risco_credito$marital)),
                        labels = ~Var1,
                        values = ~Freq,
                        type = 'pie')%>%
                        layout(title = "Graficos de Distribuição da variável Marital")
marital_pie

# Analisando a distribuição de número de filhos, vemos que a maior parte da base (82,4%) possui entre 0 e 2 filhos
plot_ly(as.data.frame(table(risco_credito$numkids)), label = ~Var1, values= ~Freq, type = 'pie') %>% layout(title="Grafico de distribuição do total de Filhos")
table(risco_credito$numkids)
# 0    1    2    3    4 
# 1372 1536 1146  513  353 

# Ao analisar a distribuição de número de cartões que cada pessoa na base possui, vemos que temos uma curva à esquerda, onde o volume maior se concentra entre 1 e 2 cartões
plot(risco_credito$numcards,ylab="left",xlab="Número de cartões",col=c('red','darkgreen'))
table(risco_credito$numcards)
# 0    1    2    3    4    5    6 
# 651 1356 1171  478  212  540  512

# Ao analisar o método de pagamento de cada usuário, vemos uma predominância no método de pagamento mensal, porém, os números são muito próximos uns dos outros.
plot(risco_credito$howpaid,ylab="left",xlab="Formas de Pagamento",col=c('red','darkgreen'))
table(risco_credito$howpaid)
# monthly  weekly 
# 2602    2318
 
par (mfrow=c(2,2))
# Analisando as outras variáveis, vemos que 90% da base possui hipoteca, somente 3% da base não possui carro, 71% possui entre 1 e 2 emprestimos e somente 32% da base é considerada um bom pagador
plot(risco_credito$mortgage, ylab="left",xlab="Hipoteca",col=c('red','darkgreen'))
plot(risco_credito$storecar, ylab="left",xlab="storecar",col=c('red','darkgreen'))
plot(risco_credito$loans, ylab="left",xlab="Emprestimos",col=c('red','darkgreen'))
plot(risco_credito$loans, ylab="left",xlab="Emprestimos",col=c('red','darkgreen'))
plot(risco_credito$goodrisk, ylab="left",xlab="Adimplentes",col=c('red','darkgreen')) 

par (mfrow=c(1,1))
# Analisando a inadimplencia, vemos que 18,4% da base é considerada inadimplente
plot(risco_credito$bad_debt, ylab="left",xlab="Inadimplentes",col=c('red','darkgreen')) 


###################################################
#### Iniciando o modelo de regressão logística ####
attach(risco_credito)


Logistica <-glm(risco_credito$target_goodrisk ~ faixa_idade + faixa_salario + bad_debt + goodrisk + loans + marital + gender + numcards + numkids + mortgage + howpaid + storecar
                , family = "gaussian")

Logistica

# Após algumas análises e testes de variáveis, removi todas as variáveis que tinham baixa significância para o modelo e mantive as que
# geravam melhores resultados, são elas: faixa_idade + faixa_salario + bad_debt 
summary(Logistica) 

### Modelo Final
Logistica <-glm(risco_credito$target_goodrisk ~ faixa_idade + faixa_salario + bad_debt + mortgage, family = "gaussian")
summary(Logistica) 

# Adicionando a probabilidade de inadimplencia
risco_credito$probabilidade <-predict(Logistica, risco_credito, type = "response")

# Gerando a coluna de predito
risco_credito$predito <- ifelse(risco_credito$probabilidade >= 0.5,1,0)

# Gerando o percentual de acerto do modelo
acerto <-table(risco_credito$target_goodrisk,risco_credito$predito) 
acerto

# Calculando assertividade do modelo: 85.06098
diagonal <- diag(acerto)
perc.acerto <- sum(diagonal)/sum(acerto)*100
perc.acerto

###################################################
#### Iniciando o modelo de Arvore de decisao ####
risco_credito$target_goodrisk <- as.factor(risco_credito$target_goodrisk)

modelo_tree <- rpart (risco_credito$target_goodrisk ~ faixa_idade + faixa_salario + bad_debt + mortgage
                      ,  data=risco_credito, cp = 0.001,minsplit = 100,maxdepth=10)

summary(modelo_tree)
# Faz o Grafico
rpart.plot(modelo_tree, type=1, extra=100, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=TRUE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=2,
           compress=TRUE,
           snip=FALSE)
 
# Gerando a coluna de predito
risco_credito$predito_arvore <- predict(modelo_tree,risco_credito,type='class')

# Gerando o percentual de acerto do modelo
acerto_arvore <-table(risco_credito$target_goodrisk,risco_credito$predito_arvore) 
acerto_arvore


# Calculando assertividade do modelo: 85.83333
diagonal <- diag(acerto_arvore)
perc.acerto <- sum(diagonal)/sum(acerto_arvore)*100
perc.acerto
