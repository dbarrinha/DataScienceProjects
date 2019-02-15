#setwd("C:/Users/danilo.barrinha/OneDrive/Testes R")#Endereço no tce
#setwd("C:/Users/dbarr/OneDrive/Testes R")#Endereço Danilo

#0-Configuracao de variaveis
DATASET_PATH = "Intel-Lab-original"
TRAINING = 10

# Variavel dependente a ser utilizada na construcao do modelo linear
DEPENDENT_VARIABLE = "Temperatura"

# THRESHOLD
LINEAR_THRESHOLD = 0.5

# PARAMETROS REFERENTE AO FORMATO DOS ARQUIVOS CSV
SEP_FIELD = ","
DEC_FIELD = "."

# PARAMETRO REFERENTE AO NÚMERO DE LINHAS À SEREM LIDAS
LINE_NUM = 100

# PARAMETRO REFERENTE AO NOME DO ID
ID_NAME = "Id"

#1-instala e carrega as bibliotecas
##install.packages("dplyr")
library("dplyr")

#2-Fun??o para simular o gasto de energia
gastarEnergia = function() {
  return((3 * 21.5 * 0.26 * 288) / 1000000)
  
}

#3.1-Corpo principal do programa
#3.2-Parametros:
#caminho = tipo(string), define o camino onde os n?s estao armazenados
#training = tipo(int), define a quantidade de n?s a serem lidos na janela de treino
#parametro1 = indica o parametro que ser? prevido por meio da regress?o
#linearThreshold = tipo(float), define o limite do erro permitido entre a regressao linear e a verdadeira medida 

#4.1-Carregar a pasta dos n?s
arquivos = list.files(path = DATASET_PATH, pattern = ".csv", full.names = TRUE)
#4.2-Data frame que armazenar? os dados preditos 
data = data.frame()
#4.3-Data frame que armazenar? os MSE
MSEData = data.frame()
#4.4-Data frame que armazenará os gastos de energia
energiaData = data.frame()
#erroTotal = Soma dos erros ao quadrado
erroTotal = 0
#errrCont = Conta quantos erros temos
erroCont = 0
#energiaTotal = Soma a energia total gasta
energiaTotal = 0

#5.1-Laco para ler um no por vez
for (arquivo in arquivos) {
  print(paste0("Processando arquivo: ", basename(arquivo)))
  #5.2-Variaveis:
  #condition = representa a condicao para permanecer no laco ate ler a ultima linha
  condition = 0
  #trainingCounter = variavel utilizada para percorrer as linhas de data frame
  trainingCounter = 1
  #trainingWindow = variavel que determina quantas linhas serao percorridas 
  trainingWindow = TRAINING
  #erroNo = soma de erros ao quadrado do no
  erroNo = 0
  #contNo = quantidade de erros do no
  contNo = 0
  #energiaNo = soma a energia gasta pelo nó
  energiaNo = 0
  #no = variavel que armazena um no no data frame
  no = read.csv(arquivo, sep = SEP_FIELD, dec = DEC_FIELD)
  names(no) = c("Data","Hora","Epoca", "Id", "Temperatura", "Humidade" , "Luz", "Tensao")
  #6-Laco para ler todas as linhas do data frame(no)
  no <- no[complete.cases(no),]
  while (condition == 0) {
    
    trainingWindow = (trainingCounter - 1) + TRAINING
    #7.1-Laco para iniciar a leitura dos dados que serao utilizados no modelo linear
    #medidas = data frame para amarzenar os dados q serao utilizadas para fazer o modelo linear
    medidas = data.frame()
    #numTraining = vetor que armazena os "trainingWindow" utilizados para fazer o modelo linear
    numTraining = c()
    while (trainingCounter <= trainingWindow ) {
      numTraining = rbind(numTraining,trainingCounter)
      medidas = rbind(medidas, no[trainingCounter,])
      energiaNo = energiaNo + gastarEnergia()
      energiaTotal = energiaTotal + gastarEnergia()
      trainingCounter = trainingCounter + 1
      #7.2-Condi??o para se ultrapassar o número de linhas desejados, sair do laço
      if(trainingCounter > LINE_NUM) {
        medidas = cbind(medidas,numTraining)
        condition = 1
        break
      }
    }
    medidas = cbind(medidas,numTraining)
    #8-Gerar o modelo linear
    formula_lm = as.formula(paste0(DEPENDENT_VARIABLE, " ~ ", "numTraining"))
    linearMod = lm(formula_lm, data = medidas)
    summary(linearMod)
    energiaNo = energiaNo + gastarEnergia()
    energiaTotal = energiaTotal + gastarEnergia()
    #9.1-Condicao para continuar somente se nao houver ultrapassado a ultima linha do data frame(no)
    if(trainingCounter <= LINE_NUM) {
      #aux = data frame auxiliar que armazena o valor utilizado para fazer a previs?o 
      aux = data.frame(numTraining = c(trainingCounter))
      #9.2-Prever o valor do parametro1 
      pred = predict(linearMod,aux)
      #9.3-Calcular o erro entre o valor real e o previsto 
      error = abs((no[trainingCounter,DEPENDENT_VARIABLE] - pred))
      #Id = pega o id de da medicao,substituir o (ID) para a nomeacao utilizada no dataset
      id = no[trainingCounter, ID_NAME]
      
      #9.4-Laco para continuar lendo at? que o erro ultrapasse o permitido 
      while (error < LINEAR_THRESHOLD) {
        data = rbind(data,c(id,trainingCounter,no[trainingCounter,DEPENDENT_VARIABLE],pred))
        
        erroTotal = erroTotal + (error * error)
        erroCont = erroCont + 1
        
        erroNo = erroNo + (error * error)
        contNo = contNo + 1
        
        trainingCounter = trainingCounter + 1
        #(7.2)
        if(trainingCounter > LINE_NUM) {
          condition = 1
          break
        }
        aux = data.frame(numTraining = c(trainingCounter))
        pred = predict(linearMod,aux)
        error = abs((no[trainingCounter,DEPENDENT_VARIABLE] - pred))
      }
    }
  }
  MSE = erroNo/ contNo
  MSEData = rbind(MSEData,c(id,MSE))
  print(paste0("MSE: ", round(MSE, 3)))
  
  energiaData = rbind(energiaData,c(id,energiaNo))
  print(paste0("Energia: ", round(energiaNo, 3)))
}

energiaData = rbind(energiaData,c("TOTAL",energiaTotal))
colnames(energiaData) = c("ID","Energia(mJ)")
MSE = erroTotal / erroCont
MSEData = rbind(MSEData,c("TOTAL",MSE))
colnames(MSEData) = c("ID","MSE")
colnames(data)=c("Id","TrainingCounter","Med.Real","Med.Predita")
#Salva os data frames em csv
##Write.csv(data,file = "ErrorsData.csv")
##write.csv(MSEData, file = "MSEData.csv")
##write.csv(energiaData, file = "EnergiaData.csv")

