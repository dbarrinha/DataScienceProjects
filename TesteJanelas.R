#setwd("C:/Users/danilo.barrinha/OneDrive/Testes R")#Endereço no tce
setwd("C:/Users/dbarr/OneDrive/Testes R")#Endereço Danilo
#install.packages("dplyr")
library(dplyr)

arquivos = list.files(path = "Intel-lab-original", pattern = ".csv", full.names = TRUE)

energia_sem_reducao = c();

gastarEnergia = function(num) {
  return((3 * 7.0 * 0.26 * num * 32) / 1000000)
  
}

silulacao_energiaPCA = data.frame()
silulacao_energia = data.frame()
EQM_PCA = data.frame()

for (arquivo in arquivos) {
  
  print(arquivo)

#Leitura e Titulo das Variáveis
no9 = read.csv(arquivo)
names(no9) = c("Data", "Hora", "Epoca", "No", "Temperatura", "Hum", "Luz", "Volt")
no9 <- no9[complete.cases(no9),]
no9 = no9[,5:8]           

i = 1#Contador
k = 0#numero de janelas

janelaInicial = 10;

num_linhas_totais = nrow(no9);
VetorDeGasto = c();
eqm = c()#Erros quadráticos
while(i < num_linhas_totais){
  
  if((i+janelaInicial) > num_linhas_totais){
    #print("Fim do DataSet");
    break;
  }

  #inicializa Data Frame Temporário
  dataset_temp = data.frame();
  dataset_temp <- data.frame(no9[i:(i+janelaInicial),])#100 é o tamanho da janela mínima
  i = i +janelaInicial;
  exit = 0 ;#Flag de saída do loop
  contador = 0;#Contador que talvez não senha necessário
  
  colunas = as.integer(c());
  valores = c();
  colunasSemVariancia = data.frame();
  
  while(i < num_linhas_totais && exit == 0){ 
    i = i +1;
    #Insere nova linha
    if( length(colunas) != 0 ){
      dataset_temp <- rbind(dataset_temp, no9[i, -colunas])
    }else{
      dataset_temp <- rbind(dataset_temp, no9[i, ])
    }
    
    ####### COLUNAS SEM VARIÂNCIA E SEUS RESPECTIVOS VALORES ##########
    colunas = c( colunas , which(apply(dataset_temp, 2, var) == 0));
    valores = c( dataset_temp[1 , apply(dataset_temp, 2, var) == 0])
    colunasSemVariancia =rbind(colunasSemVariancia, colunas,valores ) # Adiconando colunas e valores sem variância 
    dataset_temp = dataset_temp[ , apply(dataset_temp, 2, var) != 0] #Retirando do dataset as colunas sem variância
    
    if(length(colunas) == ncol(no9)){
      exit = i;
      #print("todas as colunas são constantes");
      eqm  = c(eqm , 0)
      break;
    }
    
    #Gerando o Modelo
    model = prcomp(dataset_temp , scale. = TRUE)
    variancia = model$sdev^2 / sum(model$sdev^2);
    contador = contador +1;
    
    if(contador >= 500){ # parâmetro enviado pelo usuário, porém nos testes do projeto sejam abordados vários valores
      exit = i;
      #print("ALCANÇOU 500 AMOSTRAS ");
      #print(i);
      
      #Calculando erro quadrático médio
      dados2 = t(t(model$x[,1:1] %*% t(model$rotation[,1:1])) * model$scale + model$center)
      aux = round(sqrt(mean((dados2-dataset_temp)^2)), digits = 5)
      eqm  = c(eqm , aux)
      #print("Valor do erro:");
      #print(aux)
    }
    
    if(variancia[1] >= 0.9){
      #print("É MAIOR QUE 90% ");
      #print(i);
      exit = i;
      
      #Calculando erro quadrático médio
      dados2 = t(t(model$x[,1:1] %*% t(model$rotation[,1:1])) * model$scale + model$center)
      aux = round(sqrt(mean((dados2-dataset_temp)^2)), digits = 5)
      eqm  = c(eqm , aux)
      #print("Valor do erro:");
      #print(aux)
      
    }
  }
  GastoEnergia = gastarEnergia(nrow(dataset_temp));
  
  if(length(colunas) >0){
    GastoEnergia = GastoEnergia + gastarEnergia(length(colunas));
    
    if(length(colunas) == ncol(no9)){
      GastoEnergia = gastarEnergia(length(colunas));
    }
  }
  
  #print("ENERGIA GASTA: ");
  #print(GastoEnergia);
  
  
  VetorDeGasto = rbind(VetorDeGasto,GastoEnergia);
  k = k +1;
  
}
  EQM_PCA = rbind(EQM_PCA,  sum(eqm)/nrow(no9))
  silulacao_energiaPCA = rbind(silulacao_energiaPCA,sum(VetorDeGasto))
  silulacao_energia =  rbind(silulacao_energia,gastarEnergia(nrow(no9) * ncol(no9)))  
  print("FIM DA SIMULAÇÃO!")

}
library(xlsx)
resultado <- bind_cols(silulacao_energiaPCA, silulacao_energia)
write.xlsx(EQM_PCA, "erros.xlsx")
write.xlsx(resultado, "resultado.xlsx")



