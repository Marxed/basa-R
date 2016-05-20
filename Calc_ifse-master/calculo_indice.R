#Script calculo IFSE
#autor: Beatriz Nery e Markus

#OBS: posteriormente será criada uma variável que repesente a criação de matriz, bem como a quantidade de
#suas linhas e colunas

##############1ªParte################tabelas##############################################
#carregar tabela (data set)                                                                         #
tabela_completa = read.csv2('tabela_ifse.csv', header = TRUE, sep = ';', dec = ',')  
qtd_linhas <- nrow(tabela_completa) #qtd de linhas do arquivo .csv
qtd_colunas <- ncol(tabela_completa) #qtd de coluanas 
#criar tabela de 0's para posteriormente colocar o index. 
tabela_ranqueada_abr = matrix(0, qtd_linhas, qtd_colunas)
tabela_ranqueada_fqr = matrix(0, qtd_linhas, qtd_colunas)
tabela_ranqueada_dor = matrix(0, qtd_linhas, qtd_colunas)
tabela_ranqueada_biomr = matrix(0, qtd_linhas, qtd_colunas)
tabela_ranqueada_vcmr = matrix(0, qtd_linhas, qtd_colunas)
tabela_ranqueada_usor = matrix(0, qtd_linhas, qtd_colunas)
tabela_dummy = matrix(0,qtd_linhas,6) #tabela para colocar as colunas com as variáveis dummy
#ordena todas as colunas a partir da coluna abr
index_abr<- with(tabela_completa, order(abr,fqr, dor, vcmr, biomr, usor, decreasing = TRUE))
tabela_ranqueada_abr =  tabela_completa[index_abr,]   
#ordena todas as colunas a partir de fqr
index_fqr <- with(tabela_completa, order(fqr, abr, dor, vcmr, biomr, usor, decreasing = TRUE))
tabela_ranqueada_fqr = tabela_completa[index_fqr,]
#ordena todas as colunas a partir da coluna dor
index_dor <- with(tabela_completa, order(dor, abr, fqr, vcmr, biomr, usor, decreasing = TRUE))
tabela_ranqueada_dor = tabela_completa[index_dor,]
#ordena todas as colunas a partir da coluna vcmr
index_vcmr <- with(tabela_completa, order(vcmr, abr, fqr, dor, biomr, usor, decreasing = TRUE))
tabela_ranqueada_vcmr = tabela_completa[index_vcmr,]
#ordena todas as colunas a partir da coluna biomr
index_biomr <- with(tabela_completa, order(biomr, abr, fqr, dor, vcmr, usor, decreasing = TRUE))
tabela_ranqueada_biomr = tabela_completa[index_biomr,]
#ordena 
index_usor <- with(tabela_completa, order(usor, abr, fqr, dor, vcmr, biomr, decreasing = TRUE))
tabela_ranqueada_usor = tabela_completa[index_usor,]
##############################Calculo ###############################################
t_individuos <- 36298 #nº total de indiviuos em mil
t_especies <- 898 #n? total de especies
t_parcelas_especies <- 315 #total de parcelas de ocorrencia pra todas as especies
t_area_basal <- 1321 #area basal total
t_biomassa_especies <- 26377324.25  #biomassa de todas as especies
valor_total_comercial_madeira <- 415148.77 #vcmr total em R$ Obs: esse valor esta na tabela do spss(tabelão)
t_uso_especie <- 203 #usor da especie
############dummy#############
#abr#
for (i in qtd_linhas) {
  
  n_individuos_especie_abr <- ((tabela_ranqueada_abr$abr/100) * t_individuos) #
}

dim(n_individuos_especie_abr) <- c(qtd_linhas,1) #transforma um vetor em matriz

soma <- 0 #inicializa a variável soma com 0
x <- matrix(0,qtd_linhas,1) #cria um matriz de 0's com uma coluna e 898 linhas #obs.
#for para criação das variáveis dummy
for (a in 1:qtd_linhas) {
  
  teste <- sum(n_individuos_especie_abr[a,]) #valor da linha da matriz
  soma <- soma+teste #soma as linhas da matriz
  
  if (soma <= (t_individuos/2)) {
    
    x[a,1] <- 1 #escrever variávei dummy na matriz
  }
}

#fqr#
#pegar o numero de parcelas por especie
for(f in qtd_linhas){
  n_parcelas_especie_fqr <- ((tabela_ranqueada_fqr$fqr/100) * t_parcelas_especies) #obter o valor de n_parcelas
}

dim(n_parcelas_especie_fqr) <- c(qtd_linhas,1) #transforma um vetor em matriz

soma_fqr <- 0 #inicializa a variável com 0
x_fqr <- matrix(0, qtd_linhas, 1) #cria um matriz de 0's com uma coluna e 898 linhas #obs.
#for para somar fqr e escrever coluna 
for(fq in 1:qtd_linhas){
  teste_fqr <- sum(n_parcelas_especie_fqr[fq,]) #pegar valor de cada linha da matriz
  soma_fqr <- soma_fqr + teste_fqr #soma o valor das linhas da coluna
  if (soma_fqr <= (t_parcelas_especies/2)){
    #criar matriz p/ colocar dummy
    x_fqr[fq,1] <- 1 #escrever variávei dummy na matriz
  }
}

#Area Basal ABr =  dor na tabela
for(dr in 1:qtd_linhas){
  
  area_basal <- (( tabela_ranqueada_dor$dor/100)*  t_area_basal)
  
}
dim(area_basal) <- c(qtd_linhas,1) # transforma um vetor em matriz

soma_area <- 0
x_area <- matrix(0,qtd_linhas,1) #cria uma matriz de 0's 
for (area in 1:qtd_linhas) {
  teste_area <- sum(area_basal[area,]) #pega o valor de cada linha da matriz 
  soma_area <- soma_area + teste_area #soma o valor das linhas da coluna
  
  if (soma_area <= (t_area_basal/2)) {
    x_area[area,1] <- 1 #escreve variável dummy na matriz
  }
  
}
#Valor comercial da madeira (vcmr)
for (vc in 1:qtd_linhas) {
  valor_comercial_madeira_especie <- ((tabela_ranqueada_vcmr$vcmr/100) * valor_total_comercial_madeira)
}
dim(valor_comercial_madeira_especie) <- c(qtd_linhas,1) #transforma um vetor em matriz

soma_vcmr <- 0
x_vcmr <- matrix(0, qtd_linhas, 1)
for (vcm in 1:qtd_linhas) {
  teste_vcmr <- sum(valor_comercial_madeira_especie[vcm,])
  soma_vcmr <- soma_vcmr + teste_vcmr
  
  if (soma_vcmr <= (valor_total_comercial_madeira/2)) {
    x_vcmr[vcm,1] <- 1
  }
}

#biomassa
for (bio in 1:qtd_linhas) {
  biomassa_especie <- ((tabela_ranqueada_biomr$biomr/100) * t_biomassa_especies)
}
dim(biomassa_especie) <- c(qtd_linhas,1)

soma_bio <- 0
x_bio <- matrix(0,qtd_linhas,1)
for(biom in 1:qtd_linhas){
  teste_biom <- sum(biomassa_especie[biom,])
  soma_bio = soma_bio + teste_biom
  
  if(soma_bio <= (t_biomassa_especies/2)){
    x_bio[biom,1] <- 1
  }
}
 #PFNM
 for (pf in 1:qtd_linhas) {
   pfmn_especie <- ((tabela_ranqueada_usor$usor/100) * t_uso_especie)
 }
 
 dim(pfmn_especie) <- c(qtd_linhas,1)
 
 soma_pfnmr <- 0
 x_pfnmr <- matrix(0, qtd_linhas, 1)
 for (pfnm in 1:qtd_linhas) {
   teste_pfnm <- sum(pfmn_especie[pfnm,1])
   soma_pfnmr= soma_pfnmr + teste_pfnm
   
   if(soma_pfnmr <= (t_uso_especie/2)){
     x_pfnmr[pfnm,1] <- 1
     
   }
 }