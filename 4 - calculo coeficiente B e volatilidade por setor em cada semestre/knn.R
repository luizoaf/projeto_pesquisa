# Código do algoritmo KNN com 2 abordagens diferentes para classificação, através apenas do eixo X ou pela distância de Manhattan
data(iris)
iris = iris[,c("Sepal.Length","Sepal.Width","Species")] # Extraímos apenas 2 características
pontos = iris[!duplicated(iris[,c("Sepal.Length","Sepal.Width")]), ] # Remoção das linhas repetidas, para facilitar o entendimento do algoritmo
names(pontos) = c("Sepal.Length","Sepal.Width","classe")
classes = unique(pontos$classe) # todas as instâncias de classes da base de dados

define_cor_para_plot = function(base_de_dados){
  cor = data.frame()
  for(i in 1: length(classes)){
    cor = rbind(cor,data.frame(cor=i,classe = classes[i]))
  }
  cores=c()
  for(i in 1:nrow(base_de_dados)){
    for(j in 1:nrow(cor)){
      if(base_de_dados$classe[i]==cor$classe[j]){
        cores[i]= cor$cor[j]
      }
    }
  }
  return(cores)
}

# Pontos que queremos classificar
pontos_novos = data.frame(Sepal.Length=c(7.5,4.5,5.1,6),Sepal.Width=c(4,4,2.2,3.1))

############## Algoritmo KNN ############

# Para cada ponto que queremos classificar, calculamos as distâncias para todos os pontos 
calcula_distancias = function(metodo_distancia){
  distancias = data.frame()
  for(ponto_estudado in 1:nrow(pontos_novos)){ # Para cada ponto novo que queremos classificar
    for(ponto_vizinho in 1:nrow(pontos)){ # Passe por todos os pontos para calcular as distâncias
      
      if(metodo_distancia=="euclidiana"){
        #       # Distância Euclidiana
        distancia = sqrt((pontos_novos$Sepal.Length[ponto_estudado] - pontos$Sepal.Length[ponto_vizinho])^2 + (pontos_novos$Sepal.Width[ponto_estudado] - pontos$Sepal.Width[ponto_vizinho])^2)
      }
      if(metodo_distancia=="manhattan"){
        # Distância de Manhattan
        distancia = abs(pontos_novos$Sepal.Length[ponto_estudado] - pontos$Sepal.Length[ponto_vizinho]) + abs(pontos_novos$Sepal.Width[ponto_estudado] - pontos$Sepal.Width[ponto_vizinho])    
      }
      if(metodo_distancia=="eixo_x"){
        # Distância de Manhattan
        distancia = abs(pontos_novos$Sepal.Length[ponto_estudado] - pontos$Sepal.Length[ponto_vizinho]) 
      }
      # Adicione as distâncias em um data frame 
      distancias = rbind(distancias,data.frame(distancia = distancia,ponto_estudado=ponto_estudado,ponto_vizinho=ponto_vizinho,classe=pontos$classe[ponto_vizinho]))
      colnames(distancias) = c("distancia","ponto_estudado","ponto_vizinho","classe")
      
    }
  }
  return(distancias)
}

distancias_euclidianas = calcula_distancias("euclidiana")
distancias_manhattan = calcula_distancias("manhattan")
distancias_eixo_x = calcula_distancias("eixo_x")

classificacao = function(distancias,k){
  # Para cada ponto que queremos classificar, selecione os K vizinhos mais próximos
  # e pegue o que possui mais classes, em caso de empate, pegue a primeira classe.
  definicao_classes = data.frame()
  for(ponto in 1:nrow(pontos_novos)){
    ponto_estudado = subset(distancias,distancias$ponto_estudado==ponto)
    ponto_estudado = ponto_estudado[order(ponto_estudado$distancia),]
    # Os k vizinhos mais próximos
    ponto_estudado = ponto_estudado[1:k,]
    tabela_quantidade_classes_vizinhos_mais_proximos = table(ponto_estudado$classe)
    definicao_classe_ponto_novo = names(tabela_quantidade_classes_vizinhos_mais_proximos[which.max(tabela_quantidade_classes_vizinhos_mais_proximos)])[1] 
    definicao_classes = rbind(definicao_classes,data.frame(Sepal.Length=pontos_novos$Sepal.Length[ponto],Sepal.Width=pontos_novos$Sepal.Width[ponto],classe=definicao_classe_ponto_novo))
  }
  return(definicao_classes)
}
qnt_k = 40
comparacao = data.frame()
for(k in 1:qnt_k){
  comparacao = rbind(comparacao,cbind(classificacao(distancias_euclidianas,k),classificacao(distancias_manhattan,k)$classe,classificacao(distancias_eixo_x,k)$classe))
  
}
colnames(comparacao) = c("Sepal.Length","Sepal.Width","classificacao_euclidiana","classificacao_manhattan","classificacao_eixo_x")

########################################

# Plot do antes e o depois da classificação

# par(mfrow=c(2,1))
# # Exibir todos os pontos da base de dados
plot(main=paste("Para K= ",k," (Antes)"),pontos$Sepal.Length,pontos$Sepal.Width,col = define_cor_para_plot(pontos),pch=19,xlab="Sepal Length",ylab = "Sepal Width",ylim=c(0,4.5))
points(pontos_novos,col="pink2",pch=4,lwd=7)
# # legend("bottomright", inset=.05, title="Classes",as.character(classes), fill=c(1:length(classes)), horiz=TRUE)
# legend("bottomright", inset=.05, title="Classes",c(as.character(classes),"Pontos novos"), col=c(1:length(classes),"pink2"), pch=c(19,19,19,4),horiz=TRUE)
# # Pontos que estão classificados, da base antiga
# plot(main=paste("Para K= ",k," (Depois)"),pontos$Sepal.Length,pontos$Sepal.Width,col = define_cor_para_plot(pontos),pch=19,xlab="Sepal Length",ylab = "Sepal Width",ylim=c(0,4.5))
# # Pontos que queremos prever, da base nova.
# points(definicao_classes$Sepal.Width~definicao_classes$Sepal.Length,col = define_cor_para_plot(definicao_classes),pch=4,lwd=7)
# 
# legend("bottomright", pch=19,inset=.05, title="Classes",as.character(classes), col=c(1:length(classes)), horiz=TRUE)

porcentagem_acuracia = length(comparacao$classificacao_euclidiana[comparacao$classificacao_euclidiana == comparacao$classificacao_manhattan])/length(comparacao$classificacao_euclidiana) * 100
porcentagem_acuracia_com_eixo_x = length(comparacao$classificacao_euclidiana[comparacao$classificacao_euclidiana == comparacao$classificacao_eixo_x])/length(comparacao$classificacao_euclidiana) * 100
porcentagem_acuracia
porcentagem_acuracia_com_eixo_x
