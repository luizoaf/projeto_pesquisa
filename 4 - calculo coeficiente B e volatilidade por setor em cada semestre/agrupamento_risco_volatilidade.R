# dados = read.table("calculo_b_volatilidade.csv",sep=",",head=T)
dados = read.table("calculo_b_volatilidade_sem_4_outliers.csv",sep=",",head=T)

k = 3
retorna_cluster = function(periodo,dados,k){
  #   dados = eixo_x_y
  #       k = 2
  #   dados = subset(eixo_x_y,eixo_x_y$tempo==semestre)
  iter = 45
  #   agrupamento = data.frame(coeficiente_B = dados[,c("coeficiente_B")])
  agrupamento =  data.frame(volatilidade = dados[,c("volatilidade")])
  km = kmeans (x = agrupamento, centers = k, iter.max = iter)
  agrupamento$cluster = km$cluster
  grupos = unique(km$cluster)
  #   print(grupos)
  dados$cluster = agrupamento$cluster
  dados$risco_b = ""
  dados$risco_b[dados$cluster == grupos[1]] = "moderado"
  dados$risco_b[dados$cluster == grupos[2]] = "arrojado"
  dados$risco_b[dados$cluster == grupos[3]] = "conservador"
  
  dados$cor = ""
  dados$cor[dados$cluster == grupos[1]] = "black"
  dados$cor[dados$cluster == grupos[2]] = "red"
  dados$cor[dados$cluster == grupos[3]] = "green"
  
  #   legenda = c("conservador","moderado","arrojado")
  #   plot(main= paste("Para K = ",k,sep =""),agrupamento$coeficiente_B,xlab="indexes",ylab="Coefficient B", col = dados$cor,pch = 20, cex = 0.9)
  #   plot(main= paste("Para K = ",k,sep =""),agrupamento$volatilidade,xlab="indexes",ylab="Volatility B", col = dados$cor,pch = 20, cex = 0.9)
  #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
  #   legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)
  centroides = as.data.frame(km$centers)
  #   centroides = centroides[order(centroides$coeficiente_B,decreasing=T),]
  centroides = centroides[order(centroides$volatilidade,decreasing=F),]
  centroides = cbind(centroides,data.frame(risco = c("conservador","moderado","arrojado")))
  #   centroides = cbind(centroides,data.frame(risco = c("arrojado","conservador")))
  centroides$cor = ""
  centroides$cor[centroides$risco == "conservador"] = "green"
  centroides$cor[centroides$risco == "moderado"] = "black"
  centroides$cor[centroides$risco == "arrojado"] = "red"
#   periodo = 2008
    plot(main=periodo,xlab = "indice ignorado" ,ylab = "volatilidade",centroides$centroides~rep(1,length(centroides$centroides)),col=centroides$cor, pch = 8,lwd=3)
  #     plot( centroides$volatilidade,col=centroides$cor, pch = 8,lwd=3,xlim=c(0,2.5),ylim=c(0,4.5))
  return(centroides)
}

faixa_temporal = unique(dados$tempo)
novos_pontos_classificados_com_setores = data.frame()
for( periodo in faixa_temporal){
#       periodo = 2008
  faixa_temporal_teste = periodo
  faixa_temporal_treino = setdiff(faixa_temporal, periodo)
  
  teste = subset(dados,dados$tempo == faixa_temporal_teste)
  treino = subset(dados,dados$tempo %in% faixa_temporal_treino)
  png(paste(periodo,".png",sep=""))
  centroides = retorna_cluster(periodo,treino,k)
  
  # 
  #   pontos_novos = data.frame(coeficiente_B = teste[,c("coeficiente_B")])
  pontos_novos = data.frame(volatilidade = teste[,c("volatilidade")])
  #   points(pontos_novos$coeficiente_B,col="violet")
  #   points(pontos_novos$volatilidade,col="violet")
  
  novos_pontos_classificados = data.frame()
  #   ponto_estudado = 1# indices de todos os pontos
  
  for(ponto_estudado in 1:nrow(pontos_novos)){
    
    ponto_centroide = 1:k # os 3 possiveis centroides dos riscos
    #     distancia = sqrt((pontos_novos$volatilidade[ponto_estudado] - centroides$volatilidade[ponto_centroide])^2 + (pontos_novos$coeficiente_B[ponto_estudado] - centroides$coeficiente_B[ponto_centroide])^2)
    #     distancia = sqrt((pontos_novos$coeficiente_B[ponto_estudado] - centroides$centroides[ponto_centroide])^2)
    distancia = sqrt((pontos_novos$volatilidade[ponto_estudado] - centroides$centroides[ponto_centroide])^2)
    
    todas_distancias = data.frame(distancias = distancia,risco =centroides$risco)
    indice_menor_distancia = which.min(todas_distancias$distancias)
    risco = as.character(todas_distancias$risco[indice_menor_distancia])
    
    #     agrupamento = cbind(data.frame(coeficiente_B = pontos_novos[ponto_estudado,]),risco)
    agrupamento = cbind(data.frame(volatilidade = pontos_novos[ponto_estudado,]),risco)
    distancia_euclidiana = distancia[indice_menor_distancia]
    agrupamento = cbind(agrupamento,distancia_euclidiana)
    
    # calcula SSE de B e volatilidade
    alvo_volatilidade = centroides$centroides[ponto_centroide][indice_menor_distancia]
    #     alvo_B = centroides$centroides[ponto_centroide][indice_menor_distancia]
    
    volatilidade = pontos_novos$volatilidade[ponto_estudado]
    #     coeficiente_b = pontos_novos$coeficiente_B[ponto_estudado]
    
    sse_volatilidade = sum(( volatilidade-alvo_volatilidade)^2)
    #     sse_coeficiente_b = sum(( coeficiente_b-alvo_B)^2)
    
    agrupamento = cbind(agrupamento,sse_volatilidade)
    #     agrupamento = cbind(agrupamento,sse_coeficiente_b)
    agrupamento = cbind(agrupamento,periodo)
    novos_pontos_classificados = rbind(novos_pontos_classificados,agrupamento)
  }
  agrupamento_periodo = merge(novos_pontos_classificados,teste, by = intersect(names(novos_pontos_classificados), names(teste)))
  novos_pontos_classificados_com_setores = rbind(novos_pontos_classificados_com_setores,agrupamento_periodo)
  
  agrupamento_periodo$cor = ""
  agrupamento_periodo$cor[agrupamento_periodo$risco == "moderado"] = "black"
  agrupamento_periodo$cor[agrupamento_periodo$risco == "arrojado"] = "red"
  agrupamento_periodo$cor[agrupamento_periodo$risco == "conservador"] = "green"
  points(agrupamento_periodo$volatilidade~rep(1,length(agrupamento_periodo$volatilidade)),col=agrupamento_periodo$cor)
  dev.off()
  #   print(teste)
  #   print(treino)
  #   break
}
# # novos_pontos_classificados = data.frame(pontos_novos$volatilidade)
# 

# 
# points(novos_pontos_classificados$volatilidade,novos_pontos_classificados$coeficiente_B,col=novos_pontos_classificados$cor,lwd=1)
# # aggregate(dados$a,list(dados$tempo),FUN=length)

novos_pontos_classificados_com_setores = novos_pontos_classificados_com_setores[,-9]#remove i
novos_pontos_classificados_com_setores = novos_pontos_classificados_com_setores[,-9]#remove tempo, fica só periodo(teste)

novos_pontos_classificados_com_setores$cor = ""
novos_pontos_classificados_com_setores$cor[novos_pontos_classificados$risco == "moderado"] = "black"
novos_pontos_classificados_com_setores$cor[novos_pontos_classificados$risco == "arrojado"] = "red"
novos_pontos_classificados_com_setores$cor[novos_pontos_classificados$risco == "conservador"] = "green"

write.table(novos_pontos_classificados_com_setores,file="agrupamento_sse_volatilidade.csv",row.names=F)
# write.table(novos_pontos_classificados_com_setores,file="agrupamento_sse_volatilidade_k_2.csv",row.names=F)
