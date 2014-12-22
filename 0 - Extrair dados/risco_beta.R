source("../1_funcoes.R")
dados_com_bovespa = read.csv(file="papeis_da_ibovespa_2008_a_2014_2_com_IBOVESPA.csv")
dados_com_bovespa$datas  = as.Date(dados_com_bovespa$datas)

#anual
dados_com_bovespa$datas = as.numeric(format( dados_com_bovespa$datas,"%Y"))

# semestral

# meses = as.numeric(format( dados_com_bovespa$datas,"%m"))
# datas = meses

# datas[which(meses <= 6)] = paste(format( dados_com_bovespa$datas[which(meses <= 6)],"%Y"),".1",sep="")
# datas[which(meses > 6 )] = paste(format( dados_com_bovespa$datas[which(meses > 6 )] ,"%Y"),".2",sep="")

# dados_com_bovespa$datas = datas
# 
faixa_temporal = unique(dados_com_bovespa$datas)
dados_bovespa_setores = data.frame()
for(i in 1:length(faixa_temporal)){
  dados_bovespa_setores = rbind(dados_bovespa_setores,setor_por_periodo(faixa_temporal[i],dados_com_bovespa[,-2])) # sem o ibovespa
}
dados_bovespa_setores$BVSP = dados_com_bovespa$X.BVSP
dados_bovespa_setores$datas = dados_com_bovespa$datas


risco_beta = data.frame()
for(tempo in unique(dados_bovespa_setores$datas)){
  for(indice_setor in 1:(ncol(dados_bovespa_setores)-2)){
    risco_beta = rbind(risco_beta,calcula_risco_beta(indice_setor,tempo,dados_bovespa_setores))
  }
}
# plot(risco_B_beta$beta[order(risco_B_beta$beta)],risco_B_beta$b_volatilidade[order(risco_B_beta$beta,decreasing=F)])
# cor(risco_B_beta$beta,risco_B_beta$a)
# cor(risco_B_beta$beta,risco_B_beta$coeficiente_B)
# cor(risco_B_beta$beta,risco_B_beta$b_volatilidade)
# cor(risco_B_beta$beta,risco_B_beta$volatilidade/risco_B_beta$coeficiente_B)
# cor(risco_B_beta$beta,risco_B_beta$coeficiente_B/risco_B_beta$volatilidade)
# cor(risco_B_beta$beta,risco_B_beta$volatilidade/risco_B_beta$a)
# cor(risco_B_beta$beta,risco_B_beta$coeficiente_B/a)
# cor(risco_B_beta$beta,risco_B_beta$a/risco_B_beta$coeficiente_B)
# cor(risco_B_beta$beta,risco_B_beta$a/risco_B_beta$volatilidade)
plot(risco_B_beta$beta[order(risco_B_beta$b_volatilidade)])
# points(risco_B_beta$coeficiente_B[order(risco_B_beta$beta,decreasing=F)],type="p",col=2,ylim=c(0,2))
points((((risco_B_beta$coeficiente_B*risco_B_beta$volatilidade)) )[order(risco_B_beta$b_volatilidade,decreasing=F)],col=4)
# points((risco_B_beta$volatilidade*risco_B_beta$coeficiente_B)[order(risco_B_beta$beta,decreasing=F)],col=4)
# riscos = vector(length=nrow(risco_B_beta))
# riscos[risco_B_beta$cluster==3] = 0.5
# riscos[risco_B_beta$cluster==1] = 1
# riscos[risco_B_beta$cluster==2] = 1.5
# points(dados$cluster[order(risco_beta$beta)]/2)
# 0.9   1.33 
minimo = .9
maximo = 1.1
# minimo = .95
# maximo = 1.15
# min_ = seq(0.9,1.7,by=0.01)
# max_ = seq(0.9,max(risco_beta$beta),by=0.01)
# acc = data.frame()
# for(minimo in min_ ){
#   for(maximo in min_ ){
    risco_beta$risco_beta_classificacao = ""
    risco_beta$risco_beta_classificacao[risco_beta$beta < minimo] = "conservador"
    risco_beta$risco_beta_classificacao[risco_beta$beta>= minimo & risco_beta$beta < maximo] = "moderado"
    # risco_beta$risco_beta_classificacao[risco_beta$beta == 1] = "moderado"
    risco_beta$risco_beta_classificacao[risco_beta$beta>= maximo] = "arrojado"
    
    
    # minimo = 1
    # maximo = 1.001
    # b_volatilidade = risco_B_beta$coeficiente_B/risco_B_beta$volatilidade
    # risco_B_beta$risco_b_classificacao = ""
    # risco_B_beta$risco_b_classificacao[risco_B_beta$coeficiente_B < minimo] = "arrojado"
    # risco_B_beta$risco_b_classificacao[risco_B_beta$coeficiente_B >= minimo & b_volatilidade < maximo] = "moderado"
    # risco_B_beta$risco_b_classificacao[risco_B_beta$coeficiente_B>=maximo] = "conservador"
    
    risco_B_beta = merge(risco_beta, dados, by = intersect(names(risco_beta), names(dados)))
    
    cor_beta = c()
    #   dados$cor[dados$cluster == grupos[1]] = "black"
    #   dados$cor[dados$cluster == grupos[2]] = "green"
    #   dados$cor[dados$cluster == grupos[3]] = "red"
    cor_beta[risco_B_beta$risco_beta_classificacao =="conservador"] = "green"
    cor_beta[risco_B_beta$risco_beta_classificacao == "moderado"] = "black"
    cor_beta[risco_B_beta$risco_beta_classificacao == "arrojado"] = "red"
    risco_B_beta$cor_beta = cor_beta 
    
    plot(main= paste("Para K = ",3,sep =""),risco_B_beta$coeficiente_B~risco_B_beta$volatilidade,xlab="Volatility",ylab="Coefficient B", col = risco_B_beta$cor_beta,pch = 20, cex = 0.9)
    legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)
    acertou = nrow(risco_B_beta[risco_B_beta$risco_beta_classificacao == risco_B_beta$risco_b,])
    total = nrow(risco_B_beta)
    acuracia = 100*(acertou/total)
    acuracia
#     acc = rbind(acc,c(minimo,maximo,acuracia)) 
#   } 
# }
# head(acc[order(acc[,3],decreasing=T),])

# 
# risco_B_beta = merge(risco_beta, dados, by = intersect(names(risco_beta), names(dados)))
# 
# acertou = nrow(risco_B_beta[risco_B_beta$risco_beta_classificacao == risco_B_beta$risco_b_classificacao,])
# total = nrow(risco_B_beta)
# 100*(acertou/total)