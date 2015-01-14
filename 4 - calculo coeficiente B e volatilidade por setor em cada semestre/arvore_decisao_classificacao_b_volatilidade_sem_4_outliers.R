require(rpart)
dados = read.table("agrupamento_sse_b_e_volatilidade.csv",dec=".",head=T)
dados= dados[sample(1:nrow(dados),length(1:nrow(dados))), 1:ncol(dados)]
dados$classe = dados$risco

porcentagem.treino = as.integer(0.7 * nrow(dados))
linhas.treino = 1:porcentagem.treino
linhas.teste = (porcentagem.treino+1):nrow(dados)

treino =dados[linhas.treino,]
teste =  dados[linhas.teste,]
dados = treino


arvore = rpart(classe~ coeficiente_B +volatilidade + b_volatilidade + a,data = dados)
plot(arvore,uniform=T,branch=0)
text(arvore,digits=3,cex=0.9,font=8,pretty=0,fancy=T,fwidth=0,fheight=0)
# 
previsao.treinamento = predict(arvore,dados,type='class')
matriz.confusao = table(dados$classe, previsao.treinamento)
diagonal = diag(matriz.confusao)
acuracia.treino = sum(diagonal)/sum(matriz.confusao)

previsao.teste = predict(arvore,teste,type='class')
matriz.confusao.teste = table(teste$classe, previsao.teste)
diagonal.teste = diag(matriz.confusao.teste)
acuracia.teste = sum(diagonal.teste)/sum(matriz.confusao.teste)

matriz.confusao
print(matriz.confusao.teste)
acuracia.treino
acuracia.teste
