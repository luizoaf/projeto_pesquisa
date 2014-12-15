# valor_incremento_B = 0.01
# vetor_incremento_coeficiente_B = seq(from=0.05,to=20,by=valor_incremento_B)
# coeficiente_B_e_erros = data.frame()
# iteracao = 1
# for(B in vetor_incremento_coeficiente_B){
#   #   coeficiente_B = vetor_incremento_coeficiente_B[1]
#   coeficiente_B = B
#   source("4_erro_coeficiente_B.R")  
#   coeficiente_B_e_erros = rbind(coeficiente_B_e_erros,c(coeficiente_B, sse, rmse,volatilidade,tamanho_serie))
#   colnames(coeficiente_B_e_erros) = c("coeficiente_B","sse", "rmse","volatilidade","tamanho_serie")
#   #   print(paste("Incremento: " ,incremento,"Iteracao: ",iteracao," SSE: ",sse," Coeficiente B: ", coeficiente_B," tamanho serie: ",tamanho_serie))
#   
#   # Quando o erro começar a aumentar, pare, pois teremos o melhor coeficiente B.
#   if(iteracao > 1){
#     if(sse > coeficiente_B_e_erros$sse[iteracao-1]){
#       break
#     }
#   }
#   iteracao = iteracao + 1
#   print(iteracao)
# }
# ########### calculo do B mais rapido ############# 
# i_erros = 1
# incremento = c(2,1,0.5,0.1,0.05,0.01,0.005,0.001,0.0001,0.00001)
incremento = c(2,1,0.5,0.1,0.05,0.01,0.005,0.001,0.0001)
i_incremento = 1
coeficiente_B = incremento[length(incremento)] #tenho sse, comparo o sse atual com o antigo
i_erros =1
incremento_atual = incremento[1]
coeficiente_B_e_erros = data.frame()
# eixo_x_y = data.frame()
posicoes_fitting = c()
i_posicao_fitting= 1
transicao = F
eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
alvo = funcao_distribuicao_probabilidade(serie)$frequencia_eixo_y
while(T){
#   Sys.sleep(1)
#   source("4_erro_coeficiente_B.R")  
  previsao = resultado_funcao_exponencial(eixo_x_frequencias,coeficiente_B)
  sse = sum(( previsao-alvo)^2)
  
#   plot_previsao_com_B_e_exponencial(eixo_x_frequencias,exponencial)
#   coeficiente_B_e_erros = rbind(coeficiente_B_e_erros,c(coeficiente_B, sse, rmse,volatilidade,mape))
#   colnames(coeficiente_B_e_erros) = c("coeficiente_B","sse", "rmse","volatilidade","mape")
#   coeficiente_B_e_erros = rbind(coeficiente_B_e_erros,c(coeficiente_B, sse, rmse,volatilidade,mape))
  
  coeficiente_B_e_erros = rbind(coeficiente_B_e_erros, cbind(sse,coeficiente_B))
  #,periodo,setor,coeficiente_B*volatilidade))
  colnames(coeficiente_B_e_erros) = c("sse","coeficiente_B")
  
#   print(coeficiente_B_e_erros)
  #virou o lado
  # se o erro passado for maior que o atual
  if(i_erros>2 && coeficiente_B_e_erros$sse[i_erros-1] >= coeficiente_B_e_erros$sse[i_erros] && coeficiente_B_e_erros$coeficiente_B > 0){
#     if(coeficiente_B < 0){
#       transicao = !transicao
# #       i_incremento = i_incremento + 1
#       coeficiente_B =  coeficiente_B_e_erros$coeficiente_B[i_erros-1]
#     }
    incremento_atual = incremento[i_incremento]
    #     if(coeficiente_B < 0){
    #       transicao = !transicao
    #       
    #     }
    #     i_incremento = i_incremento + 1
    #     transicao = !transicao
    if(transicao){
      coeficiente_B = coeficiente_B - ifelse(is.na(incremento_atual),0,incremento_atual)
    }else{
      coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
    }
  }else{ #if(i_erros>=2 && coeficiente_B_e_erros$sse[i_erros-1] < coeficiente_B_e_erros$sse[i_erros]){
    i_incremento = i_incremento + 1
    transicao = !transicao
    incremento_atual = incremento[i_incremento]
    if(transicao){
      coeficiente_B = coeficiente_B - ifelse(is.na(incremento_atual),0,incremento_atual)
    }else{
      coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
    }
  }
  if(i_erros==1){
    incremento_atual = incremento[i_incremento]
    coeficiente_B = coeficiente_B + ifelse(is.na(incremento_atual),0,incremento_atual)
  }
  if(is.na(incremento_atual)){
    break
  }
  i_erros = i_erros + 1
  #   i_posicao_fitting = i_posicao_fitting + 1
  #   Sys.sleep(1)
  
}

# menor_sse = coeficiente_B_e_erros$sse[which.min(coeficiente_B_e_erros$sse)]
# coeficiente_B_e_erros_menor_sse = subset(coeficiente_B_e_erros,coeficiente_B_e_erros$sse == menor_sse)

menor_sse = coeficiente_B_e_erros$sse[which.min(coeficiente_B_e_erros$sse)]
coeficiente_B_e_erros_menor_sse = subset(coeficiente_B_e_erros,coeficiente_B_e_erros$sse == menor_sse)


volatilidade = calcula_volatilidade(serie)
sse = menor_sse
coeficiente_B = coeficiente_B_e_erros_menor_sse$coeficiente_B
previsao = resultado_funcao_exponencial(eixo_x_frequencias,coeficiente_B)


# plot(main = menor_sse,coeficiente_B_e_erros_menor_sse[,1:2])
# lines(coeficiente_B_e_erros_menor_sse[,c(1,3)])

# # Coeficientes B,mape,volatilidade do menor SSE encontrado
# coeficiente_B = as.double(as.character(coeficiente_B_e_erros$coeficiente_B[which.min(coeficiente_B_e_erros$sse)]))
# sse =  as.double(as.character(coeficiente_B_e_erros$sse[which.min(coeficiente_B_e_erros$sse)]))
# # tamanho_serie =  as.double(as.character(coeficiente_B_e_erros$tamanho_serie[which.min(coeficiente_B_e_erros$sse)]))
# mape =  as.double(as.character(coeficiente_B_e_erros$mape[which.min(coeficiente_B_e_erros$sse)]))
# volatilidade =  as.double(as.character(coeficiente_B_e_erros$volatilidade[which.min(coeficiente_B_e_erros$sse)]))
# 
# eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
# exponencial = resultado_funcao_exponencial(eixo_x_frequencias)

# # Coeficientes B,mape,volatilidade do menor SSE encontrado
# coeficiente_B = as.double(as.character(coeficiente_B_e_erros$coeficiente_B[which.min(coeficiente_B_e_erros$sse)]))
# sse =  as.double(as.character(coeficiente_B_e_erros$sse[which.min(coeficiente_B_e_erros$sse)]))
# # tamanho_serie =  as.double(as.character(coeficiente_B_e_erros$tamanho_serie[which.min(coeficiente_B_e_erros$sse)]))
# mape =  as.double(as.character(coeficiente_B_e_erros$mape[which.min(coeficiente_B_e_erros$sse)]))
# volatilidade =  as.double(as.character(coeficiente_B_e_erros$volatilidade[which.min(coeficiente_B_e_erros$sse)]))
# 
# eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
# exponencial = resultado_funcao_exponencial(eixo_x_frequencias)
# png(filename=paste("Imagem_",i,"_mape_",mape,".png",sep=""),bg="transparent")

# 
# png(filename=paste(mape,".png",sep=""),bg="transparent")
# plot_previsao_com_B_e_exponencial(eixo_x_frequencias,exponencial)
# dev.off()