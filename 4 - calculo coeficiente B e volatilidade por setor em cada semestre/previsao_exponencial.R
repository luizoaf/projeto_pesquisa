# require(minpack.lm)
# dados = read.csv(file="pior_1.csv")
# eixo_x_frequencias = dados$eixo_x_frequencias
# alvo = dados$alvo
eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
alvo = funcao_distribuicao_probabilidade(serie)$frequencia_eixo_y

freq_alvo = data.frame( x = eixo_x_frequencias,y = alvo)
# freq_alvo_norm = normalizeData(freq_alvo[,2],type="0_1")
# freq_alvo = data.frame(x = freq_alvo[,1],y = freq_alvo_norm[,1])

funcao_exp <- function(b,x,a) {a*exp(-b*x) }

mod <- nlsLM(y ~ funcao_exp(b,x,a), data = freq_alvo, 
             start = list(a=0.1,b = 0.1),algorithm = "LM",
             control = list(maxiter = 500,
             tol = 1e-05, minFactor = 0.0009765625, printEval = F, 
             warnOnly = FALSE, trace = T))
previsao = predict(mod, list(x = freq_alvo$x))
# previsao = as.vector(denormalizeData(previsao,getNormParameters(freq_alvo_norm))) 

sse = sum(( previsao-alvo)^2)
# plot(main = paste("SSE: ",sse),eixo_x_frequencias, alvo)
# lines(previsao~eixo_x_frequencias , col="green")
a = coef(mod)[1]
coeficiente_B = coef(mod)[2]
volatilidade = calcula_volatilidade(serie)

plot_previsao_com_B_e_exponencial(eixo_x_frequencias,previsao)
