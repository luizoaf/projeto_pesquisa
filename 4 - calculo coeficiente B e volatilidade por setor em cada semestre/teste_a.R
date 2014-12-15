source("../1_funcoes.R")
dados = read.csv(file="pior_4.csv")[,-1]
serie = dados$alvo
dados$eixo_x_frequencias = as.numeric(as.character(dados$valor_serie_retorno_eixo_x))
dados$frequencia_eixo_y = as.numeric(as.character(dados$frequencia_eixo_y))
dados$exponencial = as.numeric(as.character(dados$exponencial))


# lines(eixo_x_frequencias~exponencial,col="green")

serie = dados$frequencia_eixo_y
source("3_define_coeficiente_B.R")


plot(dados[,1:2],type="l")
lines(dados[,c(1,3)],col="red")

# eixo_x_frequencias

sse = sum(( dados$alvo-dados$previsao)^2)


