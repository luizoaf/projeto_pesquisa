require(minpack.lm)
dados = read.csv(file="pior_1.csv")
eixo_x_frequencias = dados$eixo_x_frequencias
alvo = dados$alvo
# eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
# alvo = funcao_distribuicao_probabilidade(serie)$frequencia_eixo_y

temp = data.frame( x = eixo_x_frequencias,y = alvo)
# temp_norm = normalizeData(temp[,2],type="0_1")
# temp = data.frame(x = temp[,1],y = temp_norm[,1])

# fit non-linear model
# mod <- nls(y ~ exp(a + b * x), data = temp, start = list(a = 0, b = 0))

# f <- function(a,b,x) {exp(a + b * x) } # 0.0602209
f <- function(b,x,a) {a*exp(-b*x) } # 0.0602209

mod <- nlsLM(y ~ f(b,x,a), data = temp, 
             start = list(a=0.1,b = 0.1),algorithm = "LM",
             control = list(maxiter = 500,
                            tol = 1e-05, minFactor = 0.0009765625, printEval = F, 
                            warnOnly = FALSE, trace = T))
# add fitted curve
previsao = predict(mod, list(x = temp$x))
# previsao = as.vector(denormalizeData(previsao,getNormParameters(temp_norm))) 
#,ylim=c(0,1.5)
# plot data
sse = sum(( previsao-alvo)^2)
plot(main = paste("SSE: ",sse),eixo_x_frequencias, alvo)
lines(previsao~eixo_x_frequencias , col="green")
mod


coef(mod)
confint(mod)
deviance(mod)
df.residual(mod)
fitted(mod)
formula(mod)
logLik(mod)
predict(mod)
print(mod)
profile(mod)
residuals(mod)
summary(mod)
update(mod)
vcov(mod)
weights(mod)

# ?nls
# mod$control
# 
# volatilidade = calcula_volatilidade(serie)
# coeficiente_B = coeficiente_B_e_erros_menor_sse$coeficiente_B
# a = coeficiente_B_e_erros_menor_sse$a
# previsao = resultado_funcao_exponencial(a,eixo_x_frequencias,coeficiente_B)
# 
# # png(filename=paste(sse,".png",sep=""),bg="transparent")
# print(plot_previsao_com_B_e_exponencial(eixo_x_frequencias,previsao))
# a       b 
# 1.16426 0.04572 

