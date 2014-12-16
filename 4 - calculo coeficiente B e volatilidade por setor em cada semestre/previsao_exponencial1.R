f <- function(x1,x2,a,b1,b2) {a * (b1^x1) * (b2^x2) }

# generate some data
x1 <- 1:10
x2 <- c(2,3,5,4,6,7,8,10,9,11)
set.seed(44)
y <- 2*exp(x1/4) + rnorm(10)*2
dat <- data.frame(x1,x2, y)

# fit a nonlinear model
dat = as.data.frame(alvo)
fm <- nls(dat$alvo ~ f(x1,x2,a,b1,b2), data = dat, start = c(a=1, b1=1,b2=1))

# get estimates of a, b
co <- coef(fm)














# x1 is the variable we want to show on the x-axis
plot(eixo_x_frequencias~alvo)

# # generate data
# beta <- 0.05
# n <- 100
# temp <- data.frame(y = exp(beta * seq(n)) + rnorm(n), x = seq(n))
# 
# # plot data
# plot(temp$x, temp$y)
# 
# # fit non-linear model
# mod <- nls(y ~ exp(a + b * x), data = temp, start = list(a = 0, b = 0))
# 
# # add fitted curve
# lines(temp$x, predict(mod, list(x = temp$x)))
# 
# 

# generate data
# beta <- 0.05
# n <- 100
# temp <- data.frame(y = exp(beta * seq(n)) + rnorm(n), x = seq(n))
temp = data.frame(y = alvo, x = 1:length(alvo))


# fit non-linear model
# mod <- nls(y ~ exp(a + b * x), data = temp, start = list(a = 0, b = 0))

# f <- function(a,b,x) {exp(a + b * x) } # 0.0602209
f <- function(b,x,a) {exp(-b* x)*a } # 0.0602209
mod <- nls(y ~ f(b,x,a), data = temp, start = list(a=0.1,b = 0.1))
# add fitted curve
previsao = predict(mod, list(x = temp$x))
                   
# plot data
plot(temp$x, temp$y,ylim=c(0,1.5))
lines(temp$x, previsao)
sum(( previsao-alvo)^2)














# # generate a range of values for x1 in small increments to create a smooth line
# xRange <- seq(min(x1), max(x1), length.out = 1000)
# 
# # generate the predicted y values (for a test value of x2 = 1)
# yValues <- predict(fm, newdata=list(x1=xRange, x2=1))
# 
# #draw the curve
# lines(xRange, yValues, col="blue")
# 
# # generate the predicted y values (for a test value of x2 = 0)
# yValues <- predict(fm, newdata=list(x1=xRange, x2=0))
# 
# #draw the curve
# lines(xRange, yValues, col="red")
