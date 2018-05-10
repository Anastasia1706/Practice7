library('MASS') # набор данных Boston
library('splines') # сплайны
library('gam') # обобщённые аддитивные модели
library('akima') # график двумерной плоскости
library('ggplot2') # красивые графики

my.seed <- 1
data(Boston)
attach(Boston)

fit_logit <- glm(I(crim > 30) ~ poly(nox, 3), data = Boston, family="gaussian")
fit_logit

gam.lr <- gam(I(crim > 30) ~  s(nox, df = 5) , 
              family = 'binomial', data = Boston)
gam.lr

plot(gam.lr, se = T, col = 'green')
summary(gam.lr)
