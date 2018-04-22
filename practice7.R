library('MASS') # набор данных Boston
library('splines') # сплайны
library('gam') # обобщённые аддитивные модели
library('akima') # график двумерной плоскости
library('ggplot2') # красивые графики

my.seed <- 1
data(Boston)
attach(Boston)

gp <- ggplot(data = Boston, aes(x = nox, y = crim))
gp <- gp + geom_point() + geom_abline(slope = 0, intercept = 30, col = 'red')
gp

#полином 3ой степени
fit <- lm(crim ~ poly(nox, 3), data = Boston)
round(coef(summary(fit)), 2)
# границы изменения переменной nox
noxlims <- range(nox)
# значения nox, для которых делаем прогноз 
nox.grid <- seq(from = noxlims[1], to = noxlims[2], length.out = 506)
# рассчитать прогнозы и их стандартные ошибки
preds <- predict(fit, newdata = list(nox = nox.grid), se = T)

# границы доверительного интервала для уровня пеступности
se.bands <- cbind(lower.bound = preds$fit - 2*preds$se.fit,
                  upper.bound = preds$fit + 2*preds$se.fit)
# смотрим результат
round(head(se.bands), 2)

par(mfrow=c(1,1))
plot(nox, crim, xlim = noxlims, cex = 0.5, col = 'darkgrey')

title('Локальная регрессия')

# подгоняем модель c окном 0.8
fit1 <- loess(crim ~ nox, span = 0.8, data = Boston)


# рисум модели
lines(nox.grid, predict(fit1, data.frame(nox = nox.grid)),
      col = 'red', lwd = 2)

# доверительные интервалы прогноза
matlines(x = nox.grid, y = se.bands, lwd = 1, col = 'black', lty = 3)
# легенда
legend('topright', 
       c('s = 0.8'),
       col = c('red'), lty = 1, lwd = 2, cex = 0.8)

gam.lo <- gam(crim ~  lo(nox, span = 0.7) , 
              data = Boston)
par(mfrow = c(1, 1))
plot(gam.lo, se = T, col = 'green')

summary(gam.lo)
