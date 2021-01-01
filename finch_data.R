x = read.csv('finchBeaks.csv', stringsAsFactors = FALSE)
y = read.csv('finchBeaks.csv', na.strings=c("","NA","NA/n","NA\n","NA ")
library(ggplot2)

ggplot(x, aes(x=offspring)) + 
  geom_histogram()

offspring_filtered = subset(y, subset = x$offspring != NA)
hist(offspring_filtered)
offspring_filtered

curve(dnorm(y$offspring, 
            mean = mean(y$offspring),
            sd = mean(y$offspring)
            ),
      from = min(y$offspring),
      to = max(y$offspring))

mean(x$offspring, na.rm = T)
median(x$offspring, na.rm = T)


min(x$offspring, na.rm = T)
max(x$offspring, na.rm = T)

hist(x$offspring,
     main = 'Offspring beak size distribution',
     xlab = 'Beak size, mm',
     breaks = seq(min(x$offspring, na.rm = T), 
                  max(x$offspring, na.rm = T),
                  0.2))

plot(offspring ~ parents.avg,
     data = x,
     main = 'Offspring vs Parents beak size',
     xlab = 'Parents beak size, mm',
     ylab = 'Offspring beak size, mm')

fit <- lm(offspring ~ parents.avg, 
          data = x)
abline(fit)

plot(residuals(fit) ~ predict(fit),
     xlab = 'Predicted values',
     ylab = 'Residual values',
     main = 'Residuals plot')

cor(x$offspring, x$parents.avg, use = 'na.or.complete') ^ 2

offspring_randomized = sample(x$offspring)

plot(offspring_randomized ~ x$parents.avg,
     main = 'Offspring (sampled) vs Parents beak size',
     xlab = 'Parents beak size, mm',
     ylab = 'Randomized offspring beak size, mm')

fit_2 <- lm(offspring_randomized ~ x$parents.avg)
abline(fit_2)

gradient(fit_2)

trials = 10000
result = numeric(trials)

for (i in 1:trials) {
  result[i] = cor(sample(x$offspring), x$parents.avg, 
                  use = 'na.or.complete') ^ 2
}
mean(result)

t.test(x$survivors, x$preDrought)

t.test(x$postDroughtGeneration, x$preDrought, alternative = 'greater')

par(mfrow = c(1, 2))
hist(x$preDrought,
     main = 'Pre-drought',
     xlab = 'Beak size, mm')
hist(x$postDroughtGeneration,
     main = 'Post-drought',
     xlab = 'Beak size, mm')

mean(x$preDrought)
mean(x$postDroughtGeneration, na.rm = T)
sd(x$preDrought)
sd(x$postDroughtGeneration, na.rm = T)
median(x$preDrought)
median(x$postDroughtGeneration, na.rm = T)
