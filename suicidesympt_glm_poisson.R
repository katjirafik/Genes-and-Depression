library(ggplot2)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(carData)
library(car)

suicide <- read.csv('suicide_symptoms1.csv')
str(suicide)
colSums (is.na(suicide))

#шкала Бека

mod <- glm(ballsbeck ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+HTR1A+HTR2A+HTR1B+BDNF, data = suicide, family = 'poisson')
summary(mod)
Anova(mod)

drop1(mod, test = 'Chi')
mod2 <- update(mod, .~. -HTR2A)
drop1(mod2, test = 'Chi')
mod3 <- update(mod2, .~. -DRD4Pr)
drop1(mod3, test = 'Chi')
mod4 <- update(mod3, .~. -HTR1B)
drop1(mod4, test = 'Chi')


AIC(mod, mod2, mod3, mod4)

summary(mod4)
Anova(mod4)
drop1(mod4, test = 'Chi')

#Проверка на нелинейность связи
mod4_diag <- data.frame(.fitted = fitted(mod4, type = 'response'),
                        .resid_p = resid(mod4, type = 'pearson'))
ggplot(mod4_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess')

# Функция Бена Болкера для проверки на сверхдисперсию
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  if (inherits (model, 'negbin')) rdf <- rdf - 1
  rp <- residuals(model, type = 'pearson')
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail = FALSE)
  c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)
}

overdisp_fun(mod4) 
  