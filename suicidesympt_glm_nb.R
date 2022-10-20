library(ggplot2)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(carData)
library(car)
library(MASS)

suicide <- read.csv('diss_suicide_sympt.csv', header = TRUE)
str(suicide)
colSums (is.na(suicide))

#шкала Бека

modB <- glm.nb(ballsbeck ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+Stin2+HTR1A+HTR2A+HTR1B+OXTR, data = suicide, link = 'log')

drop1 (modB, test = 'Chi')
mod2 <- update(modB, .~. -Stin2)
drop1 (mod2, test = 'Chi')
mod3 <- update(mod2, .~. -HTR2A)
drop1 (mod3, test = 'Chi')
mod4 <- update(mod3, .~. -SERThl)
drop1 (mod4, test = 'Chi')
mod5 <- update(mod4, .~. -HTR1B)
drop1 (mod5, test = 'Chi')
mod6 <- update(mod5, .~. -HTR1A)
drop1 (mod6, test = 'Chi')
mod7 <- update(mod6, .~. -DRD4Pr)
drop1 (mod7, test = 'Chi')
mod8 <- update(mod7, .~. -DAT1)
drop1 (mod8, test = 'Chi')
mod9 <- update(mod8, .~. -COMT)
drop1 (mod9, test = 'Chi')
mod10 <- update(mod9, .~. -DRD4e3)
drop1 (mod10, test = 'Chi')
mod11 <- update(mod10, .~. -DRD2)
drop1 (mod11, test = 'Chi')

AIC (mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11)

#Проверка на нелинейность связи
modB_diag <- data.frame(.fitted = fitted(modB, type = 'response'),
                        .resid_p = resid(modB, type = 'pearson'))
ggplot(modB_diag, aes(x = .fitted, y = .resid_p)) +
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

overdisp_fun(modB)

#График остатков

modB_diag <- data.frame(.fitted = predict(modB, type = 'response'),
                       .resid_p = residuals(modB, type = 'pearson'))
gg_resid <- ggplot(mod_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

#шкала Гамильтона

modH <- glm.nb(ballsham ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+Stin2+HTR1A+HTR2A+HTR1B+OXTR, data = suicide, link = 'log')

drop1 (modH, test = 'Chi')
modH2 <- update(modH, .~. -OXTR)
drop1 (modH2, test = 'Chi')
modH3 <- update(modH2, .~. -DAT1)
drop1 (modH3, test = 'Chi')
modH4 <- update(modH3, .~. -HTR1B)
drop1 (modH4, test = 'Chi')
modH5 <- update(modH4, .~. -HTR1A)
drop1 (modH5, test = 'Chi')
modH6 <- update(modH5, .~. -DRD4e3)
drop1 (modH6, test = 'Chi')
modH7 <- update(modH6, .~. -Stin2)
drop1 (modH7, test = 'Chi')
modH8 <- update(modH7, .~. -HTR2A)
drop1 (modH8, test = 'Chi')
modH9 <- update(modH8, .~. -DRD2)
drop1 (modH9, test = 'Chi')
modH10 <- update(modH9, .~. -COMT)
drop1 (modH10, test = 'Chi')
modH11 <- update(modH10, .~. -DRD4Pr)
drop1 (modH11, test = 'Chi')

AIC (modH, modH2, modH3, modH4, modH5, modH6, modH7, modH8, modH9, modH10, modH11)

drop1 (modH11, test = 'Chi')

#Проверка на нелинейность связи
modH10_diag <- data.frame(.fitted = fitted(modH10, type = 'response'),
                       .resid_p = resid(modH10, type = 'pearson'))
ggplot(modH10_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess')

#сверхдисперсия
overdisp_fun(mod10)

#График остатков

modH_diag <- data.frame(.fitted = predict(modH, type = 'response'),
                       .resid_p = residuals(modH, type = 'pearson'))
gg_resid <- ggplot(modH_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

#Ситуационная тревога

modST <- glm.nb(ballsst ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+Stin2+HTR1A+HTR2A+HTR1B+OXTR, data = suicide, link = 'log')

drop1 (modST, test = 'Chi')
modST2 <- update(modST, .~. -DAT1)
drop1 (modST2, test = 'Chi')
modST3 <- update(modST2, .~. -OXTR)
drop1 (modST3, test = 'Chi')
modST4 <- update(modST3, .~. -HTR1B)
drop1 (modST4, test = 'Chi')
modST5 <- update(modST4, .~. -DRD2)
drop1 (modST5, test = 'Chi')
modST6 <- update(modST5, .~. -COMT)
drop1 (modST6, test = 'Chi')
modST7 <- update(modST6, .~. -DRD4Pr)
drop1 (modST7, test = 'Chi')
modST8 <- update(modST7, .~. -Stin2)
drop1 (modST8, test = 'Chi')
modST9 <- update(modST8, .~. -HTR2A)
drop1 (modST9, test = 'Chi')
modST10 <- update(modST9, .~. -SERThl)
drop1 (modST10, test = 'Chi')
modST11 <- update(modST10, .~. -HTR1A)
drop1 (modST11, test = 'Chi')

AIC (modST, modST2, modST3, modST4, modST5, modST6, modST7, modST8, modST9, modST10, modST11)

drop1 (modST10, test = 'Chi')

#Проверка на нелинейность связи (есть нелинейность)
modST8_diag <- data.frame(.fitted = fitted(modST8, type = 'response'),
                       .resid_p = resid(modST8, type = 'pearson'))
ggplot(modST8_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess')

#сверхдисперсия
overdisp_fun(modST8)

#График остатков

modST8_diag <- data.frame(.fitted = predict(modST8, type = 'response'),
                       .resid_p = residuals(modST8, type = 'pearson'))
gg_resid <- ggplot(modST8_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

#Личностная тревога

modLT <- glm.nb(ballslt ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+Stin2+HTR1A+HTR2A+HTR1B+OXTR, data = suicide, link = 'log')

drop1 (modLT, test = 'Chi')
modLT2 <- update(modLT, .~. -HTR1A)
drop1 (modLT2, test = 'Chi')
modLT3 <- update(modLT2, .~. -DRD2)
drop1 (modLT3, test = 'Chi')
modLT4 <- update(modLT3, .~. -DAT1)
drop1 (modLT4, test = 'Chi')
modLT5 <- update(modLT4, .~. -HTR2A)
drop1 (modLT5, test = 'Chi')
modLT6 <- update(modLT5, .~. -DRD4Pr)
drop1 (modLT6, test = 'Chi')
modLT7 <- update(modLT6, .~. -COMT)
drop1 (modLT7, test = 'Chi')

AIC (modLT, modLT2, modLT3, modLT4, modLT5, modLT6, modLT7)

drop1 (modLT7, test = 'Chi')

#Проверка на нелинейность связи
modLT9_diag <- data.frame(.fitted = fitted(modLT9, type = 'response'),
                       .resid_p = resid(modLT9, type = 'pearson'))
ggplot(modLT9_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess')

#сверхдисперсия
overdisp_fun(modLT9)

#График остатков

modLT9_diag <- data.frame(.fitted = predict(modLT9, type = 'response'),
                       .resid_p = residuals(modLT9, type = 'pearson'))
gg_resid <- ggplot(modLT9_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

#Графики распределений

ggplot(data = suicide, aes(x = SERThl, y = ballsham)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Депрессия', x = 'Генотипы 5-HTTLPR+rs25531')

ggplot(data = suicide, aes(x = SERThl, y = ballslt)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Личностная тревога', x = 'Генотипы 5-HTTLPR+rs25531')

ggplot(data = suicide, aes(x = OXTR, y = ballsbeck)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Депрессия', x = 'Генотипы OXTR rs53576')

ggplot(data = suicide, aes(x = OXTR, y = ballslt)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Личностная тревога', x = 'Генотипы OXTR rs53576')
