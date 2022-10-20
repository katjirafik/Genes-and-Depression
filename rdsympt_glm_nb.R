ibrary(ggplot2)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(carData)
library(car)
library(MASS)

recdepr <- read.csv('rd_symptoms.csv', header = TRUE)
str(recdepr)
colSums (is.na(recdepr))

#шкала Бека

modB <- glm.nb(ballsbeck ~ DAT1+DRD2+DRD4Pr+DRD4E3+COMT+SERThl+STIN2+HTR1A+HTR2A+HTR1B+OXTR, data = recdepr, link = 'log')

drop1 (modB, test = 'Chi')

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
gg_resid <- ggplot(modB_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

#шкала Гамильтона

modH <- glm.nb(ballsham ~ DAT1+DRD2+DRD4Pr+DRD4E3+COMT+SERThl+STIN2+HTR1A+HTR2A+HTR1B+OXTR, data = recdepr, link = 'log')

drop1 (modH, test = 'Chi')
modH2 <- update(modH, .~. -DRD4Pr)
drop1 (modH2, test = 'Chi')


#Проверка на нелинейность связи
modH_diag <- data.frame(.fitted = fitted(modH, type = 'response'),
                          .resid_p = resid(modH, type = 'pearson'))
ggplot(modH_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess')

#сверхдисперсия
overdisp_fun(modH)

#График остатков

modH_diag <- data.frame(.fitted = predict(modH, type = 'response'),
                        .resid_p = residuals(modH, type = 'pearson'))
gg_resid <- ggplot(modH_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

#Ситуационная тревога

modST <- glm.nb(ballsst ~ DAT1+DRD2+DRD4Pr+DRD4E3+COMT+SERThl+STIN2+HTR1A+HTR2A+HTR1B+OXTR, data = recdepr, link = 'log')

drop1 (modST, test = 'Chi')
modST2 <- update(modST, .~. -DAT1)
drop1 (modST2, test = 'Chi')
modST3 <- update(modST2, .~. -DRD4Pr)
drop1 (modST3, test = 'Chi')
modST4 <- update(modST3, .~. -SERThl)
drop1 (modST4, test = 'Chi')
modST5 <- update(modST4, .~. -COMT)
drop1 (modST5, test = 'Chi')
modST6 <- update(modST5, .~. -HTR1B)
drop1 (modST6, test = 'Chi')
modST7 <- update(modST6, .~. -DRD4E3)
drop1 (modST7, test = 'Chi')
modST8 <- update(modST7, .~. -DRD2)
drop1 (modST8, test = 'Chi')

AIC (modST, modST2, modST3, modST4, modST5, modST6, modST7, modST8)

drop1 (modST8, test = 'Chi')

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
gg_resid <- ggplot(modH_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

# Личностная тревога
modLT <- glm.nb(ballslt ~ DAT1+DRD2+DRD4Pr+DRD4E3+COMT+SERThl+STIN2+HTR1A+HTR2A+HTR1B+OXTR, data = recdepr, link = 'log')

drop1 (modLT, test = 'Chi')

#Проверка на нелинейность связи (есть нелинейность)
modLT_diag <- data.frame(.fitted = fitted(modLT, type = 'response'),
                          .resid_p = resid(modLT, type = 'pearson'))
ggplot(modLT_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess')

#сверхдисперсия
overdisp_fun(modLT)

#График остатков

modLT_diag <- data.frame(.fitted = predict(modLT, type = 'response'),
                          .resid_p = residuals(modLT, type = 'pearson'))
gg_resid <- ggplot(modH_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

#Графики распределений

ggplot(data = recdepr, aes(x = HTR1A, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = 'HTR1A генотипы')

ggplot(data = recdepr, aes(x = HTR2A, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = 'HTR2A генотипы')

ggplot(data = recdepr, aes(x = OXTR, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = 'OXTR генотипы')

ggplot(data = recdepr, aes(x = STIN2, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = '5HTT 16-17 п.н.VNTR генотипы')

