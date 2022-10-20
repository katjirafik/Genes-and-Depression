library(ggplot2)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(carData)
library(car)
library(MASS)

depr <- read.csv('alldepr_sympt.csv', header = TRUE)
depr$ballsst <- as.integer(depr$ballsst)
str(depr)
colSums (is.na(depr))

#шкала Бека

modB <- glm.nb(ballsbeck ~ DAT1+DRD2+DRD4Pr+DRD4E3+COMT+SERThl+STIN2+HTR1A+HTR2A+HTR1B+OXTR, data = depr, link = 'log')

drop1 (modB, test = 'Chi')
modB2 <- update(modB, .~. -DRD4E3)
drop1 (modB2, test = 'Chi')
modB3 <- update(modB2, .~. -DRD2)
drop1 (modB3, test = 'Chi')
modB4 <- update(modB3, .~. -HTR1B)
drop1 (modB4, test = 'Chi')
modB5 <- update(modB4, .~. -DAT1)
drop1 (modB5, test = 'Chi')
modB6 <- update(modB5, .~. -SERThl)
drop1 (modB6, test = 'Chi')
modB7 <- update(modB6, .~. -HTR1A)
drop1 (modB7, test = 'Chi')
modB8 <- update(modB7, .~. -DRD4Pr)
drop1 (modB8, test = 'Chi')
modB9 <- update(modB8, .~. -OXTR)
drop1 (modB9, test = 'Chi')
modB10 <- update(modB9, .~. -STIN2)
drop1 (modB10, test = 'Chi')

AIC (modB, modB2, modB3, modB4, modB5, modB6, modB7)

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

modH <- glm.nb(ballsham ~ DAT1+DRD2+DRD4Pr+DRD4E3+COMT+SERThl+STIN2+HTR1A+HTR2A+HTR1B+OXTR, data = depr, link = 'log')

drop1 (modH, test = 'Chi')

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

modST <- glm.nb(ballsst ~ DAT1+DRD2+DRD4Pr+DRD4E3+COMT+SERThl+STIN2+HTR1A+HTR2A+HTR1B+OXTR, data = depr, link = 'log')

drop1 (modST, test = 'Chi')
modST2 <- update(modST, .~. -SERThl)
drop1 (modST2, test = 'Chi')
modST3 <- update(modST2, .~. -DRD2)
drop1 (modST3, test = 'Chi')
modST4 <- update(modST3, .~. -DRD4E3)
drop1 (modST4, test = 'Chi')
modST5 <- update(modST4, .~. -COMT)
drop1 (modST5, test = 'Chi')
modST6 <- update(modST5, .~. -DAT1)
drop1 (modST6, test = 'Chi')
modST7 <- update(modST6, .~. -HTR1B)
drop1 (modST7, test = 'Chi')


AIC (modST, modST2, modST3, modST4, modST5, modST6, modST7)

drop1 (modST7, test = 'Chi')

#Проверка на нелинейность связи (есть нелинейность)
modST7_diag <- data.frame(.fitted = fitted(modST7, type = 'response'),
                          .resid_p = resid(modST7, type = 'pearson'))
ggplot(modST7_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess')

#сверхдисперсия
overdisp_fun(modST7)

#График остатков

modST7_diag <- data.frame(.fitted = predict(modST7, type = 'response'),
                          .resid_p = residuals(modST7, type = 'pearson'))
gg_resid <- ggplot(modST7_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

# Личностная тревога
modLT <- glm.nb(ballslt ~ DAT1+DRD2+DRD4Pr+DRD4E3+COMT+SERThl+STIN2+HTR1A+HTR2A+HTR1B+OXTR, data = depr, link = 'log')

drop1 (modLT, test = 'Chi')
modLT2 <- update(modLT, .~. -COMT)
drop1 (modLT2, test = 'Chi')
modLT3 <- update(modLT2, .~. -HTR2A)
drop1 (modLT3, test = 'Chi')
modLT4 <- update(modLT3, .~. -SERThl)
drop1 (modLT4, test = 'Chi')
modLT5 <- update(modLT4, .~. -OXTR)
drop1 (modLT5, test = 'Chi')
modLT6 <- update(modLT5, .~. -DRD4E3)
drop1 (modLT6, test = 'Chi')
modLT7 <- update(modLT6, .~. -DRD2)
drop1 (modLT7, test = 'Chi')
modLT8 <- update(modLT7, .~. -HTR1B)
drop1 (modLT8, test = 'Chi')
modLT9 <- update(modLT8, .~. -STIN2)
drop1 (modLT9, test = 'Chi')
modLT10 <- update(modLT9, .~. -DAT1)
drop1 (modLT10, test = 'Chi')

AIC (modLT, modLT2, modLT3, modLT4, modLT5, modLT6, modLT7,modLT8, modLT9, modLT10)

drop1 (modLT10, test = 'Chi')

#Проверка на нелинейность связи (есть нелинейность)
modLT9_diag <- data.frame(.fitted = fitted(modLT9, type = 'response'),
                         .resid_p = resid(modLT9, type = 'pearson'))
ggplot(modLT9_diag, aes(x = .fitted, y = .resid_p)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'loess')

#сверхдисперсия
overdisp_fun(modLT9)

#График остатков

modLT9_diag <- data.frame(.fitted = predict(modLT, type = 'response'),
                         .resid_p = residuals(modLT, type = 'pearson'))
gg_resid <- ggplot(modLT9_diag, aes(x =.fitted, y = .resid_p)) + 
  geom_point() +
  geom_hline(yintercept = 0) 
gg_resid

#Графики распределений

ggplot(data = depr, aes(x = HTR1A, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = 'HTR1A генотипы')

ggplot(data = depr, aes(x = HTR2A, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = 'HTR2A генотипы')

ggplot(data = depr, aes(x = OXTR, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = 'OXTR генотипы')

ggplot(data = depr, aes(x = STIN2, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = 'SLC6A4 16-17 п.н.VNTR генотипы')

ggplot(data = depr, aes(x = DRD4Pr, y = ballsst)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Ситуационная тревога', x = 'DRD4 120 п.н.VNTR генотипы')

ggplot(data = depr, aes(x = HTR1A, y = ballslt)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Личностная тревога', x = 'HTR1A генотипы')

ggplot(data = depr, aes(x = DRD4Pr, y = ballslt)) +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1)) +
  labs (y = 'Личностная тревога', x = 'DRD4 120 п.н.VNTR генотипы')

