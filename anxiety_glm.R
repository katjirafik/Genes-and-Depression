library(ggplot2)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(carData)
library(car)
library(dplyr)

# Genotypes co-dominant
anxiety <- read.csv('anxiety_glm.csv', header = TRUE)
str(anxiety)
head (anxiety)
colSums (is.na(anxiety))
anxiety$samp <- ifelse (test = anxiety$sample == 'd', yes = 1, no = 0)
mod <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = anxiety)
Anova(mod)

drop1 (mod, test = 'Chi')
mod2 <- update(mod, .~. -HTR1B)
drop1 (mod2, test = 'Chi')
mod3 <- update(mod2, .~. -DRD2)
drop1 (mod3, test = 'Chi')
mod4 <- update(mod3, .~. -Stin2)
drop1 (mod4, test = 'Chi')
mod5 <- update(mod4, .~. -DRD4E3)
drop1 (mod5, test = 'Chi')
mod6 <- update(mod5, .~. -COMT)
drop1 (mod6, test = 'Chi')
mod7 <- update(mod6, .~. -OXTR)
drop1 (mod7, test = 'Chi')
mod8 <- update(mod7, .~. -HTR1A)
drop1 (mod8, test = 'Chi')
mod9 <- update(mod8, .~. -HTR2A)
drop1 (mod9, test = 'Chi')
mod10 <- update(mod9, .~. -DRD4Pr)
drop1 (mod10, test = 'Chi')
mod11 <- update(mod10, .~. -SERTPr)
drop1 (mod11, test = 'Chi')

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11)

drop1 (mod11, test = 'Chi')
summary(mod11)
Anova(mod11)

#Проверка на нелинейность связи
mod11_diag <- data.frame(.fitted = fitted(mod11, type = 'response'),
                         .resid_p = resid(mod11, type = 'pearson'))
ggplot (mod11_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

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

overdisp_fun(mod11) 

# Genotypes dominant

anxiety <- read.csv('anxiety_glmdom.csv', header = TRUE)
str(anxiety)
head (anxiety)
colSums (is.na(anxiety))
anxiety$samp <- ifelse (test = anxiety$sample == 'd', yes = 1, no = 0)
moddom <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = anxiety)
Anova(moddom)

drop1 (moddom, test = 'Chi')
moddom2 <- update(moddom, .~. -HTR1B)
drop1 (moddom2, test = 'Chi')
moddom3 <- update(moddom2, .~. -Stin2)
drop1 (moddom3, test = 'Chi')
moddom4 <- update(moddom3, .~. -HTR1A)
drop1 (moddom4, test = 'Chi')
moddom5 <- update(moddom4, .~. -DRD4E3)
drop1 (moddom5, test = 'Chi')
moddom6 <- update(moddom5, .~. -DRD2)
drop1 (moddom6, test = 'Chi')
moddom7 <- update(moddom6, .~. -OXTR)
drop1 (moddom7, test = 'Chi')
moddom8 <- update(moddom7, .~. -COMT)
drop1 (moddom8, test = 'Chi')
moddom9 <- update(moddom8, .~. -HTR2A)
drop1 (moddom9, test = 'Chi')

AIC(moddom, moddom2, moddom3, moddom4, moddom5, moddom6, moddom7, moddom8, moddom9)

drop1 (moddom8, test = 'Chi')
summary(moddom8)
Anova(moddom8)

#Проверка на нелинейность связи
moddom8_diag <- data.frame(.fitted = fitted(moddom8, type = 'response'),
                            .resid_p = resid(moddom8, type = 'pearson'))
ggplot (moddom8_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

# Функция Бена Болкера для проверки на сверхдисперсию

overdisp_fun(moddom8) 

# Genotypes recessive

anxiety <- read.csv('anxiety_glmrec.csv', header = TRUE)
str(anxiety)
head (anxiety)
colSums (is.na(anxiety))
anxiety$samp <- ifelse (test = anxiety$sample == 'd', yes = 1, no = 0)
modrec <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = anxiety)
Anova(modrec)

drop1 (modrec, test = 'Chi')
modrec2 <- update(modrec, .~. -DRD4Pr)
drop1 (modrec2, test = 'Chi')
modrec3 <- update(modrec2, .~. -DRD4E3)
drop1 (modrec3, test = 'Chi')
modrec4 <- update(modrec3, .~. -HTR1B)
drop1 (modrec4, test = 'Chi')
modrec5 <- update(modrec4, .~. -DRD2)
drop1 (modrec5, test = 'Chi')
modrec6 <- update(modrec5, .~. -COMT)
drop1 (modrec6, test = 'Chi')
modrec7 <- update(modrec6, .~. -SERTPr)
drop1 (modrec7, test = 'Chi')
modrec8 <- update(modrec7, .~. -Stin2)
drop1 (modrec8, test = 'Chi')
modrec9 <- update(modrec8, .~. -HTR1A)
drop1 (modrec9, test = 'Chi')
modrec10 <- update(modrec9, .~. -OXTR)
drop1 (modrec10, test = 'Chi')

AIC(modrec, modrec2, modrec3, modrec4, modrec5, modrec6, modrec7, modrec8, modrec9, modrec10)

drop1 (modrec9, test = 'Chi')
summary(modrec9)
Anova(modrec9)

#Проверка на нелинейность связи
modrec9_diag <- data.frame(.fitted = fitted(modrec9, type = 'response'),
                           .resid_p = resid(modrec9, type = 'pearson'))
ggplot (modrec9_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

# Функция Бена Болкера для проверки на сверхдисперсию

overdisp_fun(modrec9) 

# Genotypes over-dominant

anxiety <- read.csv('anxiety_glmoverdom.csv', header = TRUE)
str(anxiety)
head (anxiety)
colSums (is.na(anxiety))
anxiety$samp <- ifelse (test = anxiety$sample == 'd', yes = 1, no = 0)
modovdom <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = anxiety)
Anova(modovdom)

drop1 (modovdom, test = 'Chi')
modovdom2 <- update(modovdom, .~. -OXTR)
drop1 (modovdom2, test = 'Chi')
modovdom3 <- update(modovdom2, .~. -Stin2)
drop1 (modovdom3, test = 'Chi')
modovdom4 <- update(modovdom3, .~. -HTR1B)
drop1 (modovdom4, test = 'Chi')
modovdom5 <- update(modovdom4, .~. -HTR2A)
drop1 (modovdom5, test = 'Chi')
modovdom6 <- update(modovdom5, .~. -DRD4E3)
drop1 (modovdom6, test = 'Chi')
modovdom7 <- update(modovdom6, .~. -COMT)
drop1 (modovdom7, test = 'Chi')
modovdom8 <- update(modovdom7, .~. -DRD2)
drop1 (modovdom8, test = 'Chi')
modovdom9 <- update(modovdom8, .~. -HTR1A)
drop1 (modovdom9, test = 'Chi')
modovdom10 <- update(modovdom9, .~. -SERTPr)
drop1 (modovdom10, test = 'Chi')

AIC(modovdom, modovdom2, modovdom3, modovdom4, modovdom5, modovdom6, modovdom7, modovdom8, modovdom9, modovdom10)

drop1 (modovdom10, test = 'Chi')
summary(modovdom10)
Anova(modovdom10)

#Проверка на нелинейность связи
modovdom10_diag <- data.frame(.fitted = fitted(modovdom10, type = 'response'),
                             .resid_p = resid(modovdom10, type = 'pearson'))
ggplot (modovdom10_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

# Функция Бена Болкера для проверки на сверхдисперсию

overdisp_fun(modovdom10) 

# Genotypes log-additive

anxiety <- read.csv('anxiety_glmadd.csv', header = TRUE)
str(anxiety)
head (anxiety)
colSums (is.na(anxiety))
anxiety$samp <- ifelse (test = anxiety$sample == 'd', yes = 1, no = 0)
modadd <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = anxiety)
Anova(modadd)

drop1 (modadd, test = 'Chi')
modadd2 <- update(modadd, .~. -HTR1B)
drop1 (modadd2, test = 'Chi')
modadd3 <- update(modadd2, .~. -Stin2)
drop1 (modadd3, test = 'Chi')
modadd4 <- update(modadd3, .~. -DRD2)
drop1 (modadd4, test = 'Chi')
modadd5 <- update(modadd4, .~. -DRD4E3)
drop1 (modadd5, test = 'Chi')
modadd6 <- update(modadd5, .~. -COMT)
drop1 (modadd6, test = 'Chi')
modadd7 <- update(modadd6, .~. -OXTR)
drop1 (modadd7, test = 'Chi')
modadd8 <- update(modadd7, .~. -HTR1A)
drop1 (modadd8, test = 'Chi')
modadd9 <- update(modadd8, .~. -SERTPr)
drop1 (modadd9, test = 'Chi')
modadd10 <- update(modadd9, .~. -DRD4Pr)
drop1 (modadd10, test = 'Chi')

AIC(modadd, modadd2, modadd3, modadd4, modadd5, modadd6, modadd7, modadd8, modadd9, modadd10)

drop1 (modadd9, test = 'Chi')
summary(modadd9)
Anova(modadd9)

#Проверка на нелинейность связи
modadd9_diag <- data.frame(.fitted = fitted(modadd9, type = 'response'),
                           .resid_p = resid(modadd9, type = 'pearson'))
ggplot (modadd9_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

# Функция Бена Болкера для проверки на сверхдисперсию

overdisp_fun(modadd9) 
