library(ggplot2)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(carData)
library(car)
library(dplyr)

# Genotypes co-dominant
depr <- read.csv('DE_glm.csv', header = TRUE)
str(depr)
head (depr)
colSums (is.na(depr))
depr$samp <- ifelse (test = depr$sample == 'd', yes = 1, no = 0)
mod <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = depr)
Anova(mod)

drop1 (mod, test = 'Chi')
mod2 <- update(mod, .~. -HTR1A)
drop1 (mod2, test = 'Chi')
mod3 <- update(mod2, .~. -Stin2)
drop1 (mod3, test = 'Chi')
mod4 <- update(mod3, .~. -DRD4E3)
drop1 (mod4, test = 'Chi')
mod5 <- update(mod4, .~. -HTR1B)
drop1 (mod5, test = 'Chi')
mod6 <- update(mod5, .~. -DRD4Pr)
drop1 (mod6, test = 'Chi')
mod7 <- update(mod6, .~. -SERTPr)
drop1 (mod7, test = 'Chi')
mod8 <- update(mod7, .~. -COMT)
drop1 (mod8, test = 'Chi')
mod9 <- update(mod8, .~. -DRD2)
drop1 (mod9, test = 'Chi')
mod10 <- update(mod9, .~. -OXTR)
drop1 (mod10, test = 'Chi')

AIC(mod, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10)

drop1 (mod10, test = 'Chi')
summary(mod10)
Anova(mod10)

#Проверка на нелинейность связи
mod10_diag <- data.frame(.fitted = fitted(mod10, type = 'response'),
                        .resid_p = resid(mod10, type = 'pearson'))
ggplot (mod10_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
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

overdisp_fun(mod10) 

# Genotypes dominant

depr <- read.csv('DE_glmdom.csv', header = TRUE)
str(depr)
head (depr)
colSums (is.na(suicide))
depr$samp <- ifelse (test = depr$sample == 'd', yes = 1, no = 0)
moddom <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = depr)
Anova(moddom)

drop1 (moddom, test = 'Chi')
moddom2 <- update(moddom, .~. -HTR1A)
drop1 (moddom2, test = 'Chi')
moddom3 <- update(moddom2, .~. -Stin2)
drop1 (moddom3, test = 'Chi')
moddom4 <- update(moddom3, .~. -DRD4Pr)
drop1 (moddom4, test = 'Chi')
moddom5 <- update(moddom4, .~. -SERTPr)
drop1 (moddom5, test = 'Chi')
moddom6 <- update(moddom5, .~. -DRD4E3)
drop1 (moddom6, test = 'Chi')
moddom7 <- update(moddom6, .~. -HTR2A)
drop1 (moddom7, test = 'Chi')
moddom8 <- update(moddom7, .~. -COMT)
drop1 (moddom8, test = 'Chi')
moddom9 <- update(moddom8, .~. -OXTR)
drop1 (moddom9, test = 'Chi')
moddom10 <- update(moddom9, .~. -HTR1B)
drop1 (moddom10, test = 'Chi')
moddom11 <- update(moddom10, .~. -DRD2)
drop1 (moddom11, test = 'Chi')

AIC(moddom, moddom2, moddom3, moddom4, moddom5, moddom6, moddom7, moddom8, moddom9, moddom10, moddom11)

drop1 (moddom10, test = 'Chi')
summary(moddom10)
Anova(moddom10)

#Проверка на нелинейность связи
moddom7_diag <- data.frame(.fitted = fitted(moddom7, type = 'response'),
                           .resid_p = resid(moddom7, type = 'pearson'))
ggplot (moddom7_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

# Функция Бена Болкера для проверки на сверхдисперсию

overdisp_fun(moddom7) 

# Genotypes recessive

depr <- read.csv('DE_glmrec.csv', header = TRUE)
str(depr)
head (depr)
colSums (is.na(depr))
depr$samp <- ifelse (test = depr$sample == 'd', yes = 1, no = 0)
modrec <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = depr)
Anova(modrec)

drop1 (modrec, test = 'Chi')
modrec2 <- update(modrec, .~. -HTR1A)
drop1 (modrec2, test = 'Chi')
modrec3 <- update(modrec2, .~. -Stin2)
drop1 (modrec3, test = 'Chi')
modrec4 <- update(modrec3, .~. -COMT)
drop1 (modrec4, test = 'Chi')
modrec5 <- update(modrec4, .~. -DRD4E3)
drop1 (modrec5, test = 'Chi')
modrec6 <- update(modrec5, .~. -DRD2)
drop1 (modrec6, test = 'Chi')
modrec7 <- update(modrec6, .~. -HTR1B)
drop1 (modrec7, test = 'Chi')
modrec8 <- update(modrec7, .~. -DRD4Pr)
drop1 (modrec8, test = 'Chi')
modrec9 <- update(modrec8, .~. -OXTR)
drop1 (modrec9, test = 'Chi')
modrec10 <- update(modrec9, .~. -DAT1)
drop1 (modrec10, test = 'Chi')
modrec11 <- update(modrec10, .~. -SERTPr)
drop1 (modrec11, test = 'Chi')

AIC(modrec, modrec2, modrec3, modrec4, modrec5, modrec6, modrec7, modrec8, modrec9, modrec10, modrec11)

drop1 (modrec10, test = 'Chi')
summary(modrec10)
Anova(modrec10)

#Проверка на нелинейность связи
modrec10_diag <- data.frame(.fitted = fitted(modrec10, type = 'response'),
                           .resid_p = resid(modrec10, type = 'pearson'))
ggplot (modrec10_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

# Функция Бена Болкера для проверки на сверхдисперсию

overdisp_fun(modrec10) 

# Genotypes over-dominant

depr <- read.csv('DE_glmoverdom.csv', header = TRUE)
str(depr)
head (depr)
colSums (is.na(depr))
depr$samp <- ifelse (test = depr$sample == 'd', yes = 1, no = 0)
modovdom <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = depr)
Anova(modovdom)

drop1 (modovdom, test = 'Chi')
modovdom2 <- update(modovdom, .~. -DRD4Pr)
drop1 (modovdom2, test = 'Chi')
modovdom3 <- update(modovdom2, .~. -DRD4E3)
drop1 (modovdom3, test = 'Chi')
modovdom4 <- update(modovdom3, .~. -HTR1A)
drop1 (modovdom4, test = 'Chi')
modovdom5 <- update(modovdom4, .~. -Stin2)
drop1 (modovdom5, test = 'Chi')
modovdom6 <- update(modovdom5, .~. -SERTPr)
drop1 (modovdom6, test = 'Chi')
modovdom7 <- update(modovdom6, .~. -HTR1B)
drop1 (modovdom7, test = 'Chi')
modovdom8 <- update(modovdom7, .~. -COMT)
drop1 (modovdom8, test = 'Chi')
modovdom9 <- update(modovdom8, .~. -DRD2)
drop1 (modovdom9, test = 'Chi')
modovdom10 <- update(modovdom9, .~. -OXTR)
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

depr <- read.csv('DE_glmadd.csv', header = TRUE)
str(depr)
head (depr)
colSums (is.na(depr))
depr$samp <- ifelse (test = depr$sample == 'd', yes = 1, no = 0)
modadd <- glm(samp ~ DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+Stin2+HTR1A+HTR2A+HTR1B+OXTR, family = binomial(link = 'logit'), data = depr)
Anova(modadd)

drop1 (modadd, test = 'Chi')
modadd2 <- update(modadd, .~. -Stin2)
drop1 (modadd2, test = 'Chi')
modadd3 <- update(modadd2, .~. -HTR1A)
drop1 (modadd3, test = 'Chi')
modadd4 <- update(modadd3, .~. -OXTR)
drop1 (modadd4, test = 'Chi')
modadd5 <- update(modadd4, .~. -DRD4E3)
drop1 (modadd5, test = 'Chi')
modadd6 <- update(modadd5, .~. -COMT)
drop1 (modadd6, test = 'Chi')
modadd7 <- update(modadd6, .~. -DRD4Pr)
drop1 (modadd7, test = 'Chi')
modadd8 <- update(modadd7, .~. -SERTPr)
drop1 (modadd8, test = 'Chi')
modadd9 <- update(modadd8, .~. -HTR2A)
drop1 (modadd9, test = 'Chi')
modadd10 <- update(modadd9, .~. -HTR1B)
drop1 (modadd10, test = 'Chi')
modadd11 <- update(modadd10, .~. -DRD2)
drop1 (modadd11, test = 'Chi')

AIC(modadd, modadd2, modadd3, modadd4, modadd5, modadd6, modadd7, modadd8, modadd9, modadd10, modadd11)

drop1 (modadd10, test = 'Chi')
summary(modadd10)
Anova(modadd10)

#Проверка на нелинейность связи
modadd6_diag <- data.frame(.fitted = fitted(modadd6, type = 'response'),
                           .resid_p = resid(modadd6, type = 'pearson'))
ggplot (modadd6_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

# Функция Бена Болкера для проверки на сверхдисперсию

overdisp_fun(modadd6) 

AIC (mod9, moddom7, modrec7, modovdom10, modadd6)

#Alleles
suicideal <- read.csv('suicidealleles_glm.csv', header = TRUE)
str(suicideal)
head (suicideal)
colSums (is.na(suicideal))
suicideal$samp <- ifelse (test = suicideal$sample == 's', yes = 1, no = 0)
mod <- glm(samp ~ BDNF+DAT1+DRD2+DRD4E3+DRD4Pr+COMT+SERTPr+HTR1A+HTR2A+HTR1B, family = binomial(link = 'logit'), data = suicideal)
Anova(mod)

drop1 (mod, test = 'Chi')
mod2 <- update(mod, .~. -HTR1A)
drop1 (mod2, test = 'Chi')
mod3 <- update(mod2, .~. -DRD4E3)
drop1 (mod3, test = 'Chi')
mod4 <- update(mod3, .~. -DRD4Pr)
drop1 (mod4, test = 'Chi')
mod5 <- update(mod4, .~. -HTR1B)
drop1 (mod5, test = 'Chi')
mod6 <- update(mod5, .~. -HTR2A)
drop1 (mod6, test = 'Chi')

AIC(mod, mod2, mod3, mod4, mod5, mod6)

summary(mod5)
Anova(mod5)
drop1 (mod5, test = 'Chi')

mod5_diag <- data.frame(.fitted = fitted(mod5, type = 'response'),
                        .resid_p = resid(mod5, type = 'pearson'))
ggplot (mod5_diag, aes (y = .resid_p, x = .fitted)) + geom_point() +
  geom_hline (yintercept = 0) + geom_smooth(method = 'loess')

overdisp_fun(mod5) 

