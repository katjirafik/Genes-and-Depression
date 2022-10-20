library(ggplot2)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(carData)
library(car)

suicide <- read.csv('suicide_symptoms.csv')
str(suicide)
colSums (is.na(suicide))
shapiro.test(suicide$ballsbeck)
shapiro.test(suicide$ballsst)
shapiro.test(suicide$ballslt)
shapiro.test(suicide$ballsham)
qqPlot(suicide$ballsbeck)
qqPlot(suicide$ballsst)
qqPlot(suicide$ballslt)
qqPlot(suicide$ballsham)

suicide$DAT1 <- as.factor(suicide$DAT1)
suicide$DRD2 <- as.factor(suicide$DRD2)
suicide$DRD4Pr <- as.factor(suicide$DRD4Pr)
suicide$DRD4e3 <- as.factor(suicide$DRD4e3)
suicide$COMT <- as.factor(suicide$COMT)
suicide$SERThl <- as.factor(suicide$SERThl)
suicide$HTR1A <- as.factor(suicide$HTR1A)
suicide$HTR2A <- as.factor(suicide$HTR2A)
suicide$HTR1B <- as.factor(suicide$HTR1B)
suicide$BDNF <- as.factor(suicide$BDNF)

#шкала Бека

mod <- (lm(ballsbeck ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+HTR1A+HTR2A+HTR1B+BDNF, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

#ситуационная тревога
mod <- (lm(ballsst ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+HTR1A+HTR2A+HTR1B+BDNF, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

#личностная тревога
mod <- (lm(ballslt ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+HTR1A+HTR2A+HTR1B+BDNF, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

#шкала Гамильтона
mod <- (lm(ballsham ~ DAT1+DRD2+DRD4Pr+DRD4e3+COMT+SERThl+HTR1A+HTR2A+HTR1B+BDNF, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)


