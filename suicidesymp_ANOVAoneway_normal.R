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

# DAT1 шкала Бека
suicide$DAT1 <- as.factor(suicide$DAT1)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DAT1, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$DAT1)
levels(suicide$DAT1)
mod <- (lm(ballsbeck ~ DAT1, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DAT1, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DAT1 ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DAT1, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ DAT1, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DAT1, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DAT1 личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DAT1, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ DAT1, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DAT1, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DAT1 шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DAT1, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ DAT1, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DAT1, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD2 шкала Бека
suicide$DRD2 <- as.factor(suicide$DRD2)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD2, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$DRD2)
levels(suicide$DRD2)
mod <- (lm(ballsbeck ~ DRD2, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD2, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD2 ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD2, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ DRD2, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD2, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD2 личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD2, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ DRD2, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = drd2lt_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD2, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD2 шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD2, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ DRD2, data = suicide))
mod
mod_diag <- fortify(mod)
head(dmod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD2, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD4Pr шкала Бека
suicide$DRD4Pr <- as.factor(suicide$DRD4Pr)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD4Pr, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$DRD4Pr)
levels(suicide$DRD4Pr)
mod <- (lm(ballsbeck ~ DRD4Pr, data = suicide))
mod
mod_diag <- fortify(mod)
head(drd4prbeck_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD4Pr, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD4Pr ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD4Pr, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ DRD4Pr, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD4Pr, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD4Pr личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD4Pr, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ DRD4Pr, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD4Pr, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD4Pr шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD4Pr, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ DRD4Pr, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid_mod <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid_mod
ggplot(data = mod_diag, aes(x = DRD4Pr, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD4e3 шкала Бека
suicide$DRD4e3 <- as.factor(suicide$DRD4e3)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD4e3, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$DRD4e3)
levels(suicide$DRD4e3)
mod <- (lm(ballsbeck ~ DRD4e3, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD4e3, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD4e3 ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD4e3, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ DRD4e3, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD4e3, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD4e3 личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD4e3, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ DRD4e3, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD4e3, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# DRD4e3 шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = DRD4e3, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') + 
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ DRD4e3, data = suicide))
mod
mod_diag <- fortify(drd4e3ham)
head(mod_diag)
resid<- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = DRD4e3, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# COMT шкала Бека
suicide$COMT <- as.factor(suicide$COMT)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = COMT, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$COMT)
levels(suicide$COMT)
mod <- (lm(ballsbeck ~ COMT, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = COMT, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# COMT ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = COMT, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ COMT, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = COMT, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# COMT личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = COMT, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ COMT, data = suicide))
mod
mod_diag <- fortify(comtlt)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = COMT, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# COMT шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = COMT, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ COMT, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = COMT, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT 3al шкала Бека
suicide$SERT3al <- as.factor(suicide$SERT3al)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERT3al, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$SERT3al)
levels(suicide$SERT3al)
mod <- (lm(ballsbeck ~ SERT3al, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERT3al, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT 3al ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERT3al, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ SERT3al, data = suicide))
mod
mod_diag <- fortify(sert3al_st)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERT3al, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT 3al личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERT3al, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ SERT3al, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERT3al, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# Sert 3al шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERT3al, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ SERT3al, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERT3al, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT hl шкала Бека
suicide$SERThl <- as.factor(suicide$SERThl)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERThl, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$SERThl)
levels(suicide$SERThl)
mod <- (lm(ballsbeck ~ SERThl, data = suicide))
mod
mod_diag <- fortify(serthl_beck)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERThl, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT hl ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERThl, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ SERThl, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERThl, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT hl личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERThl, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ SERThl, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERThl, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# Sert hl шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERThl, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ SERThl, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERThl, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT ls шкала Бека
suicide$SERTls <- as.factor(suicide$SERTls)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERTls, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$SERTls)
levels(suicide$SERTls)
mod <- (lm(ballsbeck ~ SERTls, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERTls, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT ls ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERTls, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ SERTls, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERTls, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT ls личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERTls, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ SERTls, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERTls, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# Sert ls шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERTls, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ SERTls, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = serthl_ham_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERTls, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT ag шкала Бека
suicide$SERTag <- as.factor(suicide$SERTag)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERTag, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$SERTag)
levels(suicide$SERTag)
mod <- (lm(ballsbeck ~ SERTag, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERTag, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT ag ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERTag, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ SERTag, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERTag, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# SERT ag личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERTag, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ SERTag, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERTag, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# Sert ag шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = SERTag, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ SERTag, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = serthl_ham_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = SERTag, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR1A шкала Бека
suicide$HTR1A <- as.factor(suicide$HTR1A)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR1A, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$HTR1A)
levels(suicide$HTR1A)
mod <- (lm(ballsbeck ~ HTR1A, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR1A, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR1A ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR1A, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ HTR1A, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR1A, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR1A личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR1A, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ HTR1A, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR1A, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR1A шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR1A, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ HTR1A, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = serthl_ham_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR1A, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR2A шкала Бека
suicide$HTR2A <- as.factor(suicide$HTR2A)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR2A, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$HTR2A)
levels(suicide$HTR2A)
mod <- (lm(ballsbeck ~ HTR2A, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR2A, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR2A ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR2A, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ HTR2A, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR2A, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR2A личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR2A, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ HTR2A, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR2A, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR2A шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR2A, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ HTR2A, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = serthl_ham_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR2A, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR1B шкала Бека
suicide$HTR1B <- as.factor(suicide$HTR1B)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR1B, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$HTR1B)
levels(suicide$HTR1B)
mod <- (lm(ballsbeck ~ HTR1B, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR1B, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR1B ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR1B, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ HTR1B, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR1B, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR1B личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR1B, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ HTR1B, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR1B, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# HTR1B шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = HTR1B, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ HTR1B, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = serthl_ham_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = HTR1B, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# BDNF шкала Бека
suicide$BDNF <- as.factor(suicide$BDNF)
theme_set(theme_bw())
ggplot(data = suicide, aes(x = BDNF, y = ballsbeck)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
table(suicide$BDNF)
levels(suicide$BDNF)
mod <- (lm(ballsbeck ~ BDNF, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = BDNF, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# BDNF ситуационная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = BDNF, y = ballsst)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsst ~ BDNF, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = BDNF, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# BDNF личностная тревога
theme_set(theme_bw())
ggplot(data = suicide, aes(x = BDNF, y = ballslt)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballslt ~ BDNF, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = BDNF, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

# BDNF шкала Гамильтона
theme_set(theme_bw())
ggplot(data = suicide, aes(x = BDNF, y = ballsham)) +
  geom_point(alpha = 0.5, colour = 'steelblue') +
  stat_summary(geom = 'pointrange', fun.data = mean_cl_normal, position = position_nudge(x = 0.1))
mod <- (lm(ballsham ~ BDNF, data = suicide))
mod
mod_diag <- fortify(mod)
head(mod_diag)
resid <- ggplot(data = serthl_ham_diag, aes(x = .fitted, y = .stdresid)) + 
  geom_point() + geom_hline (yintercept = 0)
resid
ggplot(data = mod_diag, aes(x = BDNF, y = .stdresid)) +
  geom_boxplot() + geom_hline(yintercept = 0)
qqPlot(mod, id = FALSE)
summary(mod)
Anova(mod)

#Поправка Хольма-Бонферрони

p_alleles <- c(0.009, 0.028, 0.029, 0.047, 0.507, 0.098, 0.115, 0.393, 0.631, 0.248, 0.184, 0.157)
p_holm <- p.adjust(p_alleles, method = 'holm')
summary(p_holm)
p_holm



