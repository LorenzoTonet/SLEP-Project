library(tidyr)
library(ggplot2)
#library(ggridges)

data <- survival::cancer

data.nm <- subset(data, select=-meal.cal)
data.nm$sex <- factor(data.nm$sex)
data.nm$status <- factor(data.nm$status)
data.nm$ph.ecog <- factor(data.nm$ph.ecog)
data.nm$ph.karno <- factor(data$ph.karno)

View(data)

# Is there indipendence from sex and status?
################################################################################
se.st.table <-table(data.nm$status, data.nm$sex)
chisq.test(se.st.table)


# How about Nas? (for sex)
################################################################################

dataM <-data.frame(
  var=colnames(data[data$sex=="1",]), 
  sex="Male", 
  na=colSums(is.na(data[data$sex=="1",]))
)

dataF <- data.frame(
  var=colnames(data[data$sex=="2",]),
  sex="Female",
  na=colSums(is.na(data[data$sex=="2",]))
)

nass <- rbind(dataM, dataF)
nas <- data.frame(var=colnames(data), na=colSums(is.na(data)))

ggplot(nas, aes(x=var, y=na))+ 
  geom_bar(stat="identity")+
  theme_grey()


ggplot(nass, aes(x=var, y=na))+ 
  geom_bar(stat="identity")+
  facet_wrap(~sex)
  theme_grey()

# Can i predict nas?
################################################################################
  
hasna <- as.numeric(rowSums(is.na(data)) > 0)
na.corr.data <- cbind(data, hasna)
na.corr.data <- na.corr.data[-9]


m <- glm(
  hasna ~ inst + ph.ecog + ph.karno + pat.karno + sex + time,
  family = binomial(link="logit"),
  data=na.corr.data
)
summary(m)

# How about for status (search nas)
################################################################################

data1 <-data.frame(
  var=colnames(data[data$status=="1",]),
  status="1",
  na=colSums(is.na(data[data$status=="1",]))
)

data2 <- data.frame(
  var=colnames(data[data$status=="2",]),
  status="2",
  na=colSums(is.na(data[data$status=="2",]))
)

nass <- rbind(data1, data2)
nas <- data.frame(var=colnames(data), na=colSums(is.na(data)))

ggplot(nas, aes(x=var, y=na))+ 
  geom_bar(stat="identity")+
  theme_grey()

ggplot(nass, aes(x=var, y=na))+ 
  geom_bar(stat="identity")+
  facet_wrap(~status) +
  theme_bw()

# Some plots
################################################################################

ggplot(data.nm, aes(x=age, fill=status))+
  geom_histogram()+
  facet_wrap(~status)+
  theme_bw()


ggplot(data.nm, aes(x=age, fill=sex))+
  geom_histogram()+
  facet_wrap(~status)+
  theme_bw()


ggplot(data.nm, aes(x=age, fill=sex))+
  geom_density(alpha=0.5)+
  theme_bw()

ggplot(data.nm, aes(x=age, fill=status))+
  geom_density(alpha=0.5)+
  theme_bw()


ggplot(data.nm, aes(x=time, y=sex)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))


ggplot(data.nm, aes(x=time, y=status)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))

# time vs karno index
ggplot(data.nm, aes(x=time, group=pat.karno, fill=pat.karno))+
  geom_boxplot()

ggplot(data.nm, aes(x=time, group=ph.karno, fill=ph.karno))+
  geom_boxplot()

ggplot(data.nm, aes(x=time, y=ph.ecog, fill=ph.ecog))+
  geom_density_ridges(na.rm=T)

ggplot(data.nm, aes(x=time))+
  geom_boxplot()

ggplot(data.nm, aes(x=wt.loss)) +
  geom_histogram()

ggplot(data.nm, aes(x=wt.loss, y=time)) +
  geom_point(aes(colour=status)) +
  theme_bw()

hist(data.nm$time, breaks=12)
hist(log(data.nm$time), breaks=12)

ggplot(data.nm, aes(x=wt.loss, y=time)) +
  geom_point(aes(colour=status)) +
  theme_bw()

ggplot(data.nm, aes(x=time, fill=status))+
  geom_histogram(na.rm=T)

sorted <- data.nm[order(data.nm$time),]
plot(sorted$time, col=as.numeric(sorted$status), pch=19)
abline(h=mean(sorted$time), add=T)
abline(h=median(sorted$time), add=T)
##########################################################################
# Correlation matrix
library(corrplot)

# only numerical data with pearson coefficent
numerical.data <- subset(data.nm, select=c(age, time, pat.karno))
corr.mat <- cor(numerical.data, use="pairwise.complete.obs", method="pearson")

corrplot(corr.mat, method="color", type="full", tl.col="black", tl.srt=45, 
         addCoef.col="black", number.cex=0.7, tl.cex=0.7, 
         title="Correlation Matrix of Numerical Variables", mar=c(0,0,1,0))

# numerical and ordinal data with spearman coefficent
ord_num = c(time, age, ph.ecog, ph.karno, pat.karno, wt.loss)
ds = subset(data, select=c(time, age, ph.ecog, ph.karno, pat.karno, wt.loss))
corr.mat <- cor(ds, use="pairwise.complete.obs", method="spearman")

corrplot(corr.mat, method="color", type="full", tl.col="black", tl.srt=45, 
         addCoef.col="black", number.cex=0.7, tl.cex=0.7, 
         title="Correlation Matrix of Numerical Variables", mar=c(0,0,1,0))

###########################################################################
# Some indipendence tests
summary(aov(data = data.nm, time ~ sex))
summary(lm(data = data.nm, time ~ sex))

###########################################################################
library(ggplot2)
table(data.nm$sex)
ggplot(data.nm, aes(x=wt.loss)) + 
  geom_histogram(na.rm=T, bins = 25) + 
  facet_wrap(~sex, scales = "fixed")

############################################################################
library(survminer)

survfit_sex = survfit(Surv(time, event=status) ~ sex, data=data)
summary(survfit_sex)
data

ggsurvplot(survfit_sex, 
           xlab="Days", 
           ylab="Overall survival probability",
           risk.table=TRUE,
           conf.int=TRUE,
           surv.median.line="hv", 
           data=data)

survfit_ecog = survfit(Surv(time, event=status) ~ ph.ecog, data=data)

ggsurvplot(survfit_ecog, 
           xlab="Days", 
           ylab="Overall survival probability",
           risk.table=TRUE,
           conf.int=FALSE,
           surv.median.line="hv", 
           data=data)

ggplot(data.nm, aes(x=age)) +
  geom_density() + 
  geom_vline(xintercept=quantile(data.nm$age))

data$age.cat <- cut(data.nm$age, breaks=c(0, 55, 70, 100), labels=c("<55", "55-70", "70+"))
survfit_ecog = survfit(Surv(time, event=status) ~ age.cat, data=data)
summary(survfit_ecog)
ggsurvplot(survfit_ecog, 
           xlab="Days", 
           ylab="Overall survival probability",
           risk.table=TRUE,
           conf.int=TRUE,
           surv.median.line="hv", 
           data=data)
