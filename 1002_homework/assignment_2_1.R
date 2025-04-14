library(plyr)
library(stargazer)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
library(lmtest)
library(sandwich)

#work directory
dir <- "0831_homework/"
dirProg<- paste0(dir, "Program/")
dirProg
dirData <- paste0(dir, "")

#-----------------------------
#Read the data
#-----------------------------

dfWomenWork <- read.csv(file = paste0(dirData, "DiD_dataset.csv"))
str(dfWomenWork)

#change data to the correct forms
dfWomenWork$nonwhite <- as.factor(dfWomenWork$nonwhite)
dfWomenWork$work <- as.factor(dfWomenWork$work)

#check there is no NA values
colSums(is.na(dfWomenWork))

#build D and T

dfWomenWork$treatment <- ifelse(dfWomenWork$children > 0, 1, 0)
dfWomenWork$policy <- ifelse(dfWomenWork$year >= 1993, 1, 0)

dfWomenWork$policy <- as.factor(dfWomenWork$policy)


# interaction
dfWomenWork$DiD_interaction <- dfWomenWork$treatment * dfWomenWork$policy

#plot of earnings, income, work status

ggplot(dfWomenWork, aes(x = year, y = earn, color = as.factor(treatment)))+
  geom_point(stat = "summary", fun = "mean",size = 2)+
  geom_line(stat = "summary", fun = "mean",size = 1) +
  geom_vline(xintercept = 1993, linetype = "dashed", color = "black", size = 1)+
  labs(x = "Year", y = "Annual Earnings", color = "Children (1 = Yes, 0 = No)") +
  ggtitle("Difference-in-Difference Effect on Annual Earnings")
ggsave("plot.png", width = 6, height = 4, dpi = 300)


ggplot(dfWomenWork, aes(x = year, y = finc, color = as.factor(treatment)))+
  geom_point(stat = "summary", fun = "mean",size = 2)+
  geom_line(stat = "summary", fun = "mean",size = 1) +
  geom_vline(xintercept = 1993, linetype = "dashed", color = "black", size = 1)+
  labs(x = "Year", y = "Annual Family Income", color = "Children (1 = Yes, 0 = No)") +
  ggtitle("Difference-in-Difference Effect on Annual Family Income")
ggsave("plot_1.png", width = 6, height = 4, dpi = 300)

dfWomenWork$work_numeric <- as.numeric(as.character(dfWomenWork$work))

ggplot(dfWomenWork, aes(x = year, y = work_numeric, color = as.factor(treatment)))+
  geom_point(stat = "summary", fun = "mean",size = 2)+
  geom_line(stat = "summary", fun = "mean",size = 1) +
  geom_vline(xintercept = 1993, linetype = "dashed", color = "black", size = 1)+
  labs(x = "Year", y = "Proportion_Employed", color = "Children (1 = Yes, 0 = No)") +
  ggtitle("Difference-in-Difference Effect on Work Status")
ggsave("plot_2.png", width = 6, height = 4, dpi = 300)

stargazer(dfWomenWork, type = "text")


model <- lm(earn ~ policy * treatment, data = dfWomenWork)
summary(model)



#slides 35
# Find averages per period and per state
avgEarn <- ddply(dfWomenWork, .(policy, treatment), summarise,
                 avgEarns = mean(earn, na.rm=TRUE))
avgFinc <- ddply(dfWomenWork, .(policy, treatment), summarise,
                 avgFincome = mean(finc, na.rm=TRUE))
avgWork <- ddply(dfWomenWork, .(policy, treatment), summarise,
                 avgWorks = mean(work_numeric, na.rm=TRUE))

# Make table of the outcomes (transpose avgEmpl from long to
# wide format with function dcast)
tmp <- dcast(avgEarn, policy ~ treatment, value.var="avgEarns")
colnames(tmp)[which(colnames(tmp) == "0")] <- "No children"
colnames(tmp)[which(colnames(tmp) == "1")] <- "Have children"

tmp1 <- dcast(avgFinc, policy ~ treatment, value.var="avgFincome")
colnames(tmp1)[which(colnames(tmp1) == "0")] <- "No children"
colnames(tmp1)[which(colnames(tmp1) == "1")] <- "Have children"

tmp2 <- dcast(avgWork, policy ~ treatment, value.var="avgWorks")
colnames(tmp2)[which(colnames(tmp2) == "0")] <- "No children"
colnames(tmp2)[which(colnames(tmp2) == "1")] <- "Have children"

tmp <- rbind(tmp, tmp[2,]-tmp[1,])
rownames(tmp) <- c("Before", "After", "Difference")
tmp[3, "policy"] <- NA

tmp1 <- rbind(tmp1, tmp1[2,]-tmp1[1,])
rownames(tmp1) <- c("Before", "After", "Difference")
tmp1[3, "policy"] <- NA

tmp2 <- rbind(tmp2, tmp2[2,]-tmp2[1,])
rownames(tmp2) <- c("Before", "After", "Difference")
tmp2[3, "policy"] <- NA


# Make a table with the results
stargazer(tmp, summary = FALSE, align=TRUE)
stargazer(tmp1, summary = FALSE, align=TRUE)
stargazer(tmp2, summary = FALSE, align=TRUE)


# Estimate several models to illustrate difference-in-differences
mdlA <- earn ~ treatment + policy + treatment:policy
mdlB <- finc ~ treatment + policy + treatment:policy
mdlC <- work_numeric ~ treatment + policy + treatment:policy

# Estimate the models
rsltOLS1 <- lm(mdlA, data=dfWomenWork)
rsltOLS2 <- lm(mdlB, data=dfWomenWork)
rsltOLS3 <- lm(mdlC, data=dfWomenWork)

# Make table
stargazer(rsltOLS1, rsltOLS2, rsltOLS3,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, type="text")

#add other control variables
mdlD <- earn ~ treatment + policy + treatment:policy + age + ed + nonwhite
mdlE <- finc ~ treatment + policy + treatment:policy + age + ed + nonwhite
mdlF <- work_numeric ~ treatment + policy + treatment:policy + age + ed + nonwhite

# Estimate the models after adding control variables
rsltOLS4 <- lm(mdlD, data=dfWomenWork)
rsltOLS5 <- lm(mdlE, data=dfWomenWork)
rsltOLS6 <- lm(mdlF, data=dfWomenWork)

# Make table after adding control variables
stargazer(rsltOLS4, rsltOLS5, rsltOLS6,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, type="latex")

#test heteoskadicity
bptest(rsltOLS4)
bptest(rsltOLS5)
bptest(rsltOLS6)

#use robust standard errors
coeftest(rsltOLS4, vcov = vcovHC(rsltOLS4, type = "HC1"))
coeftest(rsltOLS5, vcov = vcovHC(rsltOLS4, type = "HC1"))
coeftest(rsltOLS6, vcov = vcovHC(rsltOLS4, type = "HC1"))

#build a new dataset with women having children
dfWomenWork$policy_numeric <- as.numeric(as.character(dfWomenWork$policy))
dfWomenWithChildren <- subset(dfWomenWork, children > 0)
dfWomenWithChildren$low_ed <- ifelse(dfWomenWithChildren$ed < 9, 1, 0)

dfWomenWithChildren$DiD_high_low_children <- dfWomenWithChildren$low_ed * dfWomenWithChildren$policy_numeric
model_high_low_edu_earn <- lm(earn ~ policy_numeric + low_ed + DiD_high_low_children, data = dfWomenWithChildren)
model_high_low_edu_finc <- lm(finc ~ policy_numeric + low_ed + DiD_high_low_children, data = dfWomenWithChildren)
model_high_low_edu_work <- lm(work_numeric ~ policy_numeric + low_ed + DiD_high_low_children, data = dfWomenWithChildren)

stargazer(model_high_low_edu_earn, model_high_low_edu_finc, model_high_low_edu_work,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, type="text")

#build a new dataset with low-educated women
dfWomenLowEd <- subset(dfWomenWork, ed < 9)
dfWomenLowEd$DiD_low_with_without_children <- dfWomenLowEd$treatment * dfWomenLowEd$policy_numeric
wo_ch_earn <- lm(earn ~ policy_numeric + treatment + DiD_low_with_without_children, data = dfWomenLowEd)
wo_ch_finc <- lm(finc ~ policy_numeric + treatment + DiD_low_with_without_children, data = dfWomenLowEd)
wo_ch_work <- lm(work_numeric ~ policy_numeric + treatment + DiD_low_with_without_children, data = dfWomenLowEd)

###無法跑出來
stargazer(wo_ch_earn, wo_ch_finc, wo_ch_work,
          intercept.bottom = FALSE, align = TRUE, no.space = TRUE, type="text")

#install.packages("texreg")
library(texreg)
screenreg(list(wo_ch_earn, wo_ch_finc, wo_ch_work))



