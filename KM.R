#install.packages(c("JM","survival","survminer"))
library(JM)
library(survival)
library(survminer)
data(aids, package = "JM")
head(aids)
surv_obj <- Surv(aids$Time, aids$death)
#by drug
km_fit_drug <- survfit(Surv(Time, death) ~ drug, data = aids)
summary(km_fit_drug)
ggsurvplot(
  km_fit_drug,
  data = aids,
  pval = TRUE, 
  conf.int = TRUE,
  risk.table = TRUE,
  legend.labs = c("ddC", "ddI"), 
  xlab = "time(day)", 
  ylab = "survival probability",
  title = "Kaplan-Meier survival curve", 
  surv.median.line = "hv" 
)
#by combine prevOI+AZT
aids.id$group <- factor(
paste(aids.id$prevOI, aids.id$AZT, sep = " & ")
)

table(aids.id$group)
km_fit_combined <- survfit(Surv(Time, death) ~ group, data = aids.id)
surv_test <- survdiff(Surv(Time, death) ~ group, data = aids.id)
p_value <- 1 - pchisq(surv_test$chisq, length(surv_test$n) - 1)
p_text <- ifelse(p_value < 0.001, 
                 "p < 0.001", 
                 paste("p =", round(p_value, 3)))
par(mar = c(5, 5, 4, 2))

plot(
  km_fit_combined,
  main = "Kaplan-Meier survival curve",
  xlab = "time (day)",
  ylab = "survival probability",
  col = c("red", "blue", "green", "purple"), 
  lty = 1:4, 
  mark.time = TRUE
)

group_names <- sort(unique(aids.id$group))

legend(
  "bottomleft",
  legend = group_names,
  col = c("red", "blue", "green", "purple"),
  lty = 1:4,  
  title = "prevOI & AZT",
  bty = "n"       
)
mtext(p_text, side = 1, line = -3, at = max(aids.id$Time, na.rm=T) * 0.9)