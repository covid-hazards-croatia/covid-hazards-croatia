
library(tibble)
library(dplyr)
library(ggthemes)
library(arm)
library(lme4)
library(ggplot2)
library(scales)
library(ggthemes)
library(stargazer)
library(survival)
library(coxme)
library(stringr)
library(survminer)
library(data.table)
library(readxl)
library(janitor)
library(dotwhisker)

Sys.setenv(TZ="Europe/Berlin")

getwd()


pozitivni$end <- if_else((pozitivni$death==1), pozitivni$death_date, pozitivni$cont_end)

pozitivni$time <- as.numeric(pozitivni$end - pozitivni$cont_start)

cox_model <- coxph(Surv(time, death) ~ age_group + sex + hypertension  + other_cardiovascular_diseases + immunodeficiency + malignant_neoplasms + diabetes  + chronic_kidney_disease + chronic_lower_respiratory_diseases +  developmental_index  + frailty(NAZIV_USTANOVE), data = pozitivni)

exp(coef(cox_model))
exp(confint(cox_model))

temp <- cox.zph(cox_model, transform="km")
print(temp)

concordance(cox_model)

model_orig_coef <- tidy(exp(coefficients(cox_model)))
names(model_orig_coef)[names(model_orig_coef) == "names"] <- "term"
names(model_orig_coef)[names(model_orig_coef) == "x"] <- "estimate"
intervals_orig <- as.data.frame(exp(confint(model_orig)))
model_orig_coef$conf.low <- intervals_orig$`2.5 %`
model_orig_coef$conf.high <- intervals_orig$`97.5 %` 

model_orig_coef$term <- gsub("age_group2", "age 55-64 years", model_orig_coef$term)
model_orig_coef$term <- gsub("age_group3", "age >64 years", model_orig_coef$term)
model_orig_coef$term <- gsub("sexmale", "male", model_orig_coef$term)
model_orig_coef$term <- gsub("other_cardio_all", "other cardiovascular diseases", model_orig_coef$term)
model_orig_coef$term <- gsub("chronic_lower_respiratory_diseases", "chronic lower respiratory diseases", model_orig_coef$term)
model_orig_coef$term <- gsub("other_cardiovascular_diseases", "other cardiovascular diseases", model_orig_coef$term)
model_orig_coef$term <- gsub("developmental_index", "developmental index (octiles)", model_orig_coef$term)
model_orig_coef$term <- gsub("malignant_neoplasms", "malignant neoplasms", model_orig_coef$term)
model_orig_coef$term <- gsub("chronic_kidney_disease", "chronic kidney disease", model_orig_coef$term)

model_orig_coef <- model_orig_coef[order(-model_orig_coef$estimate),]


dwplot(model_orig_coef, 
       vline = geom_vline(xintercept = 1, colour = "grey60", linetype = 2), dot_args = list(size = 3, pch = 21, fill = "white", stroke=1.2)) + 
  theme_bw(base_size = 14.5) + scale_color_brewer(palette="Dark2") +
  guides(color=guide_legend('Model version')) + theme(legend.position ="none") +
  labs(title = "Figure 2: Hazard ratios for all covariates among patients hospitalized with COVID-19;\nOutcome: mortality",
       subtitle = "Error bars represent 95% CIs", caption = "", x = "Hazard ratio") +
  theme(plot.title = element_text(size=14.5, family="serif", face="bold")) +
  theme(plot.title = element_text(hjust = 0)) + theme(plot.subtitle = element_text(hjust = 0)) +
  theme(text=element_text(size=14.5,  family="serif")) + scale_x_continuous(breaks = round(seq(0, 11, by = 1),1)) +
  expand_limits(x = 0.265, y = 0)

ggsave('croatia_hazard_ratios.pdf', width=10, height=8, unit='in', dpi=1000)
