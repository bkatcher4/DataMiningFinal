setwd("C:/Users/deepa/Documents/UVA_Data_Science/Summer_2019/STAT_6021/Project 2/")
library("MASS")
library("car")
library("olsrr")
library(data.table)
library(dplyr)
library(caret)
library(ROCR)
library(bestglm)
library(Rfast)
#install.packages('e1071', dependencies=TRUE)
#install.packages("Rfast")

df <- read.csv('question_user_metrics.csv')[,-1]

#df$answered <- as.factor(df$answered)
df$min_tag_popularity <- as.factor(df$min_tag_popularity)
df$max_tag_popularity <- as.factor(df$max_tag_popularity)

df <- sample_n(df, 0.3*dim(df)[1])
head(df)

# Checking if n_paragraphs_body is significant ----

mod_0 <- glm(answered ~ n_paragraphs_body, family = binomial(link = "logit"), data = df)
summary(mod_0)

anova_mod_0 <- anova(mod_0)
pval <- 1 - pchisq(anova_mod_0$Deviance[2], anova_mod_0$Df[2])
pval

# Checking if n_words_body + verbosity is significant ----

mod_1 <- glm(answered ~ n_words_body + verbosity, family = binomial(link = "logit"), data = df)
summary(mod_1)

anova_mod_1 <- anova(mod_1)
pval <- 1 - pchisq(anova_mod_1$Deviance[2], anova_mod_1$Df[2])
pval

# Checking if n_code_snippets + n_plots is significant

#df <- df[order(df$n_code_snippets),]
mod_2 <- glm(answered ~ n_code_snippets + n_plots, family = binomial(link = "logit"), data = df)
summary(mod_2)

anova_mod_2 <- anova(mod_2)
pval <- 1 - pchisq(anova_mod_2$Deviance[2], anova_mod_2$Df[2])
pval


# Checking if words in question and verbosity add something to the model with n_code_snippets and n_plots

mod_3 <- glm(answered ~ n_words_body + verbosity + n_code_snippets + n_plots + n_tags_title, family = binomial(link = "logit"), data = df)
summary(mod_3)

mod_3a <- glm(answered ~ n_words_body + verbosity + n_code_snippets + n_plots + n_tags_title + n_tags_body, family = binomial(link = "logit"), data = df)
summary(mod_3a)

anova_mod_3 <- anova(mod_3,mod_3a)
anova_mod_3
pval <- 1 - pchisq(anova_mod_3$Deviance[2], anova_mod_3$Df[2])
pval

mod_3_predictions <- predict(mod_3, type='response')

pred <- prediction( mod_3_predictions, df$answered)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
points(seq(0,1,0.01),seq(0,1,0.01), type = 'l', col = 'red')

# The results are all significant.
plot(mod_3, which = 5)

# Looks like there is a lot of influence
#cooksd <- cooks.distance(mod_3)
#dim(df)
#sum(ifelse(cooksd<0.05, 1, 0))
#dim(leverage)
#rowMaxs(leverage)
#leverage$high_leverage <- apply(leverage, 1, function(x) max(x))
#Get back here ----


# Refitting mod_3 excluding leverage points
#mod_3 <- glm(answered ~ n_words_body + verbosity + n_code_snippets + n_plots + n_tags_title, family = binomial(link = "logit"), data = df)
#summary(mod_3)

# Model 4 ----

mod_4 <- glm(answered ~ . - reputation - reputation_score - min_tag_popularity_score - max_tag_popularity_score
             - min_tag_popularity - max_tag_popularity 
             - min_tag_answered_ratio
             - max_tag_answered_ratio, family = binomial(link = "logit"), data = df)
summary(mod_4)

anova_mod_4 <- anova(mod_3,mod_4)
anova_mod_4
pval <- 1 - pchisq(anova_mod_4$Deviance[2], anova_mod_4$Df[2])
pval

vif(mod_4)
#plot(mod_4)

mod_4_predictions <- predict(mod_4, type='response')
length(mod_4_predictions)
pred <- prediction( mod_4_predictions, df$answered)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
points(seq(0,1,0.01),seq(0,1,0.01), type = 'l', col = 'red')


# Model 5 ----

mod_5 <- glm(answered ~ . - n_tags - n_plots - n_chars_body - n_words_body - n_words_title - n_tags_title
               - reputation - reputation_score - min_tag_popularity_score - max_tag_popularity_score
             - min_tag_popularity - max_tag_popularity 
             - min_tag_answered_ratio
             - max_tag_answered_ratio, family = binomial(link = "logit"), data = df)
summary(mod_5)

anova_mod_5 <- anova(mod_5, mod_4)
anova_mod_5
pval <- 1 - pchisq(anova_mod_5$Deviance[2], anova_mod_5$Df[2])
pval

mod_5_predictions <- predict(mod_5, type='response')
length(mod_5_predictions)
pred <- prediction( mod_5_predictions, df$answered)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
points(seq(0,1,0.01),seq(0,1,0.01), type = 'l', col = 'red')

# Model 6 ----

mod_6 <- glm(answered ~ . - n_tags - n_plots - n_chars_body - n_words_body - n_words_title - n_tags_title
             - reputation - reputation_score , family = binomial(link = "logit"), data = df)

summary(mod_6)

anova_mod_6 <- anova(mod_5, mod_6)
anova_mod_6
pval <- 1 - pchisq(anova_mod_6$Deviance[2], anova_mod_6$Df[2])
pval
vif(mod_6)

mod_6_predictions <- predict(mod_6, type='response')
length(mod_6_predictions)
pred <- prediction( mod_6_predictions, df$answered)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
points(seq(0,1,0.01),seq(0,1,0.01), type = 'l', col = 'red')


## Model 7 ----

mod_7 <- glm(answered ~ . - n_tags - n_plots - n_chars_body - n_words_body - n_words_title - n_tags_title
             - min_tag_popularity_score - min_tag_popularity
             - reputation - reputation_score , family = binomial(link = "logit"), data = df)

summary(mod_7)

anova_mod_7 <- anova(mod_7, mod_6)
anova_mod_7
pval <- 1 - pchisq(anova_mod_6$Deviance[2], anova_mod_6$Df[2])
pval


# Keeping only significant columns ----

names(df)
df_1 <- df[,-c(2,5,6,7,11,13,17)]
df_1$max_tag_popularity3 <- ifelse(df_1$max_tag_popularity == 3,1,0)
names(df_1)
df_1 <- df_1[-c(10,12)]

## Model 8 ----

mod_8 <- glm(answered ~ . - reputation - reputation_score , family = binomial(link = "logit"), data = df_1)
summary(mod_8)

anova_mod_8 <- anova(mod_8, mod_7)
anova_mod_8
pval <- 1 - pchisq(anova_mod_8$Deviance[2], anova_mod_8$Df[2])
pval


mod_8_predictions <- predict(mod_8, type='response')
length(mod_8_predictions)
pred <- prediction( mod_8_predictions, df$answered)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
points(seq(0,1,0.01),seq(0,1,0.01), type = 'l', col = 'red')


cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.4))

mod_8_preds <- as.factor(ifelse(mod_8_predictions<0.33, 0, 1))
conf_matrix <- confusionMatrix(mod_8_preds, as.factor(df$answered))
conf_matrix$table/dim(df)[1]


# Trying best_glm approach to avoid overfitting ---

names(df_1)
best_df <- df_1[-1]
best_df$y <- df_1$answered
names(best_df)
head(best_df)
names(best_df[-c(12,13)])

# Best GLM AIC ----

all_mods_aic <- bestglm(best_df[-c(12,13)], IC = "AIC", family = binomial(link = "logit"))
all_mods_aic

# Best GLM CV ----

names(best_df)
all_mods_cv <- bestglm(best_df[-c(12,13)], IC = "CV" ,family = binomial(link = "logit"))
all_mods_cv


## Final model ----

final_mod <- glm(y ~ n_paragraphs_body + n_code_snippets + n_stop_words_body + verbosity + n_tags_title + ques_marks_title + min_tag_answered_ratio + max_tag_popularity + max_tag_answered_ratio + max_tag_popularity_score, family = binomial(link = "logit"), data = best_df)

final_mod
mod_9_predictions <- predict(final_mod, type='response')
length(mod_9_predictions)
pred <- prediction( mod_9_predictions, best_df$y)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
points(seq(0,1,0.01),seq(0,1,0.01), type = 'l', col = 'red')

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.4))

mod_9_preds <- as.factor(ifelse(mod_9_predictions<0.34, 0, 1))
conf_matrix <- confusionMatrix(mod_9_preds, as.factor(df$answered))
conf_matrix$table/dim(df)[1]


## Final Model - V2 with user ---- 

final_mod_2 <- glm(y ~ n_paragraphs_body + n_code_snippets + n_stop_words_body + verbosity + n_tags_title + ques_marks_title 
                   + min_tag_answered_ratio + max_tag_popularity + max_tag_answered_ratio + max_tag_popularity_score
                   + reputation + reputation_score, family = binomial(link = "logit"), data = best_df)

final_mod_2

mod_10_predictions <- predict(final_mod_2, type='response')
length(mod_10_predictions)
pred <- prediction( mod_10_predictions, best_df$y)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
points(seq(0,1,0.01),seq(0,1,0.01), type = 'l', col = 'red')

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.4))

mod_10_preds <- as.factor(ifelse(mod_10_predictions<0.34, 0, 1))
conf_matrix <- confusionMatrix(mod_10_preds, as.factor(df$answered))
conf_matrix$table/dim(df)[1]
