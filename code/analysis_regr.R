library(glmmTMB)
library(DHARMa)# for diagnostics
library(purrr)#map_dfc
library(kableExtra)
source('data_preproc.R')

##----- for generating the latex of the summary table of regression models-----
summary_latex = function(m){ 
  s = summary(m)
  coefs <- summary(m)$coefficients$cond
  coef_table <- data.frame(Estimate = round(coefs[, "Estimate"], 3))
  coef_table$`Standard Error` = round(coefs[, "Std. Error"], 3)
  coef_table$`z value` = round(coefs[, "z value"], 2)
  coef_table$`p value` = ifelse(coefs[, "Pr(>|z|)"] < 0.001, "< 0.001", 
                                sprintf("%.4f", coefs[, "Pr(>|z|)"]))
  coef_table$`z value (p)` <- paste0(coef_table$`z value`, 
                                     " (p = ", coef_table$`p value`, ")")
  coef_table <- coef_table[, c("Estimate", "Standard Error", "z value (p)")]
  print(kable(coef_table, format = "latex", booktabs = TRUE, caption = "Negative binomial regression results"))
  print(s[["varcor"]][["cond"]][["IIntID"]][1])
}

##--------poisson regression (using as level 50-95)-----------------------------
## assume mean and variance of outcome are equal
df_as_num_grid = readRDS("data/df_as_num_grid_inout.rds")%>%
  pivot_longer(cols = -IIntID)%>%
  mutate(name = as.numeric(sub("^num_grid_as_", "", name)))%>%
  rename(as_level = name, n_grid = value)%>%
  filter(as_level >= 50 & as_level<=95) %>% #use as level 50-95
  left_join(df_ret[,c("IIntID", "Sex", "Age.x")]) %>%
  mutate(IIntID = as.factor(IIntID),
         Age_scaled = scale(Age.x, center = TRUE, scale = TRUE))
# use centered Age: usually fine if linear term is at similar scale as other terms
#                   when polynomial terms are included, better scale it

# 1. with no age effect
m_p1 <- glmmTMB(n_grid ~ Sex + (1 | IIntID) + as_level, family = poisson, 
               data = df_as_num_grid)
summary_latex(m_p1)
# 2. with linear effect of age
m_p2 <- glmmTMB(n_grid ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3) + (1 | IIntID) + as_level, family = poisson, 
                 data = df_as_num_grid)
summary_latex(m_p2)


m_p4 <- glmmTMB(n_grid ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3) + (1 | IIntID) + Sex, family = poisson, 
                data = df_as_num_grid)

# 3. with polynomial effects of age
m_p3 <- glmmTMB(n_grid ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3) + Sex + (1 | IIntID)+as_level, family = poisson, 
                 data = df_as_num_grid)
summary_latex(m_p3)

anova(m_p1, m_p3, test = "LRT")
anova(m_p2, m_p3, test = "LRT")
anova(m_p4, m_p3, test = "LRT")

##----negative binomial mixed model (use all levels)----------------------------
## assume variance is greater than mean (to accomodate overdispersion)
df_as_num_grid = readRDS("data/df_as_num_grid_inout.rds")%>%
  pivot_longer(cols = -IIntID)%>%
  mutate(name = as.numeric(sub("^num_grid_as_", "", name)))%>%
  rename(as_level = name, n_grid = value)%>%
  left_join(df_ret[,c("IIntID", "Sex", "Age.x")]) %>%
  mutate(Age_scaled = scale(Age.x, center = TRUE, scale = TRUE))

# 1. with no age effect
m_nb <- glmmTMB(n_grid ~ Sex + (1 | IIntID) + as_level, family = nbinom1, 
                                   data = df_as_num_grid)
summary_latex(m_nb)
simulateResiduals(m_nb, plot = TRUE) 
# 2. with linear effect of age
m_nb1 <- glmmTMB(n_grid ~ Age_scaled + Sex + (1 | IIntID) + as_level, family = nbinom1, 
                data = df_as_num_grid)
summary_latex(m_nb1)
# 3. with polynomial effects of age
m_nb2 <- glmmTMB(n_grid ~ Age_scaled + I(Age_scaled^2) + I(Age_scaled^3) + Sex + (1 | IIntID)+as_level, family = nbinom1, 
                data = df_as_num_grid)
summary_latex(m_nb2)

anova(m_nb1, m_nb2, test = "LRT")

##-----------other regression models for exploration----------------------------
#linear regression
m_lm <- lm(n_grid ~ Age_group + Sex + as_level, data = df_as_num_grid)
summary(m_lm)

#poisson mixed model
m_p <- glmmTMB(n_grid ~ Age_group+Sex + (1 | IIntID)+as_level, family = poisson, 
                data = df_as_num_grid)
summary(m_p)
simulateResiduals(m_p, plot = TRUE) #too dispersed

#negative binomial mixed model
m_nb <- glmmTMB(n_grid ~ Age_group+Sex + (1 | IIntID)+as_level, family = nbinom2, 
                data = df_as_num_grid)
summary(m_nb)
simulateResiduals(m_nb, plot = TRUE)



