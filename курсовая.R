# ============================================================
# 1. УСТАНОВКА ПАКЕТОВ 
# ============================================================
# Установка основных пакетов
install.packages("readr")    
install.packages("dplyr")    
install.packages("psych")    
install.packages("bruceR")   
install.packages("lavaan")   
install.packages("tidyr")    
library(dplyr)
# ============================================================
# 2. ЗАГРУЗКА ДАННЫХ
# ============================================================
library(readr)
df <- read_csv("survey_data - Nice.csv", locale = locale(encoding = "UTF-8"))
names(df)

# Переименовываем  
df <- df %>%
  rename(
    ID = `ID`,
    Gender = Gender,
    Age_group = Age,
    Status = Status,
    Employment = Employment,
    Experience = Experience,
    Field = `Field of work`,
    Empl_format = `Employment format`,
    # Personal Brand Equity items (5 пунктов)
    PBE1 = `My colleagues are aware of my professional expertise and accomplishments.`,
    PBE2 = `I am known as an expert in my field within my professional network`,
    PBE3 = `My professional reputation distinguishes me clearly from my peers`,
    PBE4 = `Others seek my advice or opinion because of my recognized expertise`,
    PBE5 = `My professional image and reputation are consistently perceived in the same way by others`,
    # Role Breadth Self-Efficacy items (7 пунктов)
    RBSE1 = `Analyzing a long-term problem to find a solution`,
    RBSE2 = `Representing your work area in meetings with senior management`,
    RBSE3 = `Designing new procedures for your work area`,
    RBSE4 = `Making presentations to groups of people`,
    RBSE5 = `Monitoring and responding to changing situations in your workplace`,
    RBSE6 = `Keeping up with developments in your area`,
    RBSE7 = `Coordinating work across different parts of the organization`,
    # Psychological Empowerment items (12 пунктов)
    PE1 = `The work I do is very important to me`,
    PE2 = `My job activities are personally meaningful to me`,
    PE3 = `The work I do is meaningful to me`,
    PE4 = `I am confident about my ability to do my job`,
    PE5 = `I am self-assured about my capabilities to perform my work activities`,
    PE6 = `I have mastered the skills necessary for my job`,
    PE7 = `I have significant autonomy in determining how I do my job`,
    PE8 = `I can decide on my own how to go about doing my work`,
    PE9 = `I have considerable opportunity for independence and freedom in how I do my job`,
    PE10 = `My impact on what happens in my department is large`,
    PE11 = `I have a great deal of control over what happens in my department`,
    PE12 = `I have significant influence over what happens in my department`,
    # Proactive Behavior items (9 пунктов)
    PRO1 = `Coming up with ideas to improve the way your core tasks are done`,
    PRO2 = `Taking action to develop new skills that will benefit your work`,
    PRO3 = `Suggesting changes to how your individual work is carried out`,
    PRO4 = `Developing new and improved methods to help your team perform better`,
    PRO5 = `Improving the way your team works together`,
    PRO6 = `Suggesting ways to make the team work more effectively`,
    PRO7 = `Making suggestions about how the organization can be more effective`,
    PRO8 = `Raising issues about your organization's effectiveness with others`,
    PRO9 = `Developing ways to make the organization perform better`,
    # Innovative Work Behavior items (10 пунктов)
    IWB1 = `Do you pay attention to issues that are not part of your daily work?`,
    IWB2 = `Do you wonder how things can be improved?`,
    IWB3 = `Do you search out new working methods, techniques, or instruments?`,
    IWB4 = `Do you generate original solutions to problems?`,
    IWB5 = `Do you find new approaches to execute tasks?`,
    IWB6 = `Do you make important organizational members enthusiastic about innovative ideas?`,
    IWB7 = `Do you attempt to convince people to support an innovative idea?`,
    IWB8 = `Do you systematically introduce innovative ideas into work practices?`,
    IWB9 = `Do you contribute to the implementation of new ideas?`,
    IWB10 = `Do you evaluate the utility of innovative ideas?`
  )

# ============================================================
# 3. ПРЕДОБРАБОТКА ДЕМОГРАФИЧЕСКИХ ПЕРЕМЕННЫХ
# ============================================================
#Gender: превращаем в бинарную переменную (Female = 1, Male = 0)
df$Gender_bin <- ifelse(df$Gender == "Female", 1, 0)
#Возраст: преобразуем категории в середины интервалов
#Опыт работы: превращаем текстовые метки в упорядоченный фактор
df <- df %>%
  mutate(
    Age_num = case_when(
      Age_group == "18-20" ~ 19,
      Age_group == "21-23" ~ 22,
      Age_group == "24-26" ~ 25,
      Age_group == "27 and older" ~ 27,
      TRUE ~ NA_real_
    ),
    Experience_ord = case_when(
      Experience == "<6 months"   ~ 1,
      Experience == "6-12 months" ~ 2,
      Experience == "1-3 years"   ~ 3,
      Experience == "more than 3 years" ~ 4,
      TRUE ~ NA_real_
    )
  )
# Employment status: дамми "Currently working" 
df <- df %>%
  mutate(Currently_working = ifelse(grepl("am currently working", Employment), 1, 0))

# ============================================================
# 4. ПОДСЧЁТ СРЕДНИХ БАЛЛОВ ПО ШКАЛАМ
# ============================================================
df <- df %>%
  mutate(
    PBE_mean = rowMeans(select(., PBE1:PBE5), na.rm = TRUE),
    RBSE_mean = rowMeans(select(., RBSE1:RBSE7), na.rm = TRUE),
    PE_mean = rowMeans(select(., PE1:PE12), na.rm = TRUE),
    PRO_mean = rowMeans(select(., PRO1:PRO9), na.rm = TRUE),
    IWB_mean = rowMeans(select(., IWB1:IWB10), na.rm = TRUE)
  )

# Удаляем строки с пропусками если есть по ключевым шкалам
df <- df %>% filter(!is.na(PBE_mean), !is.na(RBSE_mean), !is.na(PE_mean),
                    !is.na(PRO_mean), !is.na(IWB_mean))
# ============================================================
# 5. АЛЬФА КРОНБАХА
# ============================================================
library(psych)
cat("\n========= Альфа Кронбаха =========\n")
alpha_PBE <- psych::alpha(df %>% select(PBE1:PBE5))
print(alpha_PBE$total$std.alpha)

alpha_RBSE <- psych::alpha(df %>% select(RBSE1:RBSE7))
print(alpha_RBSE$total$std.alpha)

alpha_PE <- psych::alpha(df %>% select(PE1:PE12))
print(alpha_PE$total$std.alpha)

alpha_PRO <- psych::alpha(df %>% select(PRO1:PRO9))
print(alpha_PRO$total$std.alpha)

alpha_IWB <- psych::alpha(df %>% select(IWB1:IWB10))
print(alpha_IWB$total$std.alpha)

# ============================================================
# 6. ОПИСАТЕЛЬНАЯ СТАТИСТИКА И КОРРЕЛЯЦИИ
# ============================================================
key_vars <- df %>% select(PBE_mean, RBSE_mean, PE_mean, PRO_mean, IWB_mean,
                          Age_num, Gender_bin, Experience_ord, Currently_working)
desc <- psych::describe(key_vars)
print(desc)

cor_matrix <- cor(key_vars, use = "complete.obs")
print(round(cor_matrix, 3))
describe(key_vars)[,c("skew","kurtosis")]
# ============================================================
# 7. ПРОВЕРКА ПРЯМЫХ ГИПОТЕЗ (H1–H4) 
# ============================================================
# Контрольные переменные: Age_num, Gender_bin, Experience_ord, Currently_working
controls <- "Age_num + Gender_bin + Experience_ord + Currently_working"

# H1: PBE → RBSE
model_H1_step1 <- lm(as.formula(paste("RBSE_mean ~", controls)), data = df)
model_H1_step2 <- lm(as.formula(paste("RBSE_mean ~ PBE_mean +", controls)), data = df)
cat("\n========= H1: PBE → RBSE =========\n")
summary(model_H1_step2)
cat(sprintf("ΔR² = %.4f\n", summary(model_H1_step2)$r.squared - summary(model_H1_step1)$r.squared))

# H2: RBSE → PE
model_H2_step1 <- lm(as.formula(paste("PE_mean ~", controls)), data = df)
model_H2_step2 <- lm(as.formula(paste("PE_mean ~ RBSE_mean +", controls)), data = df)
cat("\n========= H2: RBSE → PE =========\n")
summary(model_H2_step2)

# H3: PE → Proactivity
model_H3_step1 <- lm(as.formula(paste("PRO_mean ~", controls)), data = df)
model_H3_step2 <- lm(as.formula(paste("PRO_mean ~ PE_mean +", controls)), data = df)
cat("\n========= H3: PE → Proactivity =========\n")
summary(model_H3_step2)

# H4: Proactivity → IWB
model_H4_step1 <- lm(as.formula(paste("IWB_mean ~", controls)), data = df)
model_H4_step2 <- lm(as.formula(paste("IWB_mean ~ PRO_mean +", controls)), data = df)
cat("\n========= H4: Proactivity → IWB =========\n")
summary(model_H4_step2)

# ============================================================
# 8. СЕРИЙНАЯ МЕДИАЦИЯ (H5) 
# ============================================================
library(lavaan)

# Спецификация модели последовательной медиации (H5)
model <- '
  RBSE_mean ~ a1*PBE_mean + Age_num + Gender_bin + Experience_ord + Currently_working
  PE_mean   ~ a2*RBSE_mean + Age_num + Gender_bin + Experience_ord + Currently_working
  PRO_mean  ~ a3*PE_mean + Age_num + Gender_bin + Experience_ord + Currently_working
  IWB_mean  ~ c*PBE_mean + b1*PRO_mean + Age_num + Gender_bin + Experience_ord + Currently_working

  serial_indirect := a1 * a2 * a3 * b1   
  direct         := c                    
  total_effect   := c + (a1 * a2 * a3 * b1)   
'

# Оценка модели с bootstrap
fit <- sem(model, data = df, se = "bootstrap", bootstrap = 10000)
summary(fit, fit.measures = TRUE, standard = TRUE)

# ============================================================
# 9. ROBUSTNESS CHECKS
# ============================================================
parameterEstimates(fit, boot.ci.type = "bca", level = .95) %>%
  filter(op == ":=")
library(car)
vif(model_H1_step2)   
vif(model_H2_step2)   
vif(model_H3_step2)   
vif(model_H4_step2)  

# ============================================================
# 10. ТАБЛИЦА ДЕМОГРАФИЯ
# ============================================================
df <- df %>%
  mutate(
    Experience_label = case_when(
      Experience_ord == 1 ~ "<6 months",
      Experience_ord == 2 ~ "6-12 months",
      Experience_ord == 3 ~ "1-3 years",
      Experience_ord == 4 ~ "more than 3 years",
      TRUE ~ NA_character_
    )
  )
demo_table <- bind_rows(
  # Возраст
  tibble(Variable = "Age (years)", Category = "", 
         Value = paste0(round(mean(df$Age_num, na.rm=TRUE), 1), " (", 
                        round(sd(df$Age_num, na.rm=TRUE), 2), ")")),
  # Пол
  df %>% count(Gender) %>% 
    mutate(Percent = round(n / sum(n) * 100, 1),
           Value = paste0(n, " (", Percent, "%)"),
           Variable = "Gender", Category = Gender) %>%
    select(Variable, Category, Value),
  # Статус занятости
  df %>% mutate(Currently_working_label = ifelse(Currently_working == 1, "Yes", "No")) %>%
    count(Currently_working_label) %>%
    mutate(Percent = round(n / sum(n) * 100, 1),
           Value = paste0(n, " (", Percent, "%)"),
           Variable = "Currently employed", Category = Currently_working_label) %>%
    select(Variable, Category, Value),
  # Опыт работы
  df %>% count(Experience_label) %>%
    mutate(Percent = round(n / sum(n) * 100, 1),
           Value = paste0(n, " (", Percent, "%)"),
           Variable = "Work experience", Category = Experience_label) %>%
    select(Variable, Category, Value)
)

print(demo_table)
write.csv(demo_table, "demographic_table.csv", row.names = FALSE) 

library(dplyr)
library(tidyr)

# ============================================================
# 11. ТАБЛИЦА PRELIMINARY АНАЛИЗ
# ============================================================

# 1. Собираем средние и стандартные отклонения
desc_stats <- df %>%
  summarise(
    across(c(PBE_mean, RBSE_mean, PE_mean, PRO_mean, IWB_mean),
           list(Mean = ~ round(mean(., na.rm = TRUE), 2),
                SD   = ~ round(sd(., na.rm = TRUE), 2)))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(Mean|SD)"
  )

# 2. Таблица с альфа Кронбаха
alpha_values <- c(
  PBE_mean = alpha_PBE$total$std.alpha,
  RBSE_mean = alpha_RBSE$total$std.alpha,
  PE_mean = alpha_PE$total$std.alpha,
  PRO_mean = alpha_PRO$total$std.alpha,
  IWB_mean = alpha_IWB$total$std.alpha
)

alpha_df <- tibble(
  Variable = names(alpha_values),
  Alpha = round(alpha_values, 2)
)

# 3. Объединяем 
table3 <- desc_stats %>%
  left_join(alpha_df, by = "Variable") %>%
  mutate(
    Scale = case_when(
      Variable == "PBE_mean"  ~ "PBE",
      Variable == "RBSE_mean" ~ "RBSE",
      Variable == "PE_mean"   ~ "PE",
      Variable == "PRO_mean"  ~ "Proactivity",
      Variable == "IWB_mean"  ~ "IWB"
    )
  ) %>%
  select(Scale, M = Mean, SD, Alpha)

print(table3)
write.csv(table3, "table3_descriptives.csv", row.names = FALSE)

scales <- c("PBE", "RBSE", "PE", "Proactivity", "IWB")
mat <- matrix("", 5, 5, dimnames = list(scales, scales))
# α Кронбаха (из table3)
diag(mat) <- sprintf("%.2f", table3$Alpha)
# 3. Заполняем нижний треугольник корреляциями
cormat <- round(cor(df[, c("PBE_mean","RBSE_mean","PE_mean","PRO_mean","IWB_mean")], 
                    use = "complete.obs"), 2)
mat[lower.tri(mat)] <- sprintf("%.2f", cormat[lower.tri(cormat)])
# 4. Превращаем в data frame 
cor_alpha_df <- as.data.frame(mat, stringsAsFactors = FALSE)
cor_alpha_df$Variable <- rownames(cor_alpha_df)
cor_alpha_df <- cor_alpha_df[, c("Variable", scales)]

# 5. Добавляем строки M и SD в конец
table3 <- table3[match(scales, table3$Scale), ]
means_sd <- table3 %>% select(M, SD)
means_row <- c("M", sprintf("%.2f", means_sd$M))
sd_row   <- c("SD", sprintf("%.2f", means_sd$SD))

cor_alpha_df <- rbind(cor_alpha_df, means_row, sd_row)

# 6. Сохраняем
write.csv(cor_alpha_df, "table3_full.csv", row.names = FALSE)
print(cor_alpha_df)

# ============================================================
# 12. ТАБЛИЦА Tests of Direct Hypotheses (H1–H4)
# ============================================================
extract_reg <- function(step2, step1, focal_var, hyp_label) {
  s2 <- summary(step2)
  
  # Коэффициенты из step2
  coef <- s2$coefficients[focal_var, ]
  B <- round(coef["Estimate"], 2)
  SE <- round(coef["Std. Error"], 2)
  t_val <- round(coef["t value"], 2)
  p_val <- coef["Pr(>|t|)"]
  p_str <- ifelse(p_val < 0.001, "<.001", format(round(p_val, 3), nsmall = 3))
  
  # Стандартизованный β (вручную через SD)
  sd_y <- sd(step2$model[[1]], na.rm = TRUE)          # зависимая переменная
  sd_x <- sd(step2$model[[focal_var]], na.rm = TRUE)  # предиктор
  beta <- round(B * sd_x / sd_y, 2)
  
  # R² и ΔR²
  r2_step2 <- round(s2$r.squared, 2)
  if (!missing(step1) && !is.null(step1)) {
    r2_step1 <- round(summary(step1)$r.squared, 2)
    delta_r2 <- round(r2_step2 - r2_step1, 2)
  } else {
    r2_step1 <- NA
    delta_r2 <- NA
  }
  
  data.frame(
    Hypothesis = hyp_label,
    Predictor  = focal_var,
    B = B,
    SE = SE,
    beta = beta,
    t = t_val,
    p = p_str,
    R2_Step1 = r2_step1,
    R2_Step2 = r2_step2,
    deltaR2  = delta_r2
  )
}
table4 <- bind_rows(
  extract_reg(model_H1_step2, model_H1_step1, "PBE_mean",  "H1: PBE → RBSE"),
  extract_reg(model_H2_step2, model_H2_step1, "RBSE_mean", "H2: RBSE → PE"),
  extract_reg(model_H3_step2, model_H3_step1, "PE_mean",   "H3: PE → Proactivity"),
  extract_reg(model_H4_step2, model_H4_step1, "PRO_mean",  "H4: Proactivity → IWB")
)

print(table4)
write.csv(table4, "table4_regressions.csv", row.names = FALSE) 
# ============================================================
# 13. ТАБЛИЦА Tests of Serial Mediation Hypothesis (H5)
# ============================================================
pe <- parameterEstimates(fit, boot.ci.type = "bca", level = .95)
defined_labels <- c("serial_indirect", "direct", "total_effect")

# 3. Фильтруем строки 
defined_effects <- pe %>%
  filter(op == ":=") %>%
  mutate(
    Effect = case_when(
      label == "serial_indirect" ~ "Serial indirect effect",
      label == "direct"          ~ "Direct effect (c')",
      label == "total_effect"    ~ "Total effect"
    )
  ) %>%
  select(Effect, est, se, ci.lower, ci.upper)

# 4. Отдельные пути (a1, a2, a3, b1)
paths <- pe %>%
  filter(op == "~", label %in% c("a1", "a2", "a3", "b1")) %>%
  mutate(
    Effect = case_when(
      label == "a1" ~ "a1 (PBE → RBSE)",
      label == "a2" ~ "a2 (RBSE → PE)",
      label == "a3" ~ "a3 (PE → Proactivity)",
      label == "b1" ~ "b1 (Proactivity → IWB)"
    )
  ) %>%
  select(Effect, est, se, ci.lower, ci.upper)

# 5. Объединяем всё в одну таблицу
table5 <- bind_rows(defined_effects, paths) %>%
  rename(
    Estimate = est,
    SE = se,
    `95% CI Lower` = ci.lower,
    `95% CI Upper` = ci.upper
  )

table5 <- table5 %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
write.csv(table5, "table5_mediation.csv", row.names = FALSE)
# ============================================================
# 14. ПРОВЕРКА УХУДШИТСЯ ЛИ МОДЕЛЬ
# ============================================================
# Выделяем все пункты шкал (43 столбца)
items <- df %>% 
  select(PBE1:PBE5, RBSE1:RBSE7, PE1:PE12, PRO1:PRO9, IWB1:IWB10)

model_no_paths <- '
  RBSE_mean ~ 0*PBE_mean + Age_num + Gender_bin + Experience_ord + Currently_working
  PE_mean ~ 0*RBSE_mean + Age_num + Gender_bin + Experience_ord + Currently_working
  PRO_mean ~ 0*PE_mean + Age_num + Gender_bin + Experience_ord + Currently_working
  IWB_mean ~ c*PBE_mean + 0*PRO_mean + Age_num + Gender_bin + Experience_ord + Currently_working
'
fit_no_paths <- sem(model_no_paths, data = df)
anova(fit_no_paths, fit)   # проверяем, ухудшилась ли модель

# ============================================================
# 15.HARMAN TEST
# ============================================================
harman <- psych::fa(items, nfactors = 1, rotate = "none")
print(harman$Vaccounted)
