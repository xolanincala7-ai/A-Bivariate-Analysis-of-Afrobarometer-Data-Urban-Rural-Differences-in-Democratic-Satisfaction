## ----1, include=FALSE------------------
knitr::opts_chunk$set(echo = FALSE)


## ----2, echo=FALSE,warning=FALSE, message=FALSE----
library(haven)

AfroB <- "afrobarometer_release-dataset_gha_r9_en_2023-04-01.sav"


AfroB.9 <- read_spss(AfroB)

library(dplyr)

AB.Ghana.R9 <- AfroB.9 %>%
  select(RESPNO, URBRUR, Q31)

# Recode Satisfaction With democracy 
AB.Ghana.R9 <- AB.Ghana.R9 %>%
  mutate(
    SWD = case_when(
      Q31 %in% c(3, 4) ~ 0,  
      Q31 %in% c(1, 2) ~ 1,  
      Q31 %in% c(8, 9, 0) ~ NA_real_  
    )
  )

library(knitr)
AB.Ghana.R9 <- AB.Ghana.R9 %>%
  mutate(
    URBRUR_label = as_factor(URBRUR),
    Q31_label = as_factor(Q31)
  )


before_recode_ur <- AB.Ghana.R9 %>%
  count(URBRUR_label, Q31_label) %>%
  rename(Residence = URBRUR_label, Response = Q31_label, Count = n)

# Display table
kable(before_recode_ur, caption = "Table 1: Sample Distribution of Satisfaction with Democracy (Q31) by Urban/Rural Residence (Before Recoding)")

after_recode_ur <- AB.Ghana.R9 %>%
  mutate(SWD_label = case_when(
    SWD == 1 ~ "Very Satisfied",
    SWD == 0 ~ "Not very Satisfied"
    # I do not include is.na(SWD) here to avoid NA category
  )) %>%
  filter(!is.na(SWD_label) & !is.na(URBRUR_label)) %>%  # This Removes NAs in both SWD and residence
  count(URBRUR_label, SWD_label) %>%
  rename(Residence = URBRUR_label, SWD = SWD_label, Count = n)

kable(after_recode_ur, caption = "Table 2: Sample Distribution of Recoded SWD by Urban/Rural")


## ----3, echo=FALSE, warning=FALSE, message=FALSE----

library(ggplot2)
ggplot(AB.Ghana.R9 %>% filter(!is.na(URBRUR_label)), 
       aes(x = URBRUR_label, fill = URBRUR_label)) +
  geom_bar() +
  scale_fill_manual(values = c("Urban" = "grey", "Rural" = "orange")) +
  labs(
    title = "Distribution of Urban vs Rural Respondents",
    x = "Residence",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


## ----4, echo=FALSE, warning=FALSE, message=FALSE----
library(ggplot2)

AB.Ghana.R9 <- AB.Ghana.R9 %>%
  mutate(SWD_label = case_when(
    SWD == 1 ~ "Very Satisfied",
    SWD == 0 ~ "Not Very Satisfied"
  )) %>%
  filter(!is.na(SWD_label))

# Plot
ggplot(AB.Ghana.R9, aes(x = SWD_label, fill = SWD_label)) +
  geom_bar() +
  scale_fill_manual(values = c("Not Very Satisfied" = "red", 
                               "Very Satisfied" = "green")) +
  labs(
    title = "Distribution of Satisfaction With Democracy (Recoded)",
    x = "Satisfaction Level",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


## ----5, fig.width=9, fig.height=5,echo=FALSE, warning=FALSE, message=FALSE----
library(scales)
library(dplyr)

# Contingency table with column percentages
contingency_table <- AB.Ghana.R9 %>%
  filter(!is.na(Q31_label) & !is.na(URBRUR_label)) %>%
  count(Q31_label, URBRUR_label) %>%
  group_by(URBRUR_label) %>%
  mutate(percent = n / sum(n) * 100) %>%
  rename(Residence = URBRUR_label,Response = Q31_label, Count = n, Percent = percent)%>%
select(Residence, Response, Count, Percent)

kable(contingency_table, digits = 2, caption = "Table 3: Contingency Table of Detailed Satisfaction with Democracy (Q31) by Residence with Column Percentages")


## ----6 mosaic-plot, fig.width=9, fig.height=5,echo=FALSE, warning=FALSE, message=FALSE----
library(ggmosaic)
library(ggplot2)
library(dplyr)


AB_mosaic <- AB.Ghana.R9 %>%
  filter(Q31 %in% 1:4, !is.na(URBRUR_label)) %>%
  mutate(
    Q31_label = droplevels(as_factor(Q31))  
  )


ggplot(AB_mosaic) +
  geom_mosaic(aes(x = product(URBRUR_label), fill = Q31_label), na.rm = TRUE) +
  labs(
    title = "Mosaic Plot of Satisfaction with Democracy (Q31) by Residence",
    x = "Residence",
    y = "Proportion",
    fill = "Satisfaction"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )




## ----7,echo=FALSE, warning=FALSE, message=FALSE----

sw_table <- AB.Ghana.R9 %>%
  filter(!is.na(SWD), !is.na(URBRUR_label)) %>%
  mutate(
    SWD_label = case_when(
      SWD == 1 ~ "Satisfied",
      SWD == 0 ~ "Not Satisfied"
    )
  ) %>%
  count(URBRUR_label, SWD_label) %>%
  group_by(URBRUR_label) %>%
  mutate(Percent = n / sum(n) * 100) %>%
  rename(Residence = URBRUR_label, SWD = SWD_label, Count = n) %>%
  select(Residence, SWD, Count, Percent)  

kable(sw_table, digits = 2, caption = "Table 4: Recoded Satisfaction with Democracy (SWD) by Residence with Column Percentages")


## ----8,echo=FALSE, warning=FALSE, message=FALSE----
 library(ggplot2)
library(scales)


plot_data <- AB.Ghana.R9 %>%
  filter(!is.na(SWD_label) & !is.na(URBRUR_label)) %>%
  count(URBRUR_label, SWD_label) %>%
  group_by(URBRUR_label) %>%
  mutate(Percent = n / sum(n) * 100) %>%
  ungroup()

# Plot
ggplot(plot_data, aes(x = URBRUR_label, y = Percent, fill = SWD_label)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_fill_manual(values = c("Very Satisfied" = "green", 
                               "Not Very Satisfied" = "red")) +
  labs(
    title = "Proportional Distribution of Satisfaction with Democracy (Recoded)",
    x = "Residence",
    y = "Percentage",
    fill = "Satisfaction Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


## ----9, echo=FALSE, warning=FALSE, message=FALSE----

prop_table <- AB.Ghana.R9 %>%
  filter(!is.na(SWD), !is.na(URBRUR_label)) %>%
  group_by(URBRUR_label) %>%
  summarise(
    n_total = n(),
    n_satisfied = sum(SWD == 1),
    prop_satisfied = n_satisfied / n_total
  )


library(knitr)
library(kableExtra)


prop_table %>%
  mutate(
    prop_satisfied = round(prop_satisfied * 100, 1) 
  ) %>%
  rename(
    Residence = URBRUR_label,
    `Total Respondents` = n_total,
    `Satisfied Respondents` = n_satisfied,
    `Proportion Satisfied (%)` = prop_satisfied
  ) %>%
  kable(
    caption = "Table 5: Proportion of Respondents Satisfied with Democracy by Residence",
    format = "html",
    align = "lccc"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))


## ----10, echo=FALSE, warning=FALSE, message=FALSE----
# Here we Pull group sizes and successes
urban <- prop_table %>% filter(URBRUR_label == "Urban")
rural <- prop_table %>% filter(URBRUR_label == "Rural")

# Then we extract values
p1 <- urban$prop_satisfied
p2 <- rural$prop_satisfied
n1 <- urban$n_total
n2 <- rural$n_total

#The difference in proportions
diff_prop <- p1 - p2

# The SE
se_diff <- sqrt((p1 * (1 - p1)) / n1 + (p2 * (1 - p2)) / n2)

# Z-score for 90% CI (two-tailed)
z_90 <- qnorm(0.95)

# The confidence Interval
ci_lower <- diff_prop - z_90 * se_diff
ci_upper <- diff_prop + z_90 * se_diff

# Output

publish_table <- data.frame(
  `Proportion Satisfied (Urban)` = round(p1 * 100, 1),
  `Proportion Satisfied (Rural)` = round(p2 * 100, 1),
  `Difference in Proportions (Urban - Rural)` = round(diff_prop * 100, 1),
  `Standard Error` = round(se_diff, 3),
  `90% CI Lower Bound` = round(ci_lower * 100, 1),
  `90% CI Upper Bound` = round(ci_upper * 100, 1)
)

library(knitr)
library(kableExtra)

kable(publish_table, caption = "Table 6: Difference in Proportion Satisfied with Democracy by Residence (with 90% CI)",
      align = "c", format = "html") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))




## ----11, echo=FALSE, warning=FALSE, message=FALSE----

test_result <- prop.test(
  x = c(urban$n_satisfied, rural$n_satisfied),
  n = c(urban$n_total, rural$n_total),
  conf.level = 0.90,
  correct = FALSE
)

test_summary <- data.frame(
  `Group 1 (Urban) Proportion` = round(test_result$estimate[1] * 100, 1),
  `Group 2 (Rural) Proportion` = round(test_result$estimate[2] * 100, 1),
  `Difference (Urban - Rural)` = round(diff(test_result$estimate) * 100, 1),
  `90% CI Lower Bound` = round(test_result$conf.int[1] * 100, 1),
  `90% CI Upper Bound` = round(test_result$conf.int[2] * 100, 1),
  `p-value` = signif(test_result$p.value, 3)
)


library(knitr)
library(kableExtra)

kable(test_summary, caption = "Table 7: Two-Proportion Z-Test Comparing Satisfaction with Democracy by Residence",
      format = "html", align = "c") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))



## ----12, message=FALSE, warning=FALSE, echo=FALSE----
library(haven)

AfroB_NG <- read_spss("afrobarometer_release-dataset_nig_r9_en_2023-04-01.sav")


AB.Nigeria.R9 <- AfroB_NG %>%
  select(RESPNO, URBRUR, Q31)


AB.Nigeria.R9 <- AB.Nigeria.R9 %>%
  mutate(
    SWD = case_when(
      Q31 %in% c(3, 4) ~ 0,
      Q31 %in% c(1, 2) ~ 1,
      Q31 %in% c(8, 9, 0) ~ NA_real_
    ),
    URBRUR_label = as_factor(URBRUR),
    Q31_label = as_factor(Q31),
    SWD_label = case_when(
      SWD == 1 ~ "Satisfied",
      SWD == 0 ~ "Not Satisfied"
    )
  )



library(knitr)

table1_ng <- AB.Nigeria.R9 %>%
  count(URBRUR_label, Q31_label) %>%
  rename(Residence = URBRUR_label, Response = Q31_label, Count = n)

kable(table1_ng, caption = "Table 1: Sample Distribution of Satisfaction with Democracy (Q31) by Urban/Rural Residence — Nigeria")




## ----13, message=FALSE, warning=FALSE, echo=FALSE----

table4_ng <- AB.Nigeria.R9 %>%
  filter(!is.na(SWD), !is.na(URBRUR_label)) %>%
  mutate(
    SWD_label = case_when(
      SWD == 1 ~ "Satisfied",
      SWD == 0 ~ "Not Satisfied"
    )
  ) %>%
  count(URBRUR_label, SWD_label) %>%
  group_by(URBRUR_label) %>%
  mutate(Percent = n / sum(n) * 100) %>%
  rename(Residence = URBRUR_label, SWD = SWD_label, Count = n) %>%
  select(Residence, SWD, Count, Percent)


library(knitr)
kable(table4_ng, digits = 2, caption = "Table 2: Recoded Satisfaction with Democracy (SWD) by Residence with Column Percentages — Nigeria")




library(ggplot2)
library(scales)


plot_data_ng <- AB.Nigeria.R9 %>%
  filter(!is.na(SWD), !is.na(URBRUR_label)) %>%
  mutate(
    SWD_label = case_when(
      SWD == 1 ~ "Satisfied",
      SWD == 0 ~ "Not Satisfied"
    )
  ) %>%
  count(URBRUR_label, SWD_label) %>%
  group_by(URBRUR_label) %>%
  mutate(Percent = n / sum(n) * 100) %>%
  ungroup()


ggplot(plot_data_ng, aes(x = URBRUR_label, y = Percent, fill = SWD_label)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_fill_manual(values = c("Satisfied" = "green", "Not Satisfied" = "red")) +
  labs(
    title = "Proportional Distribution of Satisfaction with Democracy by Residence — Nigeria",
    x = "Residence",
    y = "Percentage",
    fill = "Satisfaction Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



## ----14, warning=FALSE, message=FALSE, echo=FALSE----

prop_table_ng <- AB.Nigeria.R9 %>%
  filter(!is.na(SWD), !is.na(URBRUR_label)) %>%
  group_by(URBRUR_label) %>%
  summarise(
    n_total = n(),
    n_satisfied = sum(SWD == 1),
    prop_satisfied = round(n_satisfied / n_total * 100, 1)
  ) %>%
  rename(
    Residence = URBRUR_label,
    `Total Respondents` = n_total,
    `Satisfied Respondents` = n_satisfied,
    `Proportion Satisfied (%)` = prop_satisfied
  )

kable(prop_table_ng, caption = "Table 3: Proportion of Respondents Satisfied with Democracy by Residence — Nigeria")


## ----15, echo=FALSE, warning=FALSE, message=FALSE----

urban_ng <- prop_table_ng %>% filter(Residence == "Urban")
rural_ng <- prop_table_ng %>% filter(Residence == "Rural")

p1_ng <- urban_ng$`Proportion Satisfied (%)` / 100
p2_ng <- rural_ng$`Proportion Satisfied (%)` / 100
n1_ng <- urban_ng$`Total Respondents`
n2_ng <- rural_ng$`Total Respondents`


diff_prop_ng <- p1_ng - p2_ng
se_diff_ng <- sqrt((p1_ng * (1 - p1_ng)) / n1_ng + (p2_ng * (1 - p2_ng)) / n2_ng)
z_90 <- qnorm(0.95)
ci_lower_ng <- diff_prop_ng - z_90 * se_diff_ng
ci_upper_ng <- diff_prop_ng + z_90 * se_diff_ng


table6_ng <- data.frame(
  `Proportion Satisfied (Urban)` = round(p1_ng * 100, 1),
  `Proportion Satisfied (Rural)` = round(p2_ng * 100, 1),
  `Difference (Urban - Rural)` = round(diff_prop_ng * 100, 1),
  `Standard Error` = round(se_diff_ng, 3),
  `90% CI Lower Bound` = round(ci_lower_ng * 100, 1),
  `90% CI Upper Bound` = round(ci_upper_ng * 100, 1)
)

kable(table6_ng, caption = "Table 4: Difference in Proportion Satisfied with Democracy by Residence (with 90% CI) — Nigeria")


## ----16, echo=FALSE, warning=FALSE, message=FALSE----

test_result_ng <- prop.test(
  x = c(urban_ng$`Satisfied Respondents`, rural_ng$`Satisfied Respondents`),
  n = c(urban_ng$`Total Respondents`, rural_ng$`Total Respondents`),
  conf.level = 0.90,
  correct = FALSE
)


table7_ng <- data.frame(
  `Urban Proportion (%)` = round(test_result_ng$estimate[1] * 100, 1),
  `Rural Proportion (%)` = round(test_result_ng$estimate[2] * 100, 1),
  `Difference (Urban - Rural)` = round(diff(test_result_ng$estimate) * 100, 1),
  `90% CI Lower` = round(test_result_ng$conf.int[1] * 100, 1),
  `90% CI Upper` = round(test_result_ng$conf.int[2] * 100, 1),
  `p-value` = signif(test_result_ng$p.value, 3)
)

kable(table7_ng, caption = "Table 5: Two-Proportion Z-Test Comparing Satisfaction with Democracy by Residence — Nigeria")

