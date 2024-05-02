# Importing Dependencies =====
library(tidyverse); library(haven); library(magrittr); library(showtext); library(ggrepel); library(gt)
font_add_google("Nanum Myeongjo", "nanum")
showtext_auto()

# Study Time dataset =====
## Importing & Tidying Data ======
study_time <- read_csv("study_time.csv", na = "m", locale = locale(encoding = "utf-8")) %>% 
  group_by(Country) %>% 
  mutate(total = sum(Science + Math + Language + ForeignLanguage + OtherSubjects, na.rm = T)) %>% 
  filter(total != 0) %>% 
  arrange(desc(total))

## Visualizing Study Time by Country ======
study_time %>% 
  ggplot(mapping = aes(x = reorder(Country, total, decreasing = T), y = total)) +
  geom_col(fill = "orange") +
  geom_col(data = filter(study_time, Country == "Korea"), fill = "red") +
  theme_gray(base_family = "nanum") +
  labs(title = "국가별 학교 수업 외 학습 시간",
       x = "국가", y = "일주일 당 학습 시간", caption = "출처: PISA 보도자료 (2015), 제작: 윤수민") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_text_repel(
    data = study_time[c(1:5, 17, 54:58),],
    aes(label = paste(Country, ": ", total)), nudge_y = 1
    )

# Earnings by Education Level dataset =====
## Importing & Tidying Data ======
earnings <- read_csv("EAG_EARNINGS_01052024153531938.csv") 

### Education levels from lower to higher =======
education_levels <- c("Below upper secondary education",
                      "Post-secondary non-tertiary education",
                      "Tertiary education",
                      "Upper secondary, post-secondary non-tertiary education and short-cycle tertiary education",
                      "Short-cycle tertiary education",
                      "Bachelor’s or equivalent education",
                      "Bachelor's, Master’s, Doctoral or equivalent education",
                      "Master’s, Doctoral or equivalent education"
                      )

### Mutating character columns to factors =======
earnings %<>%
  map_df(\(x) if(typeof(x) == "character") factor(x) else x) %>% 
  mutate(ISC11A...4 = fct_relevel(ISC11A...4, education_levels))

earnings_tidy <- earnings %>% 
  filter(Indicator == "Relative earnings - upper secondary education = 100",
         Gender == "Total",
         Age == "25-64 years",
         EARN_CATEGORY...10 == "All earners") %>% 
  select(Country, ISC11A...4, Value) %>% 
  filter(ISC11A...4 %in% c("Below upper secondary education", "Bachelor’s or equivalent education")) %>% 
  filter(!is.na(Value)) %>% 
  spread(ISC11A...4, Value) %>% 
  mutate(rate = `Bachelor’s or equivalent education` / `Below upper secondary education`) %>% 
  filter(!is.na(rate)) %>% 
  arrange(desc(rate))

## Visualizing Relative Earnings by Country ======
earnings_tidy %>% 
  ggplot(mapping = aes(x = reorder(Country, rate, decreasing = T), y = rate)) +
  geom_col(fill = "orange") +
  geom_col(data = filter(earnings_tidy, Country == "Korea"), fill = "red") +
  theme_gray(base_family = "nanum") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(title = "국가별 교육 수준 간 소득 격차",
       x = "국가", y = "대졸 이상 연봉 / 고졸 이하 연봉", caption = "출처: OECD, 제작: 윤수민") +
  geom_text_repel(
    data = earnings_tidy[c(1:5, 21, 29:33),],
    aes(label = paste(Country, ": ", round(rate, 2)))
  )
  

# Joining two datasets =====
## Data joining & tidying ======
study_join <- study_time %>% 
  left_join(select(earnings_tidy, Country, rate), by = join_by(Country == Country)) %>% 
  filter(!is.na(rate))

study_lm <- study_join %>% 
  lm(formula = rate ~ total)

study_formula <- paste0("Formula: y = ",
                        round(study_lm$coefficients[2], 4),
                        "x + ",
                        round(study_lm$coefficients[1], 4))

## Visualizing =======
study_join %>% 
  ggplot(mapping = aes(x = total, y = rate)) +
  geom_point() +
  geom_point(data = filter(study_join, Country == "Korea"), color = "red") +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = paste(Country))) +
  theme_gray(base_family = "nanum") +
  labs(title = "국가별 교육 수준 간 소득 격차와 학습 시간 간의 관계",
       x = "일주일 평균 학습 시간", y = "대졸 이상 연봉 / 고졸 이하 연봉", caption = "출처: OECD, PISA(2015), 제작: 윤수민") +
  geom_text(aes(x = 13, y = 1.83), label = study_formula, angle = (study_lm$coefficients[2] / pi * 180 + 1.1))

