# 직무 만족도 - 이직 여부

# 마지막 평가점수 - 진행 프로젝트 수와 월평균 근무시간 

#진행 프로젝트 수 - 월평균 근무시간



# 1. 데이터 불러오기
install.packages('readcsv')
install.packages('ggplot2')
install.packages('dplyr')
library('readcsv')
library('ggplot2')
library('dplyr')

df_Hr <- read.csv("HR_comma_sep.csv")

# 2. 데이터 파악
head(df_Hr)

table(is.na(df_Hr)) # 결측치는 없음


# 3. 집계

#  연차별 회사 만족도
df_Hr %>% 
  filter(df_Hr$satisfaction_level > 0.5) %>%
  select(time_spend_company) %>%
  table()

ggplot(data = df_Hr, aes(x = time_spend_company, y = satisfaction_level)) + geom_point(alpha=0.3)


# 직무 만족도 - 이직여부
table(df_Hr %>% select(left, satisfaction_level))

df_Hr %>%
  group_by(left) %>%
  summarise(mean_sat = mean(satisfaction_level))



ggplot(data = df_Hr, aes(x = left, y = satisfaction_level)) + geom_point()


# 마지막 평가점수 - 진행 프로젝트 수와 월평균 근무시간 

df_Hr %>% mutate(eval_protime = (number_project + average_montly_hours)/2)

ggplot(data = df_Hr, aes(x = number_project, y = average_montly_hours)) + geom_point()
boxplot(df_Hr$last_evaluation)$stats


df_Hr %>%
  group_by(last_evaluation) %>%
  summarise(mean_pro = mean(number_project)) %>%
  table()

table(df_Hr %>% select(last_evaluation))

df_Hr_s <- data.frame()

ggplot(data =df_Hr)


boxplot(df_Hr$satisfaction_level)$stats

