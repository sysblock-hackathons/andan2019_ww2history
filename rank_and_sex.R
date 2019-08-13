## Ivan Shamalo, Natalia Devyatova, Marina Vergeles, Kirill Ivanov


#ND

#MV

library(data.table)
library(ggplot2)

data <- fread("chelovek_vpp_5_percent_sex_corrected.tsv", encoding = "UTF-8")

unique(data$rank)
unique(data$sex)

data_female_priziv <- data[sex == 0, .N, by = c('date_birth:year', 'data_i_mesto_priziva:year')]
data_male_priziv <- data[sex == 1, .N, by = c('date_birth:year', 'data_i_mesto_priziva:year')]

data_female_priziv_NA <- na.omit(data_female_priziv)
colnames(data_female_priziv_NA) <- c("year_of_birth", "year_of_priziv", "chislo")
data_female_priziv_NA <- data_female_priziv_NA[year_of_priziv>=1940]

data_male_priziv_NA <- na.omit(data_male_priziv)
colnames(data_male_priziv_NA) <- c("year_of_birth", "year_of_priziv", "chislo")
data_male_priziv_NA <- data_male_priziv_NA[year_of_priziv>=1940 & year_of_priziv<=1945]




library(readxl)
ranks_code <- data.table(read_excel("ranks.xlsx"))

dataRank <- data[1:1000,c("id","rank")]
View(dataRank)
vector1 <- ranks_code[rank_group == 1, x]
vector1
vector2 <- ranks_code[rank_group == 2, x]
vector3 <- ranks_code[rank_group == 3, x]
vector0 <- ranks_code[rank_group == 0, x]

dataRank[rank %in% vector1, rank_group :='1']
dataRank[rank %in% vector2, rank_group :='2']
dataRank[rank %in% vector3, rank_group :='3']
dataRank[rank %in% vector0, rank_group :='0']

dataRankF <- data[, c("id","rank")]
View(dataRank)
vector1 <- ranks_code[rank_group == 1, x]
vector1
vector2 <- ranks_code[rank_group == 2, x]
vector3 <- ranks_code[rank_group == 3, x]
vector0 <- ranks_code[rank_group == 0, x]

dataRankF <- dataRankF[, c("id","rank")]

dataRankF[rank %in% vector1, rank_group :=1]
dataRankF[rank %in% vector2, rank_group :=2]
dataRankF[rank %in% vector3, rank_group :=3]
dataRankF[rank %in% vector0, rank_group :=0]
dataRankF[, .N, by = rank_group]


vec0 <- ranks_code[branch == 0, x]
vec1 <- ranks_code[branch == 1, x]
vec2 <- ranks_code[branch == 2, x]
vec3 <- ranks_code[branch == 3, x]
vec4 <- ranks_code[branch == 4, x]
vec5 <- ranks_code[branch == 5, x]
vec6 <- ranks_code[branch == 6, x]
vec7 <- ranks_code[branch == 7, x]

ranks_code[x == "телеграфист", branch := 1]

dataRankF[rank %in% vec1, branch :=1]
dataRankF[rank %in% vec2, branch :=2]
dataRankF[rank %in% vec3, branch :=3]
dataRankF[rank %in% vec4, branch :=4]
dataRankF[rank %in% vec5, branch :=5]
dataRankF[rank %in% vec6, branch :=6]
dataRankF[rank %in% vec7, branch :=7]
dataRankF[rank %in% vec0, branch :=0]
dataRankF[, .N, by = branch]

data_age_sex <- data[, c("id", "date_birth:year", "data_i_mesto_priziva:year", "sex")]
final_data <- merge(dataRankF, data_age_sex, by = 'id')

write.csv(final_data, "final_data_on_rank_age_sex_branch.csv")

final_data[sex==0, table(rank_group)]
final_data[sex==1, table(rank_group)]

final_data[sex==0, table(branch)]
final_data[sex==1, table(branch)]

final_data[, age := (`data_i_mesto_priziva:year` - `date_birth:year`)]
final_data[age <= 10, age := NA]

names(final_data) <- c("id","rank","rank_group","branch","birth_year","draft_year","sex","age")

chisq.test(final_data$rank_group, final_data$age)

final_data[, mean(age, na.rm = T), by = c("rank_group", "sex")]

data_female_priziv_NA[, age := year_of_priziv - year_of_birth]
data_male_priziv_NA[, age := year_of_priziv - year_of_birth]

ggplot(data_female_priziv_NA, aes(age, chislo, colour = as.factor(year_of_priziv))) + theme_bw() + geom_line(size=1.2) + labs(colour = "year", x = "age", y = "number", title = 'USSR Military Draft 1940-1945. Females') + theme(text = element_text(size=30)) 

ggplot(data_male_priziv_NA, aes(age, chislo, colour = as.factor(year_of_priziv))) + theme_bw() + geom_line(size=1.2) + labs(colour = "year", x = "age", y = "number", title = 'USSR Military Draft 1940-1945. Males') + theme(text = element_text(size=30))

options(scipen=999)
?t.test()
t.test(final_data[rank_group==1 & sex==1, .SD, .SDcol="age"], final_data[rank_group==2 & sex==1, .SD, .SDcol="age"])
t.test(final_data[rank_group==1 & sex==1, .SD, .SDcol="age"], final_data[rank_group==3 & sex==1, .SD, .SDcol="age"])
t.test(final_data[rank_group==2 & sex==1, .SD, .SDcol="age"], final_data[rank_group==3 & sex==1, .SD, .SDcol="age"])

t.test(final_data[rank_group==1 & sex==0, .SD, .SDcol="age"], final_data[rank_group==2 & sex==0, .SD, .SDcol="age"])
t.test(final_data[rank_group==1 & sex==0, .SD, .SDcol="age"], final_data[rank_group==3 & sex==0, .SD, .SDcol="age"])
t.test(final_data[rank_group==2 & sex==0, .SD, .SDcol="age"], final_data[rank_group==3 & sex==0, .SD, .SDcol="age"])

t.test(final_data[rank_group==1 & sex==0, .SD, .SDcol="age"], final_data[rank_group==1 & sex==1, .SD, .SDcol="age"])
t.test(final_data[rank_group==2 & sex==0, .SD, .SDcol="age"], final_data[rank_group==2 & sex==1, .SD, .SDcol="age"])
t.test(final_data[rank_group==3 & sex==0, .SD, .SDcol="age"], final_data[rank_group==3 & sex==1, .SD, .SDcol="age"])


wilcox.test(final_data_nona_age[rank_group==1 & sex==1, age], final_data_nona_age[rank_group==2 & sex==1, age])
wilcox.test(final_data_nona_age[rank_group==1 & sex==1, age], final_data_nona_age[rank_group==3 & sex==1, age])
wilcox.test(final_data_nona_age[rank_group==2 & sex==1, age], final_data_nona_age[rank_group==3 & sex==1, age])

wilcox.test(final_data_nona_age[rank_group==1 & sex==0, age], final_data_nona_age[rank_group==2 & sex==0, age])
wilcox.test(final_data_nona_age[rank_group==1 & sex==0, age], final_data_nona_age[rank_group==3 & sex==0, age])
wilcox.test(final_data_nona_age[rank_group==2 & sex==0, age], final_data_nona_age[rank_group==3 & sex==0, age])

wilcox.test(final_data_nona_age[rank_group==1 & sex==1, age], final_data_nona_age[rank_group==1 & sex==0, age])
wilcox.test(final_data_nona_age[rank_group==2 & sex==1, age], final_data_nona_age[rank_group==2 & sex==0, age])
wilcox.test(final_data_nona_age[rank_group==3 & sex==1, age], final_data_nona_age[rank_group==3 & sex==0, age])


wilcox.test(final_data_nona_age[branch==2 & sex==1, age], final_data_nona_age[branch==7 & sex==1, age])
wilcox.test(final_data_nona_age[branch==4 & sex==1, age], final_data_nona_age[branch==1 & sex==1, age])
wilcox.test(final_data_nona_age[branch==4 & sex==1, age], final_data_nona_age[branch==2 & sex==1, age])
wilcox.test(final_data_nona_age[branch==4 & sex==1, age], final_data_nona_age[branch==3 & sex==1, age])
wilcox.test(final_data_nona_age[branch==4 & sex==1, age], final_data_nona_age[branch==7 & sex==1, age])

hist(final_data_nona_age$age)

final_data_nona_age <- na.omit(final_data)
final_data_nona_age[sex==1, mean(age), by = rank_group]
final_data_nona_age[sex==1, sd(age), by = rank_group]

final_data_nona_age[sex==0, mean(age), by = rank_group]
final_data_nona_age[sex==0, sd(age), by = rank_group]

final_data_nona_age[sex==1, mean(age), by = branch]
final_data_nona_age[sex==1, sd(age), by = branch]

final_data_nona_age[sex==0, mean(age), by = branch]
final_data_nona_age[sex==0, sd(age), by = branch]

final_data[sex==0 & branch==2, unique(rank)]
final_data[sex==1 & branch==2, unique(rank)]

data_sex_corrected <- fread("chelovek_vpp_5_percent_sex_corrected.tsv", encoding = "UTF-8")
final_data <- final_data[, !"sex"]

final_data <- merge(final_data, data_sex_corrected[, c("id", "sex")], by = "id")
write.csv(final_data, "final_data_on_rank_age_sex_branch2.csv")

final_data[sex==1 & branch==2, .N, by = rank]
final_data[sex==0 & branch==2, .N, by = rank]

#IS
library(data.table)
library(foreign)
library(stringr)

dataTestMit <- dataTest[id%in%sample(unlist(unique(dataTest[,id])),size=100)]#выбирает 100 случайных по id

setwd('D:/AnDan/Ne(izvestniy)')
dat<- fread("DataSet_Ne(izvestniy)/chelovek_vpp_reduced_100k.tsv", encoding = "UTF-8")

names(dat)
View(dat)

dataRank <- dat[,c("id","rank")]#выбор двух столбцов для присвоения званий и родов войск
View(dataRank)


dataTest <- dat[,c("id","place_birth:main_region","place_birth:oblast","date_birth:year","data_i_mesto_priziva:year","data_i_mesto_priziva:main_region","data_i_mesto_priziva:republic","data_i_mesto_priziva:oblast")]#выбрать данные по дате,региону и месту призыва, с id


b <- unlist(unique(dataTest[,"priziv_oblast"]))#превратить список областей в лист
View(b)

V <- stringr::str_sort(b,decreasing = F)#отсортировать список по алфавиту
View(V)

setnames(dataTest,old=c("place_birth:main_region","place_birth:oblast","date_birth:year","data_i_mesto_priziva:year","data_i_mesto_priziva:main_region","data_i_mesto_priziva:republic","data_i_mesto_priziva:oblast"),new=c("birth_region","birth_oblast","birth_year","priziv_year","priziv_main_region","priziv_republic","priziv_oblast"))#поменять имена на нормальные, т.к. старое название нихрена не выбиралось в строчке ниже с убиранием na
View(dataTest)

na_rm_regions_prizyv <- dataTest[!(is.na(priziv_year))&priziv_year>1940&!(is.na(priziv_republic))]#смотрим, без 
View(na_rm_regions_prizyv)

vector1 <- c('Калин','Курск','Брест','Вилен','Киев','Полтав','Смолен','Сталин','Сумск')#паттерн для исследуемых областей

spisok_reg <- grep(paste(vector1,collapse="|"),b,value = T)#выбрать все упоминания нужных нам регионов

  na_rm_regions_prizyv_tselev <- na_rm_regions_prizyv[priziv_oblast%in%spisok_reg,.N,by=c("priziv_oblast","priziv_year")]

View(na_rm_regions_prizyv_tselev)


#KI
library(data.table)
df_raw <- fread("chelovek_vpp_5_percent.tsv")

View(df_raw)[1:10]

ranks <- unique(df_raw$rank)
write.csv(ranks, "ranks.csv", fileEncoding = "UTF-8")

nrow(df_raw[df_raw$rank == "МК-1"])

table(df_raw$rank)

df_raw <- as.data.table(df_raw)

library(readxl)
ranks_codes <- read_excel("newdata/ranks.xlsx")

df1 <- df_raw

final_df <- fread("newdata/final_data_on_rank_age_sex_branch_UTF.csv", encoding = "UTF-8")
final_df <- as.data.table(final_df)

final_df[, age := (`data_i_mesto_priziva:year` - `date_birth:year`)]
final_df[age <= 10, age := NA]

names(final_df) <- c("V1","id","rank","rank_group","branch","birth_year","draft_year","sex","age")
#names(final_df_1940) <- c("V1","id","rank","rank_group","branch","birth_year","draft_year","sex","age")

final_df[, ]

final_df_1940 <- final_df[draft_year >= 1940]
final_df_1940 <- final_df_1940[draft_year <= 1945]
final_df_1940[is.na(rank_group) == T, rank_group := 0]



library(ggplot2)
library(plotly)
library(dplyr)

# new datasets
# absolute
final_df_1940m <- final_df_1940 %>% filter(sex == 1) %>% count(draft_year, rank_group) %>% 
  rename(count = n) %>% mutate(year = as.factor(draft_year)) %>%
  mutate(rank_group = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer")))

final_df_1940f <- final_df_1940 %>% filter(is.na(rank_group) == F) %>% filter(sex == 0) %>% count(draft_year, rank_group) %>% 
  rename(count = n) %>% mutate(year = as.factor(draft_year)) %>%
  mutate(rank_group = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer")))  
# share
final_df_1940m_share <- final_df_1940 %>% filter(sex == 1) %>% count(draft_year, rank_group) %>% 
  rename(count = n) %>% mutate(draft_year = as.factor(draft_year)) %>%
  mutate(rank_group = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer"))) %>% group_by(draft_year) %>% 
  mutate(share = count/(sum(count)))

final_df_1940f_share <- final_df_1940 %>% filter(is.na(rank_group) == F)%>% filter(sex == 0) %>% count(draft_year, rank_group) %>% 
  rename(count = n) %>% mutate(draft_year = as.factor(draft_year)) %>%
  mutate(rank_group = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer"))) %>% group_by(draft_year) %>% 
  mutate(share = count/(sum(count)))


# df <- cbind(c(1940, 1940, 1944, 1945), 
#             c(NA, "Unknown", "Officer", "Officer"),
#             c(0,0,0,0),
#             c(0,0,0,0))
# names(df) <- c("draft_year", "rank_group", "count", "year")
# final_df_1940f <- rbind(final_df_1940f, df)
# 
# # bars - absolute values - by rank group
# 
# split_by_rank_abs <- final_df_1940 %>% count(draft_year, rank_group) %>% 
#   rename(count = n) %>% mutate(year = as.factor(draft_year)) %>%
#   mutate(rank_group = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer"))) %>% 
#   plot_ly() %>% # all
#   add_bars(y = ~count, x = ~draft_year, color = ~rank_group, showlegend = T, legendgroup = ~rank_group, visible = T) %>% 
#   layout(title = "Absolute Numbers",
#     xaxis = list(title = 'Draft Year'), 
#     yaxis = list(title = 'Number'),
#     barmode = "stack"
#   ) %>% # males
#   add_bars(y = final_df_1940m$count, x = final_df_1940m$draft_year, color = final_df_1940m$rank_group, showlegend = T, legendgroup = ~rank_group, visible = F) %>% 
#   layout(title = "Absolute Numbers",
#          xaxis = list(title = 'Draft Year'), 
#          yaxis = list(title = 'Number'),
#          barmode = "stack"
#   ) %>% # females
#   add_bars(y = final_df_1940f$count, x = final_df_1940f$draft_year, color = final_df_1940f$rank_group, showlegend = T, legendgroup = ~rank_group, visible = F) %>% 
#   layout(title = "Absolute Numbers",
#          xaxis = list(title = 'Draft Year'), 
#          yaxis = list(title = 'Number'),
#          barmode = "stack",
#          updatemenus = list(
#            list(
#              type = 'buttons',
#              buttons = list(
#                list(
#                  method = 'restyle',
#                  args = list("visible", list(T,F,F)),
#                  label = 'All'
#                ),
#                list(
#                  method = 'restyle',
#                  args = list("visible", list(F,T,F)),
#                  label = 'Males'
#                ),
#                list(
#                  method = 'restyle',
#                  args = list("visible", list(F,F,T)),
#                  label = 'Females'
#                )
#              ),
#              active = 0,
#              x = -0.1, xanchor = 'right'
#            )
#          )
#   ) 
# 
# # bars - share - by rank group
# 
# split_by_rank_share <- final_df_1940 %>% count(draft_year, rank_group) %>% 
#   rename(count = n) %>% mutate(draft_year = as.factor(draft_year)) %>%
#   mutate(rank_group = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer"))) %>% group_by(draft_year) %>% 
#   mutate(share = count/(sum(count))) %>%
#   plot_ly() %>% 
#   add_bars(y = ~share, x = ~draft_year, color = ~rank_group, showlegend = F, legendgroup = ~rank_group) %>% 
#   layout(title = "Persentage",
#     xaxis = list(title = 'Draft Year'), 
#     yaxis = list(title = 'Number'),
#     barmode = "stack"
#   )
#  # grouping: split by rank groups
# 
# subplot(split_by_rank_abs, split_by_rank_share, nrows = 2, shareX = T) %>% layout(title = 'Military Draft 1940-1945: Split by Rank Groups')
# 





## 
# bars - absolute values - by rank group, all

split_by_rank_abs <- final_df_1940 %>% count(draft_year, rank_group) %>% 
  rename(count = n) %>% mutate(year = as.factor(draft_year)) %>%
  mutate(rank_group = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer"))) %>% 
  plot_ly() %>% 
  add_bars(y = ~count, x = ~draft_year, color = ~rank_group, showlegend = T, legendgroup = ~rank_group) %>% 
  layout(title = "Absolute Numbers",
         xaxis = list(title = 'Draft Year'), 
         yaxis = list(title = 'Number'),
         barmode = "stack"
  )


# bars - share - by rank group

split_by_rank_share <- final_df_1940 %>% count(draft_year, rank_group) %>% 
  rename(count = n) %>% mutate(draft_year = as.factor(draft_year)) %>%
  mutate(rank_group = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer"))) %>% group_by(draft_year) %>% 
  mutate(share = count/(sum(count))) %>%
  plot_ly() %>% 
  add_bars(y = ~share, x = ~draft_year, color = ~rank_group, showlegend = F, legendgroup = ~rank_group) %>% 
  layout(title = "Percentage",
         xaxis = list(title = 'Draft Year'), 
         yaxis = list(title = 'Number'),
         barmode = "stack"
  )
# grouping: split by rank groups

subplot(split_by_rank_abs, split_by_rank_share, nrows = 2, shareX = T) %>% layout(title = 'USSR Military Draft 1940-1945: Split by Rank Groups, Both Sexes')




# bars - absolute values - by rank group, males

split_by_rank_abs_m <- final_df_1940m  %>% 
  plot_ly() %>% 
  add_bars(y = ~count, x = ~draft_year, color = ~rank_group, showlegend = T, legendgroup = ~rank_group) %>% 
  layout(title = "Absolute Numbers",
         xaxis = list(title = 'Draft Year'), 
         yaxis = list(title = 'Number'),
         barmode = "stack"
  )


# bars - share - by rank group

split_by_rank_share_m <- final_df_1940m_share %>% 
  plot_ly() %>% 
  add_bars(y = ~share, x = ~draft_year, color = ~rank_group, showlegend = F, legendgroup = ~rank_group) %>% 
  layout(title = "Percentage",
         xaxis = list(title = 'Draft Year'), 
         yaxis = list(title = 'Number'),
         barmode = "stack"
  )
# grouping: split by rank groups

subplot(split_by_rank_abs_m, split_by_rank_share_m, nrows = 2, shareX = T) %>% layout(title = 'USSR Military Draft 1940-1945: Split by Rank Groups, MALES')



# bars - absolute values - by rank group, females

split_by_rank_abs_f <- final_df_1940f  %>% 
  plot_ly() %>% 
  add_bars(y = ~count, x = ~draft_year, color = ~rank_group, showlegend = T, legendgroup = ~rank_group) %>% 
  layout(title = "Absolute Numbers",
         xaxis = list(title = 'Draft Year'), 
         yaxis = list(title = 'Number'),
         barmode = "stack"
  )


# bars - share - by rank group

split_by_rank_share_f <- final_df_1940f_share %>% 
  plot_ly() %>% 
  add_bars(y = ~share, x = ~draft_year, color = ~rank_group, showlegend = F, legendgroup = ~rank_group) %>% 
  layout(title = "Percentage",
         xaxis = list(title = 'Draft Year'), 
         yaxis = list(title = 'Number'),
         barmode = "stack"
  )
# grouping: split by rank groups

subplot(split_by_rank_abs_f, split_by_rank_share_f, nrows = 2, shareX = T) %>% layout(title = 'USSR Military Draft 1940-1945: Split by Rank Groups, FEMALES')

# ggplots - bars

final_df_1940 %>% ggplot(aes(y = age, x = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer")), fill = factor(rank_group, labels = c("Unknown", "Soldier", "NCO", "Officer")))) + geom_boxplot(alpha = 0.65) + theme_bw() + labs(x = "", y = "Age", fill = "Rank group")

final_df_1940 %>% ggplot(aes(y = age, x = factor(sex, labels = c("Female", "Male")), fill = factor(sex, labels = c("Female", "Male")))) + geom_boxplot(alpha = 0.65) + theme_bw() + labs(x = "", y = "Age", fill = "Sex")

final_df_1940 %>% filter(branch %in% c(0, 2, 4, 7)) %>% ggplot(aes(y = age, x = factor(branch, labels = c("Miscellaneous", "Medic", "Navy", "Engineer")), fill = factor(branch, labels = c("Miscellaneous", "Medic", "Navy","Engineer")))) + geom_boxplot(alpha = 0.65) + theme_bw() + labs(x = "", y = "Age", fill = "Branch") + ggtitle("Age by Branch")

final_df_1940 %>% filter(branch == 2) %>% ggplot(aes(y = age, x = factor(sex, labels = c("Female", "Male")), fill = factor(sex, labels = c("Female", "Male")))) + geom_boxplot(alpha = 0.65) + theme_bw() + labs(x = "", y = "Age", fill = "Sex") + ggtitle("Age of Medics by Sex")
