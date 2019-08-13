library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
theme_set(theme_light(base_size = 14))

Sys.setlocale('LC_CTYPE', 'UTF-8')  # if for any reason this is not default
library(jsonlite)
df_raw <- stream_in(file('chelovek_vpp_5_percent.tsv'))

library(data.table)
df <- fread('chelovek_vpp_5_percent.tsv')


soldier <- fread("chelovek_vpp_5_percent.tsv", encoding = "UTF-8")

setwd("~/Hackaton/chelovek_vpp_5_percent.tsv")
soldier <- fread("chelovek_vpp_5_percent.tsv", encoding = "UTF-8")
head(soldier)

#Убрали дубликаты (их нет!)
soldier <- distinct(soldier)


#График по полу
soldier %>% select(sex) %>% count(sex) %>% plot_ly(color = ~sex, alpha = 0.6) %>% 
  add_bars(x = ~sex, y = ~n, bar = list(color = "black")) %>% 
  layout(title = "Доля мужчин и женщин на фронте", xaxis = list(title = 'Пол'), yaxis = list(title = ''))



#Смотрим призыв женщин по годам
soldier %>% select(sex, "data_i_mesto_priziva:year", rank) %>% drop_na("data_i_mesto_priziva:year") %>% rename(year = "data_i_mesto_priziva:year") %>% filter(sex == 0, year != 1917, year != 1919, year != 1926)  %>% count(year)

Избавиться от 1917 (это мужчина)

#Вычисляем женщину
soldier %>% select(sex, date_birth, first_name, last_name, middle_name, "data_i_mesto_priziva:year", rank) %>% drop_na("data_i_mesto_priziva:year") %>% rename(year = "data_i_mesto_priziva:year") %>% filter(sex == 0, year == 1938) 
                                                                                                                     

100 случайных женщин, оценить, кто из них женщина
Устранить мужчин (отчество на ич, фамилии на ов, ин, имя с точкой и фамилия не на а одновременно)
(Топ-1000 женских имен)


100 случайных женщин после эвристики
soldier %>% select(sex, first_name, last_name, middle_name) %>% filter(sex == 0) %>% do(sample_n(.,100))



#100 случайных женщин
data <- soldier %>% select(sex, first_name, last_name, middle_name) %>% filter(sex == 0) %>% do(sample_n(.,100)) %>% select(first_name, last_name, middle_name)
write.csv(data, file = "Name.csv")

#Создаем столбик с последними двумя буквами фамилии
soldier[, letters_of_last_name := substr(last_name, nchar(last_name)-0, nchar(last_name))]
soldier[, letters_of_last_name2 := substr(last_name, nchar(last_name)-1, nchar(last_name))]

#Создаем столбик с последним знаком точкой в имени
soldier[, tot_in_first_name := substr(first_name, nchar(first_name)-0, nchar(first_name))]
#Создаем столбик с последними двумя буквами отчества
soldier[, letters_of_middle_name := substr(middle_name, nchar(middle_name)-1, nchar(middle_name))]
soldier[, letters_of_middle_name2 := substr(middle_name, nchar(middle_name)-0, nchar(middle_name))]

#Женщины с отчеством, оканчивающимся на "ич"
soldier %>% select(sex, letters_of_middle_name) %>% filter(sex == 0, letters_of_middle_name == "ич")  


Эвристика: имя на точку и фамилия заканчивается не на а, отчество заканчивается не на "а"
name <- soldier %>% select(sex, first_name, middle_name, last_name, tot_in_first_name, letters_of_middle_name2, letters_of_last_name) %>% filter(sex == 0) %>%  filter(tot_in_first_name == ".") %>% filter(letters_of_middle_name2 != "а") %>% filter(letters_of_last_name != "а") %>% filter(letters_of_last_name != "я")

#!!! Отфильтровали мужчин среди женщин по следующей эвристике: имя заканчивается на точку и фамилия заканчивается не на "а" и не на "ая"
name1 <- soldier %>% select(sex, first_name, middle_name, last_name, tot_in_first_name, letters_of_middle_name2, letters_of_last_name, letters_of_last_name2) %>% filter(sex == 0) %>%  filter(tot_in_first_name == ".") %>% filter(letters_of_last_name != "а") %>% filter(letters_of_last_name2 != "ая")








#v <- soldier[1, last_name]
#substr(v, nchar(v)-0, nchar(v))
#lapply
#a <- soldier[1, first_name]
#b <- soldier[20, middle_name]
#substr(b, nchar(b)-0, nchar(b))
