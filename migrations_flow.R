library(data.table)
library(R.utils)
library(dplyr)
library(readxl)
library(plotly)

Sys.setlocale('LC_CTYPE', 'UTF-8')
df <- fread('/Users/Пользователь/Downloads/chelovek_vpp_reduced_100k.tsv', encoding='UTF-8')

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c('АзССР','АрССР','БССР','ГрузССР','Зарубежье','КазССР','Карело-Финская ССР','КирССР','Латвийская ССР','Литовская ССР','Молдавская ССР','РСФСР','ТаджССР','ТуркССР','УзССР','УССР','Эстонская ССР','АзССР','АрССР','БССР','ГрузССР','Зарубежье','КазССР','Карело-Финская ССР','КирССР','Латвийская ССР','Литовская ССР','Молдавская ССР','РСФСР','ТаджССР','ТуркССР','УзССР','УССР','Эстонская ССР'),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = indices$`place_birth:main_region`,
    target = indices$`data_i_mesto_priziva:main_region`,
    value =  indices$N
  )
) %>% 
  layout(
    title = "Куда мигрировали солдаты до призыва (данные по всем республикам)",
    font = list(
      size = 10
    )
  )
p

birth <- as.data.frame(table(df$`place_birth:main_region`))
prizyv <- as.data.frame(table(df$`data_i_mesto_priziva:main_region`))

prizyvy <- merge(birth,prizyv,by='Var1',all=T)
prizyvy

cutcutcut <- df %>% select(id,`place_birth:main_region`,`data_i_mesto_priziva:main_region`)
#==============
cutcutcut <- as.data.table(cutcutcut)
places <- cutcutcut[`place_birth:main_region`!= `data_i_mesto_priziva:main_region`, .N, by = .(`data_i_mesto_priziva:main_region`, `place_birth:main_region`)]
#==============
rep_births <- birth %>% arrange(Var1)

write.csv2(places,"places.csv")
indices <- read_xlsx('/Users/Пользователь/Downloads/Telegram Desktop/indices.xlsx')

