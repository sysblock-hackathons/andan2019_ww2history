library(data.table)
library(dplyr)
library(readxl)
library(plotly)

Sys.setlocale('LC_CTYPE', 'UTF-8')
df <- fread('/Users/Пользователь/Downloads/chelovek_vpp_reduced_100k.tsv', encoding='UTF-8')
indices <- read_xlsx('/Users/Пользователь/Downloads/Telegram Desktop/indices.xlsx')
rsfsr <- indices %>% filter(`place_birth:main_region`==28)

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
    source = rsfsr$`place_birth:main_region`,
    target = rsfsr$`data_i_mesto_priziva:main_region`,
    value =  rsfsr$N
  )
) %>% 
  layout(
    title = "Куда мигрировали солдаты до призыва (данные по всем республикам)",
    font = list(
      size = 10
    )
  )
p