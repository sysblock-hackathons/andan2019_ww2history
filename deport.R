library(foreign)
library(data.table)
VOV <- fread('chelovek_vpp_5_percent.tsv', encoding = "CP1251")

#депортированные
#тут мы открываем отдельный датасет, где сохранены только те наблюдения, где указано название республики рождения солдата (наблюдения выделены из всех данных, а не предложенных ранее 5%)   
VOV_rpbk <- fread('rsfsr_republics_reduced.tsv', encoding = "UTF-8")
unique(VOV_rpbk[,`place_birth:republic`])
VOV_rpnk1 <- VOV_rpbk[`place_birth:republic`%in% c("Чечено-Ингушская АССР","Северо-Осетинская АССР","Немцев Поволжья АССР","Калмыцкая АССР")]
VOV_rpnk1 <- VOV_rpnk1[`data_i_mesto_priziva:year`>"1937"]

library(dplyr)
testdf <- VOV_rpnk1 %>% count(`place_birth:republic`, `data_i_mesto_priziva:year`)
#график с количеством призывников из 4 выбранных республик (абсолютные величины)
VOV_rpnk1 %>% count(`place_birth:republic`, `data_i_mesto_priziva:year`) %>% 
ggplot2::ggplot() +geom_line(aes(x = `data_i_mesto_priziva:year`, y = n, color = `place_birth:republic`), size=1.2) + theme_bw() + labs(x="", y = "Число призывников", color = "Республика")
#N-население республик по переписи 1939 года
populus <- data.frame(`place_birth:republic` = c("Калмыцкая АССР", "Чечено-Ингушская АССР", "Северо-Осетинская АССР", "Немцев Поволжья АССР"), N = c(
  179400, 697009, 329205, 606532), check.names = F)

testdf1 <- merge(testdf, populus, by = "place_birth:republic", all.x = T)
#ratio - количество призывников на каждые 10 тысяч человек населения республики
testdf1$ratio <- round(testdf1$n/testdf1$N*100000, 0)

#график с количеством призывников из 4 выбранных республик (относительные величины)
testdf1 %>% ggplot2::ggplot(aes(x = `data_i_mesto_priziva:year`, y = ratio, color = `place_birth:republic`)) +geom_line(size=1.2) + geom_point(size = 2.5) + theme_bw() + ylab("Число призывников на 10 тыс. чел.") + labs(color = "Республика") + xlab("") 

#Добавляем для сравнения наблюдения из Свердловской области
VOV_sverd <- fread('sverdlovskaya.tsv', encoding="UTF-8")
VOV_sverd1 <- VOV_sverd[`data_i_mesto_priziva:year`>"1937"]
VOV_sverd1 <- VOV_sverd1[`data_i_mesto_priziva:year`<"1946"]
testdf2 <- VOV_sverd1 %>% count(`place_birth:oblast`, `data_i_mesto_priziva:year`)
populus1 <- data.frame(`place_birth:oblast` = "Свердловская обл.", N = 2511809, check.names = F)
testdf2 <- merge(testdf2, populus1, by = "place_birth:oblast", all.x = T)
testdf2$ratio <- round(testdf2$n/testdf2$N*100000, 0)
colnames(testdf2)[1] <- "place_birth:republic"
testdf3 <- rbind(testdf1,testdf2)
#финальный график с 4 республиками и Свердловской областью, относительные величины
testdf3 %>% ggplot2::ggplot(aes(x = `data_i_mesto_priziva:year`, y = ratio, color = `place_birth:republic`)) +geom_line(size=1.2) + geom_point(size = 2.5) + theme_bw() + ylab("Число призывников на 10 тыс. чел.") + labs(color = "Место рождения") + xlab("")
