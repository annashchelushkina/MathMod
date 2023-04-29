#Щелушкина А.А., Д-А 132, задание 1

install.packages("tidyverse")
install.packages("rnoaa")
library(tidyverse)
library(rnoaa)
library(dplyr)
library(lubridate)

#Загрузим данные по всем метеостанциям, переведем их в формат таблицы
station_data = ghcnd_stations()
write.csv(station_data, file = "station_data.csv")
station_data = read_csv("station_data.csv")

#Загрузим данные по региону 51 (Мурманск)
murmansk = data.frame(id="MURMANSK", latitude =68.970663, longitude = 33.074918)
murmansk_around = meteo_nearby_stations(lat_lon_df = murmansk, 
                                        station_data = station_data, 
                                        limit = 20, var = c("PRCP","TAVG"), 
                                        year_min = 2007, year_max = 2008)

#Фильтруем данные по метеостанциям по расстоянию до 250 км от Мурманска
murmansk_table = murmansk_around[[1]]
murmansk_table = filter(murmansk_table,distance > 0 & distance < 251)

#Найдем id и загрузим все метеоданные 
murmansk_id = murmansk_around$MURMANSK$id[1]
all_murmansk_data = meteo_tidy_ghcnd(stationid = murmansk_id)

#Изменим класс данных
all_murmansk_data$date
class(all_murmansk_data$date)
all_murmansk_data$date+1
as.numeric(all_murmansk_data$date)

#Создадим новую таблицу с колонками "год", "месяц", "день" и "средняя температура"
all_murmansk_data1 = all_murmansk_data %>% mutate(
  year = year(all_murmansk_data$date), 
  month = month(all_murmansk_data$date), 
  day = yday(all_murmansk_data$date)
) %>% select(year, month, day, tavg)

#Приведем значения средних температур в нужный вид
all_murmansk_data2 = all_murmansk_data1 %>% mutate(tavg = case_when(TRUE ~ tavg/10)
) 

#Фильтруем данные, выбираем 2007 год
all_murmansk_data2 = filter(all_murmansk_data2,year > 2006 & year < 2008)

#Приравняем NA и температуры < 5 градусов к 0
all_murmansk_data2[is.na(all_murmansk_data2$tavg),"tavg"] = 0
all_murmansk_data2[all_murmansk_data2$tavg<5, "tavg"] = 0

#Сгруппируем данные по году и месяцу
group__murmansk_meteodata =all_murmansk_data2 %>% group_by(year,month)

#Суммируем температуры
sumT_group_murmansk_meteodata = group__murmansk_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_murmansk_meteodata%>%group_by(month) 
sumT_month=groups_month%>%summarise(St=mean(tsum))

#Добавим данные
y = 1.0
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300
Qj = 1600
Lj = 2.2
Ej = 25

#Расчитаем Fi, Yi и урожайность
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
Yield = (sum(sumT_month$Yi))

#Урожайность = 8.15 ц/га
