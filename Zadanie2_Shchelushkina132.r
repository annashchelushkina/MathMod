

library(dplyr)
library(tidyverse)
library(ellipse)
library(lubridate)
install.packages("car")
install.packages("caret")


#Загрузим необходимые данные
tbl = read.csv("eddypro.csv", skip = 1, 
                na =c("","NA","-9999","-9999.0"), comment=c("["))

#Подготовим данные к анализу
tbl = tbl[-1]
tbl
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)
tbl$date <- as.Date (tbl$date, format = "%Y-%m-%d")


#Оставляем необходимый год
tbl = filter(tbl, year %in% 2013)

#Оставляем необходимые месяцы
tbl = filter(tbl, month %in% "Май")

#Убираем значения NA
tbl = drop_na(tbl)


#Приводим данные к числовым
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]

#Проводим корреляционный анализ
cor_td = cor(drop_na(tbl_numeric), method = "spearman") %>% as.data.frame %>% select(co2_flux)
cor_td

#Оставляем столбец с эмиссией CO2 и со всеми значениями
vars = row.names(cor_td)

#Оставляем столбец с эмиссией CO2 и значениями > 0,1
vars = row.names(cor_td)[cor_td$co2_flux^2 > 0.1] %>% na.exclude
vars


#Запишем параметры (>0.1) для составления модели регрессионного анализа
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))
formula

#модель 1
model1=lm(co2_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
            rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
            H_strg + h2o_time_lag + sonic_temperature + air_temperature + 
            air_density + air_molar_volume + es + RH + VPD + max_speed + 
            u. + TKE + L + X.z.d..L + T. + x_offset + x_50. + x_70. + 
            x_90. + un_Tau + un_H + un_LE + LE_scf + un_co2_flux + co2_scf + 
            un_h2o_flux + h2o_scf + u_var + v_var + w_var + h2o_var + 
            w.ts_cov + w.co2_cov + w.h2o_cov + flowrate, data = tbl_numeric)

#Вывод результатов модели, коэффициентов, остатков, оценка значимости переменных
coef(model1)
resid(model1)
confint(model1)
summary(model1)
anova(model1)


#модель 2
model2=lm(co2_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
            rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
            H_strg + h2o_time_lag + sonic_temperature + air_temperature + 
            air_density + air_molar_volume + es + VPD + max_speed + 
            u. + TKE + X.z.d..L + T. + x_offset + x_50. + x_70. + 
            x_90. + un_H + un_LE + LE_scf + un_co2_flux + co2_scf + 
            un_h2o_flux + h2o_scf + v_var + w_var + h2o_var + 
            w.ts_cov + w.co2_cov + w.h2o_cov + flowrate, data = tbl_numeric)

#Вывод результатов модели, коэффициентов, остатков, оценка значимости переменных
coef(model2)
resid(model2)
confint(model2)
summary(model2)
anova(model2)


#модель 3
model3=lm(co2_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
            rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
            H_strg + sonic_temperature + air_temperature + 
            air_density + air_molar_volume + es + VPD + max_speed + 
            u. + TKE + X.z.d..L + T. + x_offset + x_50. + x_70. + 
            x_90. + un_H + un_LE + LE_scf + un_co2_flux + co2_scf + 
            un_h2o_flux + h2o_scf + h2o_var + 
            w.ts_cov + w.h2o_cov + flowrate, data = tbl_numeric)

#Вывод результатов модели, коэффициентов, остатков, оценка значимости переменных
coef(model3)
resid(model3)
confint(model3)
summary(model3)
anova(model3)


#Лучшая модель - №3
#(по сравнению с моделью №2 - отклонение изменяется незначительно
#при этом в модели №6 все параметры имеют значимое влиение - "***")


