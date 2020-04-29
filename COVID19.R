#### COVID-19 CH Analysis

#### Cleaning ####
library(tidyverse)

df1 <- read_csv("COVID19_CH.csv")
View(df1)
df1 <- df1[-2] # deletes time
df1 <- df1[-10] # deletes source

df1[is.na(df1)] <- 0

df1 <- mutate(df1, ln_tested = log(df1$ncumul_tested))
df1 <- mutate(df1, ln_conf = log(df1$ncumul_conf))
df1 <- mutate(df1, ln_hosp = log(df1$ncumul_hosp))
df1 <- mutate(df1, ln_ICU = log(df1$ncumul_ICU))
df1 <- mutate(df1, ln_vent = log(df1$ncumul_vent))
df1 <- mutate(df1, ln_released = log(df1$ncumul_released))
df1 <- mutate(df1, ln_deceased = log(df1$ncumul_deceased))

# Create a data frame for the Canton of Bern
df_be <- filter(df1, abbreviation_canton_and_fl == "BE")

# Create a data frame for Switzerland
df1 <- group_by(df1, date)

tested_ch <- summarize(df1, ncumul_tested = sum(ncumul_tested)) 
conf_ch <- summarize(df1, ncumul_conf = sum(ncumul_conf))
hosp_ch <- summarize(df1, ncumul_hosp = sum(ncumul_hosp))
ICU_ch <- summarize(df1, ncumul_ICU = sum(ncumul_ICU))
vent_ch <- summarize(df1, ncumul_vent = sum(ncumul_vent))
rel_ch <- summarize(df1, ncumul_released = sum(ncumul_released))
dec_ch <- summarize(df1, ncumul_deceased = sum(ncumul_deceased))

df_ch <-  data.frame(tested_ch, conf_ch, hosp_ch, ICU_ch, vent_ch, rel_ch, dec_ch)
rm(tested_ch, conf_ch, hosp_ch, ICU_ch, vent_ch, rel_ch, dec_ch)

df_ch <- df_ch[-3]
df_ch <- df_ch[-4]
df_ch <- df_ch[-5]
df_ch <- df_ch[-6]
df_ch <- df_ch[-7]
df_ch <- df_ch[-8]

df_ch <- mutate(df_ch, ln_tested = log(df_ch$ncumul_tested))
df_ch <- mutate(df_ch, ln_conf = log(df_ch$ncumul_conf))
df_ch <- mutate(df_ch, ln_hosp = log(df_ch$ncumul_hosp))
df_ch <- mutate(df_ch, ln_ICU = log(df_ch$ncumul_ICU))
df_ch <- mutate(df_ch, ln_vent = log(df_ch$ncumul_vent))
df_ch <- mutate(df_ch, ln_released = log(df_ch$ncumul_released))
df_ch <- mutate(df_ch, ln_deceased = log(df_ch$ncumul_deceased))

df_ch <- df_ch[-c(nrow(df_ch)-1, nrow(df_ch)), ] # data from the last two days is usually incomplete


#### Canton of Bern ####
df_be %>% ggplot(aes(x = date, y = ncumul_conf)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Confirmed Cases",
       title="Confirmed COVID-19 Cases in the Canton of Bern") +
  theme_bw()

df_be %>% ggplot(aes(x = date, y = ln_conf)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Confirmed Cases, Logarithmic Scale",
       title="Confirmed COVID-19 Cases in the Canton of Bern (Log Scale)") +
  ylim(0, 10) +
  theme_bw()

df_be %>% ggplot(aes(x = date, y = ncumul_hosp)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="People Hospitalized",
       title="People Hospitalized in the Canton of Bern") +
  theme_bw()

df_be %>% ggplot(aes(x = date, y = ncumul_ICU)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="ICU Cases",
       title="ICU Cases in the Canton of Bern") +
  theme_bw()

df_be %>% ggplot(aes(x = date, y = ncumul_vent)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Ventilator Cases",
       title="Ventilator Cases in the Canton of Bern") +
  theme_bw()

df_be %>% ggplot(aes(x = date, y = ncumul_released)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="People Released",
       title="People Released in the Canton of Bern") +
  theme_bw()

df_be %>% ggplot(aes(x = date, y = ncumul_deceased)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Deaths",
       title="Deaths due to COVID-19 in the Canton of Bern") +
  theme_bw()

df_be %>% ggplot(aes(x = date, y = ln_deceased)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Deaths, Logarithmic Scale",
       title="Deaths due to COVID-19 in the Canton of Bern (Log Scale)") +
  ylim(0,4)
  theme_bw()


ggplot(df_be, aes(df_be$date)) +
  geom_line(aes(y = df_be$ncumul_conf, colour = "Confirmed Cases")) +
  geom_line(aes(y = df_be$ncumul_hosp, colour = "People Hospitalized")) +
  geom_line(aes(y = df_be$ncumul_ICU, colour = "ICU Cases")) +
  geom_line(aes(y = df_be$ncumul_deceased, colour = "Deaths")) +
  geom_line(aes(y = df_be$ncumul_released, colour = "People Released")) +
  scale_colour_manual("", 
                     breaks = c("Confirmed Cases", "People Hospitalized", "ICU Cases", "Deaths", "People Released"),
                     values = c("red", "black", "skyblue", "orange", "green3")) +
  labs(title="COVID-19 in the Canton of Bern", y = "Amount", x = "Time") +
  theme_bw()


#### Switzerland ####
df_ch %>% ggplot(aes(x = date, y = ncumul_conf)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Confirmed Cases",
       title="Confirmed COVID-19 Cases in Switzerland") +
  theme_bw()

df_ch %>% ggplot(aes(x = date, y = ln_conf)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Confirmed Cases, Logarithmic Scale",
       title="Confirmed COVID-19 Cases in Switzerland (Log Scale)") +
  ylim(0, 10) +
  theme_bw()

df_ch %>% ggplot(aes(x = date, y = ncumul_hosp)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="People Hospitalized",
       title="People Hospitalized in Switzerland") +
  theme_bw()

df_ch %>% ggplot(aes(x = date, y = ncumul_ICU)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="ICU Cases",
       title="ICU Cases in Switzerland") +
  theme_bw()

df_ch %>% ggplot(aes(x = date, y = ncumul_vent)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Ventilator Cases",
       title="Ventilator Cases in Switzerland") +
  theme_bw()

df_ch %>% ggplot(aes(x = date, y = ncumul_released)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="People Released",
       title="People Released in Switzerland") +
  theme_bw()

df_ch %>% ggplot(aes(x = date, y = ncumul_deceased)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Deaths",
       title="Deaths due to COVID-19 in Switzerland") +
  theme_bw()

df_ch %>% ggplot(aes(x = date, y = ln_deceased)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Deaths, Logarithmic Scale",
       title="Deaths due to COVID-19 in Switzerland (Log Scale)") +
  ylim(0,10) +
  theme_bw()

ggplot(df_ch, aes(df_ch$date)) +
  geom_line(aes(y = df_ch$ncumul_conf, colour = "Confirmed Cases")) +
  geom_line(aes(y = df_ch$ncumul_hosp, colour = "People Hospitalized")) +
  geom_line(aes(y = df_ch$ncumul_ICU, colour = "ICU Cases")) +
  geom_line(aes(y = df_ch$ncumul_deceased, colour = "Deaths")) +
  geom_line(aes(y = df_ch$ncumul_released, colour = "People Released")) +
  scale_colour_manual("", 
                      breaks = c("Confirmed Cases", "People Hospitalized", "ICU Cases", "Deaths", "People Released"),
                      values = c("red", "black", "skyblue", "orange", "green3")) +
  labs(title="COVID-19 in the Switzerland", y = "Amount", x = "Time") +
  theme_bw()
