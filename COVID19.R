#### COVID-19 Analysis CH

library(tidyverse)
library(devtools)
library(usethis)
setwd("~/Documents/04_Misc/Coding/covid19-master")

#### Github ####
# The functions' gist ID is 4466237
source_gist("4466237")

# Make sure the URL is for the "raw" version of the file
UrlAddress <- "https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total.csv"

# Download data
df1 <- source_GitHubData(url = UrlAddress)

#### Cleaning ####
#df1 <- read_csv("covid_19-master/COVID19_Fallzahlen_CH_total.csv")
df1 <- select(df1, date, abbreviation_canton_and_fl, ncumul_tested,
              ncumul_conf, ncumul_hosp, ncumul_ICU, ncumul_vent, 
              ncumul_released, ncumul_deceased)
df1 <- mutate(df1, date=as.Date(date))

# add log scale values
df1 <- df1 %>% 
  mutate(ln_tested = log(ncumul_tested)) %>% 
  mutate(ln_conf = log(ncumul_conf)) %>% 
  mutate(ln_hosp = log(ncumul_hosp)) %>% 
  mutate(ln_ICU = log(ncumul_ICU)) %>% 
  mutate(ln_vent = log(ncumul_vent)) %>% 
  mutate(ln_released = log(ncumul_released)) %>% 
  mutate(ln_deceased = log(ncumul_deceased))

# Create a data frame for the Canton of Bern
df_be <- filter(df1, abbreviation_canton_and_fl == "BE")

# Create a data frame for Switzerland
df1 <- group_by(df1, date)

tested_ch <- summarize(df1, ncumul_tested = sum(ncumul_tested, na.rm = T)) 
conf_ch <- summarize(df1, ncumul_conf = sum(ncumul_conf, na.rm = T))
hosp_ch <- summarize(df1, ncumul_hosp = sum(ncumul_hosp, na.rm = T))
ICU_ch <- summarize(df1, ncumul_ICU = sum(ncumul_ICU, na.rm = T))
vent_ch <- summarize(df1, ncumul_vent = sum(ncumul_vent, na.rm = T))
rel_ch <- summarize(df1, ncumul_released = sum(ncumul_released, na.rm = T))
dec_ch <- summarize(df1, ncumul_deceased = sum(ncumul_deceased, na.rm = T))

df_ch <-  data.frame(tested_ch, conf_ch, hosp_ch, ICU_ch, vent_ch, rel_ch, dec_ch)
rm(tested_ch, conf_ch, hosp_ch, ICU_ch, vent_ch, rel_ch, dec_ch)

df_ch <- df_ch[-c(3,5,7,9,11,13)]

df_ch <- df_ch %>% 
  mutate(ln_tested = log(ncumul_tested)) %>% 
  mutate(ln_conf = log(ncumul_conf)) %>% 
  mutate(ln_hosp = log(ncumul_hosp)) %>% 
  mutate(ln_ICU = log(ncumul_ICU)) %>% 
  mutate(ln_vent = log(ncumul_vent)) %>% 
  mutate(ln_released = log(ncumul_released)) %>% 
  mutate(ln_deceased = log(ncumul_deceased))

df_ch <- df_ch[-c(nrow(df_ch)-1, nrow(df_ch)), ] # data from the last two days is usually incomplete

#### Plots CH ####
jpeg(file="plots/confirmed.jpeg")
df_ch %>% 
  ggplot(aes(x = date, y = ncumul_conf)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Confirmed Cases",
       title="Confirmed COVID-19 Cases in Switzerland") +
  theme_bw()
dev.off()

jpeg(file="plots/hosp.jpeg")
df_ch %>% ggplot(aes(x = date, y = ncumul_hosp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="People Hospitalized",
       title="People Hospitalized in Switzerland") +
  theme_bw()
dev.off()

jpeg(file="plots/icu.jpeg")
df_ch %>% ggplot(aes(x = date, y = ncumul_ICU)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="ICU Cases",
       title="ICU Cases in Switzerland") +
  theme_bw()
dev.off()

jpeg(file="plots/vent.jpeg")
df_ch %>% ggplot(aes(x = date, y = ncumul_vent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Ventilator Cases",
       title="Ventilator Cases in Switzerland") +
  theme_bw()
dev.off()

jpeg(file="plots/deaths.jpeg")
df_ch %>% ggplot(aes(x = date, y = ncumul_deceased)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x="Time", y="Deaths",
       title="Deaths due to COVID-19 in Switzerland") +
  theme_bw()
dev.off()

jpeg(file="plots/total.jpeg")
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
  theme_bw() +
  theme(legend.position = c(.3,.7))
dev.off()
