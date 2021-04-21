#install.packages("tidyverse")
library("tidyverse")

# install.packages("nycflights13")
library("nycflights13")

fdata <- flights
summary(fdata)
glimpse(fdata)
str(fdata)

grades <- tibble(
    name = c("Erik", "Eric", "Thomas"),
    quiz_1 = c(10, 9, 6),
    exam = c(7, 6, NA)
)
grades

arrange(fdata, arr_delay)
arrange(fdata, -arr_delay)
arrange(fdata, carrier, -arr_delay)

fdata_0501 <- filter(fdata, month == 5, day == 1)
filter(fdata, !is.na(arr_time))

select(fdata, arr_delay, carrier)
select(fdata, month : arr_time )

mutate(fdata, arr_delay_2 = arr_delay^2 )
mutate(fdata, arr_delay_s = arr_delay/60)

summarize(fdata, 
          ave_arr_delay = mean(arr_delay),
          sd_arr_delay = sd(arr_delay)
          )

summarize(fdata, 
          ave_arr_delay = mean(arr_delay, na.rm = TRUE),
          sd_arr_delay = sd(arr_delay, na.rm = TRUE)
)

rename(fdata, 
       airline = carrier)

distinct(fdata, carrier)

summarize(fdata, count = n() )

fdata_carrier <- group_by(fdata, carrier)
summarize(fdata_carrier, 
          mean_arr_delay = mean(arr_delay, na.rm = TRUE),
          number = n())

fdata_carrier_month <- group_by(fdata, carrier, month)
summarize(fdata_carrier_month, 
          mean_arr_delay = mean(arr_delay, na.rm = TRUE),
          number = n())

fdata_carrier <- fdata %>%
            group_by(carrier) %>%
            summarize(
                mean_arr_delay = mean(arr_delay, na.rm = TRUE),
                number = n())

ggplot(fdata_carrier, aes(x = reorder(carrier, -mean_arr_delay), y = mean_arr_delay)) + 
    geom_bar(stat = "identity") +
    labs(x = "Airline code", y = "Mean arrival delay")

fdata_carrier_month <- fdata %>%
    group_by(carrier, month) %>%
    summarize(
        mean_arr_delay = mean(arr_delay, na.rm = TRUE),
        number = n())

ggplot(fdata_carrier_month, aes(x = carrier, y = mean_arr_delay)) + 
    geom_bar(stat = "identity") + 
    theme_bw() + 
    labs(x = "Airline code", y = "Mean arrival delay") + 
    theme(text = element_text(size = 12) , 
          axis.text.x = element_text(angle=90, hjust=1))  + 
    facet_wrap(~month)

fdata_carrier_month

grades <- tibble(
    name = c("Erik", "Eric", "Thomas", "Jos"),
    quiz_1 = c(10, 9, 6, 8),
    quiz_2 = c(9, 9, 5, 7),
    exam = c(7, 6, NA, 7)
)

grades

grades2 <- grades %>% 
    pivot_longer(quiz_1:exam, names_to = "assessment", values_to = "grade") %>% 
    arrange(name, assessment)
grades2

relig_income

billboard

billboard2 <- billboard %>% 
    pivot_longer(
        wk1:wk76, 
        names_to = "week", 
        values_to = "rank", 
        values_drop_na = TRUE
    )

who

who1 <- who %>% 
    pivot_longer(
        cols = new_sp_m014:newrel_f65, 
        names_to = "key", 
        values_to = "cases", 
        values_drop_na = TRUE
    )
who1

who2 <- who1 %>% 
    mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>% 
    separate(key, c("new", "type", "sexage"), sep = "_")
who3

who4 <- who3 %>% 
    select(-new, -iso2, -iso3)

who5 <- who4 %>% 
    separate(sexage, c("sex", "age"), sep = 1)
who5

weather <- tibble(
    station = c("Amsterdam", "Amsterdam", "Maastricht", "Maastricht"),
    element = c("min", "max", "min", "max"),
    d1 = c(4, 11, 3, 15),
    d2 = c(7, 10, 2, 10),
    d3 = c(5, 14, 5, 16)
)
weather

weather2 <- weather %>% 
    pivot_longer(
        d1:d3, 
        names_to = "day", 
        values_to = "temperature", 
        values_drop_na = TRUE
    ) 
weather2

weather2 %>% 
    pivot_wider(names_from = element, values_from = temperature)

housing_prices <- tibble(
    prices = c(600000,500000, 470000),
    city = c("Aadam", "Bedam", "Cedam"), 
    historic_centre = c(0, 0, 1)
)
housing_prices

housing_prices <- housing_prices %>%
    mutate(obs = seq(1: nrow(housing_prices))) %>%
    mutate(chosen = rep(1, nrow(housing_prices) ) ) %>%
    pivot_wider(names_from = city, values_from = chosen) %>%
    replace(is.na(.), 0)
    
housing_prices
    
housing_prices <- housing_prices %>%
    pivot_longer(Aadam:Cedam, 
                 names_to = "city",
                 values_to =  "chosen")

housing_prices
