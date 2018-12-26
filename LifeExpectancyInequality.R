library(tidyverse)
life_exp <- read_csv("/Users/ShuLFO/Downloads/UNdata_Export_20181226_154053374.csv")
View(life_exp)
head(life_exp$`Country or Area`)

subdata <- life_exp %>%  
        filter(Year == "2000-2005") %>% 
        select("Country or Area", "Subgroup", "Value") %>% 
        spread("Subgroup", "Value")

head(subdata)

ggplot(subdata, aes(Male, Female)) +
        geom_point(color = "blue", shape = 16, size =5, alpha = .5) +
        geom_abline(intercept = 0, slope = 1, linetype = 2) +
        scale_x_continuous(limits = c(35, 85)) +
        scale_y_continuous(limits = c(35,85)) +
        labs(title = "Life Expectancy at Birth by Country",
             subtitle = "Period: 2000-2005, Average",
             caption = "Source: United Nations Statistic Division",
             x = "Males",
             y = "Females")

head(subdata)
names(subdata)[1] <- "Country"
head(subdata)


top_male = subdata %>% arrange(Male - Female) %>% head(3)
top_female = subdata %>% arrange(Female - Male) %>% head(3)

ggplot(subdata, aes(Male, Female, label = Country)) +
        geom_point(color = "blue", shape = 16, size =5, alpha = .5) +
        geom_abline(intercept = 0, slope = 1, linetype = 2) +
        scale_x_continuous(limits = c(35, 85)) +
        scale_y_continuous(limits = c(35,85)) +
        labs(title = "Life Expectancy at Birth by Country",
             subtitle = "Period: 2000-2005, Average",
             caption = "Source: United Nations Statistic Division",
             x = "Males",
             y = "Females") +
        geom_text(data = top_female, size = 4, color = "red") +
        geom_text(data = top_male, size = 4, color = "green")
head(life_exp)
names(life_exp)[1] <- "Country"

subdata2 <- life_exp %>% 
        filter(Year %in% c("1985-1990", "2000-2005")) %>% 
        mutate(SubYear = paste(Subgroup, Year, sep = "_")) %>% 
        mutate(SubYear = gsub("-", "_", SubYear)) %>% 
        select(-Subgroup, -Year) %>% 
        spread(SubYear, Value) %>% 
        mutate(diff_Male = Male_2000_2005 - Male_1985_1990,
               diff_Female = Female_2000_2005 - Female_1985_1990)
head(subdata2)
glimpse(subdata2)
range(subdata2$diff_Female)

ggplot(subdata2, aes(diff_Male, diff_Female, label = Country)) +
        geom_point(col = "blue", alpha = .5, size = 4, shape = 16) +
        geom_abline(intercept = 0, slope = 1, linetype = 2) +
        scale_x_continuous(limits = c(-25, 25)) +
        scale_y_continuous(limits = c(-25,25)) +
        labs(title = "Life Expectancy at Birth by Country in Years",
             subtitle = "Difference between 1985-1990 and 2000-2005, Average",
             caption = "Sorce: United Nationa Statistic Division",
             x = "Males",
             y = "Females") +
        theme_classic() +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) 

top_diff <- subdata2 %>% arrange(diff_Male - diff_Female) %>% head(3)
botttom_diff <- subdata2 %>% arrange(diff_Female - diff_Male) %>% head(3)


ggplot(subdata2, aes(diff_Male, diff_Female, label = Country)) +
        geom_point(col = "blue", alpha = .5, size = 4, shape = 16) +
        geom_abline(intercept = 0, slope = 1, linetype = 2) +
        scale_x_continuous(limits = c(-25, 25)) +
        scale_y_continuous(limits = c(-25,25)) +
        labs(title = "Life Expectancy at Birth by Country in Years",
             subtitle = "Difference between 1985-1990 and 2000-2005, Average",
             caption = "Sorce: United Nationa Statistic Division",
             x = "Males",
             y = "Females") +
        geom_hline(yintercept = 0, linetype = 2) +
        geom_vline(xintercept = 0, linetype = 2) +
        geom_text(data = top_diff, size = 3, col = "red") +
        geom_text(data = botttom_diff, size = 3, col = "green") +
        theme_bw()

