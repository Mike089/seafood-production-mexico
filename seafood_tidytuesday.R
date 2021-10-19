library(tidyverse)
library(rvest)
library(poissoned)
library(ggthemes)
library(scales)
library(showtext)
library(gganimate)
library(viridis)

options(scipen = 999)



production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

#Filter data from Mexico

mx_prod <- production %>%
        filter(Code == "MEX") %>% 
        pivot_longer(4:last_col(), names_to = "fish", values_to = "tonnes") %>%
        janitor::clean_names() %>%
        mutate(
               fish =str_extract(fish, "(?<=lent - ).+(?= - 27)"),
               fish = str_remove(fish, ",.+"),
               decade = paste0((year -1 )%/% 10 * 10)
        ) %>%
        group_by(decade, fish ) %>%
        summarise(tonnes_med = median(tonnes)) %>%
        ungroup() 


mx_prod_by_year <- production %>%
        filter(Code == "MEX") %>% 
        pivot_longer(4:last_col(), names_to = "fish", values_to = "tonnes") %>%
        janitor::clean_names() %>%
        mutate(
                fish =str_extract(fish, "(?<=lent - ).+(?= - 27)"),
                fish = str_remove(fish, ",.+")
        ) %>%
        group_by(year, fish ) %>%
        summarise(tonnes_med = median(tonnes)) %>%
        ungroup() 




#Plot colors

my_colors <- c("#0077b6","#0096c7","#00b4d8","#48cae4","#90e0ef","#ade8f4","#caf0f8")

background  <- c("#1C181E")

text_color  <- c("#FFFFFF")


font_add_google("Roboto", "roboto")

showtext_auto()


#Line plot

mx_prod_by_year <- production %>%
        filter(Code == "MEX") %>% 
        pivot_longer(4:last_col(), names_to = "fish", values_to = "tonnes") %>%
        janitor::clean_names() %>%
        mutate(
                fish =str_extract(fish, "(?<=lent - ).+(?= - 27)"),
                fish = str_remove(fish, ",.+")
        ) %>%
        group_by(year, fish ) %>%
        summarise(tonnes_med = median(tonnes)) %>%
        ungroup()

names(mx_prod_by_year) [2] <- "Fish"


 p2 <- mx_prod_by_year %>%
        ggplot(aes(x = year, y = tonnes_med, color = Fish))+
        geom_line(size = 2, alpha = 0.75) +
        theme_solarized_2(light = FALSE) +
        labs(title = "Seafood production in Mexico", 
              subtitle = "Median production by year (1961-2013) in tonnes rounded to the nearest tenth",
              caption = "Source: OurWorldinData.org + Graphic: Miguel Hernandez",
             y = "Median Production (Tonnes)") +
        theme(text = element_text(family = "Roboto", color = "#EEEEEE"),
              title = element_text(color = "#EEEEEE"),
              axis.title.x = element_blank(),
              panel.background = element_rect(fill = NA),
              plot.background = element_rect(fill = "#111111"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.position = "bottom")+
        scale_color_viridis(discrete = TRUE)+
        geom_point() +
        scale_x_continuous(breaks = seq(from = 0, to = 2013, by = 10))+
        guides(color=guide_legend(nrow=2,byrow=TRUE))+
        scale_y_continuous(label = comma)+
        theme(plot.title = element_text(size=20))+
        theme(axis.text.x = element_text(color = "white", size = 12))+
        theme(axis.text.y = element_text(color = "white", size = 12))+
        theme(plot.subtitle = element_text(color = "white", size = 12))+
        theme(plot.caption = element_text(color = "white", size = 11))+
        theme(legend.key.size = unit(2, 'cm'))
 
 p2


graph2.animation <- p2 +
        transition_reveal(year) +
        view_follow(fixed_y = TRUE)



animate(graph2.animation, height = 900 , width = 900, fps = 30, duration = 10,
        end_pause = 60, res = 100) 


anim_save("seafood mexico graph.gif")













 



