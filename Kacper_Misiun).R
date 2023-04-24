library('tidyverse')
library('plotly')
library('prophet')
#Get data
task2 <- readxl::read_excel('./task1_2.xlsx', sheet = 'Clean data')

# I figure out there is hidden sheet named data, because without specific sheet name as above i get other data like below
test <- readxl::read_excel('./task1_2.xlsx', sheet = 1)

# check null and duplicates
sum(is.null(task2))
sum(duplicated(task2))
sum(duplicated(task2$campaign_id))
glimpse(task2) #check column type

#TASK 2.A
task2 <-  task2 %>%
    mutate(month = as.Date(month)) %>% 
    filter(month >= '2014-08-01' & month <= '2015-05-31') %>% #chosen period 
    mutate(PPL = EPL - CPL, #Profit Per Lead (PPL) 
#How to getl leads? clics * convertion_rate? 
    leads = clicks * conversion_rate) %>% 
    mutate(total_profit = leads * PPL)
# summarize total_profit by month to get best product
product_profit <- task2 %>%
    group_by(month, product) %>% 
    summarise(total_profit = sum(total_profit))


plot<- ggplot(product_profit, aes(x=month, y = total_profit, color = product)) +
    geom_line() +
    labs(x = 'Date', y = 'Total Profit', title = 'Total profit per product 2014/08/01 - 2015/05/31 \n', color = 'Product Name') +
    scale_x_date(date_labels = "%b \n %Y", date_breaks = "1 month") +
    theme_bw() +
    theme(text = element_text(family = "Pacifico"),
              plot.title = element_text(face = "bold", hjust = 0.5)) # to get Pacifico front install.packages(extrafont), extrafont::font_import(paths = NULL, recursive = TRUE)  // extrafont::loadfonts(device = "win")

ggplotly(plot) 

#to save TASK 2.c
ggsave("task2_a_plot.png", plot, width = 8, height = 6, dpi = 900)

#TASK 2.A recommendation: Product A
# If I understand of what lead is and calculation of profit were right product A
# had best total_profit per month in chosen period and nice increase in the last months of the analyzed period
# Product B should probably be closed, because of huge decrase but i would keep it one/two month to be shure of decrease trend (this product had big fluctuation so trend may change)
# Product C had lowest profit but is relatively safe and generate positive value
# General invest in product A but keep calm

#TASK 2.b
# For the selected product choose the best campaign_type.
product_A <- task2 %>% 
    filter(product == 'A') %>%  #filter A
    group_by(`campaign type`) %>% 
    summarise(total_profit = sum(total_profit), 
              number_of_campaigns = n(),
              profit_per_campaig = total_profit/number_of_campaigns)

# Definitly campaign SOS had best total profit, but it was because realise alot of campains
# CPM had best profit ratio per campaign but they realise only 30
# RECOMENDATION: increase the number of campaigns of CPM (for example to 100), decrease SOS but at the same time keep hight level of campaign, for example 200 (they had nice average return on hight volume of campaigns)
