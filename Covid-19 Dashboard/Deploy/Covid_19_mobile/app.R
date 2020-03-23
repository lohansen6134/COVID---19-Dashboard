# GLOBAL ------------------------------------------------------------------------------------------------------------

library(shiny)
library(shinyMobile)
library(fastmap)

library(tidyverse)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(lubridate)
library(ggdark)
library(plotly)

library(leaflet)
library(leaflet.providers)

# Input

selectNation <- c("Indonesia", "Singapore", "Japan", "Thailand", "Malaysia", "Australia")

# Data

data <- read.csv("covid_19_data.csv")
data <- data[,c(1:2,4,6:8)]

data$isChina <- ifelse(data$Country %in% c("Mainland China", "China"),"China","Not China")
data$Date <- as.Date(data$ObservationDate, format = "%m/%d/%y")

data <- data[,-2]
data

# Data Plot 1

Conf <- data %>%
    group_by(isChina, Date) %>%
    summarise(x = sum(Confirmed))

# Data Plot 2

Dea <- data %>%
    group_by(isChina, Date) %>%
    summarise(x = sum(Deaths))

# Data Plot 3

Rec <- data %>%
    group_by(isChina, Date) %>%
    summarise(x = sum(Recovered))

# Data Plot 4

Tog2 <- cbind(Dea, Conf)
Tog2 <- Tog2[,c(1,2,3,6)]
names(Tog2)[3:4] <- c("Deaths", "Total")
Tog2$Dea2All <- Tog2$Deaths/Tog2$Total

# Data Plot 5

Tog3 <- cbind(Rec, Conf)
Tog3 <- Tog3[,c(1,2,3,6)]
names(Tog3)[3:4] <- c("Rec", "Total")
Tog3$Rec2All <- Tog3$Rec/Tog3$Total

# Data Plot 6

Tog <- cbind(Dea, Rec)
Tog <- Tog[,c(1,2,3,6)]
names(Tog)[3:4] <- c("Deaths", "Recovered")
Tog$Rec2Dea <- Tog$Recovered/Tog$Deaths
Tog <- Tog[month(Tog$Date)==2 | month(Tog$Date)==3,]
Tog <- Tog[-c(1,37),]

# Data Nation (Confirmed, Recovered, Death)

cw <- read.csv("country_wise.csv")
cw

country <- c("Indonesia", "Singapore", "Japan", "Hong Kong", "South Korea", "Thailand", "Taiwan", "Malaysia", "Australia")

cw_melt <- rbind(
    cw %>% dplyr::filter(Confirmed == "Indonesia"),
    cw %>% dplyr::filter(Confirmed == "Singapore"),
    cw %>% dplyr::filter(Confirmed == "Japan"),
    cw %>% dplyr::filter(Confirmed == "Hong Kong"),
    cw %>% dplyr::filter(Confirmed == "South Korea"),
    cw %>% dplyr::filter(Confirmed == "Thailand"),
    cw %>% dplyr::filter(Confirmed == "Taiwan"),
    cw %>% dplyr::filter(Confirmed == "Malaysia"),
    cw %>% dplyr::filter(Confirmed == "Australia"))

# Confirmed

# Indonesia
Indonesia <- data.frame(Country.Region <- c("Indonesia","Indonesia","Indonesia","Indonesia","Indonesia") %>% as.factor(),
                        variable <- c('2', '4', '6', '9', '12'),
                        value <- c(62, 117, 235, 369, 450)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Indonesia....Indonesia....Indonesia....Indonesia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.62..117..235..369..450.)

# Singapore
Singapore <- data.frame(Country.Region <- c("Singapore","Singapore","Singapore","Singapore","Singapore") %>% as.factor(),
                        variable <- c('2', '4', '6', '9', '12'),
                        value <- c(15, 38, 73, 178, 254)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Singapore....Singapore....Singapore....Singapore...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.15..38..73..178..254.)

# Japan
Japan <- data.frame(Country.Region <- c("Japan","Japan","Japan","Japan","Japan") %>% as.factor(),
                    variable <- c('2', '4', '6', '9', '12'),
                    value <- c(81, 145, 254, 378, 639)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Japan....Japan....Japan....Japan....Japan.......,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.81..145..254..378..639.)

# Thailand
Thailand <- data.frame(Country.Region <- c("Thailand","Thailand","Thailand","Thailand","Thailand") %>% as.factor(),
                       variable <- c('2', '4', '6', '9', '12'),
                       value <- c(253, 326, 521, 743, 873)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Thailand....Thailand....Thailand....Thailand...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.253..326..521..743..873.)

# Malaysia
Malaysia <- data.frame(Country.Region <- c("Malaysia","Malaysia","Malaysia","Malaysia","Malaysia") %>% as.factor(),
                       variable <- c('2', '4', '6', '9', '12'),
                       value <- c(237, 498, 657, 934, 1030)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Malaysia....Malaysia....Malaysia....Malaysia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.237..498..657..934..1030.)

# Australia
Australia <- data.frame(Country.Region <- c("Australia","Australia","Australia","Australia","Australia") %>% as.factor(),
                        variable <- c('2', '4', '6', '9', '12'),
                        value <- c(322, 593, 726, 973, 1023)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Australia....Australia....Australia....Australia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.322..593..726..973..1023.)

cts_melt <- rbind(Indonesia, Singapore, Japan, Thailand, Malaysia, Australia)

# Recovered

# Indonesia
Indonesia.r <- data.frame(Country.Region <- c("Indonesia","Indonesia","Indonesia","Indonesia","Indonesia") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(6, 9, 12, 17, 20)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Indonesia....Indonesia....Indonesia....Indonesia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.6..9..12..17..20.)

# Singapore
Singapore.r <- data.frame(Country.Region <- c("Singapore","Singapore","Singapore","Singapore","Singapore") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(13, 25, 57, 96, 131)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Singapore....Singapore....Singapore....Singapore...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.13..25..57..96..131.)

# Japan
Japan.r <- data.frame(Country.Region <- c("Japan","Japan","Japan","Japan","Japan") %>% as.factor(),
                      variable <- c('2', '4', '6', '9', '12'),
                      value <- c(57, 92, 118, 173, 215)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Japan....Japan....Japan....Japan....Japan.......,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.57..92..118..173..215.)

# Thailand
Thailand.r <- data.frame(Country.Region <- c("Thailand","Thailand","Thailand","Thailand","Thailand") %>% as.factor(),
                         variable <- c('2', '4', '6', '9', '12'),
                         value <- c(2, 7, 20, 31, 42)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Thailand....Thailand....Thailand....Thailand...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.2..7..20..31..42.)

# Malaysia
Malaysia.r <- data.frame(Country.Region <- c("Malaysia","Malaysia","Malaysia","Malaysia","Malaysia") %>% as.factor(),
                         variable <- c('2', '4', '6', '9', '12'),
                         value <- c(13, 34, 52, 65, 87)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Malaysia....Malaysia....Malaysia....Malaysia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.13..34..52..65..87.)

# Australia
Australia.r <- data.frame(Country.Region <- c("Australia","Australia","Australia","Australia","Australia") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(8, 15, 24, 31, 46)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Australia....Australia....Australia....Australia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.8..15..24..31..46.)

cts_melt.r <- rbind(Indonesia.r, Singapore.r, Japan.r, Thailand.r, Malaysia.r, Australia.r)

# Death

# Indonesia
Indonesia.d <- data.frame(Country.Region <- c("Indonesia","Indonesia","Indonesia","Indonesia","Indonesia") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(7, 12, 19, 25, 38)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Indonesia....Indonesia....Indonesia....Indonesia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.7..12..19..25..38.)

# Singapore
Singapore.d <- data.frame(Country.Region <- c("Singapore","Singapore","Singapore","Singapore","Singapore") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(0, 0, 0, 1, 2)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Singapore....Singapore....Singapore....Singapore...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..0..0..1..2.)

# Japan
Japan.d <- data.frame(Country.Region <- c("Japan","Japan","Japan","Japan","Japan") %>% as.factor(),
                      variable <- c('2', '4', '6', '9', '12'),
                      value <- c(4, 7, 12, 24, 35)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Japan....Japan....Japan....Japan....Japan.......,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.4..7..12..24..35.)

# Thailand
Thailand.d <- data.frame(Country.Region <- c("Thailand","Thailand","Thailand","Thailand","Thailand") %>% as.factor(),
                         variable <- c('2', '4', '6', '9', '12'),
                         value <- c(0, 0, 0, 0, 1)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Thailand....Thailand....Thailand....Thailand...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..0..0..0..1.)

# Malaysia
Malaysia.d <- data.frame(Country.Region <- c("Malaysia","Malaysia","Malaysia","Malaysia","Malaysia") %>% as.factor(),
                         variable <- c('2', '4', '6', '9', '12'),
                         value <- c(0, 0, 1, 2, 3)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Malaysia....Malaysia....Malaysia....Malaysia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..0..1..2..3.)

# Australia
Australia.d <- data.frame(Country.Region <- c("Australia","Australia","Australia","Australia","Australia") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(0, 1, 3, 4, 7)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Australia....Australia....Australia....Australia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..1..3..4..7.)

cts_melt.d <- rbind(Indonesia.d, Singapore.d, Japan.d, Thailand.d, Malaysia.d, Australia.d)

# Leaflet Data

indo_ll <- data.frame(city = c("Penjaringan","Tanjong Priok", "Kelapagading", "Cengkareng", "Kembangan", "Kebon Jeruk", "Kebayoran Lama", "Mampang Prapatan", "Pancoran", "Kramat Jati", "Denpasar", "Singaraja"),
                      latitude = c(-6.126741, -6.132055, -6.160455, -6.148665, -6.176274, -6.195942, -6.244392, -6.250614, -6.252300, -6.273298, -8.650000, -8.116611),
                      longitude = c(106.782443, 106.871483, 106.905462, 106.735260, 106.746540, 106.773595, 106.776544, 106.820788, 106.847338, 106.869465, 115.216667, 115.083952))

wc3 <- read.csv("sea_city.csv")

# Timeline

items <- tagList(
    f7TimelineItem(
        "Cases of pneumonia detected in Wuhan, China were reported to the World Health Organization.",
        date = "31 Dec 2019",
        card = TRUE,
        time = "12:30",
        title = "First Case in China",
        side = "right"
    ),
    f7TimelineItem(
        "Chinese health authorities closed the seafood market Wuhan after it was discovered that the 
        virus originated from wild animals in the market",
        date = "01 Jan 2020",
        card = TRUE,
        time = "13:00",
        title = "Close the Wuhan Seafood Market"
    ),
    f7TimelineItem(
        "WHO has identified and formalized the new virus as 2019 - nCoV",
        date = "07 Jan 2020",
        card = TRUE,
        time = "14:45",
        title = "2019-nCoV is inaugurated"
    ),
    f7TimelineItem(
        "The first death was due to the corona virus in one of the Wuhan hospitals",
        date = "11 Jan 2020",
        card = TRUE,
        time = "09.00",
        title = "First Death in Wuhan"
    ),
    f7TimelineItem(
        "Thailand reports cases of corona virus infection in Thailand",
        date = "13 Jan 2020",
        card = TRUE,
        time = "10.00",
        title = "Corona in Thailand"
    ),
    f7TimelineItem(
        "Japan has confirmed that a man from Wuhan identified a corona virus in Japan",
        date = "16 Jan 2020",
        card = TRUE,
        time = "12.00",
        title = "The first Corona in Japan"
    ),
    f7TimelineItem(
        "The Wuhan government closed all public areas such as airports, stations and so on in the city",
        date = "22 Jan 2020",
        card = TRUE,
        time = "08.00",
        title = "Wuhan Lock Down"
    ),
    f7TimelineItem(
        "Philippines announced the first death outside of China due to the corona virus",
        date = "02 Feb 2020",
        card = TRUE,
        time = "13.00",
        title = "First Death Outside China"
    ),
    f7TimelineItem(
        "America announced the first death outside of China due to the corona virus",
        date = "29 Feb 2020",
        card = TRUE,
        time = "17.00",
        title = "First Death in America"
    ),
    f7TimelineItem(
        "The Indonesian government announced 2 Indonesian citizens who were Corona positive, transmitted 
        from a Japanese citizen in Jakarta",
        date = "02 Mar 2020",
        card = TRUE,
        time = "05.00",
        title = "Indonesia Corona Cases"
    ),
    f7TimelineItem(
        "The death of one of the corona patients in Indonesia (the 25th patient) became the first death in Indonesia",
        date = "11 Mar 2020",
        card = TRUE,
        time = "16.00",
        title = "First Death in Indonesia"
    ),
    f7TimelineItem(
        "Until now there have been 117 positive cases of corona, 8 patients recovered from corona, 
        and 5 patients died from corona in Indonesia",
        date = "16 Mar 2020",
        card = TRUE,
        time = "07.00",
        title = "Update Covid-19 in Indonesia"
    ),
    f7TimelineItem(
        "to date there have been more than 300 positive cases of corona in Indonesia, more than 30 deaths, 
        and recovery in 17 patients, the majority of which are located in Jakarta, the capital of Indonesia.",
        date = "21 Mar 2020",
        card = TRUE,
        time = "07.00",
        title = "Corona cases in Indonesia have increased dramatically"
    )
)

# UI -----------------------------------------------------------------------------------------------------------------

shiny::shinyApp(
    ui=f7Page(
        title = "Tab Layout",
        init = f7Init(theme = "dark"),
        f7TabLayout(
            # Panel are not mandatory. These are similar to sidebars
            panels = tagList(
                f7Panel(side = "left", theme = "dark", effect = "cover")
            ),
            navbar = f7Navbar(
                title = "Covid-19 Update",
                # Enable both panels
                left_panel = FALSE,
                theme = "dark"
            ),
            # f7Tabs is a special toolbar with included navigation
            f7Tabs(
                animated = TRUE,
                id = "tabs",
                f7Tab(
                    tabName = "Timeline",
                    icon = icon("calendar-times"),
                    active = TRUE,
                    # tabs 2 content
                    f7BlockTitle(title = "Covid-19 Timeline", size = "large") %>%
                        f7Align(side = "center"),
                    f7Timeline(
                        sides = TRUE,
                        items
                    )
                ),
                # Other Tab 2
                f7Tab(
                    tabName = "Global",
                    icon = icon("globe"),
                    active = FALSE,
                    # tab 1 content
                    f7Card("To begin with, we look at the number of people infected in China and the rest of the world.
                           The number of patients is incomparably higher in China and is growing at a very fast pace ). 
                           We have much less infected in other countries, but their numbers, unlike China, are growing 
                           at an alarming rate.",
                           title = "Covid-19 Growth",
                           br(),
                           br(),
                           f7Badge("China", color = "red"),
                           f7Badge("Outside China", color = "dodger-blue")),
                    plotlyOutput("plot_con"),
                    f7Card("We're looking at what raises the biggest fears - fatalities. 
                           By the end of January, there were no casualties outside of China. 
                           In January in China, the trend is constant - about 30 people need 
                           to be added to the sum of this tragic balance every day, but since 
                           4th February trend is much faster (the number of victims tripled in 
                           8 days). With the beginning of March, growth is definitely slowing down, 
                           which is a good signal for this country.",
                           title = "Covid-19 Fatalities",
                           br(),
                           br(),
                           f7Badge("China", color = "red"),
                           f7Badge("Outside China", color = "dodger-blue")),
                    plotlyOutput("plot_death"),
                    f7Card("In January while in the last period of time the number of victims doubled in 4 days, 
                           the number of healed in just two days. In the middle of February, there are about 1,000 
                           cured per day. We also have good news in the rest of the world - while there were no 
                           fatalities by the end of January, 10 people outside China already been cured.",
                           title = "Healed from Corona",
                           br(),
                           br(),
                           f7Badge("China", color = "red"),
                           f7Badge("Outside China", color = "dodger-blue")),
                    plotlyOutput("plot_heal"),
                    f7Card("In rest of the world mortality has been rising since the beginning (but at a different pace) 
                           It's worth bearing in mind that the victims are usually an elderly person or with other diseases, 
                           so this value cannot be treated as the probability of death of a newly infected person. 
                           Also, there are many indications that the virus passes many people who don't have it detected, 
                           therefore it shouldn't be considered as mortality for the entire COVID-19 disease, but only for 
                           people who have this medically confirmed disease.",
                           title = "Covid-19 Mortality",
                           br(),
                           br(),
                           f7Badge("China", color = "red"),
                           f7Badge("Outside China", color = "dodger-blue")),
                    plotlyOutput("plot_dea"),
                    f7Card("Now we look at the ratio of healed people to the dead. Values above 1 mean the advantage of the
                           cured people over the dead. In China, we are observing a steady increase in value, while outside of this 
                           country we are observing 'sharp jumps', so like the previous chart, we find that it takes time to draw 
                           compact conclusions.",
                           title = "Recovered/Death Ratio",
                           br(),
                           br(),
                           f7Badge("China", color = "red"),
                           f7Badge("Outside China", color = "dodger-blue")),
                    plotlyOutput("plot_recover")
                ),
                # Other tabs 3
                f7Tab(
                    tabName = "Data",
                    icon = icon("chart-pie"),
                    active = FALSE,
                    # tab 3 content
                    f7Card("The World Health Organization has announced that COVID-19 is a pandemic. 
                           Find out how we are monitoring and responding to the outbreak, how you can help 
                           slow the spread of COVID-19 in Australia, and what to do if you have symptoms. 
                           We also report the latest official medical advice and case numbers.",
                           title = "Corona Case by Country"),
                    selectInput(
                        inputId = "nation",
                        label = "Select Nation",
                        choices = selectNation
                    ),
                    plotlyOutput("nation_conf"),
                    f7Card("For most people, the immediate risk of becoming seriously ill from the virus that causes 
                            COVID-19 is thought to be low.Older adults and people of any age with underlying health conditions, 
                            such as diabetes, lung disease, or heart disease, are at greater risk of severe illness from COVID-19.",
                           title = "Fact: Serious Corona is Low Risk"),
                    plotlyOutput("nation_rec"),
                    f7Card("Someone who has completed quarantine or has been released from isolation does not pose a risk 
                            of infection to other people.",
                           title = "Fact: Patients Recovering from Corona are Safe"),
                    plotlyOutput("nation_dea"),
                    f7Card("Diseases can make anyone sick regardless of their race or ethnicity.Fear and anxiety about 
                           COVID-19 can cause people to avoid or reject others even though they are not at risk 
                           for spreading the virus.",
                           title = "Fact: Everyone Can Get Covid-19")
                    
                ),
                # Other tabs 4
                f7Tab(
                    tabName = "Maps",
                    icon = icon("map"),
                    active = FALSE,
                    # tabs 4 content
                    leafletOutput("leaflet"),
                    f7Card("The map above shows some areas in the countries of Southeast Asia, which have a tendency 
                           to be a red zone area, or a zone that has quite a lot of positive corona risk to the people. 
                           Please note that the data used does not accommodate all existing corona cases, but can 
                           sufficiently interpret the existing large zones.",
                           title = "Covid-19 Mapping in SEA")
                )
            )
        )
    ),
    
    # SERVER ----------------------------------------------------------------------------------------------------------
    
    server = function(input, output){
        
        output$distPlot <- renderPlot({
            # generate bins based on input$bins from ui.R
            x    <- faithful[, 2]
            bins <- seq(min(x), max(x), length.out = input$bins + 1)
            
            # draw the histogram with the specified number of bins
            hist(x, breaks = bins, col = 'darkgray', border = 'white')
        })
        
        output$plot_con <- renderPlotly({
            
            plot1 <- ggplot(Conf, aes(Date, x, colour = isChina))+
                geom_line(size = 2, alpha = 0.8)+
                # geom_point(size = 2.7)+
                # scale_y_continuous(trans="log10")+
                labs(x = "", y = "", title =  "Confirmation of virus infection", subtitle = "by China and Rest of World")+
                # geom_text_repel(aes(label = x), nudge_y = 1100, color = "black", size = 3.9)+
                scale_x_date(date_labels = "%b %d", date_breaks = "7 days")+
                scale_colour_brewer(palette = "Set1")+
                dark_theme_light()+
                theme(legend.position="none", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 8, colour = "white"), 
                      legend.text = element_text(size = 13, colour = "white"), axis.title = element_text(size = 14, colour = "white"), axis.line = element_line(size = 0.4, colour = "white"),
                      plot.background = element_rect(fill = "black"), legend.background = element_rect(fill = "black"))
            
            ggplotly(plot1, object = x)
            
        })
        
        output$plot_death <- renderPlotly({
            
            plot2 <- ggplot(Dea, aes(Date, x, colour = isChina))+
                geom_line(size = 2, alpha = 0.8)+
                #geom_point(size = 2.7)+
                #scale_y_continuous(trans="log10")+
                scale_x_date(date_labels = "%b %d", date_breaks = "7 days")+
                labs(x = "", y = "", title =  "Virus fatalities", subtitle = "by China and Rest of World")+
                #geom_text_repel(aes(label = x), nudge_y = 40, color = "black", size = 4.1)+
                scale_colour_brewer(palette = "Set1")+
                dark_theme_light()+
                theme(legend.position="none", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 8, colour = "white"), 
                      legend.text = element_text(size = 13, colour = "white"), axis.title = element_text(size = 14, colour = "white"), axis.line = element_line(size = 0.4, colour = "white"), 
                      plot.background = element_rect(fill = "black"), legend.background = element_rect(fill = "black"))
            
            ggplotly(plot2, object = x)
            
        })
        
        output$plot_heal <- renderPlotly({
            
            plot3 <- ggplot(Rec, aes(Date, x, colour = isChina))+
                geom_line(size = 2, alpha = 0.8)+
                #geom_point(size = 2.7)+
                #scale_y_continuous(trans="log10")+
                scale_x_date(date_labels = "%b %d", date_breaks = "7 days")+
                labs(x = "", y = "", title =  "Healed of the virus", subtitle = "by China and Rest of World")+
                # geom_text_repel(aes(label = x), nudge_y = 95, color = "black", size = 3.9)+
                scale_colour_brewer(palette = "Set1")+
                dark_theme_light()+
                theme(legend.position="none", legend.direction="horizontal", legend.title = element_blank(), axis.text = element_text(size = 8, colour = "white"),
                      legend.text = element_text(size = 13, colour = "white"), axis.title = element_text(size = 14, colour = "white"), axis.line = element_line(size = 0.4, colour = "white"), 
                      plot.background = element_rect(fill = "black"), legend.background = element_rect(fill = "black"))
            
            ggplotly(plot3, object = x)
            
        })
        
        output$plot_dea <- renderPlotly({
            
            plot4 <- ggplot(Tog2[-c(1,47),], aes(Date,Dea2All, colour = isChina))+
                geom_line(size = 2.2, alpha = 0.8)+
                #geom_point(size = 3)+
                labs(x = "", y = "", title =  "Virus mortality", subtitle = "by China and Rest of World", colour = "")+
                #geom_text_repel(aes(label = paste0(round(100*Dea2All,1), "%")), nudge_y = 0.0013, nudge_x = 0.2, color = "black", size = 3.8)+
                scale_y_continuous(labels = scales::percent, limits = c(0,0.04))+
                scale_x_date(date_labels = "%b %d", date_breaks = "7 days")+
                scale_colour_brewer(palette = "Set1")+
                dark_theme_light()+
                annotate("text", x = mean(Tog2$Date)-15, y = 0.0135, label = "1st death \n outside of china", size = 4, colour = "White")+
                annotate("segment", x = mean(Tog2$Date)-15.5, xend = mean(Tog2$Date)-12, y = 0.0109, yend = 0.0062, colour = "white", size=0.7, alpha=0.7, arrow=arrow())+
                theme(legend.position="none", legend.direction="horizontal", axis.text = element_text(size = 8, colour = "white"), axis.title = element_text(size = 14, colour = "white"), 
                      legend.text = element_text(size = 13, colour = "white"), axis.line = element_line(size = 0.4, colour = "white"), 
                      plot.background = element_rect(fill = "black"), legend.background = element_rect(fill = "black"))
            
            ggplotly(plot4, object = death)
            
        })
        
        output$plot_recover <- renderPlotly({
            
            plot6 <- ggplot(Tog, aes(Date, Rec2Dea, colour = isChina))+
                geom_line(size = 2.2, alpha = 0.8)+
                #geom_point(size = 3.3)+
                labs(x = "", y = "", title =  "Recovered to the dead", subtitle = "by China and Rest of World (since Feb 01)", colour = "Indicator")+
                #geom_text_repel(aes(label = round(Rec2Dea,1)), nudge_y = 0.115, color = "black", size = 4.5)+
                scale_colour_brewer(palette = "Set1")+
                scale_y_continuous(limits = c(0,36))+
                scale_x_date(date_labels = "%b %d", date_breaks = "6 days")+
                geom_hline(yintercept = 10, linetype = 2, alpha = 0.5)+
                annotate("text", x = mean(Tog$Date)-4, y = 10.9, label = "10 Recovered = 1 Death", size = 4, colour = "white")+
                dark_theme_light()+
                theme(legend.position="none", legend.direction="vertical", axis.text = element_text(size = 8, colour = "white"), axis.title = element_text(size = 14, colour = "white"), 
                      legend.text = element_text(size = 13, colour = "white"), axis.line = element_line(size = 0.4, colour = "white"), 
                      plot.background = element_rect(fill = "black"), legend.background = element_rect(fill = "black"))
            
            ggplotly(plot6, object = Recovered)
            
        })
        
        output$nation_conf <- renderPlotly({
            
            cts <- cts_melt %>% 
                dplyr::filter(Country.Region == input$nation)
            
            cts_p <- ggplot(cts, aes(reorder(variable, value), value, label = value))+
                geom_point(stat='identity', fill="yellow", size=6, color = "yellow")+
                geom_segment( aes(x=variable, xend=variable, y=0, yend=value), size=1, color="yellow", linetype="dotdash")+
                geom_text(color = 'black', size = 4)+
                dark_theme_light()+
                labs(x = "", y = "", title =  "Potitive Rate")+
                scale_x_discrete(labels=c("2" = "Feb 28", "4" = "Mar 1", "6" = "Mar 10", "9" = "Mar 16", "12" = "Mar 21"))
            
            plotly::ggplotly(cts_p, object = value)
            
        })
        
        output$nation_rec <- renderPlotly({
            
            cts.r <- cts_melt.r %>% 
                dplyr::filter(Country.Region == input$nation)
            
            cts_p.r <- ggplot(cts.r, aes(reorder(variable, value), value, label = value))+
                geom_point(stat='identity', fill="green", size=6, color = "green")+
                geom_segment( aes(x=variable, xend=variable, y=0, yend=value), size=1, color="green", linetype="dotdash")+
                geom_text(color = 'black', size = 4)+
                dark_theme_light()+
                labs(x = "", y = "", title =  "Recovered Rate")+
                scale_x_discrete(labels=c("2" = "Feb 28", "4" = "Mar 1", "6" = "Mar 10", "9" = "Mar 16", "12" = "Mar 21"))
            
            plotly::ggplotly(cts_p.r, object = value)
            
        })
        
        output$nation_dea <- renderPlotly({
            
            cts.d <- cts_melt.d %>% 
                dplyr::filter(Country.Region == input$nation)
            
            cts_p.d <- ggplot(cts.d, aes(reorder(variable, value), value, label = value))+
                geom_point(stat='identity', fill="red", size=6, color = "red")+
                geom_segment( aes(x=variable, xend=variable, y=0, yend=value), size=1, color="red", linetype="dotdash")+
                geom_text(color = 'black', size = 4)+
                dark_theme_light()+
                labs(x = "", y = "", title =  "Death Rate")+
                scale_x_discrete(labels=c("2" = "Feb 28", "4" = "Mar 1", "6" = "Mar 10", "9" = "Mar 16", "12" = "Mar 21"))
            
            plotly::ggplotly(cts_p.d, object = value)
            
        })
        
        output$leaflet <- renderLeaflet({

            leaflet(wc3) %>% 
                addProviderTiles("CartoDB.DarkMatter", layerId = "Road Dark", group = "Road Dark") %>%  
                addCircleMarkers(~lng, ~lat, popup=wc3$city, weight = 6, radius=8, 
                                 color="red", stroke = F, fillOpacity = 0.5)

        })

    }
)

