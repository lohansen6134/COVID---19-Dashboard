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
                        value <- c(2, 18, 24, 36, 65)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Indonesia....Indonesia....Indonesia....Indonesia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.2..18..24..36..65.)

# Singapore
Singapore <- data.frame(Country.Region <- c("Singapore","Singapore","Singapore","Singapore","Singapore") %>% as.factor(),
                        variable <- c('2', '4', '6', '9', '12'),
                        value <- c(15, 38, 73, 154, 178)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Singapore....Singapore....Singapore....Singapore...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.15..38..73..154..178.)

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
                       value <- c(5, 12, 23, 56, 70)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Thailand....Thailand....Thailand....Thailand...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.5..12..23..56..70.)

# Malaysia
Malaysia <- data.frame(Country.Region <- c("Malaysia","Malaysia","Malaysia","Malaysia","Malaysia") %>% as.factor(),
                       variable <- c('2', '4', '6', '9', '12'),
                       value <- c(13, 63, 76, 129, 149)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Malaysia....Malaysia....Malaysia....Malaysia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.13..63..76..129..149.)

# Australia
Australia <- data.frame(Country.Region <- c("Australia","Australia","Australia","Australia","Australia") %>% as.factor(),
                        variable <- c('2', '4', '6', '9', '12'),
                        value <- c(3, 12, 32, 45, 128)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Australia....Australia....Australia....Australia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.3..12..32..45..128.)

cts_melt <- rbind(Indonesia, Singapore, Japan, Thailand, Malaysia, Australia)

# Recovered

# Indonesia
Indonesia.r <- data.frame(Country.Region <- c("Indonesia","Indonesia","Indonesia","Indonesia","Indonesia") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(0, 0, 1, 2, 4)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Indonesia....Indonesia....Indonesia....Indonesia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..0..1..2..4.)

# Singapore
Singapore.r <- data.frame(Country.Region <- c("Singapore","Singapore","Singapore","Singapore","Singapore") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(2, 10, 32, 57, 96)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Singapore....Singapore....Singapore....Singapore...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.2..10..32..57..96.)

# Japan
Japan.r <- data.frame(Country.Region <- c("Japan","Japan","Japan","Japan","Japan") %>% as.factor(),
                      variable <- c('2', '4', '6', '9', '12'),
                      value <- c(11, 43, 75, 92, 118)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Japan....Japan....Japan....Japan....Japan.......,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.11..43..75..92..118.)

# Thailand
Thailand.r <- data.frame(Country.Region <- c("Thailand","Thailand","Thailand","Thailand","Thailand") %>% as.factor(),
                         variable <- c('2', '4', '6', '9', '12'),
                         value <- c(1, 6, 12, 14, 34)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Thailand....Thailand....Thailand....Thailand...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.1..6..12..14..34.)

# Malaysia
Malaysia.r <- data.frame(Country.Region <- c("Malaysia","Malaysia","Malaysia","Malaysia","Malaysia") %>% as.factor(),
                         variable <- c('2', '4', '6', '9', '12'),
                         value <- c(5, 9, 11, 14, 26)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Malaysia....Malaysia....Malaysia....Malaysia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.5..9..11..14..26.)

# Australia
Australia.r <- data.frame(Country.Region <- c("Australia","Australia","Australia","Australia","Australia") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(2, 3, 7, 14, 21)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Australia....Australia....Australia....Australia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.2..3..7..14..21.)

cts_melt.r <- rbind(Indonesia.r, Singapore.r, Japan.r, Thailand.r, Malaysia.r, Australia.r)

# Death

# Indonesia
Indonesia.d <- data.frame(Country.Region <- c("Indonesia","Indonesia","Indonesia","Indonesia","Indonesia") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(0, 0, 0, 0, 1)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Indonesia....Indonesia....Indonesia....Indonesia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..0..0..0..1.)

# Singapore
Singapore.d <- data.frame(Country.Region <- c("Singapore","Singapore","Singapore","Singapore","Singapore") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(0, 0, 0, 0, 0)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Singapore....Singapore....Singapore....Singapore...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..0..0..0..0.)

# Japan
Japan.d <- data.frame(Country.Region <- c("Japan","Japan","Japan","Japan","Japan") %>% as.factor(),
                      variable <- c('2', '4', '6', '9', '12'),
                      value <- c(0, 2, 4, 12, 16)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Japan....Japan....Japan....Japan....Japan.......,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..2..4..12..16.)

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
                         value <- c(0, 0, 0, 0, 0)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Malaysia....Malaysia....Malaysia....Malaysia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..0..0..0..0.)

# Australia
Australia.d <- data.frame(Country.Region <- c("Australia","Australia","Australia","Australia","Australia") %>% as.factor(),
                          variable <- c('2', '4', '6', '9', '12'),
                          value <- c(0, 0, 1, 2, 3)) %>%
    dplyr::rename(Country.Region = Country.Region....c..Australia....Australia....Australia....Australia...,
                  variable = variable....c..2....4....6....9....12..,
                  value = value....c.0..0..1..2..3.)

cts_melt.d <- rbind(Indonesia.d, Singapore.d, Japan.d, Thailand.d, Malaysia.d, Australia.d)

# Leaflet Data

leaflet <- read.csv("COVID19_open_line_list.csv")

leaflet <- leaflet %>% dplyr::select(city, latitude, longitude) 

leaflet <- leaflet %>% 
    dplyr::mutate(latitude = as.numeric(as.character(latitude))) %>% 
    dplyr::mutate(longitude = as.numeric(as.character(longitude)))

indo_ll <- data.frame(city = c("Penjaringan","Tanjong Priok", "Kelapagading", "Cengkareng", "Kembangan", "Kebon Jeruk", "Kebayoran Lama", "Mampang Prapatan", "Pancoran", "Kramat Jati", "Denpasar", "Singaraja"),
                      latitude = c(-6.126741, -6.132055, -6.160455, -6.148665, -6.176274, -6.195942, -6.244392, -6.250614, -6.252300, -6.273298, -8.650000, -8.116611),
                      longitude = c(106.782443, 106.871483, 106.905462, 106.735260, 106.746540, 106.773595, 106.776544, 106.820788, 106.847338, 106.869465, 115.216667, 115.083952))

leaflet <- rbind(leaflet, indo_ll)

# UI -----------------------------------------------------------------------------------------------------------------

shiny::shinyApp(
    ui=f7Page(
        title = "Tab Layout",
        init = f7Init(theme = "dark"),
        f7TabLayout(
            # Panel are not mandatory. These are similar to sidebars
            panels = tagList(
                f7Panel(side = "left", theme = "dark", effect = "cover"),
                f7Panel(side = "right", theme = "dark", effect = "reveal")
            ),
            navbar = f7Navbar(
                title = "Tabs",
                # Enable both panels
                left_panel = TRUE,
                right_panel = TRUE,
                theme = "dark"
            ),
            # f7Tabs is a special toolbar with included navigation
            f7Tabs(
                animated = TRUE,
                id = "tabs",
                f7Tab(
                    tabName = "Tab 1",
                    icon = f7Icon("email"),
                    active = TRUE,
                    # tab 1 content
                    plotlyOutput("plot_con"),
                    plotlyOutput("plot_death"),
                    plotlyOutput("plot_heal"),
                    plotlyOutput("plot_dea"),
                    plotlyOutput("plot_total"),
                    plotlyOutput("plot_recover")
                ),
                # Other tabs 2
                f7Tab(
                    tabName = "Tab 2",
                    icon = f7Icon("cloud"),
                    active = FALSE,
                    # tab 2 content
                    selectInput(
                        inputId = "nation",
                        label = "Select Nation",
                        choices = selectNation
                    ),
                    plotlyOutput("nation_conf"),
                    plotlyOutput("nation_rec"),
                    plotlyOutput("nation_dea")
                    
                ),
                # Other tabs 3
                f7Tab(
                    tabName = "Tab3",
                    icon = f7Icon("email"),
                    active = FALSE,
                    # tabs 3 content
                    leafletOutput("leaflet")
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
        
        output$plot_total <- renderPlotly({
            
            plot5 <- ggplot(Tog3[-c(1,47),], aes(Date,Rec2All, colour = isChina))+
                geom_line(size = 2.2, alpha = 0.8)+
                #geom_point(size = 3)+
                labs(x = "", y = "", title =  "Healed among the infected", subtitle = "by China and Rest of World", colour = "")+
                #geom_text(aes(label = paste0(round(100*Rec2All,1), "%")), nudge_y = 0.0025, nudge_x = 0.2, color = "black", size = 4.1)+
                scale_y_continuous(labels = scales::percent, limits = c(0,0.72))+
                scale_x_date(date_labels = "%b %d", date_breaks = "7 days")+
                scale_colour_brewer(palette = "Set1")+
                dark_theme_light()+
                theme(legend.position="none", legend.direction="horizontal", axis.text = element_text(size = 8, colour = "white"), axis.title = element_text(size = 14, colour = "white"), legend.text = element_text(size = 13, colour = "white"),
                      axis.line = element_line(size = 0.4, colour = "white"), plot.background = element_rect(fill = "black"), legend.background = element_rect(fill = "black"))
            
            ggplotly(plot5, object = Total)
            
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
                labs(x = "", y = "", title =  "Confirmation of virus infection")+
                scale_x_discrete(labels=c("2" = "Feb 25", "4" = "Feb 28", "6" = "Mar 4", "9" = "Mar 8", "12" = "Mar 12"))
            
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
                labs(x = "", y = "", title =  "Recovered of virus infection")+
                scale_x_discrete(labels=c("2" = "Feb 25", "4" = "Feb 28", "6" = "Mar 4", "9" = "Mar 8", "12" = "Mar 12"))
            
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
                labs(x = "", y = "", title =  "Recovered of virus infection")+
                scale_x_discrete(labels=c("2" = "Feb 25", "4" = "Feb 28", "6" = "Mar 4", "9" = "Mar 8", "12" = "Mar 12"))
            
            plotly::ggplotly(cts_p.d, object = value)
            
        })
        
        output$leaflet <- renderLeaflet({
            
            leaflet::leaflet(leaflet) %>% 
                leaflet::addProviderTiles(providers$CartoDB.DarkMatter) %>% 
                leaflet::addCircleMarkers(~longitude, ~latitude, popup=leaflet$city, weight = 3, radius=4, 
                                          color="red", stroke = F, fillOpacity = 0.5)
            
        })
        
    }
)

