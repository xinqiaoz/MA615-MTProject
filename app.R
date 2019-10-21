library(shiny)
library(tidyverse)
library(ggplot2)

## Data exacting
data<- read.csv("~/Desktop/BU/MA615/MidTermProj/Reef_Life_Survey_(RLS)#_Cryptic_Fish-cryptic_fish_surveys.csv")
data<- data[(colSums(is.na(data)) != nrow(data))]

## Data cleaning and Selection
data<- data %>% filter(Country=="Australia")
data<- data%>% filter(Class=="Actinopterygii")

## Separate date by Year, Month, Day and Time
data1<- data %>% separate(SurveyDate, c("Year","Month","Day"))
data1<- data1 %>% separate(Day, c("Day","Time"), sep="T")

## Factor the data
data1$Year<- as.factor(data1$Year)
data1$Month<- as.factor(data1$Month)
data1$Day<- as.factor(data1$Day)

## Making subsets for shiny
data_Year<- data1 %>% group_by(Year) %>% summarize(NewDepth=mean(Depth), NewBlock=sum(Block), NewTotal=sum(Total))
data_Family<- data1 %>% group_by(Family) %>% summarize(NewDepth=mean(Depth), NewBlock=sum(Block), NewTotal=sum(Total))
data_Taxon<- data1 %>% group_by(Taxon) %>% summarize(NewDepth=mean(Depth), NewBlock=sum(Block), NewTotal=sum(Total))
data_Depth<- data1 %>% group_by(Depth) %>% summarize(NewBlock=mean(Block), NewTotal=mean(Total))

#######################################

#### First Shiny App

ui1<- fluidPage(
    titlePanel("Reef Life data in Australia by Family"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "family", label = "Family", data_Family$Family)
        ),
        mainPanel(tableOutput("fa"))
    )
)


server1<- function(input, output, session){
    output$fa= renderTable({
        print(filter(data_Family, Family==input$family))
    })
}

shinyApp(ui1, server1)

#### Second Shiny App

ui2<- fluidPage(
    titlePanel("Reef Life data in Australia by Year"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "year", label = "Year", data_Year$Year)
        ), 
        mainPanel(tableOutput("ye"),
                  plotOutput("yr", click = "plot_click", hover = "plot_hover"))
    )
)


server2<- function(input, output, session){
    output$ye= renderTable({
        print(filter(data_Year, Year==input$year))
    })
    output$yr= renderPlot({
        ggplot(data_Year, aes(x=Year, y=NewBlock, group=1))+ geom_line() + geom_point() + labs(title="Trend of Block by Year") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplot(data_Year, aes(x=Year, y=NewTotal, group=1))+ geom_line() + geom_point() + labs(title="Trend of Total by Year")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}

shinyApp(ui2, server2)

