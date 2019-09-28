#author: Nicole Li
#AndrewID： honghual

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)


# Load data
airbnb<-read.csv("AB_NYC_2019.csv",header = TRUE)
airbnb<-airbnb[-c(6:8,13:14)]

pdf(NULL)

#Dashboard header----------------------------------------------
header <- dashboardHeader(title= "Airbnb New York 2019")

#Dashboard sidebar-----------------------------------------------
sidebar <-dashboardSidebar(
  
  #Menu Items--------------------------------------------
  menuItem("Prices by Room Type",icon = icon("bar-chart"), tabName= "plot1"),
  menuItem("Prices by Neighbourhood",icon = icon("bar-chart"), tabName= "plot2"),
  menuItem("Top Airbnb Hosts",icon = icon("bar-chart"), tabName= "plot3"),
  menuItem("Table", icon = icon("table"), tabName= "table"),
  
  # Room Type Check Box
  checkboxGroupInput(inputId="roomSelect", label="Room Type",
                     choices= unique(airbnb$room_type) , 
                     selected= c("Entire home/apt", "Private room", "Shared room")),
  
  # Neighbourhood Selection
  selectInput("neighbourhoodSelect", label= "Neighbourhood",
              choices= unique(airbnb$neighbourhood_group), 
              multiple= TRUE,
              selected = c("Manhattan" , "Brooklyn", "Queens", "Bronx", "Staten Island")),
  
  # Price Range Filters
  sliderInput("priceSelect", label= "Price Range",
              min= min(airbnb$price),
              max= max(airbnb$price),
              value= c(min(airbnb$price),300), step=10)
  
  
  
)
#Dashboard body-----------------------------------------------
body <- dashboardBody(tabItems(
  
  #Room Type plotpage-------------------------------------------------
  tabItem("plot1",
          
          # input and value Boxes---------------------------------------------
          fluidRow(
            infoBoxOutput("avgprice"),
            infoBoxOutput("avgreview"),
            infoBoxOutput("avgavailable")
          ),
          
          #plot of the prices and room type histogram-------------------
          fluidRow(
            tabBox(title = "Prices and Room Type",
                   width= 12,
                   tabPanel("Airbnb New York", 
                            plotlyOutput("PriceType")) 
            )
          )
  ),
  
  #Neighbourhood Prices Violin Plot--------------------------------------
  tabItem("plot2",
          fluidRow(
            tabBox(title = "Prices and Neighbourhood",
                   width= 12,
                   tabPanel("Airbnb New York"),
                   plotlyOutput("PriceNeighbourhood")
            )
          )
  ),
  
  
  # Top host in the Area Bar Plot------------------------------------------
  tabItem("plot3",
          fluidRow(
            tabBox(title = "Top Airbnb Host",
                   width= 12,
                   tabPanel("Airbnb New York"),
                   plotlyOutput("PopularHost")
            )
          ),
          fluidRow(
            box(title="Top Airbnb Host", DT::dataTableOutput("HostTable"), width = 12)
          )
  ),
  
  
  tabItem("table",
          fluidPage(
            box(title="Selected Room Information", DT::dataTableOutput("table"), width = 12)
          )
  )
))

ui <- dashboardPage(header, sidebar , body)


#Define server function to create plots and value boxes
server <- function(input, output){
  #reactive data---------------------------------------------------------------- 
  airbnbInput<- reactive ({
    airbnb <- airbnb %>% 
      
      # price range filter
      filter(price >= input$priceSelect[1] & price <= input$priceSelect[2])
    
    # neighbourhood filter and types of room filter
    if (length(input$neighbourhoodSelect)>0) {
      airbnb <- subset(airbnb, neighbourhood_group %in% input$neighbourhoodSelect &
                         room_type %in% input$roomSelect)
    }
    return (airbnb)
  })
  
  #reative data for host--------------------------------------------------
  airbnbHost <- reactive({
    airbnb1<-airbnbInput()[order(-airbnbInput()$calculated_host_listings_count),]
    airbnb2<- airbnb1[!duplicated(airbnb1$host_name),]
    return(airbnb2)
  })
  
  
  #average price info box
  output$avgprice <- renderInfoBox({
    num<- paste(round(mean(airbnbInput()$price),2),"$")
    infoBox(title= "Avg Price", value = num, icon = icon("dollar"), color = "purple")
  })
  
  #average reveiw info box
  output$avgreview <- renderInfoBox({
    num<- paste(round(mean(airbnbInput()$number_of_reviews)),"reviews")
    infoBox(title="Avg Number of Review",value = num, icon = icon("comment"), color = "red" )
  })
  
  #average available info box
  output$avgavailable <- renderInfoBox({
    num<- paste(round(mean(airbnbInput()$availability_365),2), "/365 Days")
    infoBox(title="Avg Days Available in a Year", value = num, icon = icon("calendar"), color = "green")
  })
  
  #plot1: prices vs type histogram
  output$PriceType <- renderPlotly({
    ggplot(data=airbnbInput(), aes(x=price,color=room_type, fill=room_type))+geom_histogram()
  })
  
  #plot2 neighbourhood prices violin chart
  output$PriceNeighbourhood <- renderPlotly({
    ggplot(data = airbnbInput(), aes(x = neighbourhood_group, y= price, fill=neighbourhood_group)) + geom_violin()
  })
  
  #plot3
  output$PopularHost <- renderPlotly({
    ggplot(data = airbnbHost()[1:6,], aes( x=host_name, y = calculated_host_listings_count))+
      geom_bar(stat = "identity", fill = "red")
  })
  
  
  #Entire Data table
  output$table <- DT::renderDataTable(airbnbInput(),options = list(lengthMenu = c(5, 30, 50), pageLength = 20))
  
  #Host Data table
  output$HostTable <- DT::renderDataTable(airbnbHost()[,c(2:6,10)],
                                          options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
}


shinyApp(ui=ui, server=server)



