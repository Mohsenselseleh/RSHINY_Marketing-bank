library(shiny)
library("shinydashboard")
library(tidyverse)

## loading the data
df = read.csv("bank-full.csv", sep = ";", stringsAsFactors = T)

## list of optiosn to be used for settings
## like parameter selection for x axis and y axis

xList = c("loan","default","housing")
yList = c("duration","age","balance")

xList1 = c("loan","default","housing")
yList1 = c("education","marital")






ui <- dashboardPage(
  dashboardHeader(title = "Bank Data"),
  dashboardSidebar(
    sidebarMenu(
      h3("Settings"), ## title for settings 
      menuItem("Bar Graph",  ## name of the side tab
               selectInput("x1","Select X-Axis",xList1 ), ## input selection 
               selectInput("y1","Select Y-Axis",yList1 )
      ),
      menuItem("Box Plot", 
               selectInput("x","Select X-Axis",xList ),
               selectInput("y","Select Y-Axis",yList )
      ),
      menuItem("Table",
               selectInput("x2","Select X-Axis",xList1 ),
               selectInput("y2","Select Y-Axis",yList1 ))
    )
    
  ),
  dashboardBody(
    
    
    fluidPage(
      tabsetPanel( ## tabs in the main body
        tabPanel("Bar Graph", ## tab name
                 box(status = "warning", solidHeader = FALSE, ## using box, makes it look good
                     collapsible = TRUE,width = 12,
                     plotOutput("BarGraph") ## plot output
                 )
        ),
        tabPanel("Box Plot",
                 box(status = "warning", solidHeader = FALSE,
                     collapsible = TRUE,width = 12,
                     plotOutput("BoxPlot")
                 )
                 
        ),
        tabPanel("Table", 
                 box(status = "warning", solidHeader = FALSE,
                     collapsible = TRUE,width = 12,
                     tableOutput("Table") ## table output
                 )
        )
      )

    )
  )
)


server <- function(input, output, session) {
  
  
  output$BoxPlot = renderPlot({
    req(input$x) ## waits until it gets the value from the inputr handle
    req(input$y)
    
    ## get() is used because ggplot doesnt take string as input, so get will convert it back to object
    ## box plot is used for categrical vs numeric
    ggplot(df, aes(x=get(input$x), y=get(input$y), color=get(input$x))) +
      geom_boxplot()+ theme_minimal()+
      ylab(input$y)+
      xlab(input$x) +
      ggtitle(paste("Box Plot:",input$x,"with respect to",input$y))
    
  })  
  
  output$BarGraph = renderPlot({
    req(input$x1)
    req(input$y1)
    
    ## creating new table
    ## gives the number of observation in the data in each category 
    df1 = df%>%
      group_by(get(input$x1),get(input$y1))%>%
      summarise(Count = n())
    
    ## changing column names of the new data
    names(df1) = c(input$x1,input$y1,"Count")

    ggplot(df1, aes(fill=get(input$x1) , x= get(input$y1)  , y= Count)) +
      geom_bar(position="dodge", stat="identity") +
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ylab("Number of Customers")+
      xlab(input$x1) +
      ggtitle(paste("Bar Graph:",input$x1,"with respect to",input$y1))+
      theme(legend.title=element_blank())+
      theme_classic()+ 
      guides(fill=guide_legend(title=input$x1))
    
  })
  
  output$Table = renderTable({
    req(input$x2)
    req(input$y2)
    
    
    df1 = df%>%
      group_by(get(input$x2),get(input$y2))%>%
      summarise(Count = n())
    
    names(df1) = c(input$x2,input$y2,"Count")

    df1
    
  })
  
}

shinyApp(ui, server)

