#install.packages("shinydashboard")
#install.packages("viridis")
#install.packages("DT")
#install.packages('shinyWidgets')
library(shinyWidgets)
library(DT)
library(viridis)
library(DT)
library(shiny)
library(shinydashboard)
library(xlsx)
library(dplyr)
library(ggplot2)

wac_df <- read.xlsx('./mutated_drugs_wac_5yr.xlsx', sheetName = 'Prescription Drug 5 Year Histor', as.data.frame = TRUE)

#Calculate two new columns, "previous price before WAC increase, and percentage change of WAC" 
new_wac_df = wac_df %>% mutate(., Previous.WAC = WAC.Amount - WAC.Increase.Amount) %>% mutate(., Percent.Change.WAC = ((WAC.Amount - Previous.WAC)/Previous.WAC)*100) %>% arrange(., History.Effective.Date)

#filter out any records with 0 Prev price to avoid Inf for percentage change and they are not relevant to evaluate
new_wac_df = new_wac_df %>% filter(., Previous.WAC != 0.0 )


#Create queries for clean visuals

Avg_by_treatment = new_wac_df %>% group_by(., History.Effective.Date,NDC.Treatment.For.) %>%  summarise(., 'Avg.WAC.Increase' = mean(Percent.Change.WAC, na.rm = TRUE))

price_by_treatment = new_wac_df %>%  group_by(., History.Effective.Date, Manufacturer.Name, NDC.Description,NDC.Treatment.For.) %>%  summarise(., 'min.Price' = min(WAC.Amount,na.rm = TRUE), WAC.Increase.Amount) #using min price to make sure I am only using the lowest strength for each perscription.


make_plot_matrix <- function(filtered_df){
ggplot(filtered_df, aes(x = History.Effective.Date, y = min.Price , color = NDC.Description)) +geom_point() +facet_wrap(.~NDC.Treatment.For.,scale = "free") + geom_smooth(method ="lm") + 
  theme(
    strip.text.x = element_text(
      size = 14, face = "bold"
    ),
    strip.text.y = element_text(
      size = 14, face = "bold"
    )
)
}
make_plot_histo <- function(filtered_df){
  ggplot(filtered_df, aes(x = WAC.Increase.Amount)) + geom_histogram(aes(fill = NDC.Description)) + geom_vline(aes(xintercept=median(WAC.Increase.Amount)), color="black", linetype="dashed", size=1) + facet_wrap(.~NDC.Treatment.For.,scale = "free") +
    theme(
      strip.text.x = element_text(
        size = 14, face = "bold"
      ),
      strip.text.y = element_text(
        size = 14, face = "bold"
      )
    )
}


# Define UI for application that draws a histogram
# Use the Dashboard function to lay it out.
ui <- dashboardPage( skin = 'red',
 dashboardHeader(title = "Perscription WAC"),
 
 dashboardSidebar(
   sidebarMenu(
     menuItem("Dashboard", tabName = "Visual_tab", icon = icon("dashboard")),
     menuItem("Raw Data", tabName = "data_tab", icon = icon("table")),
     menuItem("Walkthrough", tabName = "exp_tab", icon = icon("list-alt"))
     
   )
   
 ),
 dashboardBody(
   tabItems(
     tabItem("Visual_tab",
             
              id1 <-uiOutput("id1"),
              id2 <- uiOutput("id2"),
             
               tabBox(
                 id = 'tabset',
                 width = "500px",
                 height = "200px",
                 selected = "Plot Matrix",
                 tabPanel("Plot Matrix", plotOutput("price_by_treatment_m1")),
                 tabPanel("Annual Price Increase", plotOutput("frequency_by_WACA"))
                
               )#/tabbox
              ),#/visual tab
     
     tabItem("data_tab",
             fluidPage(
               h1("Raw Data"),
               dataTableOutput("raw_data")
               )), #/tabItem
     tabItem("exp_tab",
             fluidPage(
               h1("Walkthrough"),
               p( "WAC (wholesale acquisition costs) for perscriptions and medical devices are a vital metric for health insurance companies to evaluate the general change in cost of the substance overtime."),
               
               h3("Plot Matrix"),
               p("This dashboard will allow for health insurance companies to identify the trends in cost to cover a particular treatment for a client and identify alternative perscription under the same treatment."),
               h3("Annual Price Increase"),
               p("Finds the most probable price increase for the next year under that particular perscription.")
             )) #/tabItem 

 
 )
 )
  
)
  


server <- function(input, output) {
  
  
  output$id1 <- renderUI({
    pickerInput(
      inputId = "id", label = "Manufacturers :",
      choices = unique(as.character(new_wac_df$Manufacturer.Name)),
      options = list('actions-box' = TRUE),
      selected = unique(as.character(new_wac_df$Manufacturer.Name)),
      multiple = TRUE
    )
  })
  
  output$id2 <- renderUI({
    pickerInput(
      inputId = "id2", label = "Treating For :",
      choices = unique(as.character(new_wac_df$NDC.Treatment.For.)),
      options = list('actions-box' = TRUE),
      selected = unique(as.character(new_wac_df$NDC.Treatment.For.))[1:4], #preset to the first four options
      multiple = TRUE
    )
  })
  
  
  filter_Manu_Treat <-  reactive({
    price_by_treatment %>% filter(.,Manufacturer.Name %in% input$id) %>% filter(.,NDC.Treatment.For.%in% input$id2)
  })
  
  
   output$price_by_treatment_m1<- renderPlot({
     myFilteredDf <- filter_Manu_Treat()
     make_plot_matrix(myFilteredDf)
     
     
     
   })
   
   output$frequency_by_WACA <- renderPlot({
     myFilteredDf <- filter_Manu_Treat()
     make_plot_histo(myFilteredDf)
    
   })
   # output$explain_text <- renderText({ #attempted to use in explanation but coudnt
   #   
   # 
   # })
   output$raw_data <- renderDataTable(new_wac_df)
}



# Run the application 
shinyApp(ui, server)

