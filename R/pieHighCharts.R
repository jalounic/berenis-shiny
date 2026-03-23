library(c3)
library(shiny)

ui <- fluidPage(
  c3Output("gauge1"), 
  actionButton("start_button", "Start Engines")
)

server <- function(input, output, session) {
  gauge_breaks <- seq(0, 100, 1)
  is_animating <- FALSE
  current_index <- 1
  
  observeEvent(input$start_button, {
    current_index <<- 0
    is_animating <<- TRUE
  })
  
  gauge_value = reactive({
    invalidateLater(5*(1.04^current_index))
    
    if(is_animating)
      current_index <<- current_index + 1
    
    if(current_index == length(gauge_breaks))
      is_animating <<- FALSE
    
    data.frame(rpm = as.numeric(gauge_breaks[current_index]))
  })
  
  output$gauge1 <- renderC3({
    c3_gauge(c3(gauge_value()))
  })
}

shinyApp(ui, server)


# -------------------------------------

library(highcharter)
library(shiny)
library(tidyverse)
library(shinydashboard)


ui <- fluidPage(
  # use shinydashboard elements within regular shiny app
  useShinydashboard(),
  
  fluidRow(column(width = 4, highchartOutput("pieIdentified", height = "300px")),
           column(width = 4, highchartOutput("pieDiscussed", height = "300px")),
           column(width = 4, highchartOutput("pieSelected", height = "300px"))
           ),
  
  fluidRow(valueBox(subtitle="Studies identified", value=2350, icon = icon("microscope", lib="font-awesome"), color="yellow"),
           valueBox(subtitle="Studies discussed", value=200, icon("comments"), color = "aqua"),
           valueBox(subtitle="Studies selected", value=60, icon = shiny::icon("clipboard-check", lib="font-awesome"), color = "maroon")
  )
)

server <- function(input, output, session) {
 
  
   output$pieIdentified <- renderHighchart({
    
    highchart() %>% 
      hc_add_series(type = "pie", data = data.frame(y=350, name="350", color = "#FDBB2C"), innerSize = "65%", name = "Studies identified") %>%
      hc_tooltip(crosshairs = TRUE) %>%
      # hc_legend(verticalAlign = "top") %>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = "2350", style = list(fontWeight = "bold", fontSize = "35px"), align = "center", verticalAlign = "middle", y = 55) %>%
      hc_subtitle(text = "Studies identified", style = list(fontSize = "20px"), align = "center", verticalAlign = "top", y = 45) %>%
      hc_plotOptions(pie = list(dataLabels = list(enabled = FALSE), startAngle = -90, endAngle = -90, center = list("50%", "50%"), size = "80%"))
                     
  })
   
   output$pieDiscussed <- renderHighchart({
     
     highchart() %>% 
       hc_add_series(type = "pie", data = data.frame(y=350, name="350", color = "#2BB0D7"), innerSize = "65%", name = "Studies Discussed") %>%
       hc_tooltip(crosshairs = TRUE) %>%
       # hc_legend(verticalAlign = "top") %>%
       hc_legend(enabled = FALSE) %>%
       hc_title(text = "200", style = list(fontWeight = "bold", fontSize = "35px"), align = "center", verticalAlign = "middle", y = 55) %>%
       hc_subtitle(text = "Studies discussed", style = list(fontSize = "20px"), align = "center", verticalAlign = "top", y = 45) %>%
       hc_plotOptions(pie = list(dataLabels = list(enabled = FALSE), startAngle = -90, endAngle = -90, center = list("50%", "50%"), size = "80%"))
     
   })
   
   output$pieSelected <- renderHighchart({
     
     highchart() %>% 
       hc_add_series(type = "pie", data = data.frame(y=350, name="350", color = "#EF4C6A"), innerSize = "65%", name = "Studies Selected") %>%
       hc_tooltip(crosshairs = TRUE) %>%
       # hc_legend(verticalAlign = "top") %>%
       hc_legend(enabled = FALSE) %>%
       hc_title(text = "60", style = list(fontWeight = "bold", fontSize = "35px"), align = "center", verticalAlign = "middle", y = 55) %>%
       hc_subtitle(text = "Studies selected", style = list(fontSize = "20px"), align = "center", verticalAlign = "top", y = 45) %>%
       hc_plotOptions(pie = list(dataLabels = list(enabled = FALSE), startAngle = -90, endAngle = -90, center = list("50%", "50%"), size = "80%"))
     
   })
}

shinyApp(ui, server)
