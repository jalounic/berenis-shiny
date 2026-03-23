library(shiny)

navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-nav ml-auto navbar-right", align = "right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

ui <- navbarPageWithInputs(
  
  # tags$ul(
  #   class = "navbar-nav ml-auto navbar-right",
  #   rightUi,
  #   tags$li(class = "nav-item",
  #           tags$a(id = "langPicker", class = "nav-link",)

  "Test app",
  tabPanel("tab1", "tab 1", textOutput("out")),
  tabPanel("tab2", "tab 2"),
  inputs = pickerInput(inputId = "langPicker",
                       label = "",
                       choices = flags$lang,
                       choicesOpt = list(content = flags$img,
                                         style = rep(("background: #112446;"), length(flags$lang))),
                       width = "fit")
)

server <- function(input, output, session) {
  output$out <- renderText(input$search)
}

shinyApp(ui = ui, server = server)







highchart() %>%
  hc_add_series(TopPN, "column", hcaes(x = Incoming.P.N, y = 21), name = "DIH Goal", color = "#ffffff", borderColor = "#C8C8C8", borderWidth = 2, pointPadding = -0.1, 
                tooltip = list(pointFormat = "DIH Goal: 21 days<br>")) %>%
  hc_add_series(TopPN, "column", hcaes(x = Incoming.P.N, y = round(Hold.Time.Quote,1)), name = "Quote Hold Time", color = "#FF8888", 
                tooltip = list(pointFormat = "<span style='color:{point.color}'>\u25CF</span> Quote Hold Time: {point.HoldTimeQuoteTrue} days<br>")) %>%
  hc_add_series(TopPN, "column", hcaes(x = Incoming.P.N, y = round(Hold.Time.Other,1)), name = "Other Hold Time", color = "#E5A919", 
                tooltip = list(pointFormat = "{point.color} Other Hold Time: {point.HoldTimeOtherTrue} days<br>")) %>%
  hc_add_series(TopPN, "column", hcaes(x = Incoming.P.N, y = round(Shop.Time,1)), name = "Shop Time", color = "#51C1BC", 
                tooltip = list(pointFormat = "Shop Time: {point.ShopTimeTrue} days<br>Volume: {point.Quantity}")) %>%
  hc_xAxis(type = "category") %>%
  hc_yAxis(title = "days", labels = list(format = "{value} days")) %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, 
             headerFormat = "<b>PN {point.key}</b><br>")




# ---- first trial of sunburst diagram --> NA values cannot be handled by highcharter --> Option: create NULLs 
berenis_tmp <- data_to_hierarchical(tmp, group_vars = c(cat_freqRange,cat_source, cat_studyType, cat_examinationObject, cat_duration,cat_targetDimension,cat_freqRangeSimple,cat_studyTypeSimple), size_var = n)

highchart() %>% hc_chart(data=berenis_tmp, type = "sunburst")




# ---- Newsletter PDF search and Pubmed API ----
my_pdf <- ""
result <- pdfsearch::keyword_search(my_pdf, 
                         keyword = 'pubmed',
                         path = TRUE)

# find the every entry of pubmed in the newsletter
pubmed_ids <- grep("pubmed", unlist(result$token_text))
# retrieve the articleID (or pubmedID) 
articleIDs <- unlist(result$token_text)[pubmed_ids+1]

# fetch article details from the web
article_details <- RISmed::EUtilsGet(articleIDs)




# ---- Translation -----
library(shiny)
library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "data/Dictionaries/translation.json")

i18n$set_translation_language("EN")

ui <- fluidPage(usei18n(i18n),
                p(i18n$t("Study type")),
                actionButton("go", "GO!")
)

server <- function(input, output, session) {
  observeEvent(input$go,{
    update_lang(session, "DE")
  })
}

shinyApp(ui, server)




choices <- berenis_cats$Studientyp_simple[!is.na(berenis_cats$Studientyp_simple)]
choices
berenis %>% count(cat_studyTypeSimple)




