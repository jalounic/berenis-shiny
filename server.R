# *************************************
# ----------- SHINY SERVER ------------
# *************************************

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  #bs_themer()
  
  # Translator as a reactive version that reacts to changes of the language.
  # see: https://github.com/Appsilon/shiny.i18n/blob/master/examples/live_language_change/browser_app.R
  # observeEvent(input$selected_language, {
  #   # This print is just for demonstration
  #   print(paste("Language change!", input$selected_language))
  #   # Here is where we update language in session
  #   shiny.i18n::update_lang(session, input$selected_language)
  # })
  # 
  

  ########## Function Definitions ##########

  # this function creates a button within the DT table
  # https://stackoverflow.com/questions/45739303/r-shiny-handle-action-buttons-in-data-table
  buttonInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
    
  getDetailsText <- function(df, varName){ 
    
    # get row for the selected item
    # detailsRow <- as.numeric(str_split(input$selected_button, "_")[[1]][2])
    detailsRow <- input$table_rows_selected
    
    # find value 
    txt <- df[detailsRow, varName]
    
    return(txt)
  }
  
  # getNLDLtext<- function(df, varName){ 
  #   
  #   # get row for the selected item
  #   detailsRow <- as.numeric(str_split(input$selected_buttonDLNL, "_")[[1]][2])
  #   
  #   # find value 
  #   txt <- df[detailsRow, varName]
  #   
  #   return(txt)
  # }
  
  
  translate_categories <- function(cats_df, catName_DE, catLookupVals_DE, lang = "DE"){
    
    # catch empty lookups
    if(length(catLookupVals_DE) == 0 & !is.null(catLookupVals_DE)){
      return(NA)
    }
    
    # check if multiple categories (separated by ";")  
    catLookupVals_DE <- str_trim(str_split_1(catLookupVals_DE, ";"))
    
    # filter onto function arguments
    res <- berenis_catsDict %>% 
      filter(Kategorie == catName_DE, DE %in% catLookupVals_DE) %>%
      dplyr::select(all_of(lang)) %>% 
      pull(lang) %>% 
      paste(collapse = "; ")
    
    return(res)
    
  } 
  
  
  translate_manually <- function(activeLang, entxt, detxt=NULL, frtxt=NULL) {
    
    print(activeLang)
    
    if (activeLang == "EN") {
      return(entxt)
      
    } else if (activeLang == "DE") {
      
    if (is.null(detxt)) {return(entxt)}
    return(detxt)
    
    } else if (activeLang == "FR") {
      
      if (is.null(frtxt)) {return(entxt)}
      return(frtxt)
      
    } else {
      return(NULL)
    }
  }
  


  # observeEvent(input$langPicker, {
  #   
  #   if (input$langPicker == "EN") {
  #     return(entxt)
  #     
  #   } else if (input$langPicker == "DE") {
  #     if (is.NULL(detxt)) {return(entxt)} else {return(detxt)}
  #     
  #   } else if (input$langPicker == "FR") {
  #     if (is.NULL(frtxt)) {return(entxt)} else {return(frtxt)}
  #     
  #   } else {
  #     return(NULL)
  #   }
  # })
  
  
  # ########## Browser Navigation ########## 
  
  # source 1 : https://github.com/judytlewis/shiny-browser-nav/blob/master/app.R
  # source 2 : https://github.com/daattali/advanced-shiny/tree/master/navigate-history
  
  # ... not implemented yet ...
  

  
  

  ########## Reactive Variables ##########
  
  # -------- ** Shinyjs onclick Event - make BERENIS-Logo to "home button" -------- 
  shinyjs::onclick("BERENIS_img",  updateNavbarPage(session, inputId = "navbar", selected = "idExplorer"))
  
  
  # -------- ** Event - Minimize Sidebar -------- 
  
  # reactive Value that tracks whether the Sidebar is minimized or not: 
  #    0 = initial state (minimized)
  #    1 = minimized
  #    2 = maximized
  minimizeSidebar <- reactiveVal(0)

  
  # -------- ** Reactive Var - Updated BERENIS dataframe -------- 
  berenis_updated <- reactiveValues(df = berenis)

  
  # -------- ** Reactive Var - Filtered Data -------- 
  filteredData <- reactive({
    
    # import data into local variable
    data <- berenis_updated$df
  
    # filter data (incase the filter has been used, i.e. not in the initial state 0)
    if (minimizeSidebar() != 0) {

      
      # -- simple filtering
      if (is.null(input$pickerFreqRangeSimple) == FALSE) {
        # gsub takes care of the parenthesis (see https://stackoverflow.com/questions/56174805/how-to-search-for-strings-with-parentheses-in-r) and the "&"
        #    gsub adds "\\" in front of it each symbol in order to work as an regex  
        data <- data[str_detect(data$cat_freqRangeSimple, paste(gsub("(\\(|\\[|\\)|\\]|\\)|\\&)", "\\\\\\1", input$pickerFreqRangeSimple), collapse = "|")),]
      }

      if (is.null(input$pickerStudyTypeSimple) == FALSE) {
        data <- data[str_detect(data$cat_studyTypeSimple, paste(input$pickerStudyTypeSimple, collapse = "|")),]
      }
      
      if (is.null(input$pickerNLissue) == FALSE) {
        #choices <- sapply(choices, function(i) {paste0("(?<![:space:]|[:alnum:])",i,"(?![:space:]|[:alnum:])")} )
        # data <- data[str_detect(data$NL_issue, paste(sapply(input$pickerNLissue, function(i) {paste0("(?<![:space:]|[:alnum:])",i ,"(?![:space:]|[:alnum:])")}), collapse = "|")), ]
        data <- data[str_detect(data$NL_issue, paste0("\\b(", paste(input$pickerNLissue, collapse = "|"), ")\\b", sep="")), ]
      }
      

      # -- advanced filtering
      if (is.null(input$pickerNLissue_detailed) == FALSE) {
        #choices <- sapply(choices, function(i) {paste0("(?<![:space:]|[:alnum:])",i,"(?![:space:]|[:alnum:])")} )
        # data <- data[str_detect(data$NL_issue, paste(sapply(input$pickerNLissue_detailed, function(i) {paste0("(?<![:space:]|[:alnum:])",i ,"(?![:space:]|[:alnum:])")}), collapse = "|")), ]
        data <- data[str_detect(data$NL_issue, paste0("\\b(", paste(input$pickerNLissue_detailed, collapse = "|"), ")\\b", sep="")), ]
      }
      
      if (is.null(input$pickerFreqRange) == FALSE) {
        # gsub takes care of the parenthesis (see https://stackoverflow.com/questions/56174805/how-to-search-for-strings-with-parentheses-in-r)
        data <- data[str_detect(data$cat_freqRange, paste(gsub("(\\(|\\[|\\)|\\]|\\)|\\&)","\\\\\\1", input$pickerFreqRange), collapse = "|")),]
      }
      
      if (is.null(input$pickerSource) == FALSE) {
        data <- data[str_detect(data$cat_source, paste(input$pickerSource, collapse = "|")),]
      }
      
      if (is.null(input$pickerStudyType) == FALSE) {
        data <- data[str_detect(data$cat_studyType, paste(input$pickerStudyType, collapse = "|")),]
      }
      
      if (is.null(input$pickerStudyObject) == FALSE) {
        data <- data[str_detect(data$cat_studyObject, paste(input$pickerStudyObject, collapse = "|")),]
      }
      
      if (is.null(input$pickerDuration) == FALSE) {
        data <- data[str_detect(data$cat_duration, paste(input$pickerDuration, collapse = "|")),]
      }
      
      if (is.null(input$pickerTargetDim) == FALSE) {
        data <- data[str_detect(data$cat_targetDimension, paste(input$pickerTargetDim, collapse = "|")),]
      }
    
      # filter onto the selected publication year
      if (is.null(input$sliderYear) == FALSE) {
        data <- data[data$OA_year %in% seq(input$sliderYear[1], input$sliderYear[2],1),]
        data <- data[order(data$OA_year, decreasing = TRUE), ]  # order data by year
      }
    }
    
    # create a "Details-Button" for each row of the filtered data frame
    data$details = buttonInput(
      FUN = actionButton,
      len = nrow(data),
      id = 'button_',
      label = "[ ... ]",
      #icon = icon("bars", style="font-size: 18px;"),
      title = i18n$t("Study details (click)"),
      class = "detailsButton",
      onclick = 'Shiny.setInputValue(\"selected_button\",  this.id.concat(\"_\", Math.random()))' # https://stackoverflow.com/questions/63042200/action-button-in-table-does-not-respond-if-i-click-twice-in-a-row-on-the-same-on
    )

    # create text labels from the date variables: "Mon Year" in the correct language
    data$NL_date_label <- paste(monthByLang[monthToNumber(data$NL_month), input$langPicker], data$NL_year)
    #berenis$NL_date <- format(ISOdate(berenis$NL_year, monthToNumber(berenis$NL_month), 1), format = "%b %Y")
    
    
    # create download links and download-button form the URLs
    data$NL_DL_link <- (sapply(data[, paste0(input$langPicker, "_url")], createLink, buttonTxt=""))
      
    # data$NL_DL_link <- buttonInput(
    #   FUN = actionButton,
    #   len = nrow(data),
    #   id = 'buttonDLNL_',
    #   label = "",
    #   icon = icon("download", style="font-size: 18px;"),
    #   #href = createLink(data[, paste0(input$langPicker, "_url")], buttonTxt=""),
    #   #href = createLink(data[, "DE_url"], buttonTxt=""),
    #   #icon = tags$i(class = "glyphicon glyphicon-list", style="font-size: 18px;"),
    #   # onclick = 'Shiny.setInputValue(\"selected_buttonDLNL\",  this.id)'
    #   # onclick = 'Shiny.setInputValue("current_id", clicked_id, {priority: "event"})'
    #   onclick = 'Shiny.setInputValue(\"selected_buttonDLNL\",  this.id.concat(\"_\", Math.random()))' # https://stackoverflow.com/questions/63042200/action-button-in-table-does-not-respond-if-i-click-twice-in-a-row-on-the-same-on
    # )

    # create title and summary according to selected language
    data$title_byLang <- data[, paste0(input$langPicker, "_title")]
    data$summary_byLang <- data[, paste0(input$langPicker, "_summary")]

    return(data)
    
  })

  
  ########## Observe Events ##########
  

  # -------- ** Event - Toggle Sidebar --------
  
  # Toggle Sidebar Variable - switches from true to false when the toggleSidebar-Button is clicked 
  observeEvent(input$toggleSidebar, {
    #minimizeSidebar(!minimizeSidebar())
    if ((minimizeSidebar() == 0) || (minimizeSidebar() == 1)) {
      minimizeSidebar(2)
    } else {
      minimizeSidebar(1)
    }
  })
  
  
  # -------- ** Event - Select Language --------
  
  observeEvent(input$langPicker, {
    # update language in session, i.e. re-render UI with the new language
    shiny.i18n::update_lang(language = input$langPicker , session = session)
    
  
    # # create a variable that holds the language --> for usage in the UI
    # output$selectedLanguage <- renderUI({return(input$langPicker)})
    
  })

  # window_height <- reactive({
  #   shinybrowser::get_height()
  # })
  # window_width <- reactive({
  #   shinybrowser::get_width()
  # })
  # 
  # observeEvent(window_height, {
  #   showNotification(window_height)
  # })
  # observeEvent(window_width, {
  #   showNotification(window_width)
  # })
  
  
  # -------- ** Events - Testing serveral Download Handler for the Newsletter Download  --------
  
  # DLNL_buttonID <- reactive(input$selected_buttonDLNL)
  # # create the url while considering the selected language
  # NL_url <- reactive(getNLDLtext(filteredData(), paste0(input$langPicker, "_url")))
  # # DL_filename <- paste0("BERENIS_Newsletter_", getNLDLtext(filteredData(), "NL_issue") , "_", input$langPicker, ".pdf")
  # 
  # # further reading: https://stackoverflow.com/questions/57973357/how-to-use-downloadhandler-for-the-download-button-create-inside-shiny-datatable
  # observeEvent(input$selected_buttonDLNL, {
  #   
  #   # controlling - temporary
  #   print(input$selected_buttonDLNL)
  #   print(DLNL_buttonID())
  # 
  #   # retrieve selected row from the button's name
  #   selectedRow <- as.numeric(str_split(input$selected_buttonDLNL, "_")[[1]][2])
  # 
  #   # controlling - temporary
  #   print(getNLDLtext(filteredData(),"NL_DL_link"))
  #   # cat(NL_url)
  #   showNotification(input$selected_buttonDLNL)
  # 
  # })
  # 
  # # create output with downloadHandler
  # 
  # eventReactive(input$selected_buttonDLNL, {
  # output[["selected_buttonDLNL_98"]] <- downloadHandler(
  # 
  #   filename = function(){
  #     # paste0("BERENIS_Newsletter_", getNLDLtext(filteredData(), "NL_issue") , "-", input$langPicker, ".pdf")
  #     paste0("BERENIS_Newsletter_", "18" , "___", input$langPicker, ".pdf")
  #     # paste0("BERENIS_Newsletter_", input$langPicker, ".pdf")
  #   },
  # 
  #   content = function(file){
  #     # httr::GET(NL_url(), httr::write_disk(file))
  #     httr::GET("https://www.bafu.admin.ch/dam/bafu/de/dokumente/elektrosmog/fachinfo-daten/newsletter-berenis-nr-27-dezember.pdf.download.pdf/Newsletter%20BERENIS%20Nr.%2027%20-%20Dezember%202021.pdf", httr::write_disk(file), progress())
  #   }
  # )
  # })
  
  # nr <- reactive(nrow(filteredData()))
  # 
  # lapply(1:nr, function(i){
  #   
  #   print(i)
  #   
  #   output[[paste0("buttonDLNL_",i)]] <<- downloadHandler(
  #     
  #     filename = function(){
  #       # paste0("BERENIS_Newsletter_", getNLDLtext(filteredData(), "NL_issue") , "-", input$langPicker, ".pdf")
  #       paste0("BERENIS_Newsletter_", "18" , "___", input$langPicker, ".pdf")
  #       # paste0("BERENIS_Newsletter_", input$langPicker, ".pdf")
  #     },
  #     
  #     content = function(file){
  #       # httr::GET(NL_url(), httr::write_disk(file))
  #       httr::GET("https://www.bafu.admin.ch/dam/bafu/de/dokumente/elektrosmog/fachinfo-daten/newsletter-berenis-nr-27-dezember.pdf.download.pdf/Newsletter%20BERENIS%20Nr.%2027%20-%20Dezember%202021.pdf", httr::write_disk(file))
  #     }
  #   )
  #   
  # })
  
  
  # # create the url while considering the selected language
  # NL_url <- reactive(getNLDLtext(filteredData(), paste0(input$langPicker, "_url")))
  # DL_filename <- reactive(paste0("BERENIS_Newsletter_", getNLDLtext(filteredData(), "NL_issue") , "_", input$langPicker, ".pdf"))
  
  # output[["DL_TestButton"]] <- downloadHandler(
  #   
  #   filename = function(){paste0("BERENIS_Newsletter_", "18" , "___", input$langPicker, ".pdf")},
  # 
  #     # paste0("BERENIS_Newsletter_", input$langPicker, ".pdf")
  # 
  #   
  #   content = function(file){
  #     #file.copy("www/BERENIS_logo_blau.png", file)
  #     
  #     httr::GET("https://www.bafu.admin.ch/dam/bafu/de/dokumente/elektrosmog/fachinfo-daten/newsletter-berenis-nr-27-dezember.pdf.download.pdf/Newsletter%20BERENIS%20Nr.%2027%20-%20Dezember%202021.pdf", httr::write_disk(file), progress())
  #   }
  # )

  
    # create output with downloadHandler
  # observeEvent(input$selected_buttonDLNL, {
  #   
  #   print(input$selected_buttonDLNL)
  #   
  #   output[[input$selected_buttonDLNL]] <- downloadHandler(
  #     
  #     filename = function(){
  #       paste0("BERENIS_Newsletter-", getNLDLtext(filteredData(), "NL_issue") , "-", input$langPicker, ".pdf")
  #     },
  #     
  #     content  = function(file){
  #       GET(getNLDLtext(filteredData(), paste0(input$langPicker, "_url")), write_disk(file))
  #     }
  #   )
  # })
  

  # -------- ** Event - Details Button --------
  
  # switch to the Details-Tab when hitting a "Details" Button
  observeEvent(input$table_rows_selected, priority = 1, {

    # create the Details Tab and its contents 
    insertTab(inputId = "inTabset",
              
              tabPanel(title = i18n$t("Details"), value = "detailsTab", icon = icon("bars"),
                       
                                h3(textOutput("OA_title"), style ="font-weight: bold;"),
                                p(textOutput("OA_authors")),
                                
                                h4(i18n$t('Published in: '), textOutput("OA_journal", inline = TRUE), ' (', textOutput("OA_year", inline = TRUE), ')'),
                                uiOutput("OA_articleUrl"),
                                
                       fluidRow(
                         column(width = 8,
                                
                                
                                # ---------- **** Well Panel - BERENIS Summary -----------
                                
                                wellPanel(
                                  h4(i18n$t("BERENIS Summary"), style ="font-weight: bold;"),
                                  
                                  # div(class = "ui horizontal divider", icon("tag"), 
                                  #     "BERENIS Summary"),
                                  
                                  em(textOutput("NL_studyTitle")),
                                  br(),
                                  textOutput("NL_studySummary"),
                                  br(),
                                  em(i18n$t("Presented in BERENIS Newsletter "), textOutput("NL_issue", inline = TRUE), 
                                     "(", textOutput("NL_date_label", inline = TRUE), ") -> ", 
                                     tags$a(href = getDetailsText(filteredData(), paste0(input$langPicker, "_url")), i18n$t("Download"))
                                     )
                                )
                         ),
                         
                         column(width = 4,
                                
                                # ---------- **** Well Panel - Study Characteristics -----------

                                # Output well panel
                                wellPanel(
                                  h4(i18n$t("Study Characteristics"), style ="font-weight: bold;"),

                                  # Study type
                                  span(paste0(i18n$t("Study type"),":"), style ="font-weight: bold;"), 
                                  textOutput("txt_studyTypeSimple", inline = TRUE),
                                  br(),
                              
                                  # Source
                                  span(paste0(i18n$t("Source"),":"), style ="font-weight: bold;"), 
                                  textOutput("txt_source", inline = TRUE),
                                  br(),

                                  # Frequency range
                                  span(paste0(i18n$t("Frequency range"),":"), style ="font-weight: bold;"), 
                                  textOutput("txt_freqRange", inline = TRUE),
                                  br(),

                                  # Study Object
                                  span(paste0(i18n$t("Study object"),":"), style ="font-weight: bold;"), 
                                  textOutput("txt_studyObject", inline = TRUE),
                                  br(),

                                  # Target size
                                  span(paste0(i18n$t("Target size"),":"), style ="font-weight: bold;"), 
                                  textOutput("txt_targetDimension", inline = TRUE),
                                  br(),

                                  # Period of action
                                  span(paste0(i18n$t("Period of action"),":"), style ="font-weight: bold;"), 
                                  textOutput("txt_duration", inline = TRUE),
                                  br(),
                                )
                                
                                # # temporary Input well panel
                                # wellPanel(
                                #   h4(i18n$t("Study Characteristics"), style ="font-weight: bold;"),
                                #   
                                #   h4("Einfache Suche", style ="font-weight: bold;"),
                                # 
                                #   #span(paste0(i18n$t("Study Type"),":"), style ="font-weight: bold;"), textOutput("txt_studyTypeSimple", inline = TRUE),
                                #   selectInput("inputStudyType", i18n$t("Study Type"),
                                #               choices = c(NA, berenis_cats$Studientyp_simple[!is.na(berenis_cats$Studientyp_simple)]),
                                #               selected = getDetailsText(filteredData(), "cat_studyTypeSimple")),
                                # 
                                #   selectInput("inputFreqRangeSimple", i18n$t("Frequency Range"),
                                #               choices = c(NA, berenis_cats$Frequenzbereich_simple[!is.na(berenis_cats$Frequenzbereich_simple)]),
                                #               selected = getDetailsText(filteredData(), "cat_freqRangeSimple")),
                                #   
                                #   h4("Detaillierte Suche", style ="font-weight: bold;"), 
                                #   
                                #   selectInput("inputFreqRange", i18n$t("Frequency Range"),
                                #               choices = c(NA, berenis_cats$Frequenzbereich[!is.na(berenis_cats$Frequenzbereich)]),
                                #               selected = getDetailsText(filteredData(), "cat_freqRange")),
                                #   
                                #   selectInput("inputSourceType", i18n$t("Source"),
                                #               choices = c(NA, berenis_cats$Quelle[!is.na(berenis_cats$Quelle)]),
                                #               selected = getDetailsText(filteredData(), "cat_source")),
                                #   
                                #   selectInput("inputStudyObject", i18n$t("Study Object"),
                                #               choices = c(NA, berenis_cats$Untersuchungsobjekt[!is.na(berenis_cats$Untersuchungsobjekt)]),
                                #               selected = getDetailsText(filteredData(), "cat_studyObject")),
                                #   
                                #   selectInput("inputTargetDimension", i18n$t("Target Dimension"),
                                #               choices = c(NA, berenis_cats$Zielgroesse[!is.na(berenis_cats$Zielgroesse)]),
                                #               selected = getDetailsText(filteredData(), "cat_targetDimension")),
                                #   
                                #   selectInput("inputDuration", i18n$t("Exposition Duration"),
                                #               choices = c(NA, berenis_cats$Wirkungsdauer[!is.na(berenis_cats$Wirkungsdauer)]),
                                #               selected = getDetailsText(filteredData(), "cat_duration")),
                                #   
                                #   actionButton("saveData", "Speichern")
                                # 
                                # )
                         )
                       )
                       
              ), target = "dataTab", position = "after"
    )
    
    # select the details Tab
    updateTabsetPanel(session, "inTabset", selected = "detailsTab")
    # detailsStatus$result <- "on"
  })
  
  
  
  # # -------- ** Event - Save data --------
  # 
  # observeEvent(input$saveData, {
  #   
  #   id <- getDetailsText(filteredData(), "ID")
  #   
  #   # overwrite selected categories in the data_frame
  #   berenis_updated$df$cat_studyTypeSimple[id] <- input$inputStudyType
  #   berenis_updated$df$cat_freqRangeSimple[id] <- input$inputFreqRangeSimple
  #   
  #   berenis_updated$df$cat_freqRange[id] <- input$inputFreqRange
  #   berenis_updated$df$cat_source[id] <- input$inputSourceType
  #   berenis_updated$df$cat_studyObject[id] <- input$inputStudyObject
  #   berenis_updated$df$cat_targetDimension[id] <- input$inputTargetDimension
  #   berenis_updated$df$cat_duration[id] <- input$inputDuration
  #   
  #   write_excel_csv(berenis_updated$df, file = paste0("data/output/BERENIS_data_categorized_", format(Sys.time(), "%Y-%m-%d_%H%M%S") ,".csv"), delim = ";")
  #   
  #   # Write data to file
  #   #openxlsx::write.xlsx(berenis_updated$df[,-2], file = "data/BERENIS_data_Ver2.xlsx", sheetName = "")
  # })
  
  
  # -------- ** Event - Select "Studies-Tab" --------
  
  # remove Details-Tab when the Studies-Tab is selected
  observeEvent(input$inTabset, {
    if(input$inTabset ==  "dataTab") {
      removeTab(inputId = "inTabset", target = "detailsTab")
    }
    # detailsStatus$result <- "off"
  })
  

# listenToSimpleSearch <- reactive({
#   list(input$pickerFreqRangeSimple,
#        input$pickerStudyTypeSimple)
# })
# 
# listenToDetailedSearch <- reactive({
#   list(input$pickerDuration,
#        input$pickerTargetDim,
#        input$pickerNLissue,
#        input$sliderYear,
#        input$pickerFreqRange,
#        input$pickerSource,
#        input$pickerStudyType,
#        input$pickerStudyObject)
# })
# 


  
# observeEvent(input$pickerFreqRangeSimple, {  
# #observeEvent(listenToSimpleSearch(), {
#   berenis2 <- berenis[berenis$cat_freqRangeSimple %in% input$pickerFreqRangeSimple, ]
#   
#   disabled_choices <- !rownames(berenis) %in% rownames(berenis2)
# 
#   updatePickerInput(session = session, "pickerFreqRangeSimple",
#                     selected = input$pickerFreqRangeSimple,
#                     choices = berenis_cats$Frequenzbereich_simple[!is.na(berenis_cats$Frequenzbereich_simple)],
#                     choicesOpt = list(
#                       disabled = disabled_choices,
#                       style = ifelse(disabled_choices,
#                                      yes = "color: rgba(119, 119, 119, 0.5);",
#                                      no = ""),
#                       content = choicesWithCount(filteredData(), "cat_freqRangeSimple", berenis_cats$Frequenzbereich_simple[!is.na(berenis_cats$Frequenzbereich_simple)], TRUE)))
# 
#   # updatePickerInput(session = session, "pickerStudyTypeSimple",
#   #                   choices = berenis_cats$Studientyp_simple[!is.na(berenis_cats$Studientyp_simple)],
#   #                   choicesOpt = list(content = choicesWithCount(filteredData(), "cat_studyTypeSimple", berenis_cats$Studientyp_simple[!is.na(berenis_cats$Studientyp_simple)], TRUE)))
# })
  
  
  # 
  
  
  # reset all pickerInputs, when "searching option" changes from "simple" to "detailed" or vice versa
  observeEvent(input$swDetailed, {
    updatePickerInput(session = session, "pickerFreqRangeSimple", selected = character(0))
    updatePickerInput(session = session, "pickerStudyTypeSimple", selected = character(0))
    updatePickerInput(session = session, "pickerNLissue", selected = character(0))
    updatePickerInput(session = session, "pickerNLissue_detailed", selected = character(0))
    updateSliderInput(session = session, "sliderYear", value = 0)
    updatePickerInput(session = session, "pickerFreqRange", selected = character(0))
    updatePickerInput(session = session, "pickerSource", selected = character(0))
    updatePickerInput(session = session, "pickerStudyType", selected = character(0))
    updatePickerInput(session = session, "pickerStudyObject", selected = character(0))
    updatePickerInput(session = session, "pickerDuration", selected = character(0))
    updatePickerInput(session = session, "pickerTargetDim", selected = character(0))
  })
  

  ########## OUTPUTs ########## 
  
  # -------- ** OUTPUT - Explorer Tab -------- 
  
  # render Explorer Tab
  output$explorerTab <- renderUI({
  
    #if (minimizeSidebar() == FALSE) {
    if (minimizeSidebar() == 2) {
     
      tagList(
        
        sidebarLayout(
          
          # ---- **** Sidebar Filter Panel (maximized) ----
          
          sidebarPanel(width = 3,
                       
                       # sidebar minimize button
                       actionButton(
                         inputId = "toggleSidebar", 
                         label = "", 
                         icon = icon("minus"), 
                         style = 'position:relative; left:-10px; top:-14px; display:inline-block;'
                       ),
                       
                       # Sidebar Header
                       p(i18n$t("Data Filter"), style ="font-weight:bold; font-size:22px; position:relative; top:-10px; display:inline-block;"),
                       
                       
                       fluidRow(
                         
                         # Detailed search switch - Version 1
                         # switchInput(
                         #   inputId = "swDetailed",
                         #   label = i18n$t("Detailed Search"),
                         #   value = FALSE,
                         #   onLabel = i18n$t("ON"),
                         #   offLabel = i18n$t("OFF"),
                         #   inline = FALSE,
                         #   size = 'small',
                         #   onStatus = 'info',
                         #   offStatus = 'danger',
                         #   labelWidth = '160px',
                         #   handleWidth  = '80px',
                         #   width = "240px"
                         # )
                         
                         # Detailed search switch - Version 2
                         materialSwitch(
                           inputId = "swDetailed",
                           label = i18n$t("Detailed Search"),
                           status = "success",
                           right = F
                         )
                       ),
                       
                       
                       
                       #  -------- ****** Sidebar - Simple Filter Panel --------
                       
                       # if detailed search is off ....
                       conditionalPanel(
                         condition = "input.swDetailed == false",
                         
                         # ... add Newsletter issue filter
                         pickerInput(inputId = "pickerNLissue", 
                                     label = i18n$t("Newsletter Issue"),
                                     choices = berenis %>% dplyr::arrange(desc(NL_date)) %>% dplyr::select(NL_issue) %>% unique() %>% pull(NL_issue),
                                     # add the date of the newsletter issue to labels in the item-list
                                     choicesOpt = list(
                                       content = (berenis %>% 
                                                    dplyr::select(NL_issue, NL_date, NL_year, NL_month) %>% 
                                                    arrange(desc(NL_date)) %>% 
                                                    add_count(NL_date, name = "n") %>% # add number of studies per NL
                                                    unique() %>% 
                                                    mutate(date_lab = paste(monthByLang[monthToNumber(NL_month), input$langPicker], NL_year)) %>%
                                                    mutate(lab = paste0(NL_issue, " (", date_lab, ") ", " <span class='badge customBadge'>", n ,"</span>")))$lab
                                     ),
                                     # choicesOpt = list(
                                     #   content = (berenis %>% dplyr::select(NL_issue, NL_date) %>% arrange(NL_date) %>% unique() %>%
                                     #                                mutate(lab = paste0(NL_issue, " (", format(NL_date, "%b %Y"), ")")))$lab),
                                     #choicesOpt = list(content = choicesWithCount(berenis, "NL_issue", sort(unique(berenis$NL_issue)), TRUE)),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         ),
                         
                         # ... add simple study type filter
                         pickerInput("pickerStudyTypeSimple", 
                                     label = i18n$t("Study type"),
                                     # choices = berenis_cats$Studientyp_simple[!is.na(berenis_cats$Studientyp_simple)],
                                     choices =  berenis_catsDict %>% filter(Kategorie == "Studientyp_einfach") %>% dplyr::select("DE") %>% pull("DE"),
                                     choicesOpt = list(
                                       content = choicesWithCount_V2(
                                         data = berenis,
                                         cat = "cat_studyTypeSimple",
                                         choices = berenis_catsDict %>% filter(Kategorie == "Studientyp_einfach") %>% dplyr::select("DE") %>% pull("DE"),
                                         labels = berenis_catsDict %>% filter(Kategorie == "Studientyp_einfach") %>% dplyr::select(input$langPicker) %>% pull(input$langPicker), # labels in the active language
                                         rm.na = TRUE
                                       )
                                     ),
                                     # choicesOpt = list(
                                     #   content = choicesWithCount_V2(berenis, "cat_studyTypeSimple", berenis_cats$Studientyp_simple[!is.na(berenis_cats$Studientyp_simple)], rm.na=TRUE)),
                                     multiple = TRUE,
                                     options = pickerOptions(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         ),
                         
                         # ... add simple frequency range filter
                         pickerInput("pickerFreqRangeSimple",
                                     label = i18n$t("Frequency range"),
                                     #choices = sort(unique(as.character(berenis$cat_freqRangeSimple))),
                                     # choices = berenis_cats$Frequenzbereich_simple[!is.na(berenis_cats$Frequenzbereich_simple)],
                                     choices =  berenis_catsDict %>% filter(Kategorie == "Frequenzbereich_einfach") %>% dplyr::select("DE") %>% pull("DE"),
                                     choicesOpt = list(
                                       content = choicesWithCount_V2(
                                         data = berenis,
                                         cat = "cat_freqRangeSimple",
                                         choices = berenis_catsDict %>% filter(Kategorie == "Frequenzbereich_einfach") %>% dplyr::select("DE") %>% pull("DE"),
                                         labels = berenis_catsDict %>% filter(Kategorie == "Frequenzbereich_einfach") %>% dplyr::select(input$langPicker) %>% pull(input$langPicker), # labels in the active language
                                         rm.na = TRUE
                                       )
                                     ),
                                     # choicesOpt = list(
                                     #   content = choicesWithCount(berenis, "cat_freqRangeSimple", berenis_cats$Frequenzbereich_simple[!is.na(berenis_cats$Frequenzbereich_simple)], rm.na=TRUE)),
                                     #style = rep(("color: #20c997;"), length(berenis_cats$Frequenzbereich_simple[!is.na(berenis_cats$Frequenzbereich_simple)]))),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         )
                         
                       ), # end conditional panel
                       
                       
                       # -------- ****** Sidebar - Detailed Filter Panel --------
                       
                       # if detailed switch is on ...
                       conditionalPanel(
                         condition = "input.swDetailed == true",
                         
                         #setSliderColor(c("#7A4053"), c(1)),
                         chooseSliderSkin("Flat", color = "#112446"),
                         
                         # ... add slider for Year selection
                         sliderInput(inputId = "sliderYear", 
                                     label = i18n$t("Study publication year"),
                                     min = min(berenis$OA_year),
                                     max = max(berenis$OA_year),
                                     value = c(min(berenis$OA_year), max(berenis$OA_year)),
                                     step = 1,
                                     sep = ""
                         ),

                         # ... add Newsletter number filter
                         pickerInput(inputId = "pickerNLissue_detailed", 
                                     label = i18n$t("Newsletter issue (detailed)"),
                                     choices = berenis %>% dplyr::arrange(desc(NL_date)) %>% dplyr::select(NL_issue) %>% unique() %>% pull(NL_issue),
                                     # add the date of the newsletter issue to labels in the item-list
                                     choicesOpt = list(
                                       content = berenis %>% 
                                                    dplyr::select(NL_issue, NL_date, NL_year, NL_month) %>% 
                                                    add_count(NL_date, name = "n") %>% # add number of studies per NL
                                                    unique() %>% 
                                                    arrange(desc(NL_date)) %>% 
                                                    mutate(date_lab = paste(monthByLang[monthToNumber(NL_month), input$langPicker], NL_year)) %>%
                                                    mutate(lab = paste0(NL_issue, " (", date_lab, ") ", " <span class='badge customBadge'>", n ,"</span>")) %>% 
                                                    pull(lab)
                                     ),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         ),
                         
                         # ... add study type filter
                         pickerInput(inputId = "pickerStudyType",
                                     label = i18n$t("Study type (detailed)"),
                                     #choices = berenis_cats$Studientyp[!is.na(berenis_cats$Studientyp)], 
                                     choices =  berenis_catsDict %>% filter(Kategorie == "Studientyp") %>% dplyr::select("DE") %>% pull("DE"),
                                     choicesOpt = list(
                                       content = choicesWithCount_V2(
                                         data = berenis,
                                         cat = "cat_studyType",
                                         choices = berenis_catsDict %>% filter(Kategorie == "Studientyp") %>% dplyr::select("DE") %>% pull("DE"),
                                         labels = berenis_catsDict %>% filter(Kategorie == "Studientyp") %>% dplyr::select(input$langPicker) %>% pull(input$langPicker), # labels in the active language
                                         rm.na = TRUE
                                       )
                                     ),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         ),
                         
                         # ... add detailed frequency range filter
                         pickerInput(inputId = "pickerFreqRange", 
                                     label = i18n$t("Frequency range (detailed)"),
                                     #choices = sort(unique(as.character(berenis$cat_freqRange))),
                                     #choices = berenis_cats$Frequenzbereich[!is.na(berenis_cats$Frequenzbereich)],
                                     choices =  berenis_catsDict %>% filter(Kategorie == "Frequenzbereich") %>% dplyr::select("DE") %>% pull("DE"),
                                     choicesOpt = list(
                                       content = choicesWithCount_V2(
                                         data = berenis,
                                         cat = "cat_freqRange",
                                         choices = berenis_catsDict %>% filter(Kategorie == "Frequenzbereich") %>% dplyr::select("DE") %>% pull("DE"),
                                         labels = berenis_catsDict %>% filter(Kategorie == "Frequenzbereich") %>% dplyr::select(input$langPicker) %>% pull(input$langPicker), # labels in the active language
                                         rm.na = TRUE
                                       )
                                     ),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         ),
                         
                         # ... add detailed exposition source filter
                         pickerInput(inputId = "pickerSource",
                                     # label = translate(activeLang(), "Exposition source (detailed)", "Quelle (detailliert)"), 
                                     label = i18n$t("Source (detailed)"), 
                                     #choices = berenis_cats$Quelle[!is.na(berenis_cats$Quelle)], 
                                     choices =  berenis_catsDict %>% filter(Kategorie == "Quelle") %>% dplyr::select("DE") %>% pull("DE"),
                                     choicesOpt = list(
                                       content = choicesWithCount_V2(
                                         data = berenis,
                                         cat = "cat_source",
                                         choices = berenis_catsDict %>% filter(Kategorie == "Quelle") %>% dplyr::select("DE") %>% pull("DE"),
                                         labels = berenis_catsDict %>% filter(Kategorie == "Quelle") %>% dplyr::select(input$langPicker) %>% pull(input$langPicker), # labels in the active language
                                         rm.na = TRUE
                                       )
                                     ),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         ),
                         
                         # ... add Study Object filter
                         pickerInput(inputId = "pickerStudyObject",
                                     label = i18n$t("Study object (detailed)"),
                                     #choices = berenis_cats$Untersuchungsobjekt[!is.na(berenis_cats$Untersuchungsobjekt)],
                                     choices =  berenis_catsDict %>% filter(Kategorie == "Untersuchungsobjekt") %>% dplyr::select("DE") %>% pull("DE"),
                                     choicesOpt = list(
                                       content = choicesWithCount_V2(
                                         data = berenis,
                                         cat = "cat_studyObject",
                                         choices = berenis_catsDict %>% filter(Kategorie == "Untersuchungsobjekt") %>% dplyr::select("DE") %>% pull("DE"),
                                         labels = berenis_catsDict %>% filter(Kategorie == "Untersuchungsobjekt") %>% dplyr::select(input$langPicker) %>% pull(input$langPicker), # labels in the active language
                                         rm.na = TRUE
                                       )
                                     ),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         ),
                         
                         # ... add Target Dimension filter
                         pickerInput(inputId = "pickerDuration",
                                     label = i18n$t("Period of action (detailed)"), 
                                     #choices = berenis_cats$Wirkungsdauer[!is.na(berenis_cats$Wirkungsdauer)],
                                     choices =  berenis_catsDict %>% filter(Kategorie == "Wirkungsdauer") %>% dplyr::select("DE") %>% pull("DE"),
                                     choicesOpt = list(
                                       content = choicesWithCount_V2(
                                         data = berenis,
                                         cat = "cat_duration",
                                         choices = berenis_catsDict %>% filter(Kategorie == "Wirkungsdauer") %>% dplyr::select("DE") %>% pull("DE"),
                                         labels = berenis_catsDict %>% filter(Kategorie == "Wirkungsdauer") %>% dplyr::select(input$langPicker) %>% pull(input$langPicker), # labels in the active language
                                         rm.na = TRUE
                                       )
                                     ),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         ),
                         
                         # ... add Target Dimension filter
                         pickerInput(inputId = "pickerTargetDim", 
                                     label = i18n$t("Target size (detailed)"),
                                     #choices = berenis_cats$Zielgroesse[!is.na(berenis_cats$Zielgroesse)],
                                     choices =  berenis_catsDict %>% filter(Kategorie == "Zielgroesse") %>% dplyr::select("DE") %>% pull("DE"),
                                     choicesOpt = list(
                                       content = choicesWithCount_V2(
                                         data = berenis,
                                         cat = "cat_targetDimension",
                                         choices = berenis_catsDict %>% filter(Kategorie == "Zielgroesse") %>% dplyr::select("DE") %>% pull("DE"),
                                         labels = berenis_catsDict %>% filter(Kategorie == "Zielgroesse") %>% dplyr::select(input$langPicker) %>% pull(input$langPicker),
                                         rm.na = TRUE
                                         )
                                       ),
                                       # content = berenis_catsDict %>% filter(Kategorie == "Zielgroesse") %>% dplyr::select(input$langPicker) %>% t() %>% as.vector()),
                                     multiple = TRUE,
                                     options = list(
                                       size = 10,
                                       `actions-box` = TRUE,
                                       `live-search` = TRUE,
                                       `deselect-all-text` = i18n$t("Deselect all"),
                                       `select-all-text` = i18n$t("Select all"),
                                       `none-selected-text` = i18n$t("Nothing selected")
                                     )
                         )
                         
                       ) # END - conditional panel
          ), # END - SidebarPanel
          
          
          
          # -------- **** Main Panel - Studies Tab panel (--> Data Explorer) --------
          mainPanel(width = 9, 
                    
                    tabsetPanel(id = "inTabset",
                                
                                tabPanel(title = i18n$t("Studies"),
                                         value = "dataTab",
                                         icon = icon("file-medical-alt"),
                                         
                                         # show data table
                                         DT::dataTableOutput("table")
                                )
                    )
                    
                    # the Details-Tab is rendered in the "server" part of the App
          )
          
        ) # END - SidebarLayout
      ) # END - tagList
      
    } else {
      
      tagList(

        # ---- **** Sidebar Filter Panel (minimized) ----
        wellPanel(style = "height:50px;",
                  
                  actionButton(
                    inputId = "toggleSidebar", 
                    label = "", 
                    icon = icon("plus"), 
                    style='position:relative; left:-10px; top:-14px; display: inline-block;'
                    ),
                  
                  # p("Datenfilter anzeigen", style='position:relative; top:-38px; left:35px')
                  p(i18n$t("Data Filter"), style ="font-weight: bold; font-size: 22px; position:relative; top:-10px; display: inline-block;"),
                  
        ),
        
        # -------- **** Main Panel - Studies Tab panel (--> Data Explorer) --------
        column(width = 12, 
               
               tabsetPanel(id = "inTabset",
                           
                           tabPanel(title = i18n$t("Studies"),
                                    value = "dataTab",
                                    icon = icon("file-medical-alt"),
                                    
                                    # show data table
                                    DT::dataTableOutput("table")
                           )
               )
        )
        # the Details-Tab is rendered in the "server" part of the App
      )
    }
  })
  
  
  # -------- ** OUTPUT - BERENIS Logo -------- 
  
  # BERENIS Logo
  output$BERENIS_logo <- renderImage({list(src = "data/BERENIS_logo.png")}, deleteFile = FALSE)
  
  
  # -------- ** OUTPUT - data table -------- 
  
  output$table <- DT::renderDataTable(dt())
    
    # server = FALSE, # test download buttons
    # expr = 
  
  
  # build data table as a reactive variable 
  dt <- reactive(
      
    # create the dataTable object
    DT::datatable(filteredData() %>% 
                    # only select some columns for the datatable 
                    dplyr::select(OA_authors, Authors_EtAl, OA_year, OA_title, details,
                                  DE_title, DE_summary, FR_title, FR_summary, EN_title, EN_summary, 
                                  cat_freqRangeSimple, cat_source,	cat_studyTypeSimple, cat_studyObject, cat_duration, cat_targetDimension, 
                                  NL_issue, NL_date, NL_date_label, 
                                  title_byLang, NL_DL_link),  # %>% rename(!!details_text:=details)), # use !! and := to inject variable as text
                  escape = FALSE, # needed in order to transform URLs into hyperlinks
                  #filter = 'top', # add filters above each column
                  extensions = c('RowGroup', 'ColReorder', 'Scroller', "FixedHeader"),
                  selection = 'single',  # args for ColRecorder
                  colnames = c("Author(s)", i18n$t("Author(s)"), i18n$t("Year"), i18n$t("Study title (original)"), i18n$t("Details"),
                               "DE_title", "DE_summary", "FR_title", "FR_summary", "EN_title", "EN_summary",
                               "Frequency Range", "Exposition Source", "Study Type","Study object", "Duration", "Target Dimension", 
                               i18n$t("Newsletter Issue"), "Newsletter Date", i18n$t("Newsletter Date"), 
                               i18n$t("Title in the newsletter"), i18n$t("Download newsletter")),
                  rownames = FALSE,
                  options = list(language = list(search = i18n$t('Search (title and text):')),
                                 #filter = list('float' = "left", 'text-align' = "left"),
                                 #rowGroup = list(dataSrc = 4), # args for RowGroup
                                 scrollX = TRUE,
                                 searchHighlight = TRUE,
                                 order = list(list(17, 'desc')),
                                 # fixedHeader = TRUE, # the header is unfortunately fixed behind the navbar --> needs more investigation 
                                 colReorder = TRUE, # args for ColRecorder
                                 bPaginate = FALSE, # arg for filter
                                 list(deferRender = TRUE, # args for Scroller
                                      scrollY = 200,
                                      scroller = TRUE),
                                 columnDefs = list(
                                   list(orderData = 18, targets = c(17,19)), # how to order the table
                                   list(visible = FALSE, targets = c(0,4,5:16,18)), # hide some columns
                                   #list(render = JS('$.fn.dataTable.render.ellipsis( 17, true )'), targets = c(1:3)), # a try to show a tooltip
                                   list(className = 'dt-center', targets = c(4,17,19,21))), # center text in columns
                                 # # Tooltip for the data table
                                 rowCallback = JS("function(nRow, aData, iDisplayIndex, iDisplayIndexFull ) {",
                                                  "var full_text = ", i18n$t("'Study details (click)'"), # Tooltip text
                                                  # show tooltip only in the first 5 data table columns
                                                  "$('td:eq(0), td:eq(1), td:eq(2), td:eq(3), td:eq(4), td:eq(5)', nRow).attr('data-tooltip', full_text);",
                                                  #"$('td', nRow).attr('data-tooltip', full_text);",
                                                  #"$('td:eq(3)', nRow).css('cursor', 'help');",
                                                  "}"
                                 )
                                 # # for the NL download buttons
                                 #   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                 #   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
                  )
    )
    
  )
  
  # output$table <- DT::renderDataTable(
  #   
  #   # server = FALSE, # test download buttons
  #   # expr = 
  #     {
  #   
  #   # import data into local variable
  #   data <- filteredData() %>% 
  #     # only select some columns for the datatable 
  #     select(details, OA_authors, Authors_EtAl, OA_year, OA_title,
  #            DE_title, DE_summary, FR_title, FR_summary, EN_title, EN_summary, 
  #            cat_freqRangeSimple, cat_source,	cat_studyTypeSimple, cat_studyObject, cat_duration, cat_targetDimension, 
  #            NL_issue, NL_date, NL_date_label, 
  #            title_byLang, NL_DL_link) 
  # 
  #   # create the dataTable object
  #   dt <- DT::datatable(data, # %>% rename(!!details_text:=details)), # use !! and := to inject variable as text
  #                 escape = FALSE, # needed in order to transform URLs into hyperlinks
  #                 #filter = 'top', # add filters above each column
  #                 extensions = c('RowGroup', 'ColReorder', 'Scroller'),
  #                 selection = 'none',  # args for ColRecorder
  #                 colnames=c(i18n$t("Details"), "Author(s)", i18n$t("Author(s)"), i18n$t("Year"), i18n$t("Title"), 
  #                            "DE_title", "DE_summary", "FR_title", "FR_summary", "EN_title", "EN_summary",
  #                            "Frequency Range", "Exposition Source", "Study Type","Study Object", "Duration", "Target Dimension", 
  #                            i18n$t("Newsletter Issue"), "Newsletter Date", i18n$t("Newsletter Date"), 
  #                            i18n$t("Newsletter Study Title"), i18n$t("Newsletter Download")),
  #                 rownames = FALSE,
  #                 options = list(language = list(search = i18n$t('Search Newsletters:')),
  #                                #filter = list('float' = "left", 'text-align' = "left"),
  #                                #rowGroup = list(dataSrc = 4), # args for RowGroup
  #                                order = list(list(17, 'desc')),
  #                                colReorder = TRUE, # args for ColRecorder
  #                                bPaginate = FALSE, # arg for filter
  #                                list(deferRender = TRUE, # args for Scroller
  #                                     scrollY = 200,
  #                                     scroller = TRUE),
  #                                columnDefs = list(
  #                                  list(targets = c(1,5:16,18), visible = FALSE),
  #                                  list(className = 'dt-center', targets = c(0,21)))
  #                                # # for the NL download buttons
  #                                #   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
  #                                #   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
  #                                )
  #                 )
  #   
  #   
  #       
  #   return(dt)
  #   
  # })
  # 
  
  # -------- ** OUTPUT - Text -------- 
  
  # rendering of text outputs for the Details tab   
  output$OA_title <- renderText({ getDetailsText(filteredData(), "OA_title") })
  output$OA_authors <- renderText({ getDetailsText(filteredData(), "OA_authors") })
  output$OA_year <- renderText({ getDetailsText(filteredData(), "OA_year") })
  output$OA_journal <- renderText({ getDetailsText(filteredData(), "OA_journal") })
  output$OA_articleUrl <- renderUI({ tagList(a(href=getDetailsText(filteredData(), "OA_articleUrl"), getDetailsText(filteredData(), "OA_articleUrl"))) })
  
  output$NL_studySummary <- renderText({ getDetailsText(filteredData(), "summary_byLang") })
  output$NL_studyTitle <- renderText({ getDetailsText(filteredData(), "title_byLang") })
  output$NL_issue <- renderText({ getDetailsText(filteredData(), "NL_issue") })
  output$NL_date_label <- renderText({ getDetailsText(filteredData(), "NL_date_label") })
  output$NL_DL_link <- renderText({ getDetailsText(filteredData(), "NL_DL_link") })

  # output$txt_source <- renderText({ as.character(getDetailsText(filteredData(), "cat_source")) })
  output$txt_source <- renderText({ translate_categories(berenis_catsDict, "Quelle", as.character(getDetailsText(filteredData(), "cat_source")), input$langPicker) })
  output$txt_studyType <- renderText({ translate_categories(berenis_catsDict, "Studientyp", as.character(getDetailsText(filteredData(), "cat_studyType")), input$langPicker) })
  output$txt_studyTypeSimple <- renderText({ translate_categories(berenis_catsDict, "Studientyp_einfach", as.character(getDetailsText(filteredData(), "cat_studyTypeSimple")), input$langPicker) })
  output$txt_freqRange <- renderText({ translate_categories(berenis_catsDict, "Frequenzbereich",  as.character(getDetailsText(filteredData(), "cat_freqRange")), input$langPicker) })
  output$txt_freqRangeSimple <- renderText({ translate_categories(berenis_catsDict, "Frequenzbereich_einfach", as.character(getDetailsText(filteredData(), "cat_freqRangeSimple")), input$langPicker) })
  output$txt_studyObject <- renderText({ translate_categories(berenis_catsDict, "Untersuchungsobjekt", as.character(getDetailsText(filteredData(), "cat_studyObject")), input$langPicker) })
  output$txt_duration <- renderText({ translate_categories(berenis_catsDict, "Wirkungsdauer", as.character(getDetailsText(filteredData(), "cat_duration")), input$langPicker) })
  output$txt_targetDimension <- renderText({ translate_categories(berenis_catsDict, "Zielgroesse", as.character(getDetailsText(filteredData(), "cat_targetDimension")), input$langPicker) })


  # -------- ** OUTPUT - Diagrams -------- 
  
  # -------- **** Organigram - with highchart  -------- 
  output$Organigram <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = 'organization', inverted = TRUE, height = 800) %>%
      hc_title(text = 'BERENIS Organigram', margin = 20, style = list(fontSize = "x-large")) %>%
      hc_add_series(
        name = 'BERENIS',
        data = list(
          list(from = 'BAFU', to = 'BERENIS'),
          list(from = 'BAFU', to = 'Beobachtung'),
          list(from = 'BAFU', to = 'RAE'),
          list(from = 'BAFU', to = 'RIM'),
          list(from = 'BERENIS', to = 'MR'),
          list(from = 'BERENIS', to = 'PA'),
          list(from = 'BERENIS', to = 'JF'),
          list(from = 'BERENIS', to = 'JK'),
          list(from = 'BERENIS', to = 'MM'),
          list(from = 'BERENIS', to = 'DS'),
          list(from = 'BERENIS', to = 'EdS'),
          list(from = 'BERENIS', to = 'SD'),
          list(from = 'Beobachtung', to = 'EvS'),
          list(from = 'Beobachtung', to = 'RK'),
          list(from = 'Beobachtung', to = 'KM'),
          list(from = 'Beobachtung', to = 'SI')
        ),
        levels = list(
          list(level = 0, dataLabels = list(color = 'White'), height = 100),
          list(level = 1, dataLabels = list(color = 'white'), height = 40),
          list(level = 2, dataLabels = list(color = 'black'), height = 25)
          #list(level = 3, height = 100),
        ),
        nodes = list(
          list(id = 'BAFU', title = 'Auftraggeberin', name = 'BAFU', color = '#2b4252', info ='Eidgenössisches Department für Umwelt, <br>Verkehr, Energie und Kommunikation (UVEK), <br>Bundesamt für Umwelt(BAFU)'),
          #list(id = 'NIS', name = 'Sektion NIS', color = '#fcc657'),
          list(id = 'RAE', title = 'Leitung Sektion NIS', name = 'Alexander Reichenbach', column = 1, color = '#4a708a', offset = '50%', info ='Eidgenössisches Department für Umwelt, <br>Verkehr, Energie und Kommunikation (UVEK), <br>Bundesamt für Umwelt(BAFU), <br>Abteilung Lärm & NIS, <br>Sektion NIS', image = 'https://www.bafu.admin.ch/bafu/de/home/amt/abteilungen-sektionen/abteilung-laerm-und-nis/sektion-nichtionisierende-strahlung--nis-/_jcr_content/par/textimage/image.imagespooler.jpg/1540023296781/180.1000/alexander-reichenbach-bafu.png'),
          list(id = 'RIM', title = 'Projektleitung', name = 'Dr. Maurane Riesen', column = 2, color = '#4a708a', offset = '50%', info ='Eidgenössisches Department für Umwelt, <br>Verkehr, Energie und Kommunikation (UVEK), <br>Bundesamt für Umwelt(BAFU), <br>Abteilung Lärm & NIS, <br>Sektion NIS', image = 'https://www.gr.be.ch/etc/designs/gr/media.cdwsbinary.acq/efdb7b0af64e475789ab7804bd472395-1664/1/Original/Riesen_Maurane.jpg'), # color = '#007ad0', column = 3, offset = '75%'),
          # BERENIS tree
          list(id = 'BERENIS', title = 'Beratende Expertengruppe', name = 'BERENIS', color = '#277060', column = 3, layout = 'hanging', info = 'Beratende Expert*innengruppe (BERENIS)'),
          list(id = 'MR', title = 'Leitung der Expertengruppe', name = 'Prof. Dr. Martin Röösli', color = '#41c0a4', info = 'Schweizerisches Tropen- und Public Health-Institute, <br>Basel', image = 'https://www.swisstph.ch/fileadmin/user_upload/SwissTPH/TIPPPs/Persons/PER3182_PHOTO.jpg'),
          list(id = 'PA', name = 'Prof. Dr. Peter Achermann', color = '#41c0a4', info = 'The KEY Institute for Brain-Mind Research, <br>Zürich', image = 'https://www.pharma.uzh.ch/contacts/pharma/Gruppe-Achermann/pachermann/photo/Peter%20Achermann%202018%20Webseite.jpg.jpg'),
          list(id = 'JF', name = 'Dr. Jürg Fröhlich', color = '#41c0a4', info = 'Fields at Work GmbH, <br>Zürich', image = 'https://www.piomic.com/wp-content/uploads/2020/04/team_ju%CC%88rg-fro%CC%88lich-320x320.jpg'),
          list(id = 'JK', name = 'Prof. Dr. med. Jürg Kesselring', color = '#41c0a4', info = 'ehemaliger Chefarzt Neurologie und Neurorehabilitation, Rehabilitationszentrum, <br>Valens', image = 'https://www.usz.ch/app/uploads/2021/02/Kesselring.jpg'),
          list(id = 'MM', name = 'Prof. Dr. Meike Mevissen', color = '#41c0a4', info = 'Universität Bern, <br>Direktorin der Abteilung Veterinär-Pharmakologie & Toxikologie'),
          list(id = 'DS', name = 'Dr. David Schürmann', color = '#41c0a4', info = 'Molekulare Genetik Gruppe, Departement Biomedizin, <br>Universität Basel'),
          list(id = 'EdS',name = 'Dr. med. Edith Steiner', color = '#41c0a4', info = 'Ärztinnen und Ärzte für Umweltschutz, <br>Basel'),
          list(id = 'SD', title = 'Organisation', name = 'Dr. Stefan Dongus', color = '#41c0a4', info = 'Schweizerisches Tropen- und Public Health-Institute, <br>Basel', image = 'https://www.swisstph.ch/fileadmin/user_upload/SwissTPH/TIPPPs/Persons/PER4066_PHOTO.jpg'),
          # Beobachtung tree
          list(id = 'Beobachtung', titel = 'Beobachtung', name = 'Beobachtung', color= "#662a4b", layout = 'hanging', column = 3, info = 'Beobachtung'),
          list(id = 'EvS', title = 'BAG', name = 'Dr. Evelyn Stempfel', color= "#b36690", layout = 'hanging', info = 'Bundesamt für Gesundheit (BAG), Sektionsleiterin nichtionisierende Strahlung und Dosimetrie'),
          list(id = 'RK', title = 'Suva', name = 'Dr. Roland Krischek', color= "#b36690", layout = 'hanging', info = 'Schweizerische Unfallversicherungsanstalt (Suva)'),
          list(id = 'KM', title = 'SECO', name = 'Dr. Christian Monn', color= "#b36690", layout = 'hanging', info = 'Staatssekretariat für Wirtschaft'),
          list(id = 'SI', title = 'SECO', name = 'Dr. Samuel Iff', color= "#b36690", layout = 'hanging', info = 'Staatssekretariat für Wirtschaft')
        ),
        colorByPoint = FALSE,
        dataLabels = list(color = 'white'),
        borderColor = 'white',
        #nodeWidth = 45,
        nodePadding = 50
      ) %>%
      hc_tooltip(
        useHTML = TRUE, 
        headerFormat = "<b>{point.key}</b>",
        formatter = JS("function() {
                      return '<b>' + this.point.name + '</b><br>' + this.point.info;
                      }"
        ),
        outside = TRUE) %>%
      hc_add_theme(ber)
  })
  
  
  # -------- **** NL statistics - with highchart  -------- 
  
  output$NLstatistics <- renderHighchart({
  
    subtitle_txt <- ""
    
    highchart() %>%
      hc_chart(inverted = FALSE) %>%
      hc_title(text = paste0("<b>", i18n$t("BERENIS screening statistics"), "</b>"),
               style = list(fontSize = "x-large")) %>%
      # hc_subtitle(text = paste0("Übersicht über die Anzahl publizierter wissenschaftlicher Studien zu möglichen Auswirkungen von NIS, den von BERENIS diskutierten Studien und den im Newsletter veröffentlichten Studien. " , # (Beurteilungszeitraum: Mar 2015 - ",
      #                           format(min(berenis_stats$NL_published, na.rm=T), "%b %Y"), " - ",
      #                           format(max(berenis_stats$NL_published, na.rm=T), "%b %Y"), ")")) %>%
      hc_subtitle(text = "(NL = Newsletter)") %>%
      hc_add_series(data = berenis_stats, type = "column", hcaes(x = paste0("NL ", NL_issue, " (", NLpub_label, ")"), y = studies_identified), 
                    color="#FDBB2C", name = i18n$t("Total studies screened by BERENIS"), pointWidth = 7,
                    tooltip = list(pointFormat = paste0(i18n$t("Screening Interval"), ": {point.start_label} - {point.end_label}<br><span style='color:{point.color}'>\u25CF</span> {series.name}: <b>{point.y}</b> <br>"))) %>%
      hc_add_series(data = berenis_stats, type = "column", hcaes(x = paste0("NL ", NL_issue, " (", NLpub_label, ")"), y = studies_discussed),
                    color="#2BB0D7", name = i18n$t("Studies discussed in depth by BERENIS"), pointWidth = 7) %>%
      hc_add_series(data = berenis_stats, type = "column", hcaes(x = paste0("NL ", NL_issue, " (", NLpub_label, ")"), y = studies_selected + studies_additional),
                    color="#EF4C6A", name = i18n$t("Studies presented in BERENIS Newsletter"), pointWidth = 7) %>%
      hc_xAxis(type = "category") %>%
      hc_legend(enabled = FALSE) %>%
      #hc_legend(verticalAlign = "top") %>%
      hc_tooltip(shared = TRUE,
                 headerFormat = "<b>{point.key}</b><br>") %>%
      hc_add_theme(ber)
  })
  
  
  # -------- **** Screening statistics - with highchart  -------- 

  output$NLcum <- renderHighchart({
    
    highchart() %>% 
      hc_title(text = paste0("<b>", i18n$t("BERENIS Newsletter timeline"), "</b>"),
               style = list(fontSize = "x-large")) %>%
      # hc_xAxis(type ="datetime", labels = list(formatter = JS("function() {return Highcharts.dateFormat('%Y', this.value);}")),
      #          tickInterval = 1000 * 60 * 60 * 24 * 365) %>%
      # hc_subtitle(text = paste0("Mar 2015 - ",
      #                           #format(min(berenis_stats$screening_start, na.rm=T), "%b %Y"), " - ",
      #                           format(max(berenis_stats$NL_published, na.rm=T), "%b %Y"), ": ",
      #                           "<b>", max(berenis_stats$csum_ident), "</b> Studies identified   |   ",
      #                           "<b>", max(berenis_stats$csum_disc), "</b> Studies discussed   |   ",
      #                           "<b>", max(berenis_stats$csum_select), "</b> Studies presented")) %>%
      hc_add_series(type = "area", data = berenis_stats, hcaes(x = NL_published_hc, y = csum_ident), 
                    color = "#FDBB2C", marker = list(enabled = FALSE), name = i18n$t("Total studies screened by BERENIS")) %>%
      hc_add_series(type = "area", data = berenis_stats, hcaes(x = NL_published_hc, y = csum_disc), 
                    color = "#2BB0D7", marker = list(enabled = FALSE), name = i18n$t("Studies discussed in depth by BERENIS")) %>%
      hc_add_series(type = "area", data = berenis_stats, hcaes(x = NL_published_hc, y = csum_select), 
                    color = "#EF4C6A", marker = list(enabled = FALSE), name = i18n$t("Studies presented in BERENIS Newsletter")) %>%
      hc_xAxis(type ="datetime", dateTimeLabelFormats = list(year = '%Y'),
               tickInterval = 1000 * 60 * 60 * 24 * 365) %>%
      hc_yAxis(title = list(text = "Studies")) %>%
      hc_tooltip(crosshairs = TRUE) %>%
      hc_legend(verticalAlign = "top") %>%
      #hc_legend(enabled = FALSE) %>%
      hc_plotOptions(area = list(fillOpacity = 0.30)) %>%
      hc_add_theme(ber)
    
  })

  # -------- **** Impressum Map with location of SwissTPH  -------- 
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%  
  #     addMarkers(lng=7.57753, lat=47.55636, popup="BERENIS") %>% 
  #     setView(lng=7.57753, lat=47.55636, zoom = 14)
  # })
  
  
  # language settings for datatable see: https://rstudio.github.io/DT/004-i18n.html

  
  # ---- for later use --- highcharts map of switzerland (see https://www.datacamp.com/community/tutorials/data-visualization-highcharter-r)
  # hcmap("https://code.highcharts.com/mapdata/countries/ch/ch-all.js")%>%
  #   hc_title(text = "Switzerland")
  # 
}