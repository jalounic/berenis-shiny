#**************************
#---- Libs & Functions ----
#**************************


# .libPaths("C:\\Users\\Z70ASEG\\AppData\\Local\\R\\win-library\\4.4")


# import packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(readxl)
library(highcharter)
library(htmltools)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shiny.i18n) # see https://github.com/Appsilon/shiny.i18n
library(mailtoR)
library(leaflet)
library(httr)
library(gtools)
library(data.table)
#library(shinybrowser)
#library(datacleanr)
#library(bslib)
#library(shiny.semantic)
#library(rsconnect)
# thematic::thematic_shiny(font = "auto")




# function for creating links from URLs in string format
createLink <- function(url, buttonTxt, class = "downloadButton") {
  if (is.na(url)) {
    return("")
  } else {
    #tagList(href = url, target="_blank", "Download")
    #paste0("<a href=",'"', url,'"', " class='downloadButton' >", buttonTxt ,"</a>")
    paste0("<a href=",'"', url,'"',
           " title=",'"', url, '"', 
           " class=", '"', class, '"',
           "><span style='font-size:15px;' class='glyphicon glyphicon-download-alt'></span>", buttonTxt, "</a>")
  }
}


# # function to insert the language dropdown on the right side of the navigation bar
# navbarPageWithInputs <- function(..., inputs) {
#   navbar <- navbarPage(...)
#   form <- tags$form(
#     class = "navbar-nav ml-auto navbar-right",
#     # navbar navbar-expand-lg
#     #class = "navbar-nav",
#     #class = "navbar-form",
#     id = "berenisNavbar",
#     align = "right",
#     inputs)
#   navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form)
#   return(navbar)
# }




# function to abbreviate an author list with " et al." 
abbreviate_authors <- function(authors){
  
  # split authors by ","
  authors_list <- str_split(authors, ",")
  # count authors
  count_author <- map_int(.x = authors_list, length)
  # create abbreviated version of authors list with "et al." for all authors
  first_author <- map_chr(.x = authors_list, 1)
  # replace authors lists with >2 authors with "et al." 
  authors[which(count_author > 2)] <- paste(first_author[which(count_author > 2)] , "et al.")

  return(authors)
  
}


# translate <- function(activeLang, entxt, detxt=NULL, frtxt=NULL) {
#   if (activeLang == "EN") {
#     return(entxt)
#     
#   } else if (activeLang == "DE") {
#     if (is.NULL(detxt)) {return(entxt)} else {return(detxt)}
#     
#   } else if (activeLang == "FR") {
#     if (is.NULL(frtxt)) {return(entxt)} else {return(frtxt)}
#     
#   } else {
#     return(NULL)
#   }
# }


monthByLang <- data.frame(
  "DE" = c("Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"),
  "EN" = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  "FR" = c("janv.", "févr.", "mars", "avr.", "mai", "juin", "juill.", "août", "sept.", "oct.", "nov.", "déc.")
)

# create text labels from the date variables: "Mon Year" --> does not work with www.shinyapps.io
dateByLanguage <- function(dates, activeLang, format = "%b %Y") { 
  
  if (activeLang == "DE") {
    Sys.setlocale(category = "LC_TIME", locale = "German")
  } else if (activeLang == "FR") {
    Sys.setlocale(category = "LC_TIME", locale = "French")
  } else if (activeLang == "EN") {
    Sys.setlocale(category = "LC_TIME", locale = "English")
  }
  
  # format dates corresponding to input language 
  dates <- format(dates, format = format)
  
  # reset sys time to default
  Sys.setlocale("LC_TIME","")

  return(dates)
}

monthToNumber <- function(mnth = TRUE) {
  dplyr::case_when(
    mnth == "Januar" ~ 1,
    mnth == "Febuar" ~ 2,
    mnth == "März" ~ 3,
    mnth == "April" ~ 4,
    mnth == "Mai" ~ 5,
    mnth == "Juni" ~ 6,
    mnth == "Juli" ~ 7,
    mnth == "August" ~ 8,
    mnth == "September" ~ 9,
    mnth == "Oktober" ~ 10,
    mnth == "November" ~ 11,
    mnth == "Dezember" ~ 12,
    mnth == TRUE ~ 0)
}


# ---- add counts to items in a list of choices -----
choicesWithCount <- function(data, var, choices, labels=NULL, rm.na=FALSE) {
  
  # labels is optional; if "labels" has not been parsed than use choices as labels
  if (is.null(labels)) {labels <- choices}
  
  # count instances by variable
  cnt <- count(data, get(var))
  
  # remove NA-counting from the data --> usually the last row 
  if (rm.na == TRUE) {
    cnt <- cnt[complete.cases(cnt),]
  }
  
  # make sure that the factor levels are consistent between cnt and choices
  cnt <- arrange(cnt, factor(cnt[,1], levels = choices))
  
  # paste count to each choice  
  res <- paste0(labels, " [", cnt$n, "]")
  
  return(res)
}

# ---- add counts to items in a list of choices -----

#choices <- berenis_catsDict %>% filter(Kategorie == "Zielgroesse") %>% select("DE") %>% t() %>% as.vector()

choicesWithCount_V2 <- function(data, cat, choices, labels = NULL, rm.na = FALSE, int_as_str = FALSE) {
  
  # detect parentheses and add \\ before, in order to make str_detect work
  choices <- gsub("([()])","\\\\\\1", choices) # detects "(" and ")"
  choices <- gsub("\\[", "\\\\\\[", choices)   # detects "["
  choices <- gsub("\\]", "\\\\\\]", choices)   # detects "]"
  
  # transform integer to string value
  if (int_as_str == TRUE) {
    choices <- sapply(choices, function(i) {paste0("(?<![:space:]|[:alnum:])",i,"(?![:space:]|[:alnum:])")} )
  }
  # detect for each category whether it is present in the choices-vector
  res <- sapply(data[,cat], str_detect, choices)
  
  # assign row names
  rownames(res) <- choices
  
  # count hits within 
  cnt <- rowSums(res, na.rm = rm.na)
  
  # labels is optional; if "labels" has not been parsed than use choices as labels
  if (is.null(labels)) {labels <- choices}
  
  # paste count to each choice  
  # res <- paste0(labels, " [", cnt, "]")
  res <- paste0(labels, " <span class='badge customBadge'>", cnt, "</span>")
  
  return(res)

}

########

# ---------- custom highcharter theme ------------------
# ---- color scale:   #611530,#890144,#F28705,#D9B68B,#038C7F,#330918
ber <- hc_theme(#colors = c('red','green','blue'),
  chart = list(backgroundColor = "#ecf0f1"),
  title = list(style = list(fontFamily = "Lato")),
  subtitle = list(style = list(#color ='#666666',
    fontFamily = "Lato")),
  legend = list(itemStyle = list(fontFamily ='Lato'),
                itemHoverStyle = list(color ='gray')))
# ******************************************************************



#**************
#---- Main ----
#**************

# --- for debugging ---
# setwd("E:\\CODING\\BAFU_BERENIS_APP")
# --- debugging end ---


# set excel file with the data
berenis_datafile <- "data/BERENIS_data_for_shiny_app.xlsx"

# # import dictionaries with translations
i18n <- Translator$new(translation_json_path = "data/Dictionaries/translation.json")
i18n$set_translation_language("EN") # select the default translation to display


# ---- LOAD BERENIS DATA ---- 
# berenis <- as.data.frame(read_excel(path = berenis_datafile, sheet = 1, na = c("", "NA"))) # DATA
berenis <- read.csv("https://lite.framacalc.org/berenis-a1ho.csv", encoding = "UTF-8") # DATA from Framacalc
# berenis <- berenis[complete.cases(berenis[,1:32]),] # temporary - removes incomplete rows 
berenis_catsDict <- as.data.frame(read_excel(path = berenis_datafile, sheet = 2, na = "NA")) # CATEGORIES
# berenis_stats <- as.data.frame(read_excel(path = "data/BERENIS_data_Ver2.xlsx", sheet = 3, na = "NA")) # STATS --> not wanted by the group :-(



# # build a data frame that holds the languages and corresponding flags
# flags <- data.frame(lang=c("EN", "DE", "FR"))
# flags$img = c(sprintf("<img src='flags/gb.png' width=30px><span class='jhr'>%s</span></img>", flags$lang[1]),
#               sprintf("<img src='flags/de.png' width=30px><span class='jhr'>%s</span></img>", flags$lang[2]),
#               sprintf("<img src='flags/fr.png' width=30px><span class='jhr'>%s</span></img>", flags$lang[3])
# )


# # explore new data from SwissTPH
# berenis_new <- tibble(read_excel(path = "data/BERENIS_data_for_shiny_app.xlsx", sheet = 1, na = ""))
# all_kats <- berenis %>% dplyr::select(Kat_Studientyp:Kat_Zielgrösse) %>% lapply(., unique)
# 
# # ---- manual Replacements ----
# # ", " & "," --> "/" oder "|"
# # z.B. und (...) ???
# 
# all_kats <- all_kats %>%
#   # Zielgrösse
#   # map( .f = str_replace_all, pattern = "Nicht bestimmt", replacement = "andere") %>%
#   # map( .f = str_replace_all, pattern = "nicht bestimmt", replacement = "andere") %>%
#   map( .f = str_replace_all, pattern = "Studien", replacement = "Studie") %>% # Studien --> Studie
#   map( .f = str_replace_all, pattern = "studien", replacement = "studie") %>% # studien --> studie
#   map( .f = str_replace_all, pattern = "unspezifisch", replacement = "Unspezifisch") %>% # Unspezifisch --> unspezifisch
#   # map( .f = str_replace_all, pattern = "/", replacement = " | ") %>% # "/" -->  "|"
#   # map( .f = str_replace_all, pattern = "Ratten,Mäuse", replacement = "Ratten | Mäuse") %>% # "/" -->  "|"
#   map( .f = str_replace_na, replacement = "NA") %>% map( .f = str_replace_all, pattern = "^NA$", replacement = "Andere") # "/" -->  "|"
# 
# all_kats
# 
# # find unique values
# unique_kats <- all_kats %>% map( .f = str_split, pattern =";") %>% map(.f = unlist) %>% map(.f = str_trim) %>% map( .f = unique) %>% map( .f = sort)

# # convert into data frame and write to Excel
# xx <- lapply(unique_kats, unlist)
# max <- max(sapply(xx, length))
# unique_kats_df <- do.call(cbind, lapply(xx, function(z)c(z, rep(NA, max-length(z)))))
# writexl::write_xlsx(as.data.frame(unique_kats_df), "data/BERENIS_kats.xlsx")




# overwrite German column names
names(berenis) <- c(
  # grau [1]
  "ID",
  # hellblau [8]
  "NL_issue", "NL_year", "NL_month", "screening_start", "screening_end", "studies_identified", "studies_discussed", "studies_selected", "studies_additional",
  # blau [6]
  "DE_title", "DE_summary", "FR_title", "FR_summary", "EN_title", "EN_summary",
  # dunkelblau [3]
  "DE_url", "FR_url", "EN_url",
  # gelb [6]
  "OA_year", "OA_authors", "OA_title", "OA_journal", "OA_pubinfo", "OA_articleUrl",
  # hellgrün [2]
  "cat_studyTypeSimple", "cat_freqRangeSimple",
  # dunkelgrün [6]
  "cat_freqRange", "cat_source", "cat_studyType", "cat_studyObject", "cat_duration", "cat_targetDimension", "addInfos"
  )




# ---- BERENIS Dataframe ----

# filter incomplete entries --> Is this okay with BERENIS?
berenis <- berenis[berenis %>% dplyr::select(DE_title:EN_url) %>% complete.cases(), ]

# # factorize the categorical columns, in order to use them for filtering the data table
# berenis[, 25:32] <- lapply(berenis[, 25:32], as.character)

# create et al. version of the authors, i.e. "<primary author> et al.
berenis$Authors_EtAl <- abbreviate_authors(berenis$OA_authors)

# add date label in German
berenis$NL_date <- ISOdate(berenis$NL_year, monthToNumber(berenis$NL_month), 1)
berenis$NL_date_label <- paste(monthByLang[monthToNumber(berenis$NL_month), "DE"], berenis$NL_year) # use German month abrev.



# ---- Calculate BERENIS-Stats Dataframe ----

berenis_stats <- berenis %>% dplyr::select(NL_issue:studies_additional) %>% filter(!duplicated.data.frame(.)) %>% 
  mutate(NL_published = as.POSIXct(paste0("01 ", NL_month, NL_year), format='%d %B %Y'))

# create text labels from the date variables: "Mon Year"
berenis_stats$start_label <- format(berenis_stats$screening_start, format = "%b %Y")
berenis_stats$end_label <- format(berenis_stats$screening_end, format = "%b %Y")
berenis_stats$NLpub_label <- format(berenis_stats$NL_published, format = "%b %Y")

# sort by publication date
berenis_stats <- arrange(berenis_stats, NL_published)

# convert date into highchart-format
berenis_stats$NL_published_hc <- datetime_to_timestamp(berenis_stats$NL_published)

# # convert NAs in the study counters to 0 
# berenis_stats[,c(6:9)][is.na(berenis_stats[,c(6:9)])] <- 0

# remove entries with NA values in the study-statistics
berenis_stats <- berenis_stats[berenis_stats %>% 
                                 dplyr::select(NL_issue:studies_additional) %>% 
                                 complete.cases(), ]

# calculate cumulative sums for chart
berenis_stats <- mutate(berenis_stats, 
                        csum_ident = cumsum(studies_identified), 
                        csum_disc = cumsum(studies_discussed),
                        csum_select = cumsum(studies_selected + studies_additional))



# # calculate median and number of observations
# rf_data_meds <- ddply(rf_data, .(get(myvar), Umgebung), summarize, 
#                       med = median(Total, na.rm=T),
#                       mean = mean(Total, na.rm=T),
#                       min = min(Total, na.rm=T),
#                       max = max(Total, na.rm=T),
#                       n = length(Total))
# names(rf_data_meds)[names(rf_data_meds)=="get(myvar)"] <- myvar



#sts <- head(reshape2::melt(data=berenis_stats, id=1:5, variable.name = "studies_cat", value.name = "studies_nr"))


# tot <- summarize(berenis_stats, 
#           studies_identified = sum(studies_identified, na.rm = TRUE),
#           studies_discussed = sum(studies_discussed, na.rm = TRUE),
#           studies_selected = sum(studies_selected, na.rm = TRUE))
# 
# 
# highchart() %>% 
#   hc_chart(plotBackgroundColor = NULL,
#            plotBackgroundImage = NULL,
#            plotBorderWidth = 0,
#            plotShadow = FALSE) %>%
#   hc_title(text = paste0("<b>", tot$studies_identified, "</b> studies screened in total")) %>%
#   hc_pane(#center = c('50%', '80%'), size = '140%',
#           startAngle = -90, endAngle = 90, 
#           background = list(#backgroundColor = list("Highcharts.defaultOptions.legend.backgroundColor || '#ffffff'"),
#                             innerRadius = '60%',
#                             outerRadius = '100%',
#                             shape = 'arc')) %>% 
#   hc_add_series(data = tot$studies_identified, type = "solidgauge", dataLabels = format('<div style="text-align:center"><span style="font-size:25px">{point.y}</span><br/><span style="font-size:12px;opacity:0.4">km/h</span></div>'))  %>%
#   hc_yAxis(min = 0, max = max(tot), lineWidth = 0, tickWidth = 0, minorTickInterval = 0, labels = 0, series = list(color = "#611530", data = data)) %>%
#   hc_tooltip(enabled = FALSE)
#   #hc_plotOptions(solidgauge = list(dataLabels = list(y=5, borderWidth = 0, useHTML = TRUE))) %>%