#**************************
#---- Libs & Functions ----
#**************************

# import packages
library(shiny) # yes
library(shinyWidgets) #yes
library(shinythemes) #yes
library(DT) # yes
library(readxl) #yes
library(highcharter) #yes
library(tidyverse) # i'll allow it here
library(shinyjs) #yes
library(shiny.i18n) # yes, see https://github.com/Appsilon/shiny.i18n
library(mailtoR)

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

# # import dictionaries with translations
i18n <- Translator$new(translation_json_path = "data/Dictionaries/translation.json")
i18n$set_translation_language("EN") # select the default translation to display

berenis_datafile <- "data/BERENIS_data.xlsx"
# ---- LOAD BERENIS DATA ---- 
berenis <- as.data.frame(read_excel(path = berenis_datafile, sheet = 1, na = c("", "NA"))) # DATA
#berenis <- read.csv("https://lite.framacalc.org/berenis-a1ho.csv", encoding = "UTF-8") # DATA from Framacalc
berenis_catsDict <- as.data.frame(read_excel(path = berenis_datafile, sheet = 2, na = "NA")) # CATEGORIES

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
