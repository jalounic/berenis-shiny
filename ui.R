#**************************
#---- Libs & Functions ----
#**************************

# *********************************
# ----------- SHINY UI ------------
# *********************************

#ui <- fluidPage(theme = "Quartz.css",
ui <- 
  # bootstrapPage('', 
  fluidPage(
  
  title = "BERENIS",
  theme = shinytheme("flatly"),
  # theme = bs_theme(version = 4, bootswatch = "flatly"),
  
  
  # activate translation
  shiny.i18n::usei18n(i18n),
  
  # use shinydashboard elements within regular shiny app
  useShinydashboard(),
  useShinyjs(),
  
  # ---- CSS style modifications ----              
  tags$head(tags$style(HTML("
                           /* add a margin to the top NavBar to avoid overlapping */
                           body {
                              padding-top: 80px;
                              padding-bottom: 60px;
                           }
                           .customBadge {
                              background: #95A5A6;
                              position: relative;
                              top: -2px;
                              right: -2px;
                              font-weight: normal;
                           }
                           #berenisNavbar {
                              height: 55px;
                              margin-bottom: 0px;
                              margin-top: 0px;
                           }
                           /* change font color of active tab in the navBar */
                           .navbar-nav > .active > a {
                              color: #20c997 !important;
                           }
                           /* position of the flag-icons in the language-pickerInput 
                           .jhr {
                              display: inline;
                              vertical-align: middle;
                              padding-left: 10px;
                           } */
                           /* position search Box of datatable to top left */
                           #DataTables_Table_0_filter {
                              float: left !important;
                              text-align: left !important;
                           }
                           .table.dataTable tbody th, table.dataTable tbody td {
                              padding: 10px 10px; /* e.g. change 8x to 4px here */
                           }
                           /* style to imitate a download button */
                           .downloadButton {
                              color: white;
                              background: #112446;
                              padding: 10px 13px;
                              border-radius: 5px;
                              text-align: center;
                           }
                           /* style to imitate a download button */
                           .downloadButton:hover {
                              color: #20c997;
                              background: #112446;
                              padding: 11px 14px;
                              border-radius: 5px;
                              text-align: center;
                           }
                           /* style to imitate a details button */
                           .detailsButton {
                              color: white;
                              border: none;
                              background: #112446;
                              padding: 10px 13px;
                              border-radius: 5px;
                           }
                           .detailsButton:hover {
                              color: #20c997;
                              border: none;
                              background: #112446;
                              padding: 11px 14px;
                              border-radius: 5px;
                           }
                           #toggleSidebar {
                              color: white;
                              border: none;
                              background: #112446;
                              width: 30px;
                              padding: 5px;
                              font-size: 85%;
                           }
                           #toggleSidebar:hover {
                              color: #20c997;
                              border: none;
                              background: #112446;
                              width: 31px;
                              padding: 6px;
                              font-size: 85%;
                           }
                           #langPicker {
                              align:right; 
                              position:relative; 
                              top:-8px; 
                              margin-top:0px;
                              margin-bottom:0px;
                           }
                           table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                              background-color: #fffadc !important;
                           }
                           .pickerInput {
                              background-color: #112446;
                           }
                           /* https://stackoverflow.com/questions/35025145/background-color-of-tabs-in-shiny-tabpanel */
                           .tabbable > .nav > li[class=active] > a {
                              background-color: #112446; 
                              color: #20c997;
                           }
                           .tabbable > .nav > li > a {
                              background-color: #95a5a6;
                              color: white;
                           }
                           /* center the Organigram on the tabPanel */
                           #Organigram {
                              width: 60%;
                              display: block;
                              margin-left: auto;
                              margin-right: auto;
                              }
                           /* change font color of the filter-pickerInput-placeholers */
                           .bs-placeholder {
                              color: #b1b5b5 !important;
                           }
                           /* switchInput color while on  */
                           .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-info,
                           .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-info {
                              background: #20c997 !important; 
                              color: #112446 !important;
                           }
                            /* switchInput color while off  */
                           .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,
                           .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger {
                              background: #112446 !important;
                              color: #20c997 !important;
                           }
                           /* Alternative switch color */
                           .material-switch label:after {
                              background: #112446 !important;
                           }
                           /* TOOLTIP - see https://freefrontend.com/css-tooltips/ 
                                        and https://codepen.io/reiinii1/pen/aPGXEa  */
                          [data-tooltip] {
                            position: relative;
                            z-index: 10;
                          }
                          
                          /* Positioning and visibility settings of the tooltip */
                          [data-tooltip]:before,
                          [data-tooltip]:after {
                            position: absolute;
                            visibility: hidden;
                            opacity: 0;
                            left: 50%;
                            bottom: calc(100% + 5px); /* 5px is the size of the arrow */
                            pointer-events: none;
                            transition: 0.2s;
                            will-change: transform;
                          }
                          
                          /* The actual tooltip with a dynamic width */
                          [data-tooltip]:before {
                            content: attr(data-tooltip);
                            padding: 10px 18px;
                            min-width: 50px;
                            max-width: 300px;
                            width: max-content;
                            width: -moz-max-content;
                            border-radius: 6px;
                            font-size: 14px;
                            background-color: rgba(17, 36, 70, 0.8);
                            background-image: linear-gradient(30deg,
                              rgba(59, 72, 80, 0.44),
                              rgba(59, 68, 75, 0.44),
                              rgba(60, 82, 88, 0.44));
                            box-shadow: 0px 0px 24px rgba(0, 0, 0, 0.2);
                            color: #fff;
                            text-align: center;
                            white-space: pre-wrap;
                            transform: translate(-50%, -5px) scale(0.5);
                          }
                          
                          /* Tooltip arrow */
                          [data-tooltip]:after {
                            content: '';
                            border-style: solid;
                            border-width: 5px 5px 0px 5px; /* CSS triangle */
                            border-color: rgba(55, 64, 70, 0.9) transparent transparent transparent;
                            transition-duration: 0s; /* If the mouse leaves the element, 
                                                        the transition effects for the 
                                                        tooltip arrow are 'turned off' */
                            transform-origin: top;   /* Orientation setting for the
                                                        slide-down effect */
                            transform: translateX(-50%) scaleY(0);
                          }
                          
                          /* Tooltip becomes visible at hover */
                          [data-tooltip]:hover:before,
                          [data-tooltip]:hover:after {
                            visibility: visible;
                            opacity: 1;
                          }
                          /* Scales from 0.5 to 1 -> grow effect */
                          [data-tooltip]:hover:before {
                            transition-delay: 0.3s;
                            transform: translate(-50%, -5px) scale(1);
                          }
                          /* 
                            Arrow slide down effect only on mouseenter (NOT on mouseleave)
                          */
                          [data-tooltip]:hover:after {
                            transition-delay: 0.5s; /* Starting after the grow effect */
                            transition-duration: 0.2s;
                            transform: translateX(-50%) scaleY(1);
                          }
                          /* BOTTOM */
                          [data-tooltip-location='bottom']:before,
                          [data-tooltip-location='bottom']:after {
                            top: calc(100% + 5px);
                            bottom: auto;
                          }
                          
                          [data-tooltip-location='bottom']:before {
                            transform: translate(-50%, 5px) scale(0.5);
                          }
                          [data-tooltip-location='bottom']:hover:before {
                            transform: translate(-50%, 5px) scale(1);
                          }
                          
                          [data-tooltip-location='bottom']:after {
                            border-width: 0px 5px 5px 5px;
                            border-color: transparent transparent rgba(55, 64, 70, 0.9) transparent;
                            transform-origin: bottom;
                          }
                            "
                           )
                       )
  ),

  # *******************************----
  # ---- NavBar Menu - Title Panel ----  
  
  # navbarPageWithInputs(
  navbarPage(
    # id = "navbar",
    id = "berenisNavbar",
    
    # BERENIS Logo
    title = img(id = "BERENIS_img", src = "BERENIS_logo_weiss.png", style = "position:relative; top:-15px; cursor:pointer;"),
    windowTitle = "BERENIS Newsletter Explorer",
    
    # fixate the navBar at the top
    position = c("fixed-top"),
    collapsible = TRUE,
    
    
    # *******************************************----
    # -------- Navbar Menu - Explorer Tab --------
    
    tabPanel(title = i18n$t("Explorer"), value = "idExplorer", icon = icon("binoculars"), 
             
             # the explorer Tab is rendered in the "server"-file 
             uiOutput("explorerTab")
             
    ),
    
    # *******************************************----
    # -------- Navbar Menu - About BERENIS --------
    
    navbarMenu(i18n$t("About BERENIS"), #icon = icon("question"),
               
               # -------- ** Menu Tab Panel - Background, Goals & Tasks --------
               tabPanel(i18n$t("Background, Tasks and Members"),
                        
                        
                        # ---- TEMPORARY ---- language switcher
                        
                        # English
                        conditionalPanel(
                          condition = "input.langPicker == 'EN'",
                          
                          wellPanel(
                            h3("Background", style ="font-weight: bold;"),
                            "The FOEN has nominated a consultative group of Swiss experts from various disciplines with scientific competence regarding electromagnetic fields and NIR, which has commenced its work in July 2014. The group is called BERENIS, based on the acronym of the respective German term. The BERENIS experts regularly screen the scientific literature, and assess the publications which they consider relevant for the protection of humans from potentially adverse effects."
                          ),
                          
                          wellPanel(
                            h3("The Swiss expert group on electromagnetic fields and non-ionising radiation (BERENIS)", style ="font-weight: bold;"),
                            tags$div("In Switzerland, the Federal Office for the Environment (FOEN) is the responsible government body for monitoring and assessing research on health effects of non-ionising radiation (NIR) from stationary sources in the environment. This includes informing and updating the public about the current state of research, which is the basis for the ambient regulatory limits stated in the Swiss 'ordinance relating to protection from non-ionising radiation'. In the case of reliable new scientific knowledge and experiences, the FOEN would advise the Federal Council of Switzerland to adapt these ambient regulatory limits."),
                            tags$br(),
                            tags$div("Assessing the results and conclusions of scientific studies enables early detection of potential health risks of NIR. The FOEN places particular emphasis to not overlook any evidence of harmfulness for public health demanding for corrective regulatory interventions. Furthermore, critical assessment of available scientific data is required to make firm statements about the validity of the presented evidence for biological effects, their relevance for public health, and if so, to estimate the number of potentially affected people."),
                            tags$br(),
                            tags$div("NIR includes a broad spectrum of frequencies with varying intensities and radiation characteristics, which is becoming more complex and multifaceted as the development and application of technologies emitting them is very dynamic and rapidly increasing. In the context of the work of BERENIS, NIR at frequencies below 10 GHz is addressed. Potential biological effects and the underlying mechanisms of NIR are manifold, and research activities range from the molecular to the population level. As a consequence, specific expertise in various disciplines is needed to assess the related scientific studies rooted in many different biological, medical and technical scientific realms."),
                            tags$br(),
                            tags$div("The FOEN has therefore nominated a consultative group of Swiss experts from various disciplines with scientific competence regarding electromagnetic fields and NIR, which has commenced its work in July 2014. The group is called BERENIS, based on the acronym of the respective German term. The BERENIS experts regularly screen the scientific literature, and assess the publications which they consider relevant for the protection of humans from potentially adverse effects. The results of this evaluation are published in quarterly newsletters, which can be downloaded from this webpage."),
                          ),
                          
                          wellPanel(
                            h3("Members", style ="font-weight: bold;"),
                            tags$div(
                              tags$ul(
                                tags$li("Dr. David Schürmann, Molecular Genetics Group, Department of Biomedicine, University of Basel (lead)"),
                                tags$li("Prof. Dr. Meike Mevissen, Vet-Pharmacology & Toxicology, University of Bern"),
                                tags$li("Prof. Dr. Peter Achermann, The KEY Institute for Brain-Mind Research, Zurich"),
                                tags$li("Dr. Jürg Fröhlich, Fields at Work GmbH, Zurich"),
                                tags$li("Prof. Dr. med. Jürg Kesselring, previous head of the Department of Neurorehabilitation, Rehabilitation Centre, Clinics of Valens"),
                                tags$li("Dr. Marloes Eeftens, Swiss Tropical and Public Health Institute, Basel (lead)"),
                                tags$li("Dr. med. Cornel Wick, Ärztinnen und Ärzte für Umweltschutz, Basel")
                              ),
                              
                              tags$div(h4("Scientific secretariat"),
                                       tags$ul(
                                         tags$li("Dr. Stefan Dongus, Swiss Tropical and Public Health Institute, Basel")
                                       )
                              ),
                              
                              tags$div(h4("Mandant (Federal Office for the Environment)"),
                                       tags$ul(
                                         tags$li("Joseph Al Ahmar, Federal Office for the Environment (FOEN)"),
                                         tags$li("Gaëlle Bussard, Federal Office for the Environment (FOEN)")
                                       )
                              ),
                              
                              tags$div(h4("Observers"),
                                       tags$ul(
                                         tags$li("Dr. Evelyn Stempfel, Federal Office of Public Health (FOPH)"),
                                         tags$li("Dr. Roland Krischek, Swiss National Accident Insurance Fund (Suva)"),
                                         tags$li("Dr. Christian Monn, State Secretariat for Economic Affairs (SECO)"),
                                         tags$li("Dr. Samuel Iff, State Secretariat for Economic Affairs (SECO)")
                                       )
                              )
                              
                            )
                          )
                        ),
                        
                        # Francais
                        conditionalPanel(
                          condition = "input.langPicker == 'FR'",
                          
                          wellPanel(
                            h3("Contexte", style ="font-weight: bold;"),
                            "En 2014, l’OFEV a créé une structure de soutien en nommant un groupe consultatif d’experts en matière de RNI (BERENIS, acronyme de la désignation en allemand). Celui-ci examine les nouveaux travaux scientifiques relatifs à ce thème et choisit les études méritant à ses yeux une évaluation détaillée du point de vue de la protection des personnes."
                          ),
                          
                          wellPanel(
                            h3("Le groupe consultatif d'experts en matière de RNI (BERENIS) et ses tâches", style ="font-weight: bold;"),
                            tags$div("En tant que service fédéral compétent en matière d'environnement, l'OFEV a pour tâche de suivre l'évolution de la recherche en ce qui concerne les effets du rayonnement non ionisant (RNI) sur la santé, d'en évaluer les résultats et d'informer le public sur l'état des connaissances."),
                            tags$br(),
                            tags$div("Celui-ci constitue la base des valeurs limites d'immissions de l'ordonnance sur la protection contre le rayonnement non ionisant (ORNI). L'OFEV proposerait au Conseil fédéral une adaptation de celles-ci si de nouveaux résultats de la recherche scientifique ou l'expérience quotidienne l'exigeaient."),
                            tags$br(),
                            tags$div("L'évaluation des résultats d'études scientifiques sert également à la détection précoce des risques potentiels. Il faudra en effet veiller à repérer tout indice de nocivité qui appellerait une intervention. L'évaluation doit comporter des commentaires sur la pertinence avec laquelle les effets biologiques sont prouvés, préciser s'ils sont significatifs pour la santé et combien de personnes sont potentiellement concernées."),
                            tags$br(),
                            tags$div("Le RNI est un vaste thème englobant un large spectre de fréquences avec une diversité d'intensités et d'autres caractéristiques de rayonnement. À cela s'ajoute une évolution technologique dynamique, rendant les émissions de rayonnement plus variées et plus complexes. Et les systèmes biologiques pouvant potentiellement être influencés par le RNI sont tout aussi variés. En conséquence, l'évaluation des études, issues de différents domaines biologiques, médicaux et techniques spécifiques, nécessite de faire appel à des experts."),
                            tags$br(),
                            tags$div("En 2014, l'OFEV a créé une structure de soutien en nommant un groupe consultatif d'experts en matière de RNI (BERENIS, acronyme de la désignation en allemand). Celui-ci examine les nouveaux travaux scientifiques relatifs à ce thème et choisit les études méritant à ses yeux une évaluation détaillée du point de vue de la protection des personnes. Les résultats de l'évaluation sont publiés sous forme de newsletter trimestrielle sur cette page internet.")
                          ),
                          
                          wellPanel(
                            h3("Membres", style ="font-weight: bold;"),
                            tags$div(
                              tags$ul(
                                tags$li("Dr. David Schürmann, Groupe de génétique moléculaire, Département de biomédecine, Université de Bâle (direction)"),
                                tags$li("Prof. Dr. Meike Mevissen, unité de pharmacologie et de toxicologie vétérinaires, Université de Berne"),
                                tags$li("Prof. Dr. Peter Achermann, The KEY Institute for Brain-Mind Research, Zurich"),
                                tags$li("Dr. Jürg Fröhlich, Fields at Work GmbH, Zurich"),
                                tags$li("Prof. Dr. med. Jürg Kesselring, ancien médecin-chef de l'unité de neurologie et de neuroréhabilitation, Centre de réhabilitation, Valens"),
                                tags$li("Prof. Dr. Marloes Eeftens, Institut tropical et de santé publique suisse, Bâle"),
                                tags$li("Dr. med. Cornel Wick, Médecins en faveur de l'environnement, Bâle")
                              ),
                              
                              tags$div(h4("Secrétariat scientifique"),
                                       tags$ul(
                                         tags$li("Dr. Stefan Dongus, Institut tropical et de santé publique suisse, Bâle")
                                       )
                              ),
                              
                              tags$div(h4("Mandant (Office fédéral de l'environnement)"),
                                       tags$ul(
                                         tags$li("Joseph Al Ahmar, Office fédéral de l'environnement (OFEV)"),
                                         tags$li("Gaëlle Bussard, Office fédéral de l'environnement (OFEV)")
                                       )
                              ),
                              
                              tags$div(h4("Observateurs"),
                                       tags$ul(
                                         tags$li("Dr. Evelyn Stempfel, Office fédéral de la santé publique (OFSP)"),
                                         tags$li("Dr. Roland Krischek, Caisse nationale suisse d'assurance en cas d'accidents (Suva)"),
                                         tags$li("Dr. Christian Monn, Secrétariat d'État à l'économie (SECO)"),
                                         tags$li("Dr. Samuel Iff, Secrétariat d'État à l'économie (SECO)")
                                       )
                              )
                              
                            )
                          )
                        ),
                        
                        # Deutsch
                        conditionalPanel(
                          condition = "input.langPicker == 'DE'",
                          
                          # BERENIS explanation text --> could once being put into a separate "HOME" tab
                          wellPanel(
                            h3("Hintergrund", style ="font-weight: bold;"),
                            "Zur fachlichen Unterstützung hat das BAFU im Jahr 2014 eine Beratende Expertengruppe NIS (BERENIS) einberufen. Diese sichtet die neu publizierten wissenschaftlichen Arbeiten zum Thema und wählt diejenigen zur detaillierten Bewertung aus, die aus ihrer Sicht für den Schutz des Menschen von Bedeutung sind oder sein könnten."
                          ),
                          
                          wellPanel(
                            h3("Die Beratende Expertengruppe NIS (BERENIS) und ihre Aufgabe", style ="font-weight: bold;"),
                            tags$div("Das BAFU hat als Umweltfachstelle des Bundes die Aufgabe, die Forschung über gesundheitliche Auswirkungen nichtionisierender Strahlung (NIS) zu verfolgen, die Ergebnisse zu bewerten und die Öffentlichkeit über den Stand der Wissenschaft und der Erfahrung zu informieren."),
                            tags$br(),
                            tags$div("Dieser bildet die Grundlage für die Immissionsgrenzwerte der Verordnung über den Schutz vor nichtionisierender Strahlung (NISV). Das BAFU würde dem Bundesrat eine Anpassung dieser Grenzwerte empfehlen, wenn neue gesicherte Erkenntnisse aus der Forschung oder aufgrund von Alltagserfahrungen dies erforderten."),
                            tags$br(),
                            tags$div("Die Bewertung der Ergebnisse wissenschaftlicher Studien dient auch der Früherkennung potenzieller Risiken. Es soll möglichst kein Hinweis auf Schädlichkeit, der ein Handeln erfordern würde, übersehen werden. Die Bewertung muss Aussagen darüber machen, wie stichhaltig biologische Effekte nachgewiesen sind, ob sie für die Gesundheit relevant sind, und wie viele Menschen gegebenenfalls betroffen sind."),
                            tags$br(),
                            tags$div("NIS ist ein weites Feld, welches ein breites Frequenzspektrum mit einer Vielfalt an Intensitäten und anderen Strahlungscharakteristiken umfasst. Dazu kommt eine dynamische technologische Entwicklung, wodurch die Strahlungsemissionen vielfältiger und komplexer werden. Genauso mannigfaltig sind die biologischen Systeme, welche potentiell von NIS beeinflusst werden könnten. Entsprechend gibt es Studien aus vielen biologischen, medizinischen und technischen Spezialgebieten, für deren Bewertung detailliertes Expertenwissen erforderlich ist."),
                            tags$br(),
                            tags$div("Zur fachlichen Unterstützung hat das BAFU im Jahr 2014 eine Beratende Expertengruppe NIS (BERENIS) einberufen. Diese sichtet die neu publizierten wissenschaftlichen Arbeiten zum Thema und wählt diejenigen zur detaillierten Bewertung aus, die aus ihrer Sicht für den Schutz des Menschen von Bedeutung sind oder sein könnten. Die Ergebnisse der Evaluation werden vierteljährlich in Form eines Newsletters auf dieser Internetseite publiziert.")
                          ),
                          
                          wellPanel(
                            h3("Mitglieder", style ="font-weight: bold;"),
                            tags$div(
                              tags$ul(
                                tags$li("Dr. David Schürmann, Molekulare Genetik Gruppe, Departement Biomedizin, Universität Basel (Leitung)"),
                                tags$li("Prof. Dr. Meike Mevissen, Vet-Pharmakologie & Toxikologie, Universität Bern"),
                                tags$li("Prof. Dr. Peter Achermann, The KEY Institute for Brain-Mind Research, Zürich"),
                                tags$li("Dr. Jürg Fröhlich, Fields at Work GmbH, Zürich"),
                                tags$li("Prof. Dr. med. Jürg Kesselring, ehemaliger Chefarzt Neurologie und Neurorehabilitation, Rehabilitationszentrum, Valens"),
                                tags$li("Prof. Dr. Marloes Eeftens, Schweizerisches Tropen- und Public Health-Institute, Basel (Leitung)"),
                                tags$li("Dr. med. Cornel Wick, Ärztinnen und Ärzte für Umweltschutz, Basel")
                              ),
                              
                              tags$div(h4("Wissenschaftliches Sektretariat"),
                                       tags$ul(
                                         tags$li("Dr. Stefan Dongus, Schweizerisches Tropen- und Public Health-Institut, Basel")
                                       )
                              ),
                              
                              tags$div(h4("Auftraggeberin (Bundesamt für Umwelt)"),
                                       tags$ul(
                                         tags$li("Joseph Al Ahmar, Bundesamt für Umwelt (BAFU)"),
                                         tags$li("Gaëlle Bussard, Bundesamt für Umwelt (BAFU)")
                                       )
                              ),
                              
                              tags$div(h4("BeobachterInnen"),
                                       tags$ul(
                                         tags$li("Dr. Evelyn Stempfel, Bundesamt für Gesundheit (BAG)"),
                                         tags$li("Dr. Roland Krischek, Schweizerische Unfallversicherungsanstalt (Suva)"),
                                         tags$li("Dr. Christian Monn, Staatssekretariat für Wirtschaft (SECO)"),
                                         tags$li("Dr. Samuel Iff, Staatssekretariat für Wirtschaft (SECO)")
                                       )
                              )
                              
                            )
                          )
                        ),
                        
               ),
               
               
               # -------- ** Menu Tab Panel - Study Selection --------
               tabPanel(i18n$t("Study Selection"),
                        
                        fluidRow(
                          
                          # Englisch
                          conditionalPanel(
                            condition = "input.langPicker == 'EN'",
                            
                            wellPanel(
                              h3("Selection criteria for assessed publications", style ="font-weight: bold;"),
                              "Critical assessment of scientific studies is labour intensive and time consuming. BERENIS therefore does not attempt to discuss all newly published studies in detail. However, the most important and most relevant studies with regard to health risk assessment shall be identified and considered. Priority is thus given to studies that fulfil as many of the following criteria as possible:",
                              
                              h4("General"),
                              tags$ul(
                                tags$li("high scientific standards"),
                                tags$li("environmentally relevant exposures, such as NIR by infrastructure facilities"), 
                                tags$li("new or different scientific perspectives"),
                                tags$li("studies that are publicly or scientifically debated in a controversial manner")),
                              
                              h4("Epidemiological studies"),
                              tags$ul(
                                tags$li("studies that are publicly or scientifically debated in a controversial manner"),
                                tags$li("exposure in the range or below the ambient regulatory limits"), 
                                tags$li("results from Switzerland"),
                                tags$li("results that are applicable to the Swiss context")),
                              
                              h4("Experimental studies"),
                              tags$ul(
                                tags$li("findings that may be relevant for human health and well-being"),
                                tags$li("well-defined and controlled study conditions, including exposure setup"), 
                                tags$li("results that challenge previous findings or provide new insights"))
                            ),
                            
                            wellPanel(
                              h3("BERENIS Work Cycle", style ="font-weight: bold;"),
                              tags$img(src = "BERENIS_Work_Cycle.png", style = "max-width:1200px")
                            )
                          ),
                          
                          # Francais
                          conditionalPanel(
                            condition = "input.langPicker == 'FR'",
                            
                            wellPanel(
                              h3("Critères de sélection des études évaluées", style ="font-weight: bold;"),
                              "L'appréciation critique des études est chronophage ; il n'est pas possible d'analyser et de discuter en détail toutes les nouvelles études. C'est pourquoi seules les études les plus significatives en ce qui concerne l'estimation du risque seront prises en compte. En priorité on traitera celles répondant au plus grand nombre de critères présentés ci-après.",
                              
                              h4("Critères généraux"),
                              tags$ul(
                                tags$li("Bonne qualité scientifique"),
                                tags$li("Expositions significatives pour l'environnement, c'est-à-dire en premier lieu le RNI en provenance des infrastructures"), 
                                tags$li("Nouvelles perspectives scientifiques"),
                                tags$li("Études suscitant la controverse dans le domaine public ou scientifique")),
                              
                              h4("Critères pour les études épidémiologiques"),
                              tags$ul(
                                tags$li("Significatif pour la santé et le bien-être général des personnes"),
                                tags$li("Expositions au niveau des valeurs limites d'immissions ou en dessous"), 
                                tags$li("Résultats en provenance de la Suisse"),
                                tags$li("Résultats transposables à la Suisse")),
                              
                              h4("Critères pour les études expérimentales"),
                              tags$ul(
                                tags$li("Effet étudié significatif pour l'être humain"),
                                tags$li("Conditions d'études définies, exposition incluse"), 
                                tags$li("Effets non explicables par les mécanismes d'action admis"))
                            ),
                            
                            wellPanel(
                              h3("BERENIS processus de travail", style ="font-weight: bold;"),
                              tags$img(src = "BERENIS_Processus_de_travail.png", style = "max-width:1200px")
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.langPicker == 'DE'",
                            
                            wellPanel(
                              h3("Auswahlkriterien der bewerteten Studien", style ="font-weight: bold;"),
                              "Die kritische Beurteilung von Studien ist zeitaufwendig, und es ist nicht möglich, alle neu erscheinenden Studien im Detail zu analysieren und zu diskutieren. Es sollen daher die für die Risikoabschätzung relevantesten Studien berücksichtigt werden. Prioritär werden dabei Studien behandelt, die möglichst viele der folgenden Kriterien erfüllen:",
                              
                              h4("Allgemein"),
                              tags$ul(
                                tags$li("Hohe wissenschaftliche Qualität"),
                                tags$li("Umweltrelevante Expositionen, d. h. in erster Linie NIS von Infrastrukturanlagen"), 
                                tags$li("Neue wissenschaftliche Betrachtungsweisen"),
                                tags$li("In der Öffentlichkeit oder Wissenschaft kontrovers diskutierte Studien")),
                              
                              h4("Bei epidemiologischen Studien"),
                              tags$ul(
                                tags$li("Relevant für die Gesundheit oder das Wohlbefinden von Menschen"),
                                tags$li("Expositionen im Bereich oder unterhalb der Immissionsgrenzwerte"), 
                                tags$li("Ergebnisse aus der Schweiz"),
                                tags$li("Ergebnisse, welche auf die Schweiz übertragbar sind")),
                              
                              h4("Bei experimentellen Studien"),
                              tags$ul(
                                tags$li("Der untersuchte Effekt ist relevant für den Menschen"),
                                tags$li("Definierte Untersuchungsbedingungen inklusive Exposition"), 
                                tags$li("Auswirkungen, welche mit den akzeptierten Wirkungsmechanismen nicht erklärbar sind"))
                            ),
                            
                            wellPanel(
                              h3("BERENIS Selektionsprozess", style ="font-weight: bold;"),
                              tags$img(src = "BERENIS_Selektionsprozess.png", style = "max-width:1200px")
                            )
                          )
                          
                        ),

               ),
               
               
               # -------- ** Menu Tab Panel - Contact --------
               tabPanel(i18n$t("Contact"), #icon = icon("envelope"),
                        
                        fluidRow(
                          
                          column(width = 6, 
                                 
                                 wellPanel( 
                          
                                   h3(i18n$t("Swiss expert group on electromagnetic fields and non-ionising radiation (BERENIS)"), style ="font-weight: bold;"),
                                   
                                   tags$b(i18n$t("Address")), tags$br(),
                                   i18n$t("Swiss Tropical and Public Health Institute (SwissTPH)"), tags$br(),
                                   "Department Epidemiology and Public Health", tags$br(),
                                   "Environmental Exposures and Health Unit", tags$br(),
                                   "Beratende Expertengruppe NIS (BERENIS)", tags$br(),
                                   "Socinstr. 57 ", tags$br(),
                                   "Postfach, CH-4002 Basel", tags$br(),
                                   tags$br(),
                                   
                                   tags$b(i18n$t("Administration office")), tags$br(),
                                   "Dr. Stefan Dongus", tags$br(),
                                   #icon("phone"), "+41 61 284 8111", tags$br(),
                                   icon("envelope"), " ",
                                   mailtoR(email = "stefan.dongus@swisstph.ch",
                                           text = "stefan.dongus@swisstph.ch",
                                           subject = "BERENIS Newsletter"), 
                                   # 
                                   # tags$br(), tags$br(),
                                   # tags$b(i18n$t("BERENIS App author and developer")), tags$br(),
                                   # "Sebastian Egger", 
                                   # tags$br(),
                                   # icon("envelope"), " ",
                                   # mailtoR(email = "sebastian.egger@bafu.admin.ch",
                                   #         text = "sebastian.egger@bafu.admin.ch",
                                   #         subject = "BERENIS Newsletter App")
                                   # 
                                 ),
                          ),
                          
                          column(width = 6,
                                 
                                 conditionalPanel(
                                 
                                   condition = "input.langPicker == 'EN'",
                                   
                                   wellPanel(
                                     
                                     h3("Abonnieren Sie den Newsletter", style ="font-weight: bold;"),
                                     "Would you like to be informed of the publication of the newsletter?", tags$br(),
                                     "Then, send an ",
                                     mailtoR(email = "stefan.dongus@swisstph.ch",
                                             text = "e-mail with the subject ‘Abonnement Newsletter BERENIS'",
                                             subject = "Abonnement Newsletter BERENIS"),
                                     " to the scientific office of BERENIS."
                                   )
                                 ),
                                   
                                 conditionalPanel(
                                   condition = "input.langPicker == 'DE'",
                                   
                                   wellPanel(
                                     
                                     h3("Abonnieren Sie den Newsletter", style ="font-weight: bold;"),
                                     "Wünschen Sie über das Erscheinen des Newsletters benachrichtigt zu werden?", tags$br(),
                                     "Dann senden Sie eine ",
                                     mailtoR(email = "stefan.dongus@swisstph.ch",
                                             text = "E-Mail mit dem Betreff 'Abonnement Newsletter BERENIS'",
                                             subject = "Abonnement Newsletter BERENIS"),
                                     " an das wissenschaftliche Sekretariat der BERENIS."
                                   )
                                 ),
                                 
                                 conditionalPanel(
                                   
                                   condition = "input.langPicker == 'FR'",
                                   
                                   wellPanel(
                                     
                                     h3("Abonnez-vous à la newsletter"), style ="font-weight: bold;"),
                                     "Désirez-vous être informé de la publication de la newsletter ?", tags$br(),
                                     "Dans ce cas, envoyez un ",
                                     mailtoR(email = "stefan.dongus@swisstph.ch",
                                             text = "courriel avec pour objet « Abonnement Newsletter BERENIS »",
                                             subject = "Abonnement Newsletter BERENIS"),
                                     " au secrétariat scientifique de BERENIS."
                                 )
                          )
                        
                        ), # END - fluidRow
                        
                        # --------- fixed bottom Panel  -----------
                        absolutePanel(
                          bottom = 0, left = 0, right = 0, height = "30px",
                          fixed = TRUE,
                          fixedRow(column(12, 
                                          div(style = "position: relative; top: -4px; font-size: 14px;",
                                              i18n$t("BERENIS WebApp developer"),  "\u2014", "Sebastian Egger ", i18n$t("(BAFU)")
                                              # tags$br(),
                                              # tags$em("Sebastian Egger")
                                              )
                                          ),
                                   class="well well-sm",
                                   style="border-top: 1px solid #CCC; text-align: right"),
                        ),
                        
               ) # END - tapPanel "Contact"
               
               
    ),
    
    header = tags$div(
      style = "position: absolute; right: 20px; top: 15px; z-index: 9999;",
      div(class = "language-toggle",
          radioGroupButtons(
            inputId = "langPicker",
            label = NULL,
            choices = i18n$get_languages(),
            selected = "DE",
            size = "normal",
            status = "primary"
          )
      )
    )
  )
)