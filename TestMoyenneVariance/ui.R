library(shiny)
if (!require("ggplot2"))
  install.packages("ggplot2")

if (!require("shinyBS"))
  install.packages("shinyBS")

shinyUI(
fluidPage(

  
  navbarPage(title="Test de 2 moyennes ou 2 variances",
             
             tabPanel("A propos",                     
                      column(1),
                      column(5,br(),br(),br(),
                        withMathJax(p("Cette application permet de faire un test de comparaison de deux moyennes",code("two-sample t-test",style="color:navy"),
". Il permettra de choisir entre un test bilatéral",code("two.sided",style="color:navy"), "unilatéral inférieur",code("less",style="color:navy"), "ou supérieur",code("greater",style="color:navy"),". "					
                        )))),
             
             tabPanel("Choix du jeu de données",
                      tabsetPanel(          
                        tabPanel("Charger les données",
                                 fluidRow(
                                   column(3,
                                          wellPanel(
                                            fileInput("file", "",accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                            bsPopover("file","Important", "Choisissez ensuite le bon format pour votre fichier !",
                                                      trigger="hover",placement="right"),
                                            tags$hr(),
                                            radioButtons("datformat", strong("Format de données:"), choices=c(Option1=2,Option2=3), selected=3),
                                            bsPopover("datformat","Format de données", "Choisissez option1 si votre jeu de données si la première ligne des deux colonnes estim un nom de variables et choisissez option2 sinon",
                                                      trigger="hover",placement="right"),
                                            tags$hr(),
                                            
                                            radioButtons("sep", "Separateur:", choices=c(Virgule=",","Point-Virgule"=";",Tabulation="\t"), selected=";"),
                                            radioButtons("quote", "Guillemets:", choices=c(Aucun="","Double Guillemets"='"',"Simple Guillemets"="'"),selected=""),
                                            br(),br(),br()              
                                            )),
                                   column(9,
                                          conditionalPanel(
                                            condition="input.file!='NULL'",
                                            dataTableOutput("data.tab"))))),
                        
                        tabPanel("Visualisation des données",
                                 fluidRow(
                                   column(6,
                                          plotOutput("boxPlot")),
                                   column(6,br(),br(),
                                          checkboxInput("displaystats", "Afficher plus dinfos statistiques", TRUE),
                                          tableOutput("resumeStats")))))),
             
             tabPanel("Comparaison des deux moyennes",
                      fluidRow(
                        column(3,
                               wellPanel(
                                 
                                 conditionalPanel(
                                   condition="input.datformat==2 || input.datformat==3 ",
                                   h4("Hypothèses de comparaison :"),
                                   uiOutput("hypo"),
                                   tags$hr(),
                                   numericInput("difmoy", label="Valeur dans l'hypothèse:", value=0),
								   bsPopover("difmoy","Note :", "Cette valeur indique la vrai valeur de la moyenne ou la différence des deux moyennes si on effectue un test sur 2 échantillons.).",
                                                      trigger="hover",placement="right"),
							p("Cette valeur indique la vrai valeur de la moyenne ou la différence des deux moyennes si on effectue un test sur 2 échantillons."),
                                   selectInput("alt2", label="Choisissez le type de test :", choices=list("bilateral","inferieur","superieur"),selected="bilateral")),
                                 bsPopover("alt2","Important", "Choisissez  bilatéral,  inférieur ou supérieur",
                                                      trigger="hover",placement="right"),
								 sliderInput("alpha", label=HTML("Pourcentage &alpha;:"), value=.05, max=1, min=0, step=.01),
                                 tags$hr(),
                                 actionButton("teststart", strong("Démarrer le t-test")),
                                 bsPopover("teststart","Note","Assurez vous que vos données respecte bien la loi normale",trigger="hover",placement="right"),
                                 br(),br(),br()
                                 )),
                        
                        column(9,
                               br(),br(),
                               conditionalPanel(
                                 condition="input.teststart>0",
                                 column(7,
                                        plotOutput("tdistrib"),
                                        bsPopover("tdistrib","p-value","La p-value est la région en bleu. Si la p-value est élevée, on garde H0 et on rejette H1.  Sinon on rejette H0 et on garde H1.",
                                                  trigger="hover",placement="left"),br()),
                                 column(5,br(),
                                        strong("Sortie :"),
                                        tableOutput("test"),br(),
                                        checkboxInput("showpoint","Estimation des moyennes:",TRUE),
                                        uiOutput("estim"),
                                        checkboxInput("ci","Intervalle de confiance:", TRUE),
                                        tableOutput("ictab"))))
                                      
             ))
			 ,
			 tabPanel("Comparaison de 2 variances" ,
fluidRow(
                        column(3,
                               wellPanel(
                                 
                                 conditionalPanel(
                                   condition="input.datformat==2 || input.datformat==3 ",
                                   h4("Hypothèses de comparaison :"),
                                   uiOutput("hypo1"),
                                   tags$hr(),
                                   numericInput("difmoy1", label="Valeur dans l'hypothèse:", value=1),
								   bsPopover("difmoy1","Note :", "Cette valeur indique la vrai valeur de la moyenne ou la différence des deux moyennes si on effectue un test sur 2 échantillons.).",
                                                      trigger="hover",placement="right"),
							p("Cette valeur indique le ratio entre les  deux variances si on effectue un test sur 2 échantillons."),
                                   selectInput("alt3", label="Choisissez le type de test :", choices=list("bilateral","inferieur","superieur"),selected="bilateral")),
                                 bsPopover("alt3","Important", "Choisissez  bilatéral,  inférieur ou supérieur",
                                                      trigger="hover",placement="right"),
								 sliderInput("alpha1", label=HTML("Pourcentage &alpha;:"), value=.05, max=1, min=0, step=.01),
                                 tags$hr(),
                                 actionButton("test1start", strong("Démarrer le f-test")),
                                 bsPopover("test1start","Note","Assurez vous que vos données respecte bien la loi normale",trigger="hover",placement="right"),
                                 br(),br(),br()
                                 )),
                        
                        column(9,
                               br(),br(),
                               conditionalPanel(
                                 condition="input.test1start>0",
                                 column(7,
                                        plotOutput("fdistrib"),
                                        bsPopover("fdistrib","p-value","La p-value est la région en bleu. Si la p-value est élevée, on garde H0 et on rejette H1.  Sinon on rejette H0 et on garde H1.",
                                                  trigger="hover",placement="left"),br()),
                                 column(5,br(),
                                        strong("Sortie :"),
                                        tableOutput("test1"),br(),
                                        checkboxInput("showpoint","Estimation des moyennes:",TRUE),
                                        uiOutput("estim1"),
                                        checkboxInput("ci1","Intervalle de confiance:", TRUE),
                                        tableOutput("ictab1"))))
                                      
             )			 
                      )
			 
			 
			 
			 )))
             # tabPanel("Verification de la normalité",
                      # column(1),
                      # column(4,br(),
                             # p(strong("Normality test:"),"The sampling distribution of the sample means or differences in the sample means is 1) Normal if the population(s) is Normal or 2) approximately Normal if the 
                               # sample size(s) is large enough (at least 30).  In situations with small sample size(s), the approach to access 
                               # whether the sample data could have came from a Normal distribution is either through a Q-Q plot or a Normality test."),
                             # actionButton("normstart", label=strong("Perform normality test")),br(),br(),
                             # conditionalPanel(
                               # condition="input.normstart>0",
                               # wellPanel(
                               # strong("Ho: data is from a Normal distribution"),br(),
                               # strong("Ha: data is not from a Normal distribution"),br(),br(),
                               # em("Shapiro-Wilk Normality Test:"),
                               # tableOutput("sw"),
                               # bsPopover("sw","Normality test","In a normality test, a large p-value does not provide evidence that the sample data is not from a Normal distribution.",
                                         # trigger="hover",placement="bottom")))),
                      # column(6,
                             # conditionalPanel(
                               # condition="input.normstart>0",br(),br(),
                               # plotOutput("qqplot"),
                               # bsPopover("qqplot","Q-Q plot","In a Q-Q plot, points that resemble a diagonal line with no curvature implies that the sample data could have came from a Normal distribution.",
                                         # trigger="hover",placement="left"))),
                      # column(1))
             


