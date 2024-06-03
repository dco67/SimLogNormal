library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(latex2exp)

ui <- fluidPage(
  titlePanel(h3("Distribution Lognormale")),
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          6,
          switchInput(
            inputId = "input_type",
            label = "Entrée",
            onLabel = "Curseur",
            offLabel = "Numérique",
            value = TRUE,
            size = "mini"
          )
        ),
        column(3, offset = 3,
               actionBttn("reset_button",
                          "Réinit",
                          icon = icon("refresh"),
                          class = "btn-danger",
                          style = "bordered",
                          size = "xs",
                          color = "default")
        )
      ),
      conditionalPanel(
        condition = "input.input_type == true",
        sliderInput("mu",
                    "$\\mu$:",
                    min = 0,
                    max = 10,
                    step = 0.01,
                    value = 0
        ),
        sliderInput("sdev",
                    "$\\sigma$:",
                    min = 0.01,
                    max = 2,
                    step = 0.001,
                    value = 1
        )
      ),
      conditionalPanel(
        condition = "input.input_type == false",
        numericInput("mu_num",
                     "$\\mu$:",
                     min = 0,
                     max = 10,
                     step = 0.01,
                     value = 0,
                     width = 150
        ),
        numericInput("sdev_num",
                     "$\\sigma$:",
                     min = 0.01,
                     step = 0.001,
                     value = 1,
                     width = 150
        )
      ),
      
      switchInput(
        inputId = "input_mode",
        label = "Données",
        onLabel = "Générées",
        offLabel = "Chargées",
        value = TRUE,
        size = "mini"),
      
      conditionalPanel(
        condition = "input.input_mode == true",
        numericInput("num_observations",
                     "# Observations à générer :",
                     value = 500,
                     min = 1,
                     max = 10000,
                     width = 150,
                     step = 50),
        fluidRow(
          column(6,
                 actionBttn(inputId = "generate_button", 
                            label = "Générer", 
                            style = "unite",
                            color = "primary",
                            icon = icon("sliders"),
                            size = "xs"),
                 DTOutput("fittedParamsGEN")
          ),
          column(6,
                 downloadBttn(outputId = "download_data", 
                              label = "Télécharger", 
                              style = "unite",
                              color = "default",
                              icon = icon("download"),
                              size = "xs")
          )
        )
      ),
      conditionalPanel(
        condition = "input.input_mode == false",
        fileInput("file1", "Fichier :",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        DTOutput("fittedParams")
      ),
      
      p(),
      wellPanel(
        style = "background: lightblue",
        fluidRow(
          column(
            4,
            a(h4("Par Daniel Coulombe, Ph.D.")),
            p("2024")
          ),
          column(
            4,
            tags$a(
              href = "https://isteah.org",
              tags$img(
                src = "ISTEAH_LOGO.png",
                title = "ISTEAH",
                width = "160",
                height = "140"
              )
              
                         )
                  )
                  
                )
      )
    ), 
    mainPanel(
      tabsetPanel(
        tabPanel(strong(h4("Introduction")),
                 helpText(
                   h4("Introduction à la Distribution Lognormale"),
                   "La distribution lognormale est une distribution de probabilité continue qui est souvent utilisée en statistique et en sciences naturelles. Elle est caractérisée par sa forme de cloche asymétrique, où les valeurs les plus élevées sont étendues vers la droite.", p(),
                   "Cette distribution est définie par deux paramètres: le paramètre de localisation, $\\mu$, et le paramètre d'échelle, $\\sigma$, qui déterminent respectivement la position centrale et l'étalement de la distribution.", p(),
                   
                   strong("Fonction de densité de probabilité [PDF]"), " :", p(),
                   "La fonction de densité de probabilité de la distribution lognormale est définie par :",
                   "$$f(x; \\mu, \\sigma) = \\frac{1}{x \\sigma \\sqrt{2\\pi}} e^{-\\frac{(\\ln(x) - \\mu)^2}{2\\sigma^2}}$$", p(),
                   "Cette distribution est souvent utilisée pour modéliser des quantités qui ne peuvent pas être négatives, telles que les prix des actions ou les tailles des populations.", p(),
                   
                   strong("Distribution Lognormale standardisée"), " :", p(),
                   "Lorsque $\\mu = 0$ et $\\sigma = 1$, la distribution lognormale est dite standardisée. Dans ce cas, la fonction de densité de probabilité prend la forme :",
                   "$$f(x; 0, 1) = \\frac{1}{x \\sqrt{2\\pi}} e^{-\\frac{\\ln(x)^2}{2}}$$", p(),
                   "Cette distribution standardisée est souvent utilisée comme modèle de référence pour les analyses statistiques.", p(),
                   
                   strong("Fonction de densité cumulative [CDF]")," :", p(),
                   "La fonction cumulative de la distribution lognormale est définie par l'intégrale de sa fonction de densité de probabilité entre deux valeurs de $x$. ",
                   "$$F(x; \\mu_{log}, \\sigma_{log}) = \\Phi \\left[\\frac{ln(x)-\\mu_{log}}{\\sigma_{log}}\\right]$$", p(),
                   
                   strong("Paramètres de la Distribution"), " :", p(),
                   p(HTML("<b>Paramètre de Localisation [μ] :</b> Le paramètre de localisation, correspondant à la moyenne logarithmique, spécifie le point autour duquel la courbe de la distribution est centrée sur l'axe des logarithmes.")),
                   p(HTML("<b>Paramètre d'Échelle [σ] :</b> Le paramètre d'échelle spécifie l'étalement de la distribution sur l'axe des logarithmes. Des valeurs plus élevées de σ entraînent une distribution plus étalée.")),
                   
                   strong("Caractéristiques principales"), " :",
                   br(),
                   datatable(
                     data.frame(
                       Caractéristique = c("Moyenne", "Médiane", "Mode", "Variance", "Écart-Type", "Symétrie", "Voussure"),
                       Définition = c(
                         "$e^{\\mu + \\frac{\\sigma^2}{2}}$", 
                         "$e^{\\mu}$.",        
                         "$e^{\\mu - \\sigma^2}$.",  
                         "$\\left[ e^{\\sigma^2} - 1 \\right] e^{2\\mu + \\sigma^2}$.",
                         "$\\sqrt{\\left[ e^{\\sigma^2} - 1 \\right] e^{2\\mu + \\sigma^2}}$", 
                         "$\\left[e^{(\\sigma^2)+2} \\right]\\sqrt{e^{\\sigma^2}-1}$", 
                         "$e^{4\\sigma^2} + 2 e^{3\\sigma^2} + 3 e^{2\\sigma^2} - 6$") 
                     ),
                     options = list(paging = FALSE, searching = FALSE, ordering = FALSE)
                   ),

                   strong("Références utiles"), " :", p(),
                   tags$ul(
                     tags$li("Wikipedia: ", tags$a("Distribution lognormale", href = "https://fr.wikipedia.org/wiki/Loi_lognormale")),
                     tags$li("NIST/SEMATECH e-Handbook of Statistical Methods: ", tags$a("Lognormal Distribution", href = "https://www.itl.nist.gov/div898/handbook/eda/section3/eda3669.htm")),
                     tags$li("Documentation MathWorks: ", tags$a("Lognormal Distribution", href = "https://www.mathworks.com/help/stats/lognormal-distribution.html"))
                   )
                 )
        ),
        
        tabPanel(strong(h4("Simuler")),
                 fluidRow(
                   column(width = 6, 
                          plotOutput("theoretical_density")),
                   column(width = 6, 
                          plotOutput("theoretical_cdf"))
                 ),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.input_mode == true",
                     column(width = 6, 
                            plotOutput("empirical_hist")
                     ),
                     column(width = 6, 
                            plotOutput("empirical_cdf")
                     )
                   ),
                   conditionalPanel(
                     condition = "input.input_mode == false",
                     column(width = 6, 
                            plotOutput("empirical_hist_sample")
                     ),
                     column(width = 6, 
                            plotOutput("empirical_cdf_sample")
                     )
                   )
                 )
        ),
        tabPanel(strong(h4("Calcul")),
                 fluidRow(
                   column(
                     width = 6,
                     uiOutput("pdfEquation"),
                     plotOutput("probabilityPlot") 
                   ),
                   column(
                     width = 6,
                     uiOutput("cdfEquation"),
                     plotOutput("quantilePlot") 
                   )
                 ),
                 p(),
                 p(),
                 fluidRow(
                   column(
                     width = 4,
                     numericInput("x_value", 
                                  "Valeur de X:", 
                                  value = 0, 
                                  min = 0, 
                                  step = 0.1,
                                  width = "100px")
                   ),
                   column(
                     width = 4, offset = 3,
                     numericInput("prob_value", 
                                  "Rang Centile:", 
                                  value = 0.5, 
                                  min = 0, 
                                  max = 1, 
                                  step = 0.01,
                                  width = "100px")
                   )
                 ),
                 fluidRow(
                   column(
                     width = 4,
                     textOutput("computed_prob")
                   ),
                   column(
                     width = 4, offset = 3,
                     textOutput("computed_quantile")
                   )
                 )
        ),
        
        navbarMenu(strong(h4("Exercices")),
                   tabPanel("Exercise 1A",
                            fluidRow(
                              column(width = 4,
                                     h3("Problème"),
                                     h4("1A. Durée de vie des appareils électroniques"),
                                     p("Un ingénieur souhaite étudier la durée de vie des appareils électroniques d'un certain modèle. Il choisit au hasard 50 appareils et enregistre leur durée de vie en heures. Sachant que la durée de vie des appareils suit une distribution log-normale avec une moyenne logarithmique $(\\mu_{log})$ de 4.5 et un écart-type logarithmique $(\\sigma_{log})$ de 0.5, quel est le pourcentage d'appareils qui dureront plus de 100 heures ?")
                                     
                              ),
                              column(width = 8,
                                     h3("Solutions"),
                                     h4("1A. Durée de vie des appareils électroniques"),
                                     strong("Analytique"), " :", br(),
                                     "Pour ce problème, nous devons trouver la probabilité que la durée de vie d'un appareil dépasse 100 heures. La durée de vie, X, suit une distribution log-normale avec les paramètres donnés :", p(),
                                     "$ \\mu_{log} = 4.5$, et $\\sigma_{log} = 0.5 $", p(),
                                     "Nous devons trouver la probabilité que $X > 100$, ce qui revient à calculer :", p(),
                                     "$$ P(X > 100) = 1 - P(X \\leq 100) $$", p(),
                                     "Utilisons la fonction de distribution cumulative (CDF) de la distribution log-normale pour calculer cette probabilité :", p(),
                                     "$$ P(X \\leq 100) = \\Phi \\left( \\frac{\\log(100) - \\mu_{log}}{\\sigma_{log}} \\right) $$", p(),
                                     "où $\\Phi$ est la fonction de distribution cumulative de la distribution normale standard. Calculons ceci manuellement puis vérifions avec R :", p(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
mu_log <- 4.5
sigma_log <- 0.5
x <- 100

# Calculer la probabilité P(X <= 100)
p_cdf <- pnorm((log(x) - mu_log) / sigma_log)

# Calculer P(X > 100)
p_gt_100 <- 1 - p_cdf
cat('%P(X > 100) = ', round(p_gt_100 * 100, 2), '%')
                                 ")
                                     ),
                                     strong("Sous R"), " :", br(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
# Définir les paramètres de la distribution log-normale
mu_log <- 4.5
sigma_log <- 0.5

# Calculer la probabilité P(X > 100)
p_gt_100 <- plnorm(100, meanlog = mu_log, sdlog = sigma_log, lower.tail = FALSE)
p_percentage <- p_gt_100 * 100
cat('%P(X > 100) = ', round(p_percentage, 2), '%')
                                 ")
                                       )
                                     )
                              )
                            ),
                   tabPanel("Exercise 1B",
                            fluidRow(
                              column(width = 4,
                                     h3("Problème"),
                                     h4("1B. Durée de vie des appareils électroniques"),
                                     p("Un ingénieur souhaite étudier la durée de vie des appareils électroniques d'un certain modèle. Il choisit au hasard 50 appareils et enregistre leur durée de vie en heures. Sachant que la durée de vie des appareils suit une distribution log-normale avec une moyenne logarithmique $(\\mu_{log})$ de 4.5 et un écart-type logarithmique $(\\sigma_{log})$ de 0.5, quel est le pourcentage d'appareils qui dureront moins de 175 heures ?")
                              ),
                              column(width = 8,
                                     h3("Solutions"),
                                     h4("1B. Durée de vie des appareils électroniques"),
                                     strong("Analytique"), " :", br(),
                                     "Pour ce problème, $X = 175$, $\\mu_{log} = 4.5$, et $\\sigma_{log} = 0.5$.", p(),
                                     "Nous devons trouver la probabilité que la durée de vie d'un appareil ne dépasse pas 175 heures, ce qui correspond à :", p(),
                                     "$$ P(X < 175) = \\int_{0}^{175} f(x) \\, dx $$", p(),
                                     "La fonction de densité de probabilité [PDF] pour une distribution log-normale est :", p(),
                                     "$$ f(x; \\mu, \\sigma) = \\frac{1}{x \\sigma \\sqrt{2 \\pi}} \\exp\\left(-\\frac{(\\log x - \\mu)^2}{2 \\sigma^2}\\right) $$", p(),
                                     "Cependant, pour calculer cette probabilité, nous utilisons la fonction de répartition cumulative [CDF] de la distribution log-normale :", p(),
                                     "$$ P(X < 175) = F(175; \\mu, \\sigma) = \\Phi \\left( \\frac{\\log(175) - \\mu}{\\sigma} \\right) $$", p(),
                                     "où $\\Phi$ est la fonction de répartition cumulative de la distribution normale standard.", p(),
                                     "Calculons cette probabilité en utilisant R :", p(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
mu_log <- 4.5
sigma_log <- 0.5
x <- 175

# Calculer la probabilité P(X < 175)
p_cdf <- pnorm((log(x) - mu_log) / sigma_log)

# Afficher le résultat
cat('%P(X < 175) = ', round(p_cdf * 100, 2), '%')
                                 ")
                                     ),
                                     strong("Sous R"), " :", br(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
# Définir les paramètres de la distribution log-normale
mu_log <- 4.5
sigma_log <- 0.5

# Calculer la probabilité P(X < 175)
p_lt_175 <- plnorm(175, meanlog = mu_log, sdlog = sigma_log, lower.tail = TRUE)
p_percentage <- p_lt_175 * 100
cat('%P(X < 175) = ', round(p_percentage, 2), '%')
                                 ")
                                                           )
                                 )
                          )
                        ),
                   tabPanel("Exercise 1C",
                            fluidRow(
                              column(width = 4,
                                     h3("Problème"),
                                     h4("1C. Durée de vie des appareils électroniques"),
                                     p("Un ingénieur souhaite étudier la durée de vie des appareils électroniques d'un certain modèle. Il choisit au hasard 50 appareils et enregistre leur durée de vie en heures. Sachant que la durée de vie des appareils suit une distribution log-normale avec une moyenne logarithmique $(\\mu_{log})$ de 4.5 et un écart-type logarithmique $(\\sigma_{log})$ de 0.5, quel est le pourcentage d'appareils qui dureront entre 100 et 175 heures ?")
                              ),
                              column(width = 8,
                                     h3("Solutions"),
                                     h4("1C. Durée de vie des appareils électroniques"),
                                     strong("Analytique"), " :", br(),
                                     "Pour ce problème, nous devons trouver la probabilité que la durée de vie d'un appareil soit entre 100 et 175 heures, ce qui correspond à :", p(),
                                     "$$ P(100 < X < 175) = \\int_{100}^{175} f(x) \\, dx $$", p(),
                                     "La fonction de densité de probabilité [PDF] pour une distribution log-normale est :", p(),
                                     "$$ f(x; \\mu, \\sigma) = \\frac{1}{x \\sigma \\sqrt{2 \\pi}} \\exp\\left(-\\frac{(\\log x - \\mu)^2}{2 \\sigma^2}\\right) $$", p(),
                                     "Cependant, pour calculer cette probabilité, nous utilisons la fonction de répartition cumulative [CDF] de la distribution log-normale :", p(),
                                     "$$ P(100 < X < 175) = F(175; \\mu, \\sigma) - F(100; \\mu, \\sigma) $$", p(),
                                     "où $F$ est la fonction de répartition cumulative de la distribution log-normale.", p(),
                                     "Calculons cette probabilité en utilisant R :", p(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
mu_log <- 4.5
sigma_log <- 0.5
x1 <- 100
x2 <- 175

# Calculer la probabilité P(100 < X < 175)
p_cdf_175 <- pnorm((log(x2) - mu_log) / sigma_log)
p_cdf_100 <- pnorm((log(x1) - mu_log) / sigma_log)
p_cdf <- p_cdf_175 - p_cdf_100

# Afficher le résultat
cat('%P(100 < X < 175) = ', round(p_cdf * 100, 2), '%')
                                 ")
                                     ),
                                     strong("Sous R"), " :", br(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
# Définir les paramètres de la distribution log-normale
mu_log <- 4.5
sigma_log <- 0.5

# Calculer la probabilité P(100 < X < 175)
p_lt_175 <- plnorm(175, meanlog = mu_log, sdlog = sigma_log, lower.tail = TRUE)
p_lt_100 <- plnorm(100, meanlog = mu_log, sdlog = sigma_log, lower.tail = TRUE)
p_between <- p_lt_175 - p_lt_100
p_percentage <- p_between * 100
cat('%P(100 < X < 175) = ', round(p_percentage, 2), '%')
                                 ")
                                     )
                              )
                            )
                   ),
                   
                   tabPanel("Exercise 1D",
                            fluidRow(
                              column(width = 4,
                                     h3("Problème"),
                                     h4("1D. Durée de vie des appareils"),
                                     p("Un ingénieur souhaite étudier la durée de vie des appareils électroniques d'un certain modèle. Il choisit au hasard 50 appareils et enregistre leur durée de vie en heures. Sachant que la durée de vie des appareils suit une distribution log-normale avec une moyenne logarithmique $(\\mu_{log})$ de 4.5 et un écart-type logarithmique $(\\sigma_{log})$ de 0.5, quelle est la durée de vie au-delà de laquelle 90% des appareils auront une durée de vie plus courte ?")
                              ),
                              column(width = 8,
                                     h3("Solutions"),
                                     h4("1D. Durée de vie des appareils"),
                                     strong("Analytique"), " :", br(),
                                     "Pour ce problème, nous devons trouver le 90ème percentile de la distribution log-normale avec $\\mu_{log} = 4.5$ et $\\sigma_{log} = 0.5$.", p(),
                                     "Cela signifie que nous cherchons la valeur $x$ telle que :", p(),
                                     "$$ P(X < x) = 0.90 $$", p(),
                                     "Pour une distribution log-normale, cette valeur peut être trouvée en utilisant la fonction de répartition cumulative inverse de la distribution normale appliquée au logarithme de $x$. Voici les étapes détaillées :", p(),
                                     "1. Définir la variable $Z$ telle que $Z = \\frac{\\log(X) - \\mu_{log}}{\\sigma_{log}}$, où $Z$ suit une distribution normale standard $N(0, 1)$.", p(),
                                     "2. Trouver la valeur de $Z$ qui correspond au 90ème percentile de la distribution normale standard : $Z_{0.90} = \\Phi^{-1}(0.90)$.", p(),
                                     "3. Utiliser la relation inverse pour trouver $X$ :", p(),
                                     "$$ x = \\exp(\\mu_{log} + \\sigma_{log} \\cdot Z_{0.90}) $$", p(),
                                     "Utilisons R pour effectuer ces calculs :", p(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
mu_log <- 4.5
sigma_log <- 0.5
p <- 0.90

# Calculer le 90ème percentile
z_90 <- qnorm(p)
x_90 <- exp(mu_log + sigma_log * z_90)
cat('Durée de vie pour laquelle 90% des appareils auront une durée de vie plus courte = ', round(x_90, 2), 'heures')
                      ")
                                     ),
                                     strong("Sous R"), " :", br(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
# Définir les paramètres de la distribution log-normale
mu_log <- 4.5
sigma_log <- 0.5
p <- 0.90

# Calculer la valeur du 90ème percentile
q_90 <- qlnorm(p, meanlog = mu_log, sdlog = sigma_log)
cat('Durée de vie pour laquelle 90% des appareils auront une durée de vie plus courte = ', round(q_90, 2), 'heures')
                      ")
                                     )
                              )
                            )
                   ),
                   
                   tabPanel("Exercise 1E",
                            fluidRow(
                              column(width = 4,
                                     h3("Problème"),
                                     h4("1E. Temps moyen de fonctionnement"),
                                     p("Un ingénieur souhaite estimer le temps moyen de fonctionnement des appareils électroniques d'un certain modèle. Il choisit au hasard 50 appareils et enregistre leur durée de vie en heures. Sachant que la durée de vie des appareils suit une distribution log-normale avec une moyenne logarithmique $(\\mu_{log})$ de 4.5 et un écart-type logarithmique $(\\sigma_{log})$ de 0.5, quel est le temps moyen de fonctionnement des appareils ?")
                              ),
                              column(width = 8,
                                     h3("Solutions"),
                                     h4("1E. Temps moyen de fonctionnement"),
                                     strong("Analytique"), " :", br(),
                                     "Pour ce problème, nous devons trouver la moyenne de la distribution log-normale avec $\\mu_{log} = 4.5$ et $\\sigma_{log} = 0.5$.", p(),
                                     "La moyenne d'une distribution log-normale est donnée par :", p(),
                                     "$$ \\text{Moyenne} = \\exp\\left(\\mu_{log} + \\frac{\\sigma_{log}^2}{2}\\right) $$", p(),
                                     "Calculons cette valeur en utilisant R :", p(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
mu_log <- 4.5
sigma_log <- 0.5

# Calculer la moyenne
mean_lognormal <- exp(mu_log + (sigma_log^2) / 2)
cat('Temps moyen de fonctionnement des appareils = ', round(mean_lognormal, 2), 'heures')
                      ")
                                     ),
                                     strong("Sous R"), " :", br(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
# Définir les paramètres de la distribution log-normale
mu_log <- 4.5
sigma_log <- 0.5

# Calculer la moyenne
mean_lognormal <- exp(mu_log + (sigma_log^2) / 2)
cat('Temps moyen de fonctionnement des appareils = ', round(mean_lognormal, 2), 'heures')
                      ")
                                     )
                              )
                            )
                   ),
                   tabPanel("Exercise 1F",
                            fluidRow(
                              column(width = 4,
                                     h3("Problème"),
                                     h4("1F. Durée de vie médiane"),
                                     p("Un ingénieur souhaite connaître la durée de vie médiane des appareils électroniques d'un certain modèle. Il choisit au hasard 50 appareils et enregistre leur durée de vie en heures. Sachant que la durée de vie des appareils suit une distribution log-normale avec une moyenne logarithmique $(\\mu_{log})$ de 4.5 et un écart-type logarithmique $(\\sigma_{log})$ de 0.5, quelle est la durée de vie médiane des appareils ?")
                              ),
                              column(width = 8,
                                     h3("Solutions"),
                                     h4("1F. Durée de vie médiane"),
                                     strong("Analytique"), " :", br(),
                                     "Pour ce problème, nous devons trouver la médiane de la distribution log-normale avec $\\mu_{log} = 4.5$ et $\\sigma_{log} = 0.5$.", p(),
                                     "La médiane d'une distribution log-normale est donnée par :", p(),
                                     "$$ \\text{Médiane} = \\exp(\\mu_{log}) $$", p(),
                                     "Calculons cette valeur en utilisant R :", p(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
mu_log <- 4.5

# Calculer la médiane
median_lognormal <- exp(mu_log)
cat('Durée de vie médiane des appareils = ', round(median_lognormal, 2), 'heures')
                      ")
                                     ),
                                     strong("Sous R"), " :", br(),
                                     wellPanel(
                                       style = "background-color: #f5f5f5; border: 1px solid #ddd; padding: 10px;",
                                       tags$pre("
# Définir les paramètres de la distribution log-normale
mu_log <- 4.5

# Calculer la médiane
median_lognormal <- exp(mu_log)
cat('Durée de vie médiane des appareils = ', round(median_lognormal, 2), 'heures')
                      ")
                   
                    )
                    )
                    )
        )
      )
      )
    )
  )
)

                   
                   server <- function(input, output, session) {
                     
                     sample_data <- reactiveVal(NULL)
                     fit <- reactiveVal(NULL)
                     
                     observeEvent(input$reset_button, {
                       updateSliderInput(session, "mu", value = 0)
                       updateSliderInput(session, "sdev", value = 1)
                       
                       updateNumericInput(session, "mu_num", value = 0)
                       updateNumericInput(session, "sdev_num", value = 1)
                       
                       updateSwitchInput(session, "input_type", value = TRUE)
                       updateSwitchInput(session, "input_mode", value = TRUE)
                       updateNumericInput(session, "num_observations", value = 500)
                       
                       output$empirical_hist <- renderPlot(NULL)
                       output$empirical_cdf <- renderPlot(NULL)
                       output$empirical_hist_sample <- renderPlot(NULL)
                       output$empirical_qq_sample <- renderPlot(NULL)
                       output$fittedParams <- renderDT(NULL)
                       output$fittedParamsGEN <- renderDT(NULL)
                     })
                     
                     output$meanlogPlot <- renderPlot({
                       x <- seq(0, 10, length.out = 1000)
                       data <- data.frame(
                         x = rep(x, 4),
                         y = c(dlnorm(x, meanlog = 0.0, sdlog = 1), 
                               dlnorm(x, meanlog = 0.5, sdlog = 1), 
                               dlnorm(x, meanlog = 0.7, sdlog = 1),
                               dlnorm(x, meanlog = 1.0, sdlog = 1)),
                         meanlog = factor(rep(c(0, 0.5, 0.7, 1.0), each = length(x)))
                       )
                       
                       p <- ggplot(data, aes(x = x, y = y, color = meanlog)) +
                         geom_line() +
                         labs(title = "Variation de \u03BC",
                              x = "x", y = "Densité") +
                         scale_color_manual(values = c("red", "blue", "green", "orange"),
                                            labels = c("\u03BC = 0.0", "\u03BC = 0.5", "\u03BC = 0.7", "\u03BC = 1.0")) +
                         labs(color = "Location") + 
                         theme_minimal() +
                         theme(legend.position = c(0.5, 0.7),
                               legend.text = element_text(size = 12),
                               legend.title = element_text(size = 14),
                               plot.title = element_text(size = 18)) +
                         xlim(0, qlnorm(0.9, meanlog = 1, sdlog = 1))
                       print(p)
                     })
                     
                     output$sdlogPlot <- renderPlot({
                       x <- seq(0, 5, length.out = 1000)
                       data <- data.frame(
                         x = rep(x, 4),
                         y = c(dlnorm(x, meanlog = 0, sdlog = 0.1), 
                               dlnorm(x, meanlog = 0, sdlog = 0.3), 
                               dlnorm(x, meanlog = 0, sdlog = 0.5),
                               dlnorm(x, meanlog = 0, sdlog = 1.0)),
                         sdlog = factor(rep(c(.1, .3, .5, 1), each = length(x)))
                       )
                       
                       p <- ggplot(data, aes(x = x, y = y, color = sdlog)) +
                         geom_line() +
                         labs(title = "Variation de \u03C3",
                              x = "x", y = "Densité") +
                         scale_color_manual(values = c("red", "blue", "green", "orange"),
                                            labels = c("\u03C3 = 0.1", "\u03C3 = 0.3", "\u03C3 = 0.5", "\u03C3 = 1.0")) +
                         labs(color = "Échelle") + 
                         theme_minimal()  +
                         theme(legend.position = c(0.50, 0.7),
                               legend.text = element_text(size = 12),
                               legend.title = element_text(size = 14),
                               plot.title = element_text(size = 18)) +
                         xlim(0, qlnorm(0.9, meanlog = 0, sdlog = 1))
                       print(p)
                     })
                     
                     output$combinedPlot <- renderPlot({
                       x <- seq(0, 8, length.out = 1000)
                       data <- data.frame(
                         x = rep(x, 3),
                         y = c(dlnorm(x, meanlog = 0, sdlog = 0.2), 
                               dlnorm(x, meanlog = 0.5, sdlog = 0.5),
                               dlnorm(x, meanlog = 1, sdlog = 0.8)),
                         combined = factor(rep(c("\u03B3 = 0.0, \u03BC = 0.2", "\u03B3 = 0.5, \u03BC = 0.5", "\u03B3 = 1.0, \u03BC = 0.8"), each = length(x)))
                       )
                       
                       p <- ggplot(data, aes(x = x, y = y, color = combined)) +
                         geom_line() +
                         labs(title = "Variation de \u03BC & \u03C3",
                              x = "x", y = "Densité") +
                         scale_color_manual(values = c("green", "blue", "red"),
                                            labels = c("\u03BC = 0.0, \u03C3 = 0.2", "\u03BC = 0.5, \u03C3 = 0.5", "\u03BC = 1.0, \u03C3 = 0.8")) +
                         labs(color = "Location & Échelle") + 
                         theme_minimal()  +
                         theme(legend.position = c(0.5, 0.7),
                               legend.text = element_text(size = 12),
                               legend.title = element_text(size = 14),
                               plot.title = element_text(size = 18))
                       print(p)
                     })
                     
                     parms <- reactive({
                       if (input$input_type) {
                         list(
                           meanlog = input$mu,
                           sdlog = input$sdev
                         )
                       } else {
                         list(
                           meanlog = input$mu_num,
                           sdlog = input$sdev_num
                         )
                       }
                     })
                     
                     observe({
                       params <- parms()
                       mu <- params$meanlog
                       sdev <- params$sdlog
                       xlim <- c(0, qlnorm(0.99, mu, sdev))
                       
                       output$theoretical_density <- renderPlot({
                         x <- seq(xlim[1], xlim[2], length.out = 1000)
                         data <- data.frame(x, y = dlnorm(x, meanlog = mu, sdlog = sdev))
                         
                         density_equation <- TeX(sprintf("$f(x) = \\frac{1}{x \\cdot %.2f \\sqrt{2 \\pi}} \\exp \\left( -\\frac{(\\ln(x) - %.2f)^2}{2 \\cdot %.2f^2} \\right)$", sdev, mu, sdev))
                         
                         ggplot(data, aes(x, y)) +
                           geom_line() +
                           labs(
                             title = "Courbe de densité",
                             subtitle = density_equation,
                             x = "x", y = "Densité"
                           ) +
                           theme_minimal() +
                           xlim(xlim[1], xlim[2]) +
                           theme(
                             plot.subtitle = element_text(size = 15, hjust = 0.5, vjust = 1),
                             plot.title = element_text(size = 15, hjust = 0.5, vjust = 1)
                           )
                       })

                     output$theoretical_cdf <- renderPlot({
                       x <- seq(xlim[1], xlim[2], length.out = 1000)
                       theoretical_cdf <- data.frame(x, y = plnorm(x, meanlog = mu, sdlog = sdev))
                       cdf_equation <- TeX(sprintf("$F(x; \\mu, \\sigma) = \\Phi\\left[ \\frac{\\ln(x) - %.2f}{ %.2f \\sqrt{2}} \\right]$", mu, sdev))
                       
                       ggplot(theoretical_cdf, aes(x, y)) +
                         geom_line(color = "red") +
                         labs(
                           title = "Courbe de répartition cumulative",
                           subtitle = cdf_equation,
                           x = "x", y = "Probabilité cumulative"
                         ) +
                         xlim(xlim[1], xlim[2]) +
                         theme_minimal() +
                         theme(
                           plot.subtitle = element_text(size = 15, hjust = 0.5, vjust = 1),
                           plot.title = element_text(size = 15, hjust = 0.5, vjust = 1)
                         )
                       })
                     })
                     
                     
                     observeEvent(input$generate_button, {
                       params <- parms()
                       mu <- params$meanlog
                       sdev <- params$sdlog
                       n <- input$num_observations
                       
                       data <- rlnorm(n, meanlog = mu, sdlog = sdev)
                       sample_data(data)
                       
                       fit(MASS::fitdistr(sample_data(), "log-normal"))
                       updateNumericInput(session, "mu", value = as.numeric(fit()$estimate[1]))
                       updateNumericInput(session, "sdev", value = as.numeric(fit()$estimate[2]))
                       updateNumericInput(session, "mu_num", value = as.numeric(fit()$estimate[1]))
                       updateNumericInput(session, "sdev_num", value = as.numeric(fit()$estimate[2]))
                       
                       fit_params <- data.frame(
                         Param = c("\u03BC", "\u03C3"),
                         Estim = round(fit()$estimate, 4),
                         ErrStd = round(fit()$sd, 4))
                       
                       output$fittedParamsGEN <- renderDT({
                         datatable(fit_params, 
                                   options = list(paging = FALSE, searching = FALSE, ordering = FALSE),
                                   escape = FALSE,
                                   rownames= FALSE) %>% 
                           formatStyle(
                             columns = c("Param"),
                             fontSize = "16px"
                           )
                       })
                       
                       output$empirical_hist <- renderPlot({
                         ggplot(data.frame(x = sample_data()), aes(x)) +
                           geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
                           stat_function(fun = dlnorm, args = list(meanlog = fit()$estimate[1], sdlog = fit()$estimate[2]), color = "red") +
                           xlim(0, quantile(sample_data(),  0.99)) +
                           theme_minimal()
                       })
                       
                       output$empirical_cdf <- renderPlot({
                         median <- if(input$input_type) input$mu else input$mu_num
                         gamma <- if(input$input_type) input$sdev else input$sdev_num
                         
                         empirical_cdf <- ecdf(sample_data())
                         
                         ggplot(data.frame(x = sample_data()), aes(x)) +
                           stat_ecdf(geom = "step") +
                           labs(title = "Distribution cumulative empirique", x = "x", y = "Probabilité cumulative") +
                           ylim(0, 1) +
                           theme_minimal()
                       })
                     })
                     

                     observeEvent(input$file1, {
                       req(input$file1)
                       data <- read.csv(input$file1$datapath)
                       sample_data(data[, 1])
                       fit(MASS::fitdistr(sample_data(), "log-normal"))
                       
                       updateNumericInput(session, "mu", value = as.numeric(fit()$estimate[1]))
                       updateNumericInput(session, "sdev", value = as.numeric(fit()$estimate[2]))
                       updateNumericInput(session, "mu_num", value = as.numeric(fit()$estimate[1]))
                       updateNumericInput(session, "sdev_num", value = as.numeric(fit()$estimate[2]))
                       
                       fit_params <- data.frame(
                         Param = c("\u03BC", "\u03C3"),
                         Estim = round(fit()$estimate, 4),
                         ErrStd = round(fit()$sd, 4))
                       
                       output$fittedParams <- renderDT({
                         datatable(fit_params, 
                                   options = list(paging = FALSE, searching = FALSE, ordering = FALSE),
                                   escape = FALSE,
                                   rownames= FALSE) %>% 
                           formatStyle(
                             columns = c("Param"),
                             fontSize = "16px"
                           )
                       })
                       
                       output$empirical_hist_sample <- renderPlot({
                         ggplot(data.frame(x = sample_data()), aes(x)) +
                           geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
                           stat_function(fun = dlnorm, args = list(meanlog = fit()$estimate[1], sdlog = fit()$estimate[2]), color = "red") +
                           labs(title = "Histogramme empirique", x = "x", y = "Densité") +
                           theme_minimal()
                       })
                       
                       output$empirical_cdf_sample <- renderPlot({
                         empirical_cdf <- ecdf(sample_data())
                         
                         ggplot(data.frame(x = sample_data()), aes(x)) +
                           stat_ecdf(geom = "step") +
                           labs(title = "Distribution cumulative empirique", x = "x", y = "Probabilité cumulative") +
                           xlim(quantile(sample_data(), c(0.05, 0.95))) +
                           theme_minimal()
                       })
                     })

                     
                     output$pdfEquation <- renderUI({
                       params <- parms()
                       withMathJax(
                         helpText(
                           "PDF: $$f(x) = \\frac{1}{x \\sigma \\sqrt{2\\pi}} \\exp\\left[-\\frac{(\\ln x - \\mu)^2}{2\\sigma^2}\\right]$$",
                           "où: $\\mu$ =", round(params$meanlog, 6), ", $\\sigma$ =", round(params$sdlog, 6)
                         )  
                       )
                     })
                     
                     
                     output$cdfEquation <- renderUI({
                       params <- parms()
                       withMathJax(
                         helpText(
                           "CDF: $$F(x) = \\Phi\\left[ \\frac{\\ln(x) - \\mu}{\\sigma} \\right]$$",
                           "où: $\\mu$ =", round(params$meanlog, 6), ", $\\sigma$ =", round(params$sdlog, 6)
                         )  
                       )
                     })
                     
                     
                     output$probabilityPlot <- renderPlot({
                       params <- parms()
                       limdat <- c(0, qlnorm(0.975, meanlog = params$meanlog, sdlog = params$sdlog))
                       x_values <- seq(limdat[1], limdat[2], by = 0.01)
                       y_values <- dlnorm(x_values, meanlog = params$meanlog, sdlog = params$sdlog)
                       x_value <- input$x_value
                       prob <- plnorm(x_value, meanlog = params$meanlog, sdlog = params$sdlog)
                       
                       df <- data.frame(x = x_values, y = y_values)
                       shading_df <- data.frame(
                         x = c(limdat[1], subset(df, x <= x_value)$x, x_value),
                         y = c(0, subset(df, x <= x_value)$y, 0)
                       )
                       
                       ggplot(df, aes(x = x, y = y)) +
                         geom_line() +
                         geom_polygon(data = shading_df, aes(x, y), fill = "lightblue", alpha = 0.5) +
                         labs(x = "x", y = "Densité", title = "Fonction de Densité") +
                         xlim(limdat[1], limdat[2]) +
                         geom_vline(xintercept = x_value, linetype = "dashed") +
                         annotate("text", x = x_value, y = 0, vjust = 1, label = sprintf("x = %.2f", x_value), color = "red")
                     })
                     
                     output$quantilePlot <- renderPlot({
                       params <- parms()
                       prob_value <- input$prob_value
                       
                       if (is.na(prob_value) || prob_value < 0 || prob_value > 1) {
                         return()
                       }
                       
                       quantile_value <- qlnorm(prob_value, meanlog = params$meanlog, sdlog = params$sdlog)
                       
                       prob_values <- seq(0, 1, by = 0.01)
                       x_values <- qlnorm(prob_values, meanlog = params$meanlog, sdlog = params$sdlog)
                       
                       valid_indices <- is.finite(x_values) & !is.na(x_values)
                       x_values <- x_values[valid_indices]
                       prob_values <- prob_values[valid_indices]
                       
                       if (length(x_values) == 0 || length(prob_values) == 0) {
                         return()
                       }
                       
                       df <- data.frame(x = x_values, prob = prob_values)
                       
                       ggplot(df, aes(x = x, y = prob)) +
                         geom_line() +
                         geom_hline(yintercept = prob_value, linetype = "dashed", color = "blue") +
                         geom_vline(xintercept = quantile_value, linetype = "dashed", color = "red") +
                         geom_point(x = quantile_value, y = prob_value, color = "black", shape = 16) +
                         labs(x = "Quantile", y = "Probabilité Cumulative", title = "Fonction Quantile") +
                         ylim(0, 1) +
                         xlim(range(x_values))
                     })
                     
                     computed_pr <- reactive({
                       params <- parms()
                       x_value <- input$x_value
                       prob <- plnorm(x_value, meanlog = params$meanlog, sdlog = params$sdlog)
                       paste("P[X <", x_value, "] = ", round(prob, 4))
                     })
                     
                     computed_quantile <- reactive({
                       params <- parms()
                       prob_value <- input$prob_value
                       quantile <- qlnorm(prob_value, meanlog = params$meanlog, sdlog = params$sdlog)
                       paste("Q[", prob_value, "] = ", round(quantile, 4))
                     })
                     
                     output$computed_prob <- renderText({
                       computed_pr()
                     })
                     
                     output$computed_quantile <- renderText({
                       computed_quantile()
                     })

                       output$download_data <- downloadHandler(
                         filename = function() {
                           paste("LogNormal_data-", Sys.Date(), ".csv", sep = "")
                         },
                         content = function(file) {
                           write.csv(data.frame(x = sample_data()), file, row.names = FALSE)
                         })
                   }
                       
                       shinyApp(ui, server)