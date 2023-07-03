library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinyjs)
library(DT)
library(readr)
library(Hmisc)
library(ggplot2)
library(ggmosaic)
#Mot de passe
user_base <- tibble(
  user = "kpam",
  password = "kpam", 
  permissions = "standard",
  name = "User One"
)
header<-dashboardHeader(
  title = tags$h1("Analyse des dépenses par ménage", style = "font-size: 13px;color: red;font-weight: bold;"),
  tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout")))
Sidebar<-dashboardSidebar(
  
  collapsed = TRUE, sidebarMenuOutput("sidebar")
)
Body<-dashboardBody(
  shinyjs::useShinyjs(),
  
  # put the shinyauthr login ui module here
  shinyauthr::loginUI("login"),
  tabItems(
    tabItem(
      tabName = "univariate",
      h2("Analyse univariée"),
      # Sélection de la variable d'intérêt
      selectInput("univariate_variable", "Variable :", choices = NULL,selected = NULL),
      
      # Bouton pour effectuer l'analyse
      actionButton("univariate_button", "Effectuer l'analyse",icon=icon("play")),
      
      # Résultats de l'analyse
      verbatimTextOutput("univariate_analysis"),
      plotOutput("histogram"),
      plotOutput("boxplot"),
      plotOutput("pie_chart"),
      plotOutput("mosaic_plot"),
    ),
    tabItem(
      tabName = "bivariate",
      h2("Analyse bivariée"),
      # Sélection des variables pour l'analyse bivariée
      selectInput("bivariate_variable1", "Variable 1 :", choices = NULL,selected = NULL),
      selectInput("bivariate_variable2", "Variable 2 :", choices = NULL,selected = NULL),
      # Bouton pour effectuer l'analyse
      actionButton("bivariate_button", "Effectuer l'analyse",icon=icon("play")),
      # Résultats de l'analyse
      verbatimTextOutput("bivariate_analysis"),
      plotOutput("scatterplot"),
      plotOutput("boxplot_bivariate",width = "100%", height = "400px") 
      
      
    ),
    tabItem(
      tabName = "econometric",
      h2("Analyse économétrique"),
      # Sélection de la variable dépendante
      selectInput("dependent_variable", "Variable dépendante :", choices = NULL, selected = NULL),
      # Sélection des variables indépendantes
      selectInput("independent_variable1", "Variable1 indépendante1 :", choices = NULL, selected = NULL),
      selectInput("independent_variable2", "Variable2 indépendante2 :", choices = NULL, selected = NULL),
      # Bouton pour ajuster le modèle
      actionButton("logistic_regression_button", "Ajuster le modèle de régression logistique", icon = icon("play")),
      # Résultats du modèle
      verbatimTextOutput("logistic_regression_results")
    ),
    
    tabItem(
      tabName = "per_capita",
      h2("Calcul des dépenses par tête"),
      DT::dataTableOutput("data_par_capita"),
      downloadButton("download_data_par_tete", "Télécharger les données par tête")
      
      
    ),
    tabItem(
      tabName = "per_household",
      h2("Calcul des dépenses totales par ménage"),
      DT::dataTableOutput("data_par_menage"),
      downloadButton("download_data_par_menage", "Télécharger les données par menage")
      
    ),
    tabItem(
      tabName = "per_item",
      h2("Calcul des dépenses par item"),
      DT::dataTableOutput("data_par_item"),
      downloadButton("download_data_par_item", "Télécharger les données par item")
      
    ),
    tabItem(
      tabName = "data_preview",
      h2(" "),
      DT::dataTableOutput("data_table")
    ),
    tabItem(tabName = "about",
            
            h2(" "),
            verbatimTextOutput("about_text")
    ))
)
ui <- dashboardPage(header = header,sidebar = Sidebar,body = Body) 
options(shiny.maxRequestSize = 100*1024^2)  # Augmente la limite de taille à 100 Mo
server <- function(input, output, session) {
  # Importation des données en fonction du format sélectionné
  data <- reactive({
    req(input$data_input)
    file <- input$data_input$datapath
    extension <- tools::file_ext(file)
    
    if (extension == "xlsx") {
      data <- readxl::read_excel(file)
    } else if (extension == "csv") {
      data <- readr::read_csv(file)
      
    } else if (extension == "dta") {
      data <- haven::read_dta(file)
    }
    
    data
  })
  
  # Le statut de connexion et les informations seront gérés par le module shinyauthr et stockés ici
  credentials <- shinyauthr::loginServer(id= "login",
                                         data = user_base,
                                         user_col = user,
                                         pwd_col = password,
                                         log_out = reactive(logout_init()))
  
  # Le statut de déconnexion est géré par le module shinyauthr et stocké ici
  logout_init <- shinyauthr::logoutServer(id= "logout", active = reactive(credentials()$user_auth))
  
  # Cela ouvre ou ferme la barre latérale lors de la connexion/déconnexion
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  # Seulement lorsque credentials()$user_auth est TRUE, affichez le menu latéral souhaité
  output$sidebar <- renderMenu({
    req(credentials()$user_auth)
    sidebarMenu(
      menuItem(
        "Importer les données",
        tabName = "import",
        icon = icon("file-import"),
        fileInput("data_input", "Importer le fichier de données",
                  accept = c(".xlsx", ".csv", ".dta"))
      ),
      menuItem(
        "Aperçu des données",
        tabName = "data_preview",
        icon = icon("table")
      ),
      menuItem(
        "Analyse",
        tabName = "analysis",
        icon = icon("chart-line"),
        menuSubItem("Analyse univariée", tabName = "univariate", icon = icon("chart-bar")),
        menuSubItem("Analyse bivariée", tabName = "bivariate", icon = icon("chart-line")),
        menuSubItem("Analyse économétrique", tabName = "econometric", icon = icon("chart-area"))
      ),
      menuItem(
        "Calcul des dépenses",
        tabName = "expenses",
        icon = icon("calculator"),
        menuSubItem("Par tête", tabName = "per_capita", icon = icon("user")),
        menuSubItem("Par ménage", tabName = "per_household", icon = icon("home")),
        menuSubItem("Par item", tabName = "per_item", icon = icon("list"))
      ),
      
      menuItem("A propos", tabName = "about", icon = icon("info-circle"))
    )
  })
  
  
  # Mise à jour de la variable d'intérêt pour l'analyse univariée et bivariée
  observeEvent(data(), {
    updateSelectInput(session, "univariate_variable", choices = setdiff(names(data()), c("interview__key","recode_dpenses_ttle")),selected = NULL)
    updateSelectInput(session, "bivariate_variable1", choices = setdiff(names(data()), c("interview__key","recode_dpenses_ttle")),selected = NULL)
    updateSelectInput(session, "bivariate_variable2", choices = setdiff(names(data()), c("interview__key","recode_dpenses_ttle")),selected = NULL)
    updateSelectInput(session, "dependent_variable",  choices = c("recode_dpenses_ttle"), selected = NULL)
    updateSelectInput(session, "independent_variable1",choices = c("milieu_de_residence"), selected = NULL)
    updateSelectInput(session, "independent_variable2",choices = c("region"), selected = NULL)
    
  })
  
  # Analyse univariée
  observeEvent(input$univariate_button, {
    output$univariate_analysis <- renderPrint(NULL)
    output$histogram <- renderPlot(NULL)
    output$boxplot <- renderPlot(NULL)
    output$pie_chart <- renderPlot(NULL)
    variable_name <- input$univariate_variable
    if (is.numeric(data()[[variable_name]])){
      # Calculez les statistiques descriptives de la variable
      desc_stats <- summary(data()[[variable_name]])
      # Affichez les statistiques descriptives
      output$univariate_analysis <- renderPrint({
        req(credentials()$user_auth)
        desc_stats
      })
      output$histogram <- renderPlot({
        req(credentials()$user_auth)
        hist(data()[[variable_name]], main = "Histogramme", xlab = variable_name)
      })
      output$boxplot <- renderPlot({
        req(credentials()$user_auth)
        ggplot(data(), aes(y = data()[[variable_name]])) +
          geom_boxplot() +
          labs(title = "Boxplot", y = variable_name)
      })
    }else if(!is.numeric(data()[[variable_name]])){
      tab<-table(data()[[variable_name]])
      output$univariate_analysis <- renderPrint({
        req(credentials()$user_auth)
        tab
      })
      output$pie_chart <- renderPlot({
        req(credentials()$user_auth)
        data_filtered <- na.omit(data()[[variable_name]])
        data_filtered <- as.data.frame(data_filtered)
        ggplot(data_filtered, aes(x = "", fill = data_filtered)) +
          geom_bar(width = 1) +
          coord_polar(theta = "y") +
          labs(title = "Diagramme Camembert", fill = variable_name)
      })
    }
    
    
  })
  
  
  # Analyse bivariée
  
  observeEvent(input$bivariate_button, {
    output$scatterplot <- renderPlot(NULL)
    output$bivariate_analysis <- renderPrint(NULL)
    output$boxplot_bivariate<- renderPlot(NULL)
    variable1 <- input$bivariate_variable1
    variable2 <- input$bivariate_variable2
    
    # Effectuez les tests statistiques appropriés en fonction du type de variable
    if (is.numeric(data()[[variable1]]) && is.numeric(data()[[variable2]])) {
      reg<-lm(data()[[variable1]]~data()[[variable2]],data=data())
      # Afficher le nuage de points des deux variables
      output$scatterplot <- renderPlot({
        req(credentials()$user_auth)
        plot(data()[[variable1]], data()[[variable2]], main = "Nuage de points",
             xlab = variable1, ylab = variable2, pch = 16)
        abline(reg, col = "red")
      })
      #calcul du coefficient de correlation
      coef_corr<-cor(data()[[variable1]], data()[[variable2]])
      output$bivariate_analysis <- renderPrint({
        req(credentials()$user_auth)
        cat("le coefficient de correlation est",coef_corr)
        summary(reg)
      })
    }  else if (!is.numeric(data()[[variable1]]) && !is.numeric(data()[[variable2]])) {
      tabcroise<-table(data()[[variable1]], data()[[variable2]])
      # Effectuer le test du Chi carré
      chi_square_test <- chisq.test(table(data()[[variable1]], data()[[variable2]]),simulate.p.value = TRUE)
      fisher_test<-fisher.test(table(data()[[variable1]], data()[[variable2]]),simulate.p.value = TRUE)
      # Afficher les résultats du test du Chi carré
      output$bivariate_analysis <- renderPrint({
        cat("Tableau des pourcentages")
        print(tabcroise)
        cat("test de chi_carrée")
        print(chi_square_test)
        cat("Test de ficher")
        print(fisher_test)
      })
    } else if ((!is.numeric(data()[[variable1]]) && is.numeric(data()[[variable2]])) ||
               (is.numeric(data()[[variable1]]) && !is.numeric(data()[[variable2]]))) {
      
      
      data_filtered <- na.omit(data()[c(variable1, variable2)])
      
      # Comparaison des moyennes entre les groupes
      mean_groupe <- tapply(data_filtered[[variable1]], data_filtered[[variable2]], mean)
      
      
      output$bivariate_analysis <- renderPrint({
        cat("Moyenne des groupes:\n")
        print(mean_groupe)
        
      })
      output$boxplot_bivariate <- renderPlot({
        boxplot(data()[[variable1]] ~ data()[[variable2]], data = data(),
                main = "Boxplot", ylab = input$bivariate_variable1, xlab = input$bivariate_variable2, pch = 16)
      })
    }
    
    
    
  })
  # Modèle logistique
  observeEvent(input$logistic_regression_button, {
    dependent_variable <- input$dependent_variable
    independent_variable1 <- input$independent_variable1
    independent_variable2 <- input$independent_variable2
    
    # Ajuster le modèle de régression logistique avec les trois variables spécifiques
    formula <- as.formula(paste(dependent_variable, "~", independent_variable1, "+", independent_variable2))
    model <- glm(formula, data = data(), family = binomial)
    
    # Afficher les résultats du modèle
    output$logistic_regression_results <- renderPrint({
      req(credentials()$user_auth)
      summary(model)
    })
  })
  
  
  # Aperçu des données
  output$data_table <- DT::renderDataTable({
    req(credentials()$user_auth)
    data_without_labels <- haven::zap_labels(data())
    data_standard <- as.data.frame(data_without_labels, stringsAsFactors = FALSE)
    DT::datatable(data_standard, options = list(pageLength = 50,scrollX = TRUE))
  })
  
  
  # Créez un objet réactif contenant les données par item
  data_par_item_reactive <- reactive({
    selected_columns <- c(1, 2:8)
    data_par_item <- data()[, selected_columns]
    data_par_item
  })
  output$data_par_item <- DT::renderDataTable({
    req(credentials()$user_auth)
    data_par_item_reactive()
  },options = list(scrollX = TRUE))
  output$download_data_par_item <- downloadHandler(
    filename = function() {
      paste0("data_par_item_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data_par_item_reactive(), file, row.names = FALSE)
    }
  )
  # Créez un objet réactif contenant les données par menage
  data_par_menage_reactive <- reactive({
    selected_columns <- c(1,10,11)
    data_par_menage <- data()[, selected_columns]
    data_par_menage
  })
  output$data_par_menage <- DT::renderDataTable({
    req(credentials()$user_auth)
    data_par_menage_reactive() 
  },options = list(scrollX = TRUE))
  output$download_data_par_menage <- downloadHandler(
    filename = function() {
      paste0("data_par_menage_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data_par_menage_reactive(), file, row.names = FALSE)
    }
  )
  # Créez un objet réactif contenant les données par tête
  data_par_tete_reactive <- reactive({
    selected_columns <- c(1, 13)
    data_par_tete <- data()[, selected_columns]
    data_par_tete
  })
  output$data_par_capita <- DT::renderDataTable({
    req(credentials()$user_auth)
    data_par_tete_reactive()  
  },options = list(scrollX = TRUE))
  output$download_data_par_tete <- downloadHandler(
    filename = function() {
      paste0("data_par_tete_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data_par_tete_reactive(), file, row.names = FALSE)
    }
  )
  about_content <- "Bienvenue!.La base de données à importer est la base finale issue de l'apurement
  et du traitement. Pourl'analyse univariée selectionnée la variable dont vous voulez étudier et cliquer sur le boutton effectuer l'analyse
  .En se ce qui concerne l'analyse bivariée,selectionner les deux variables dont vous souhaitez
  faire le croisement.S'il s'agit du croisement entre une variable quantitative et une variable qualitative
  la variable1 doit être quantitative et l'autre variable qualitative.
  -Pour l'analyse économetrique ,il s'agit d'étudier la relation entre la variable depenses totale recoder et les variables regions,milieux de residence
  L'aglorithme pourrait ne pas converger dans notre cas!!!
  - Vous avez la possibilité de télécharger les données par tête,par item et par menage"
  
  # Rendu du texte dans l'output "about_text"
  output$about_text <- renderText({
    about_content
  })
  
}

shiny::shinyApp(ui, server)