library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Charger les fonctions
source("FUNCTIONS.R")

# CSS personnalisé
custom_css <- "
/* Styles généraux */
body {
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  background-color: #f8f9fa;
}

/* En-tête */
.navbar {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  border: none;
  border-radius: 0;
}

.navbar-brand {
  font-weight: bold;
  color: white !important;
  font-size: 1.5em;
}

/* Panneau latéral */
.sidebar {
  background-color: white;
  border-radius: 10px;
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  padding: 20px;
  margin: 10px;
}

.well {
  background-color: white;
  border: 1px solid #e0e0e0;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
}

/* Boutons */
.btn-primary {
  background: linear-gradient(135deg, #3498db, #2980b9);
  border: none;
  border-radius: 6px;
  font-weight: 600;
  transition: all 0.3s ease;
}

.btn-primary:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 8px rgba(52, 152, 219, 0.3);
}

.btn-success {
  background: linear-gradient(135deg, #27ae60, #229954);
  border: none;
  border-radius: 6px;
  font-weight: 600;
  transition: all 0.3s ease;
}

.btn-success:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 8px rgba(39, 174, 96, 0.3);
}

/* Onglets */
.nav-tabs > li > a {
  color: #555;
  font-weight: 600;
  border-radius: 8px 8px 0 0;
  margin-right: 5px;
}

.nav-tabs > li.active > a {
  background-color: #3498db;
  color: white;
  border: none;
}

/* Cartes de contenu */
.tab-content {
  background-color: white;
  border-radius: 0 8px 8px 8px;
  padding: 20px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
  min-height: 500px;
}

/* Indicateurs */
.help-block {
  color: #666;
  font-size: 0.9em;
}

/* Tableaux */
.dataTables_wrapper {
  border-radius: 8px;
  overflow: hidden;
}

/* Graphiques */
.plotly.html-widget {
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

/* Notifications */
.shiny-notification {
  border-radius: 8px;
  font-weight: 600;
}

/* En-têtes */
h3, h4 {
  color: #2c3e50;
  font-weight: 700;
}

h4 {
  border-bottom: 2px solid #3498db;
  padding-bottom: 5px;
  margin-top: 20px;
}

/* Icônes dans les titres */
.fa, .fas, .far {
  margin-right: 8px;
}

/* Responsive */
@media (max-width: 768px) {
  .sidebar {
    margin: 5px;
    padding: 15px;
  }
  
  .tab-content {
    padding: 15px;
    margin: 5px;
  }
}

/* Animation de chargement */
.shiny-progress-container {
  position: fixed;
  top: 0;
  width: 100%;
  z-index: 9999;
}

.shiny-progress .progress {
  height: 8px;
  margin-bottom: 0;
}

.shiny-progress .bar {
  background: linear-gradient(90deg, #3498db, #2980b9);
}

/* Amélioration des contrôles */
.form-control {
  border-radius: 6px;
  border: 1px solid #ddd;
  box-shadow: inset 0 1px 2px rgba(0,0,0,0.1);
}

.form-control:focus {
  border-color: #3498db;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
}

/* Checkboxes et radios */
.checkbox, .radio {
  margin-top: 10px;
  margin-bottom: 10px;
}

.checkbox-inline, .radio-inline {
  margin-right: 15px;
}

/* Séparateurs */
hr {
  border-top: 1px solid #e0e0e0;
  margin: 20px 0;
}

/* Badges pour les indicateurs */
.badge {
  background-color: #3498db;
  border-radius: 12px;
  padding: 4px 8px;
  font-size: 0.8em;
}
"
####################################################################################
##########################################################################################
##########################################################################################
##########################################################################################


ui <- fluidPage(
  
  # Inclusion du CSS personnalisé
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Segoe+UI:wght@300;400;600;700&display=swap")
  ),
  
  # HTML pour l'en-tête amélioré
  tags$div(class = "navbar",
           tags$div(class = "container-fluid",
                    tags$div(class = "navbar-header",
                             tags$h1(class = "navbar-brand",
                                     tags$i(class = "fas fa-chart-line"), 
                                     "📊 Analyse de la Qualité des Données Démographiques"
                             )
                    )
           )
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      
      # Upload de fichier
      tags$div(class = "well",
               tags$h4(tags$i(class = "fas fa-file-upload"), "📁 Import des données"),
               fileInput("file1", "Choisir un fichier Excel",
                         accept = c(".xlsx", ".xls"),
                         buttonLabel = "Parcourir...",
                         placeholder = "Aucun fichier sélectionné"),
               
               tags$div(class = "help-block",
                        tags$i(class = "fas fa-info-circle"),
                        "✅ Le fichier doit contenir les colonnes : AGE, Homme, Femme, Total"
               )
      ),
      
      # Sélection des indicateurs
      tags$div(class = "well",
               tags$h4(tags$i(class = "fas fa-chart-bar"), "Indicateurs à calculer"),
               checkboxGroupInput("indicateurs", "",
                                  choices = c(
                                    "Indice de Whipple" = "whipple",
                                    "Indice de Myers" = "myers", 
                                    "Indice de Bachi" = "bachi",
                                    "Indice combiné Nations Unies" = "nu"
                                  ),
                                  selected = c("whipple", "myers", "bachi", "nu"))
      ),
      
      # Paramètres pyramide
      tags$div(class = "well",
               tags$h4(tags$i(class = "fas fa-chart-pie"), "Pyramide des âges"),
               radioButtons("type_pyramide", "Type:",
                            choices = c("Âge simple" = "simple",
                                        "Groupée" = "grouped"),
                            selected = "simple"),
               
               conditionalPanel(
                 condition = "input.type_pyramide == 'grouped'",
                 sliderInput("largeur_groupe", "Largeur groupe (années):",
                             min = 1, max = 10, value = 5, step = 1)
               ),
               
               numericInput("age_max", "Âge maximum:", value = 80, min = 10, max = 120)
      ),
      
      # Boutons d'action
      tags$div(class = "well",
               tags$h4(tags$i(class = "fas fa-cogs"), "Actions"),
               actionButton("calculate", "Calculer les indicateurs", 
                            class = "btn-primary", width = "100%"),
               tags$br(), tags$br(),
               actionButton("plot_pyramid", "Générer pyramide", 
                            class = "btn-success", width = "100%")
      ),
      
      # Téléchargement
      tags$div(class = "well",
               tags$h4(tags$i(class = "fas fa-download"), "Téléchargement"),
               downloadButton("downloadResults", "💾 Télécharger résultats", 
                              class = "btn-primary", width = "100%"),
               tags$br(), tags$br(),
               downloadButton("downloadPyramid", " Télécharger pyramide", 
                              class = "btn-success", width = "100%")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          title = tags$span(tags$i(class = "fas fa-table"), "Données"),
          DTOutput("contents")
        ),
        
        tabPanel(
          title = tags$span(tags$i(class = "fas fa-calculator"), "Whipple"),
          tags$div(class = "result-card",
                   verbatimTextOutput("whipple_results"),
                   plotOutput("whipple_plot")
          )
        ),
        
        tabPanel(
          title = tags$span(tags$i(class = "fas fa-chart-line"), "Myers"),
          tags$div(class = "result-card",
                   verbatimTextOutput("myers_results"),
                   plotOutput("myers_plot")
          )
        ),
        
        tabPanel(
          title = tags$span(tags$i(class = "fas fa-bullseye"), "Bachi"),
          tags$div(class = "result-card",
                   verbatimTextOutput("bachi_results"),
                   plotOutput("bachi_plot")
          )
        ),
        
        tabPanel(
          title = tags$span(tags$i(class = "fas fa-globe-americas"), "Nations Unies"),
          tags$div(class = "result-card",
                   verbatimTextOutput("nu_results"),
                   plotOutput("nu_plot")
          )
        ),
        
        tabPanel(
          title = tags$span(tags$i(class = "fas fa-chart-bar"), "Pyramide"),
          tags$div(class = "result-card",
                   plotlyOutput("pyramid_plot"),
                   DTOutput("pyramid_data")
          )
        ),
        
        tabPanel(
          title = tags$span(tags$i(class = "fas fa-file-alt"), " Rapport complet"),
          tags$div(class = "result-card",
                   verbatimTextOutput("full_report")
          )
        )
      )
    )
  )
)

####################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

# Le serveur reste identique au code précédent
server <- function(input, output) {
  
  # Chargement des données
  data <- reactive({
    req(input$file1)
    
    tryCatch({
      df <- read_excel(input$file1$datapath)
      return(df)
    }, error = function(e) {
      showNotification("Erreur de chargement du fichier", type = "error")
      return(NULL)
    })
  })
  
  # Affichage des données
  output$contents <- renderDT({
    req(data())
    datatable(data(), 
              options = list(scrollX = TRUE, pageLength = 10),
              caption = "Données chargées")
  })
  
  # Préparation des données complètes
  donnees_completes <- reactive({
    req(data())
    
    df_complet <- data.frame(age = 0:99) %>%
      left_join(data() %>% rename(age = AGE), by = "age") %>%
      mutate(
        Homme = ifelse(is.na(Homme), 0, Homme),
        Femme = ifelse(is.na(Femme), 0, Femme),
        Total = ifelse(is.na(Total), 0, Total)
      )
    
    return(df_complet)
  })
  
  # Données pour pyramide
  pyramid_data <- reactive({
    req(donnees_completes(), input$age_max)
    
    df <- donnees_completes() %>%
      filter(age <= input$age_max)
    
    if(input$type_pyramide == "grouped") {
      largeur_groupe <- input$largeur_groupe
      df <- df %>%
        mutate(age_group = cut(age, 
                               breaks = seq(0, max(age) + largeur_groupe, largeur_groupe),
                               right = FALSE,
                               labels = paste0(seq(0, max(age), largeur_groupe), 
                                               "-", 
                                               seq(largeur_groupe - 1, max(age) + largeur_groupe - 1, largeur_groupe)))) %>%
        group_by(age_group) %>%
        summarise(Homme = sum(Homme, na.rm = TRUE),
                  Femme = sum(Femme, na.rm = TRUE)) %>%
        rename(age = age_group)
    } else {
      df <- df %>%
        select(age, Homme, Femme)
    }
    
    # Préparation pour pyramide
    df_long <- df %>%
      pivot_longer(cols = c(Homme, Femme), 
                   names_to = "Sexe", 
                   values_to = "Effectif") %>%
      mutate(Effectif = ifelse(Sexe == "Homme", -Effectif, Effectif))
    
    return(df_long)
  })
  
  # Création pyramide
  create_pyramid <- function() {
    req(pyramid_data())
    
    data_plot <- pyramid_data()
    
    p <- ggplot(data_plot, aes(x = age, y = Effectif, fill = Sexe, 
                               text = paste("Âge:", age, "<br>Effectif:", abs(Effectif)))) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.8) +
      scale_fill_manual(values = c("Homme" = "#3498db", "Femme" = "#e74c3c"),
                        labels = c("Homme" = "Hommes", "Femme" = "Femmes")) +
      coord_flip() +
      labs(title = paste("Pyramide des âges -", 
                         ifelse(input$type_pyramide == "simple", "Âge simple", "Groupée")),
           x = "Âge", y = "Effectif", fill = "Sexe") +
      scale_y_continuous(labels = function(x) format(abs(x), big.mark = " ")) +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.3, y = -0.1))
  }
  
  # Affichage pyramide
  output$pyramid_plot <- renderPlotly({
    req(input$plot_pyramid)
    create_pyramid()
  })
  
  # Données pyramide
  output$pyramid_data <- renderDT({
    req(pyramid_data())
    
    data_table <- pyramid_data() %>%
      mutate(Effectif = abs(Effectif)) %>%
      pivot_wider(names_from = Sexe, values_from = Effectif)
    
    datatable(data_table, 
              options = list(scrollX = TRUE, pageLength = 10),
              caption = "Données de la pyramide des âges")
  })
  
  # Calcul indicateurs
  results <- eventReactive(input$calculate, {
    req(data(), donnees_completes())
    
    showNotification("Calcul en cours...", type = "message")
    
    results_list <- list()
    
    # Whipple
    if("whipple" %in% input$indicateurs) {
      results_list$whipple <- indice_whipple(data())
    }
    
    # Myers
    if("myers" %in% input$indicateurs) {
      results_list$myers <- indice_myers(data())
    }
    
    # Bachi
    if("bachi" %in% input$indicateurs) {
      results_list$bachi <- indice_bachi(donnees_completes()$Homme, donnees_completes()$Femme)
    }
    
    # Nations Unies
    if("nu" %in% input$indicateurs) {
      # Préparer données quinquennales
      groupes <- data.frame(
        age = 0:99,
        groupe = cut(0:99, breaks = seq(0, 100, 5), right = FALSE, labels = FALSE)
      ) %>% filter(age < 75)
      
      pop_m_quinquenal <- groupes %>%
        left_join(donnees_completes(), by = "age") %>%
        group_by(groupe) %>%
        summarise(hommes = sum(Homme, na.rm = TRUE)) %>%
        pull(hommes)
      
      pop_f_quinquenal <- groupes %>%
        left_join(donnees_completes(), by = "age") %>%
        group_by(groupe) %>%
        summarise(femmes = sum(Femme, na.rm = TRUE)) %>%
        pull(femmes)
      
      taille_pop <- sum(pop_m_quinquenal) + sum(pop_f_quinquenal)
      
      results_list$nu <- indice_combine_nu(pop_m_quinquenal, pop_f_quinquenal, taille_pop)
    }
    
    showNotification("Calcul terminé !", type = "message")
    return(results_list)
  })
  
  # Affichage résultats Whipple
  output$whipple_results <- renderPrint({
    req(results()$whipple)
    
    cat("=== INDICE DE WHIPPLE ===\n\n")
    cat("Homme    :", round(results()$whipple$homme, 3), "\n")
    cat("Femme    :", round(results()$whipple$femme, 3), "\n")
    cat("Ensemble :", round(results()$whipple$ensemble, 3), "\n\n")
    
    cat("📊 Interprétation:\n")
    cat("• 1.000 = Aucune attraction/répulsion\n")
    cat("• 5.000 = Tous les âges terminent par 0 ou 5\n")
    cat("• <1.000 = Répulsion pour ces âges\n")
  })
  
  # Graphique Whipple
  output$whipple_plot <- renderPlot({
    req(results()$whipple)
    
    df <- data.frame(
      Sexe = c("Homme", "Femme", "Ensemble"),
      Valeur = c(results()$whipple$homme, results()$whipple$femme, results()$whipple$ensemble)
    )
    
    ggplot(df, aes(x = Sexe, y = Valeur, fill = Sexe)) +
      geom_col(alpha = 0.8) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      labs(title = "Indice de Whipple par sexe", y = "Valeur de l'indice") +
      scale_fill_manual(values = c("Homme" = "#3498db", "Femme" = "#e74c3c", "Ensemble" = "#2ecc71")) +
      theme_minimal()
  })
  
  # Affichage résultats Myers
  output$myers_results <- renderPrint({
    req(results()$myers)
    
    cat("=== INDICE DE MYERS ===\n\n")
    
    print_myers <- function(nom, data) {
      cat(nom, ":\n", sep = "")
      cat("   Indice :", round(data$indice, 3), "\n")
      cat("   Tu     :", round(data$Tu, 1), "\n\n")
    }
    
    print_myers("HOMME", results()$myers$homme)
    print_myers("FEMME", results()$myers$femme)
    print_myers("ENSEMBLE", results()$myers$ensemble)
    
    cat("Interprétation:\n")
    cat("• ≈0  = Déclarations d'âge exactes\n")
    cat("• >0  = Préférences pour certains chiffres\n")
    cat("• 180 = Maximum (un seul chiffre préféré)\n")
  })
  
  # Graphique Myers
  output$myers_plot <- renderPlot({
    req(results()$myers)
    
    df <- data.frame(
      Sexe = c("Homme", "Femme", "Ensemble"),
      Valeur = c(results()$myers$homme$indice, results()$myers$femme$indice, results()$myers$ensemble$indice)
    )
    
    ggplot(df, aes(x = Sexe, y = Valeur, fill = Sexe)) +
      geom_col(alpha = 0.8) +
      labs(title = "Indice de Myers par sexe", y = "Valeur de l'indice") +
      scale_fill_manual(values = c("Homme" = "#3498db", "Femme" = "#e74c3c", "Ensemble" = "#2ecc71")) +
      theme_minimal()
  })
  
  # Affichage résultats Bachi
  output$bachi_results <- renderPrint({
    req(results()$bachi)
    
    cat("=== INDICE DE BACHI ===\n\n")
    
    print_bachi <- function(nom, data) {
      cat(nom, ":\n", sep = "")
      cat("   Indice :", round(data$indice, 3), "\n")
      cat("   ru (%) :", round(data$ru, 1), "\n\n")
    }
    
    print_bachi("MASCULIN", results()$bachi$masculin)
    print_bachi("FÉMININ", results()$bachi$feminin)
  })
  
  # Graphique Bachi
  output$bachi_plot <- renderPlot({
    req(results()$bachi)
    
    df <- data.frame(
      Sexe = rep(c("Homme", "Femme"), each = 10),
      Chiffre = rep(0:9, 2),
      Pourcentage = c(results()$bachi$masculin$ru, results()$bachi$feminin$ru)
    )
    
    ggplot(df, aes(x = factor(Chiffre), y = Pourcentage, fill = Sexe)) +
      geom_col(position = "dodge", alpha = 0.8) +
      geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
      labs(title = "Distribution des chiffres terminaux - Indice de Bachi",
           x = "Chiffre terminal", y = "Pourcentage (%)") +
      scale_fill_manual(values = c("Homme" = "#3498db", "Femme" = "#e74c3c")) +
      theme_minimal()
  })
  
  # Affichage résultats Nations Unies
  output$nu_results <- renderPrint({
    req(results()$nu)
    
    cat("=== INDICE COMBINÉ DES NATIONS UNIES ===\n\n")
    cat("Indice brut (I_brut) :", round(results()$nu$I_brut, 2), "\n")
    cat("Indice net (I_net)   :", round(results()$nu$I_net, 2), "\n")
    cat("J_m (irrégularité M) :", round(results()$nu$J_m, 2), "\n")
    cat("J_f (irrégularité F) :", round(results()$nu$J_f, 2), "\n")
    cat("K (variation masc.)  :", round(results()$nu$K, 2), "\n")
    if(!is.na(results()$nu$S_correction)) {
      cat("Correction S         :", round(results()$nu$S_correction, 2), "\n")
    }
    
    cat("\n QUALITÉ DES DONNÉES:\n")
    net <- results()$nu$I_net
    if (net < 20) {
      cat("EXCELLENTE qualité (indice < 20)\n")
    } else if (net < 40) {
      cat("BONNE qualité (indice 20-40)\n")
    } else if (net < 60) {
      cat("Qualité ACCEPTABLE (indice 40-60)\n")
    } else if (net < 80) {
      cat("Qualité MÉDIOCRE (indice 60-80)\n")
    } else {
      cat("TRÈS MAUVAISE qualité (indice > 80)\n")
    }
  })
  
  # Graphique Nations Unies
  output$nu_plot <- renderPlot({
    req(results()$nu)
    
    df <- data.frame(
      Composante = c("J_m", "J_f", "3×K"),
      Valeur = c(results()$nu$J_m, results()$nu$J_f, 3 * results()$nu$K)
    )
    
    ggplot(df, aes(x = Composante, y = Valeur, fill = Composante)) +
      geom_col(alpha = 0.8) +
      labs(title = "Composition de l'indice Nations Unies", y = "Valeur") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()
  })
  
  # Rapport complet
  output$full_report <- renderPrint({
    req(results())
    
    cat("=== RAPPORT COMPLET - QUALITÉ DES DONNÉES ===\n\n")
    cat("Date :", format(Sys.Date(), "%d/%m/%Y"), "\n")
    cat("Fichier :", input$file1$name, "\n\n")
    
    # Whipple
    if(!is.null(results()$whipple)) {
      cat("1. INDICE DE WHIPPLE:\n")
      cat("   • Homme    :", round(results()$whipple$homme, 3), "\n")
      cat("   • Femme    :", round(results()$whipple$femme, 3), "\n")
      cat("   • Ensemble :", round(results()$whipple$ensemble, 3), "\n\n")
    }
    
    # Myers
    if(!is.null(results()$myers)) {
      cat("2. INDICE DE MYERS:\n")
      cat("   • Homme    :", round(results()$myers$homme$indice, 3), "\n")
      cat("   • Femme    :", round(results()$myers$femme$indice, 3), "\n")
      cat("   • Ensemble :", round(results()$myers$ensemble$indice, 3), "\n\n")
    }
    
    # Bachi
    if(!is.null(results()$bachi)) {
      cat("3. INDICE DE BACHI:\n")
      cat("   • Masculin :", round(results()$bachi$masculin$indice, 3), "\n")
      cat("   • Féminin  :", round(results()$bachi$feminin$indice, 3), "\n\n")
    }
    
    # Nations Unies
    if(!is.null(results()$nu)) {
      cat("4. INDICE COMBINÉ NATIONS UNIES:\n")
      cat("   • Indice net :", round(results()$nu$I_net, 2), "\n")
      cat("   • Qualité    : ")
      net <- results()$nu$I_net
      if (net < 20) cat("EXCELLENTE\n")
      else if (net < 40) cat("BONNE\n")
      else if (net < 60) cat("ACCEPTABLE\n")
      else if (net < 80) cat("MÉDIOCRE\n")
      else cat("TRÈS MAUVAISE\n")
    }
    
    cat("\n--- FIN DU RAPPORT ---\n")
  })
  
  # Téléchargement résultats
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("resultats-qualite-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(capture.output({
        cat("RAPPORT D'ANALYSE DE LA QUALITÉ DES DONNÉES DÉMOGRAPHIQUES\n")
        cat("===========================================================\n\n")
        print(output$full_report())
      }), file)
    }
  )
  
  # Téléchargement pyramide
  output$downloadPyramid <- downloadHandler(
    filename = function() {
      paste("pyramide-ages-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      htmlwidgets::saveWidget(create_pyramid(), file)
    }
  )
}

shinyApp(ui = ui, server = server)