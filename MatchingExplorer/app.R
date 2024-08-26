library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(shinythemes)

# charge les données
data_2024_1 <- read.csv(file = "simulation-tour-1.csv", header = F)
data_2024_2 <- read.csv(file = "simulation-tour-2.csv", header = F)
data_2024_3 <- read.csv(file = "simulation-tour-3.csv", header = F)
data_2024_4 <- read.csv(file = "simulation-tour-4.csv", header = F)
data_2024_5 <- read.csv(file = "simulation-tour-5.csv", header = F)
data_2024_6 <- read.csv(file = "simulation-tour-6.csv", header = F)
data_2024_blanc_1 <- read.csv(file = "simulation-tour-blanc-1.csv", header = F)
data_2023 <- read.csv(file = "rang-limites-2023.csv", header = F)

nb_poste_2024 = 7689
nb_poste_2023 = 9232

# mise en forme des données
clean_data_2024 <- function(data) {
  names(data) <- c("Groupe", "Spécialité", "Ville", "Disponible", "Pourvu", "Rangs")
  return(data %>%
           mutate(Groupe = NULL) %>%
           mutate(SpécialitéShort = case_when(
             Spécialité == "Neurochirurgie" ~ "NCU",
             Spécialité == "Oto-rhino-laryngologie - chirurgie cervico-faciale" ~ "ORL",
             Spécialité == "Ophtalmologie" ~ "OPH",
             Spécialité == "Chirurgie plastique, reconstructrice et esthétique" ~ "CPR",
             Spécialité == "Chirurgie maxillo-faciale" ~ "CMF",
             Spécialité == "Chirurgie orale" ~ "COR",
             Spécialité == "Urologie" ~ "URO",
             Spécialité == "Gynécologie obstétrique" ~ "GYO",
             Spécialité == "Chirurgie viscérale et digestive" ~ "CVD",
             Spécialité == "Chirurgie vasculaire" ~ "CVA",
             Spécialité == "Chirurgie thoracique et cardiovasculaire" ~ "CTC",
             Spécialité == "Chirurgie pédiatrique" ~ "CPD",
             Spécialité == "Chirurgie orthopédique et traumatologique" ~ "COT",
             Spécialité == "Médecine d’urgence" ~ "MUR",
             Spécialité == "Médecine intensive-réanimation" ~ "MIR",
             Spécialité == "Anesthésie-réanimation" ~ "ARE",
             Spécialité == "Anatomie et cytologie pathologiques" ~ "ACP",
             Spécialité == "Radiologie et imagerie médicale" ~ "RAI",
             Spécialité == "Médecine nucléaire" ~ "NUC",
             Spécialité == "Génétique médicale" ~ "GEN",
             Spécialité == "Biologie médicale" ~ "BM",
             Spécialité == "Médecine et santé au travail" ~ "MTR",
             Spécialité == "Médecine légale et expertises médicales" ~ "MLE",
             Spécialité == "Santé publique" ~ "SPU",
             Spécialité == "Gynécologie médicale" ~ "GYM",
             Spécialité == "Endocrinologie-diabétologie-nutrition" ~ "EDN",
             Spécialité == "Médecine interne et immunologie clinique" ~ "MII",
             Spécialité == "Maladies infectieuses et tropicales" ~ "MIT",
             Spécialité == "Gériatrie" ~ "GER",
             Spécialité == "Dermatologie et vénéréologie" ~ "DVE",
             Spécialité == "Allergologie" ~ "ALL",
             Spécialité == "Rhumatologie" ~ "RHU",
             Spécialité == "Neurologie" ~ "NEU",
             Spécialité == "Psychiatrie" ~ "PSY",
             Spécialité == "Médecine physique et de réadaptation" ~ "MPR",
             Spécialité == "Hépato-gastro-entérologie" ~ "HGE",
             Spécialité == "Hématologie" ~ "HEM",
             Spécialité == "Oncologie" ~ "ONC",
             Spécialité == "Pédiatrie" ~ "PED",
             Spécialité == "Médecine vasculaire" ~ "MVA",
             Spécialité == "Médecine cardiovasculaire" ~ "MCA",
             Spécialité == "Néphrologie" ~ "NEP",
             Spécialité == "Pneumologie" ~ "PNE",
             Spécialité == "Médecine générale" ~ "MGE"
           )) %>%
           mutate(VilleShort = case_when(
             Ville == "AMIENS" ~ "AMI",
             Ville == "ANGERS" ~ "ANG",
             Ville == "AP-HM" ~ "MAR",
             Ville == "AP-HP" ~ "PAR",
             Ville == "BESANCON" ~ "BES",
             Ville == "BORDEAUX" ~ "BOR",
             Ville == "BREST" ~ "BRE",
             Ville == "CAEN" ~ "CAE",
             Ville == "CLERMONT-FERRAND" ~ "CLF",
             Ville == "DIJON" ~ "DIJ",
             Ville == "GRENOBLE" ~ "GRE",
             Ville == "HCL" ~ "HCL",
             Ville == "LA REUNION" ~ "LRE",
             Ville == "LILLE" ~ "LIL",
             Ville == "LIMOGES" ~ "LIM",
             Ville == "MARTINIQUE / GUADELOUPE" ~ "M/G",
             Ville == "MONTPELLIER" ~ "MON",
             Ville == "NANCY" ~ "NAY",
             Ville == "NANTES" ~ "NAT",
             Ville == "NICE" ~ "NIC",
             Ville == "POITIERS" ~ "POI",
             Ville == "REIMS" ~ "REI",
             Ville == "RENNES" ~ "REN",
             Ville == "ROUEN" ~ "ROU",
             Ville == "SAINT-ETIENNE" ~ "SE",
             Ville == "STRASBOURG" ~ "STR",
             Ville == "TOULOUSE" ~ "TOL",
             Ville == "TOURS" ~ "TOR",
           )) %>%
           rowwise() %>%
           mutate(RangSplit = strsplit(Rangs, " - ")) %>%
           mutate(RangPremier = ifelse(length(RangSplit) == 0, NA, RangSplit[1])) %>%
           mutate(RangPremier = as.numeric(RangPremier)) %>%
           mutate(RangLimite = ifelse(length(RangSplit) == 2, RangSplit[2], RangSplit)) %>%
           mutate(RangLimite = as.numeric(RangLimite)) %>%
           # mutate(RangLimite = ifelse(Disponible != 0, nb_poste_2024, RangLimite)) %>%
           mutate(Rangs = NULL) %>%
           mutate(RangSplit = NULL) %>%
           mutate(Total = Disponible + Pourvu))
}

names(data_2023) <- c("Spécialité", "Ville", "Total", "Disponible", "RangPremier", "RangLimite")

data_2023_clean <- data_2023 %>%
  mutate(SpécialitéShort = case_when(
    Spécialité == "Neurochirurgie" ~ "NCU",
    Spécialité == "Oto-rhino-laryngologie - chirurgie cervico-faciale" ~ "ORL",
    Spécialité == "Ophtalmologie" ~ "OPH",
    Spécialité == "Chirurgie plastique, reconstructrice et esthétique" ~ "CPR",
    Spécialité == "Chirurgie maxillo-faciale" ~ "CMF",
    Spécialité == "Chirurgie orale" ~ "COR",
    Spécialité == "Urologie" ~ "URO",
    Spécialité == "Gynécologie obstétrique" ~ "GYO",
    Spécialité == "Chirurgie viscérale et digestive" ~ "CVD",
    Spécialité == "Chirurgie vasculaire" ~ "CVA",
    Spécialité == "Chirurgie thoracique et cardiovasculaire" ~ "CTC",
    Spécialité == "Chirurgie pédiatrique" ~ "CPD",
    Spécialité == "Chirurgie orthopédique et traumatologique" ~ "COT",
    Spécialité == "Médecine d’urgence" ~ "MUR",
    Spécialité == "Médecine intensive-réanimation" ~ "MIR",
    Spécialité == "Anesthésie-réanimation" ~ "ARE",
    Spécialité == "Anatomie et cytologie pathologiques" ~ "ACP",
    Spécialité == "Radiologie et imagerie médicale" ~ "RAI",
    Spécialité == "Médecine nucléaire" ~ "NUC",
    Spécialité == "Génétique médicale" ~ "GEN",
    Spécialité == "Biologie médicale" ~ "BM",
    Spécialité == "Médecine et santé au travail" ~ "MTR",
    Spécialité == "Médecine légale et expertises médicales" ~ "MLE",
    Spécialité == "Santé publique" ~ "SPU",
    Spécialité == "Gynécologie médicale" ~ "GYM",
    Spécialité == "Endocrinologie-diabétologie-nutrition" ~ "EDN",
    Spécialité == "Médecine interne et immunologie clinique" ~ "MII",
    Spécialité == "Maladies infectieuses et tropicales" ~ "MIT",
    Spécialité == "Gériatrie" ~ "GER",
    Spécialité == "Dermatologie et vénéréologie" ~ "DVE",
    Spécialité == "Allergologie" ~ "ALL",
    Spécialité == "Rhumatologie" ~ "RHU",
    Spécialité == "Neurologie" ~ "NEU",
    Spécialité == "Psychiatrie" ~ "PSY",
    Spécialité == "Médecine physique et de réadaptation" ~ "MPR",
    Spécialité == "Hépato-gastro-entérologie" ~ "HGE",
    Spécialité == "Hématologie" ~ "HEM",
    Spécialité == "Oncologie" ~ "ONC",
    Spécialité == "Pédiatrie" ~ "PED",
    Spécialité == "Médecine vasculaire" ~ "MVA",
    Spécialité == "Médecine cardiovasculaire" ~ "MCA",
    Spécialité == "Néphrologie" ~ "NEP",
    Spécialité == "Pneumologie" ~ "PNE",
    Spécialité == "Médecine générale" ~ "MGE"
  )) %>%
  mutate(Ville = ifelse(Ville == "GUADELOUPE", "MARTINIQUE / GUADELOUPE", Ville)) %>%
  mutate(Ville = ifelse(Ville == (data_2023 %>% filter(grepl("REUNION", Ville)))$Ville[1], "LA REUNION", Ville)) %>%
  mutate(VilleShort = case_when(
    Ville == "AMIENS" ~ "AMI",
    Ville == "ANGERS" ~ "ANG",
    Ville == "AP-HM" ~ "MAR",
    Ville == "AP-HP" ~ "PAR",
    Ville == "BESANCON" ~ "BES",
    Ville == "BORDEAUX" ~ "BOR",
    Ville == "BREST" ~ "BRE",
    Ville == "CAEN" ~ "CAE",
    Ville == "CLERMONT-FERRAND" ~ "CLF",
    Ville == "DIJON" ~ "DIJ",
    Ville == "GRENOBLE" ~ "GRE",
    Ville == "HCL" ~ "HCL",
    Ville == "LA REUNION" ~ "LRE",
    Ville == "LILLE" ~ "LIL",
    Ville == "LIMOGES" ~ "LIM",
    Ville == "MARTINIQUE / GUADELOUPE" ~ "M/G",
    Ville == "MONTPELLIER" ~ "MON",
    Ville == "NANCY" ~ "NAY",
    Ville == "NANTES" ~ "NAT",
    Ville == "NICE" ~ "NIC",
    Ville == "POITIERS" ~ "POI",
    Ville == "REIMS" ~ "REI",
    Ville == "RENNES" ~ "REN",
    Ville == "ROUEN" ~ "ROU",
    Ville == "SAINT ETIENNE" ~ "SE",
    Ville == "STRASBOURG" ~ "STR",
    Ville == "TOULOUSE" ~ "TOL",
    Ville == "TOURS" ~ "TOR",
  )) %>%
  mutate(Disponible = ifelse(is.na(Disponible), 0, Disponible)) %>%
  mutate(Tour = 2023) %>%
  mutate(Pourvu = Total - Disponible) %>%
  relocate(Pourvu, .after = Disponible) %>%
  relocate(Total, .after = RangLimite) %>%
  relocate(SpécialitéShort, .after = Pourvu) %>%
  relocate(VilleShort, .after = SpécialitéShort)

data_2024_1_clean <- clean_data_2024(data_2024_1) %>%
  mutate(Tour = 1)

data_2024_2_clean <- clean_data_2024(data_2024_2) %>%
  mutate(Tour = 2)

data_2024_3_clean <- clean_data_2024(data_2024_3) %>%
  mutate(Tour = 3)

data_2024_4_clean <- clean_data_2024(data_2024_4) %>%
  mutate(Tour = 4)

data_2024_5_clean <- clean_data_2024(data_2024_5) %>%
  mutate(Tour = 5)

data_2024_6_clean <- clean_data_2024(data_2024_6) %>%
  mutate(Tour = 6)

data_2024_blanc_1_clean <- clean_data_2024(data_2024_blanc_1) %>%
  mutate(Tour = 7)

data_main <- rbind(data_2023_clean, 
                   data_2024_1_clean, 
                   data_2024_2_clean, 
                   data_2024_3_clean, 
                   data_2024_4_clean,
                   data_2024_5_clean,
                   data_2024_6_clean,
                   data_2024_blanc_1_clean)

# prépare des listes pour les codes de villes et spécialités
specialties <- sort(c(
    "Neurochirurgie" = "NCU",
    "Oto-rhino-laryngologie - chirurgie cervico-faciale" = "ORL",
    "Ophtalmologie" = "OPH",
    "Chirurgie plastique, reconstructrice et esthétique" = "CPR",
    "Chirurgie maxillo-faciale" = "CMF",
    "Chirurgie orale" = "COR",
    "Urologie" = "URO",
    "Gynécologie obstétrique" = "GYO",
    "Chirurgie viscérale et digestive" = "CVD",
    "Chirurgie vasculaire" = "CVA",
    "Chirurgie thoracique et cardiovasculaire" = "CTC",
    "Chirurgie pédiatrique" = "CPD",
    "Chirurgie orthopédique et traumatologique" = "COT",
    "Médecine d’urgence" = "MUR",
    "Médecine intensive-réanimation" = "MIR",
    "Anesthésie-réanimation" = "ARE",
    "Anatomie et cytologie pathologiques" = "ACP",
    "Radiologie et imagerie médicale" = "RAI",
    "Médecine nucléaire" = "NUC",
    "Génétique médicale" = "GEN",
    "Biologie médicale" = "BM",
    "Médecine et santé au travail" = "MTR",
    "Médecine légale et expertises médicales" = "MLE",
    "Santé publique" = "SPU",
    "Gynécologie médicale" = "GYM",
    "Endocrinologie-diabétologie-nutrition" = "EDN",
    "Médecine interne et immunologie clinique" = "MII",
    "Maladies infectieuses et tropicales" = "MIT",
    "Gériatrie" = "GER",
    "Dermatologie et vénéréologie" = "DVE",
    "Allergologie" = "ALL",
    "Rhumatologie" = "RHU",
    "Neurologie" = "NEU",
    "Psychiatrie" = "PSY",
    "Médecine physique et de réadaptation" = "MPR",
    "Hépato-gastro-entérologie" = "HGE",
    "Hématologie" = "HEM",
    "Oncologie" = "ONC",
    "Pédiatrie" = "PED",
    "Médecine vasculaire" = "MVA",
    "Médecine cardiovasculaire" = "MCA",
    "Néphrologie" = "NEP",
    "Pneumologie" = "PNE",
    "Médecine générale" = "MGE"
))

cities <- sort(c(
  "AMIENS" = "AMI",
  "ANGERS" = "ANG",
  "AP-HM" = "MAR",
  "AP-HP" = "PAR",
  "BESANCON" = "BES",
  "BORDEAUX" = "BOR",
  "BREST" = "BRE",
  "CAEN" = "CAE",
  "CLERMONT-FERRAND" = "CLF",
  "DIJON" = "DIJ",
  "GRENOBLE" = "GRE",
  "HCL" = "HCL",
  "LA REUNION" = "LRE",
  "LILLE" = "LIL",
  "LIMOGES" = "LIM",
  "MARTINIQUE / GUADELOUPE" = "M/G",
  "MONTPELLIER" = "MON",
  "NANCY" = "NAY",
  "NANTES" = "NAT",
  "NICE" = "NIC",
  "POITIERS" = "POI",
  "REIMS" = "REI",
  "RENNES" = "REN",
  "ROUEN" = "ROU",
  "SAINT ETIENNE" = "SE",
  "STRASBOURG" = "STR",
  "TOULOUSE" = "TOL",
  "TOURS" = "TOR"
))


# mise en forme de l'interface
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  navbarPage("Matching Explorer 2024",
             tabPanel("Exploration",
                      sidebarLayout(
                        # barre latérale pour choisir les données à afficher
                        sidebarPanel(
                          selectInput("dataset_reference", "Choisis un tour de référence",
                                      choices = list("2024 Blanc 1" = "7",
                                                     "2024 Simulation 1" = "1",
                                                     "2024 Simulation 2" = "2",
                                                     "2024 Simulation 3" = "3",
                                                     "2024 Simulation 4" = "4",
                                                     "2024 Simulation 5" = "5",
                                                     "2024 Simulation 6" = "6",
                                                     "2023 Affectation réelle" = "2023")),
                          
                          selectInput("specialty", "Choisis une spécialité",
                                      choices = specialties),
                          
                          selectInput("city", "Choisis une ville",
                                      choices = cities),
                          selectInput("dataset_compared", "Choisis un tour pour comparer",
                                      choices = list("2024 Blanc 1" = "7",
                                                     "2024 Simulation 1" = "1",
                                                     "2024 Simulation 2" = "2",
                                                     "2024 Simulation 3" = "3",
                                                     "2024 Simulation 4" = "4",
                                                     "2024 Simulation 5" = "5",
                                                     "2024 Simulation 6" = "6",
                                                     "2023 Affectation réelle" = "2023")),
                          numericInput("rangECN2024", "Entre ton rang des ECN 2024", value = 1),
                          actionButton("computeR2C", "Calcule ton rang R2C")
                        ),
                        
                        mainPanel(
                          # affiche les chiffres clés sur le poste
                          fluidRow(
                            valueBoxOutput("valueTotal", width = 4),
                            valueBoxOutput("valuePourvu", width = 4),
                            valueBoxOutput("valueDisponible", width = 4)
                          ),
                          fluidRow(
                            valueBoxOutput("valueRangLimite", width = 6),
                            # valueBoxOutput("valueRangLimiteAdapté", width = 6)
                          ),
                          tabsetPanel(
                            # affiche des graphiques et tables en lien avec la spécialité choisie
                            tabPanel("Spécialité", 
                                     plotOutput("plot1_specialty"),
                                     DT::dataTableOutput("table_specialty"),
                                     plotOutput("plot2_specialty")
                            ),
                            # affiche des graphiques et tables en lien avec la ville choisie
                            tabPanel("Ville", 
                                     plotOutput("plot1_city"),
                                     DT::dataTableOutput("table_city"),
                                     plotOutput("plot2_city")
                            ),
                            # affiche des graphiques et tables pour montrer l'évolution au fil des tours de simulation
                            tabPanel("Évolution",
                                     plotOutput("plot1_evolution"),
                                     DT::dataTableOutput("table1_evolution"),
                                     plotOutput("plot2_evolution"),
                                     DT::dataTableOutput("table2_evolution")),
                            # affiche les données brutes du tour de référence
                            tabPanel("Données brutes",
                                     DT::dataTableOutput("table1_raw"))
                          )
                        )
                      )),
             # donne des détails sur le projet
             tabPanel("À propos",
                      h2("Crédits"),
                      p(style = "text-align: justify;",
                        HTML("Application ShinyApp codée avec l'aide de ChatGPT sur RStudio avec les packages <code>dplyr</code> et <code>ggplot2</code>. Vous pouvez trouver l'intégralité du code sur <a href='https://github.com/NekoSama8723/matching-explorer-2024' target='_blank'>GitHub</a>.")
                      ),
                      
                      p(style = "text-align: justify;",
                        HTML("J'écris du contenu en lien avec les études de médecine sur <a href='https://picat.fr/teaching/teaching-main.html' target='_blank'>mon site internet</a> et j'ai <a href='https://buymeacoffee.com/leopicat' target='_blank'>un lien BuyMeACoffee</a> si vous voulez encourager ma procrastination.")
                      ),
                      
                      h2("Mise à jour"),
                      p(style = "text-align: justify;",
                        "Dernière mise à jour le vendredi 16 août 2024."
                        ),
                      
                      h2("Helth"),
                      p(style = "text-align: justify;",
                        HTML("La période actuelle est particulièrement difficile pour certain·e·s. N'hésitez pas à demander de l'aide si vous en ressenter le besoin. <a href='https://www.nightline.fr/soutien-etudiant' target='_blank'>Ce site</a> recense un certain nombre de service gratuit.")
                        ),
                      
                      h2("Méthodologie"),
                      p(style = "text-align: justify;",
                        "Si il reste des places non pourvues, le rang limite n'est pas interprétable. Dans les versions précédentes, pour les postes non pourvus, je fixais le rang limite au nombre de place disponible (7689). Cependant, l'application du CNG affiche des rangs limites supérieurs à ce nombre. Je ne sais pas trop à quoi c'est dû (peut-être les CESP ?). Dans le doute, je n'effectue plus cette correction."
                        ),
                      p(style = "text-align: justify;",
                        HTML("Les rangs 2023 sont difficilement comparables avec ceux de cette année pour au moins 2 raisons :
                        <ul>
                          <li>il y a eu une réduction des postes ;</li>
                          <li>les classements 2024 incluent des coefficients et ne sont donc pas directement transposables en classement 2023.</li>
                      </ul>")
                      ),
                      p(style = "text-align: justify;",
                        "J'ai donc utilisé un produit en croix pour adapter les rangs limites de 2023 (en utilisant le pourcentage de réductions pour chaque poste et non le pourcentage global). Je fais par conséquent l'hypothèse que les étudiant·e·s manquants se seraient équitablement réparti·e·s dans notre classement (hypothèse sûrement un peu fausse qui contribue à surestimer les rangs limites 2023). Une autre technique serait de prendre le rang du x-ième pris en 2023 (x étant le nombre de postes en 2024). Elle n'est pas implémentée pour le moment. Cela conduirait peut-être à une sous-estimation des rangs limites."
                        ),
                      
                      h2("Warning"),
                      p(style = "text-align: justify;",
                        "Il y a potentiellement des erreurs de codage, que ce soit dans la récupération, le traitement, ou l'affichage des données. La source la plus fiable reste l'application du CNG. Ne vous basez pas sur cette application si vous participez au matching 2024 !"
                        )
)))

# remplit les données de l'interface
server <- function(input, output, session) {
  # sélectionne les tours voulus
  datasetInput <- reactive({
    data_main %>% filter(Tour == input$dataset_reference)
  })
  
  datasetInputCompared <- reactive({
    data_main %>% filter(Tour == input$dataset_compared)
  })
  
  # remplit les chiffres clés
  output$valueTotal <- renderValueBox({
    data <- datasetInput()
    total <- (data %>% filter(VilleShort == input$city & SpécialitéShort == input$specialty))$Total
    valueBox(value = total, 
             subtitle = "Total")
  })
  
  output$valuePourvu <- renderValueBox({
    data <- datasetInput()
    pourvu <- (data %>% filter(VilleShort == input$city & SpécialitéShort == input$specialty))$Pourvu
    valueBox(value = pourvu, 
             subtitle = "Pourvus")
  })
  
  output$valueDisponible <- renderValueBox({
    data <- datasetInput()
    dispo <- (data %>% filter(VilleShort == input$city & SpécialitéShort == input$specialty))$Disponible
    valueBox(value = dispo, 
             subtitle = "Disponibles")
  })
  
  output$valueRangLimite <- renderValueBox({
    data <- datasetInput()
    rangLimite <- (data %>% filter(VilleShort == input$city & SpécialitéShort == input$specialty))$RangLimite
    valueBox(value = rangLimite,
             subtitle = "Rang limite")
  })
  
  output$valueRangLimiteAdapté <- renderValueBox({
    data <- datasetInput()
    rangLimite <- (data %>% filter(VilleShort == input$city & SpécialitéShort == input$specialty))$RangLimite
    valueBox(value = round(rangLimite * 7800 / 9727),
             subtitle = "Rang limite adapté")
  })
  
  observeEvent(input$computeR2C, {
    rangR2C <- round(input$rangECN2024 * 7423 / 9727)
    
    updateNumericInput(session, "rangECN2024", value = rangR2C)
  })
  
  # remplit l'onglet Spécialité
  output$plot1_specialty <- renderPlot({
    data <- datasetInput()
    ggplot(data = data %>% 
             filter(SpécialitéShort == input$specialty & !is.na(RangLimite)), 
           aes(x = reorder(VilleShort, RangLimite), y = RangLimite)) +
      geom_point() +
      labs(y = "Rang limite", x = "Ville", title = input$specialty)
  })
  
  output$table_specialty <- DT::renderDataTable({
    data <- datasetInput()
    data %>% filter(SpécialitéShort == input$specialty) %>% select(Ville, VilleShort, RangLimite, Total, Disponible) %>%
      rename(`Code ville` = VilleShort) %>%
      rename(`Rang limite` = RangLimite)
  })
  
  output$plot2_specialty <- renderPlot({
    data_1 <- datasetInput() %>% filter(SpécialitéShort == input$specialty)
    data_2 <- datasetInputCompared() %>% filter(SpécialitéShort == input$specialty)
    data_fig <- data_1 %>%
      group_by(VilleShort) %>%
      summarise(RangLimite1 = max(RangLimite, na.rm = T),
                Total1 = sum(Total, na.rm = T)) %>%
      mutate(RangLimite2 = (data_2 %>%
                              group_by(VilleShort) %>%
                              summarise(RangLimite2 = max(RangLimite, na.rm = T)))$RangLimite2) %>%
      mutate(Total2 = (data_2 %>%
                         group_by(VilleShort) %>%
                         summarise(Total2 = sum(Total, na.rm = T)))$Total2) %>%
      mutate(RangLimiteAdapté2 = RangLimite2 * Total1/Total2) %>%
      mutate(Below2 = ifelse(RangLimite1 < RangLimiteAdapté2, "↙", "↗")) %>%
      filter(RangLimite1 != -Inf)
    
    ggplot(data = data_fig) +
      geom_point(aes(x = reorder(VilleShort, RangLimite1), y = RangLimite1)) +
      geom_point(aes(x = reorder(VilleShort, RangLimite1), y = RangLimiteAdapté2, color = Below2)) +
      labs(y = "Rang limite", x = "Ville", color = "", title = input$specialty)
  })
  
  # remplit l'onglet Ville
  output$plot1_city <- renderPlot({
    data <- datasetInput()
    ggplot(data = data %>% 
             filter(VilleShort == input$city & !is.na(RangLimite)), 
           aes(x = reorder(SpécialitéShort, RangLimite), y = RangLimite)) +
      geom_point() +
      labs(y = "Rang limite", x = "Spécialité", title = input$city)
  })
  
  output$table_city <- DT::renderDataTable({
    data <- datasetInput()
    data %>% filter(VilleShort == input$city) %>% select(Spécialité, SpécialitéShort, RangLimite, Total, Disponible) %>%
      rename(`Code spécialité` = SpécialitéShort) %>%
      rename(`Rang limite` = RangLimite)
  })
  
  output$plot2_city <- renderPlot({
    data_1 <- datasetInput() %>% filter(VilleShort == input$city)
    data_2 <- datasetInputCompared() %>% filter(VilleShort == input$city)
    data_fig <- data_1 %>%
      group_by(SpécialitéShort) %>%
      summarise(RangLimite1 = max(RangLimite, na.rm = T),
                Total1 = sum(Total, na.rm = T)) %>%
      mutate(RangLimite2 = (data_2 %>%
                              group_by(SpécialitéShort) %>%
                              summarise(RangLimite2 = max(RangLimite, na.rm = T)))$RangLimite2) %>%
      mutate(Total2 = (data_2 %>%
                         group_by(SpécialitéShort) %>%
                         summarise(Total2 = sum(Total, na.rm = T)))$Total2) %>%
      mutate(RangLimiteAdapté2 = RangLimite2 * Total1/Total2) %>%
      mutate(Below2 = ifelse(RangLimite1 < RangLimiteAdapté2, "↙", "↗")) %>%
      filter(RangLimite1 != -Inf)
    
    ggplot(data = data_fig) +
      geom_point(aes(x = reorder(SpécialitéShort, RangLimite1), y = RangLimite1)) +
      geom_point(aes(x = reorder(SpécialitéShort, RangLimite1), y = RangLimiteAdapté2, color = Below2)) +
      labs(y = "Rang limite", x = "Spécialités", color = "", title = input$city)
  })
  
  # remplit l'onglet Évolution
  output$plot1_evolution <- renderPlot({
    ggplot(data = data_main %>% 
             filter(VilleShort == input$city & SpécialitéShort == input$specialty) %>%
             filter(Tour != 2023)) +
      geom_point(aes(x = Tour, y = RangLimite)) +
      geom_line(aes(x = Tour, y = RangLimite)) +
      labs(y = "Rang limite", x = "Tour de simulation", title = paste(input$specialty, input$city, sep = " "))
  })
  
  output$table1_evolution <- DT::renderDataTable({
    data_main %>% 
      filter(VilleShort == input$city & SpécialitéShort == input$specialty) %>%
      filter(Tour != 2023) %>%
      select(Ville, Spécialité, Tour, RangLimite, Total, Disponible) %>%
      rename(`Rang limite` = RangLimite)
  })
  
  output$plot2_evolution <- renderPlot({
    ggplot(data = data_main %>% 
             filter(SpécialitéShort == input$specialty) %>%
             filter(Tour != 2023) %>%
             group_by(Tour) %>%
             summarise(RangLimiteMax = max(RangLimite, na.rm = T))) +
      geom_point(aes(x = Tour, y = RangLimiteMax)) +
      geom_line(aes(x = Tour, y = RangLimiteMax)) +
      labs(y = "Rang limite", x = "Tour de simulation", title = paste(input$specialty, "au national", sep = " "))
  })
  
  output$table2_evolution <- DT::renderDataTable({
    data_main %>% 
      filter(SpécialitéShort == input$specialty) %>%
      filter(Tour != 2023) %>%
      group_by(Tour) %>%
      summarise(Spécialité = input$specialty, RangLimiteMax = max(RangLimite, na.rm = T), Total = sum(Total, na.rm = T), Disponible = sum(Disponible, na.rm = T)) %>%
      rename(`Rang limite` = RangLimiteMax)
  })
  
  # remplit l'onglet Données brutes
  output$table1_raw <- DT::renderDataTable({
    datasetInput() %>%
      rename(`Rang limite` = RangLimite) %>%
      rename(`Code spécialité` = SpécialitéShort) %>%
      rename(`Code ville` = VilleShort) %>%
      rename(`Rang major` = RangPremier)
  })

}


# et zé parti
shinyApp(ui = ui, server = server)
