###################################################
# KAPACITY AI COMPETITION
# Desktop version

# April 2022
# Peer Christensen
###################################################

# shiny packages
library(shiny)
library(shinyWidgets)
library(flexdashboard)
library(bslib)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(shinyvalidate)

# R/tidyverse and ML
library(tidyverse)
library(lubridate)
library(randomForest)
library(mlr)

# Azure storage
library(AzureStor)


# --- CONNECTING TO BLOB STORAGE ---------------------------------

readRenviron(".Renviron")
sas_token <- Sys.getenv("SAS_TOKEN")
endpoint  <-
  storage_endpoint("https://demoeventstorage.blob.core.windows.net",
                   sas = sas_token)
container <- storage_container(endpoint, "aicompetition")

# --- STYLING -----------------------------------------------------

### colours
gold  <- "#c39f5e"
#light_gold <- "#f9f5ef"
blue  <- "#85c8f0"

theme <- bs_theme(
  #bg = "white",
  #fg = "black",
  primary = blue,
  secondary = blue,
  #base_font = font_google("Open Sans"),
  #heading_font = font_google("Ubuntu"),
  )
  
### CSS

  # This could be placed in a separate file
  # There's a lot of repetition here,
  # which can be avoided by using inheritance among elements
  # The commented lines with font-family are currently ignored,
  # because some desktop and mobile systems seem to override these fonts
  # in favour of Times New Roman. However, the Shiny default (used here) works fine.
  
css <- HTML(
  "
.html-widget.gauge svg {
  height: 400px;
  width: 800px;}

.js-irs-0 .irs-single,
       .js-irs-0 .irs-bar-edge,
       .js-irs-0 .irs-bar,
       .js-irs-0 .irs-slider
       {background: #85c8f0;}
       .irs-handle {background: #85c8f0 !important;}

#title_panel {
  background-color: black;
  background: black;
  color: white;
  font-size: 30px;
  margin:-1em;
  padding: 1em}

.form-control {
  border-radius: 25px 25px 25px 25px;
  border: 2px solid black}

#sidebar {
  background-color: white;}
"
  )

black_style <- "
padding:1em;
background-color:
black;color:white;
font-size: 18px !important;
line-height: 1.5;
"
#font-family: 'Open Sans';

black_style_header <- "
padding:0em;
background-color:black;
color:white;
font-weight:300 !important;
font-size: 56px !important;
padding-top: 1em;
"
#font-family: 'Ubuntu' !important;

black_style_title <- "
font-weight:300 !important;
padding-left: 150px;
"
# font-family: 'Ubuntu' !important;

white_style <- "
margin:0em;
padding:2em;
background-color:white;
color:black;
font-size: 18px !important;
padding-left: -125px;
padding-right: 150px;
line-height: 1.5;
"
#font-family: 'Open Sans' !important;

white_style_header <- "
margin:0em;
padding:1em;
background-color:white;
color:black;
font-weight: 300 !important;
font-size: 32px !important;
padding-top: 1em;
padding-left: 150px;
"
# font-family: 'Ubuntu' !important;

white_style_header2 <- "
margin:0em;
padding:1em;
background-color:white;
color:black;
font-weight:300 !important;
font-size: 32px !important;
padding-top: 1em;
padding-left: 50px;
"
#font-family: 'Ubuntu' !important;

rendered_text <- "
margin-top:30px;
margin-bottom:-10px;
background-color:white;
color:black;
font-size: 24px !important;
"
# font-family: 'Open Sans' !important;

button_style <- "
background-color: #85c8f0;
border-color: #85c8f0;
border-radius: 12px;
font-size: 18px !important;
"
# font-family: 'Open Sans';


# --- DATA -----------------------------------------------------

vars  <- read_rds("vars_attr.rds")
train <- read.csv("attr_train.csv") %>%
  mutate_if(is.character, factor)
test  <- read.csv("attr_test.csv") %>%
  mutate_if(is.character, factor)


# --- UI ---------------------------------------------------------

ui <- fluidPage(
  theme = theme,
  useShinyjs(),
  tags$head(tags$style(css), tags$title('kapacity AI konkurrence')),
  
  # TITLE
  titlePanel(p(
    id = "title_panel", "kapacity", style = black_style_title
  )),
  
  # HEADER
  fluidRow(column(
    12, align = "center", style = black_style_header,
    p("AI konkurrence!")
  )),
  
  # INTRO
  fluidRow(
    column(4, style = black_style),
    column(
      4,
      align = "center",
      style = black_style,
      p(
        "Kan du træne vores AI-model, så du stopper medarbejderflugten i din virksomhed?"
      )
    ),
    column(4, style = black_style)
  ),
  # Use this row to mention the competition pize
  
  # fluidRow(
  #    column(4, style = black_style),
  #    column(4, align="center", style = black_style,
  #          p("Vi belønner dagens bedste forsøg med en champagne-smagekasse til en værdi af kr. 2.899,-")
  #  ),
  #   column(4, style = black_style)
  #  ),
  
  # INSTRUCTIONS
  fluidRow(
    style = "margin:0em:",
    column(6,
           p("Start her!", style = white_style_header)),
    column(
      6,
      style = white_style,
      p(
        "I 2021 overvejede 40 pct. af alle medarbejdere at opsige deres stilling. Din opgave er at træne en AI-model, der kan forudsige, hvilke ansatte det drejer sig om, så opsigelserne kan undgås."
      ),
      p(
        "Vælg en kombination af de rette variabler (features) der bedst forudsiger medarbejderflugt og indstil dine hyperparametre for at træne en model."
      ),
      p(
        "Din score viser andelen af korrekte gæt på hvorvidt hver af de 616 ansatte forlader jeres virksomhed. På vores leaderboard, kan du se hvor god din model var i forhold til andres."
      ),
      p(
        "Du er velkommen til at prøve flere gange, men kun det bedste af dine tre første forsøg tæller med i konkurrencen."
      ),
      
      # Specify when the competition ends
      # p("Konkurrencen slutter kl. 17:00, og vi kontakter vinderen umiddelbart efter."),
      
      p(
        "Deltagelse forudsætter tilmelding til Kapacitys nyhedsmail om AI.",
        tags$br(),
        
        # Specify who can participate
        # "Kun deltagere i Kunstig Intelligens i praksis-konferencen kan deltage i konkurrencen."
      ),
    )
  ),
  hr(),
  
  sidebarLayout(
    
    # SIDEBAR
    sidebarPanel(
      id = "sidebar",
      style = "margin-left:150px;",
      width = 4,
      
      # GET USER INFO AND PERMISSIONS
      textInput("name", label = NULL, placeholder = "Dit navn"),
      textInput("mail", label = NULL, placeholder = "E-mailadresse"),
      textInput("initials", label = NULL, placeholder = "Dine initialer (vises på leaderboard)"),
      checkboxInput(
        "confirm_mail_list",
        "Jeg accepterer at modtage relevante e-mails fra Kapacity *",
        FALSE
      ),
      checkboxInput(
        "confirm",
        p(
          "Jeg godkender",
          tags$a(href = "https://www.kapacity.dk/cookies/", "Kapacitys betingelser"),
          "for opbevaring af mine data *"
        ),
        FALSE
      ),
      
      # USER CHOICES
      pickerInput(
        "vars",
        label = tags$span(
          "Vælg features",
          tags$i(class = "glyphicon glyphicon-exclamation-sign",
                 title = "Data er ikke gratis og der trækkes 1 procentpoint fra din score for hver ekstra variabel du inkluderer i din model efter de første tre.")
        ),
        choices = vars,
        multiple = TRUE,
        options = list(`none-selected-text` = "Intet valgt")
        #options = list(`max-options` = 4)
      ),
      sliderInput(
        "mtry",
        label = tags$span(
          "Maksimum features/split",
          tags$i(class = "glyphicon glyphicon-info-sign",
                 title = "Random Forest-algoritmen laver mange 'beslutningstræer' der består af regler. Her skal du bestemme hvor mange variabler træerne kan vælge imellem ved hver 'forgrening'. En forgrening fungerer som en regel og kunne f.eks. lyde: 'hvis ALDER ER OVER 50, SÅ...'")
        ),
        min = 1,
        max = 7,
        value = 3
      ),
      sliderInput(
        "nodesize",
        label = tags$span(
          "Minimum node size",
          tags$i(class = "glyphicon glyphicon-info-sign",
                 title = "Her skal du beslutte hvor mange datapunkter der må være i hvert 'blad', dvs. efter sidste forgrening. Jo flere datapunkter, desto mindre bliver 'træerne'. For små træer kan lede til såkaldt 'underfitting', hvorimod for store træer kan lede til 'overfitting'.")
        ),
        min = 1,
        max = 50,
        value = 25
      ),
      sliderInput(
        "ntree",
        label = tags$span(
          "Antal træer",
          tags$i(class = "glyphicon glyphicon-info-sign",
                 title = "Hvor mange beslutningstræer vil du bygge og aggregere? Flere træer kan øge din models performance. MEN, computerkraft koster, og der trækkes 0.01 procentpoint fra din score for hvert træ der bygges under træningen af din model.")
        ),
        min = 1,
        max = 100,
        value = 50
      ),
      
      # ACTION BUTTONS
      column(
        12,
        splitLayout(
          cellWidths = c("40%", "40%"),
          actionButton("start", "Start!", style = button_style),
          actionButton("reset", "Nulstil", style = button_style),
          align = "center",
          style = "margin-top: 50px;"
        )
      )
    ), #sidebarPanel end
    
    # OUTPUT PANEL
    mainPanel(
      p("Din score", style = white_style_header2),
      gaugeOutput("gauge", height = "30%"),
      fluidRow(column(8,
                      p(
                        textOutput("result_tp", inline = TRUE),
                        #align = "center",
                        style = rendered_text
                      ))),
      fluidRow(column(8,
                      p(
                        textOutput("result_fp", inline = TRUE),
                        #align = "center",
                        style =  rendered_text
                      ))),
      fluidRow(column(8,
                      p(
                        textOutput("thanks", inline = TRUE),
                        #align = "center",
                        style = rendered_text
                      )))
      
    ) # main panel end
  ), # sidebar layout end
  
  # ADD SPACE AT BOTTOM
  fluidRow(h1("")),
  fluidRow(h1(""))
) # fluidPage end


# --- SERVER -----------------------------------------------------

server <- function(input, output, session) {
  
  # INPUT VALIDATION
  iv <- InputValidator$new()
  iv$add_rule("name", sv_required(message = "Påkrævet"))
  iv$add_rule("initials", sv_required(message = "Påkrævet"))
  iv$add_rule("mail", sv_email(message = "Indtast venligt en gyldig email"))
  iv$add_rule(
    "confirm_mail_list",
    sv_equal(TRUE,
             message_fmt = "Acceptér venligst for at deltage i konkurrencen")
  )
  iv$add_rule(
    "confirm",
    sv_equal(TRUE,
             message_fmt = "Godkend venligst for at deltage i konkurrencen")
  )
  iv$disable()
  
  # LOGIC WHEN USER CLICKS "START"
  observeEvent(input$start, {
    iv$enable()
    
    # TRAIN MODEL, GET RESULTS
    set.seed(7223)
    train <- train %>% select(all_of(input$vars), Attrition)
    test  <- test  %>% select(all_of(input$vars), Attrition)
    task  <- makeClassifTask(data = train, target = "Attrition")
    base_clf  <-
      makeLearner("classif.randomForest", fix.factors.prediction = FALSE)
    tuned_clf <- setHyperPars(
      base_clf,
      ntree = input$ntree,
      mtry  = input$mtry,
      nodesize = input$nodesize
    )
    mod <- mlr::train(tuned_clf, task)
    pred <- predict(mod, newdata = test)
    n_vars <- length(input$vars)
    n_trees <- input$ntree
    
    true_pos  <- pred %>% 
      as_tibble() %>% 
      filter(truth == "Yes" & response == "Yes") %>% 
      nrow()
    
    false_pos <- pred %>% 
      as_tibble() %>% 
      filter(truth == "No"  & response == "Yes") %>% 
      nrow()
    
    all_pos   <- pred %>% 
      as_tibble() %>% 
      filter(truth == "Yes") %>% 
      nrow()
    
    all_neg   <- pred %>% 
      as_tibble() %>% 
      filter(truth == "No") %>% 
      nrow()
    
    # RESULTS TEXT
    result_text_tp <-
      paste0(
        "Din model var i stand til at finde ",
        true_pos,
        " ud af ",
        all_pos,
        " ansatte, der var på vej væk."
      )
    result_text_fp <-
      paste0(
        "I ",
        false_pos,
        " ud af ",
        all_neg,
        " tilfælde, hvor medarbejderne blev i virksomheden, gættede modellen forkert"
      )
    
    # PENALIZING BASED ON N VARIABLES SELECTED AN N TREES
    if (n_vars > 3) {
      acc <-
        (round(calculateROCMeasures(pred)$measures$acc, 4) * 100) - ((n_vars -
                                                                        3) * 1) - (n_trees * 0.01)
    } else {
      acc <-
        round(calculateROCMeasures(pred)$measures$acc, 4) * 100 - (n_trees * 0.01)
    }
    
    # COLLECT USER DATA AND SCORE
    name       <- input$name
    mail       <- input$mail
    initials   <- input$initials
    score      <- acc
    permission_mail <- input$confirm_mail_list
    permission <- input$confirm
    time       <- now(tzone = "Europe/Copenhagen")
    
    data <-
      tibble(name,
             mail,
             initials,
             score,
             permission_mail,
             permission,
             time) %>%
      mutate(time = as.character(time)) %>%
      filter(permission == T,
             permission_mail == T,
             name != "",
             mail != "",
             initials != "")
    print(data)
    
    # PROCEED IF ALL USER DATA IS PROVIDED
    if (nrow(data) == 1) {
      
      # WRITE CSV
      filename <- paste0(mail, ".csv")
      write_csv(data, filename, col_names = F)
      
      # APPEND TO LEADERBOARD BLOB
      storage_upload(
        container,
        src = filename,
        dest = "leaderboard/leaderboard.csv",
        type = "AppendBlob",
        append = TRUE
      )
      
      # STORE BACKUP FILE
      storage_upload(container,
                     src = filename,
                     dest = paste0("archive/", filename))
      
      
      # RENDER GAUGE CHART
      delay(
        1500, # msec delay
        output$gauge <- renderGauge({
          gauge(
            acc,
            min = 0,
            max = 100,
            symbol = "%",
            sectors = gaugeSectors(
              success = c(0, 40),
              warning = c(40, 60),
              danger = c(60, 100),
              colors = c(gold, gold, gold)
            )
          ) # gauge end
        }) # renderGauge end
        ) # delay end
        
      # RENDER RESULTS TEXT
      delay(3000,
              output$result_tp <-
                renderText({
                  result_text_tp
                })
            )
        
      delay(4500,
              output$result_fp <-
                renderText({
                  result_text_fp
                })
            )
        
        # SAY THANKS
      delay(6000,
              output$thanks <-
                renderText({
                  "Tak for din deltagelse!"
                })
            )
        
        } # if nrow data == 1 end
    }) # observeEvent start end
      
      # LOGIC WHEN USER CLICKS "RESET"
      observeEvent(input$reset, {
        
        iv$disable()
        reset("vars")
        reset("mtry")
        reset("nodesize")
        reset("ntree")
        reset("name")
        reset("mail")
        reset("initials")
        reset("confirm_mail_list")
        reset("confirm")
        
        output$gauge <- renderGauge({})
        
        output$result_tp <- renderText({})
        
        output$result_fp <- renderText({})
        
        output$thanks <- renderText({})
        
      }) # observeEvent reset end
      
  } # Server end

# Run the application
shinyApp(ui = ui, server = server)
