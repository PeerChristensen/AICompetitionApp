library(shiny)
library(shinyWidgets)
library(flexdashboard)
library(bslib)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(randomForest)
library(mlr)
library(shinyvalidate)
library(AzureStor)

# CONNECTING TO BLOB STORAGE
readRenviron(".Renviron")
sas_token <- Sys.getenv("SAS_TOKEN")
endpoint <- storage_endpoint("https://demoeventstorage.blob.core.windows.net", sas=sas_token)
container <- storage_container(endpoint, "aicompetition")

### STYLING ###
gold  <- "#c39f5e"
theme <- bs_theme(
    bg = "black", fg = "white", primary = gold, secondary = gold,
    base_font = font_google("Open Sans"),
    heading_font = font_google("Ubuntu")
)

css <- HTML("
.html-widget.gauge svg {
  height: 400px;
  width: 800px;}

.js-irs-0 .irs-single, 
       .js-irs-0 .irs-bar-edge, 
       .js-irs-0 .irs-bar, 
       .js-irs-0 .irs-slider 
       {background: #c39f5e;}
       .irs-handle {background: #c39f5e !important;}")

### DATA ###
vars  <- read_rds("vars_attr.rds")
train <- read_csv("attr_train.csv") %>% 
    mutate_if(is.character,factor) 
test  <- read_csv("attr_test.csv") %>% 
    mutate_if(is.character,factor) 

### UI ###
ui <- fluidPage(theme = theme,
                useShinyjs(),
                tags$head(tags$style(css)),
    titlePanel("Kapacity AI Udfordring"),
        sidebarLayout(
        sidebarPanel(width = 3,
            pickerInput("vars", 
                        label = tags$span(
                          "Vælg features",
                          tags$i(
                            class = "glyphicon glyphicon-exclamation-sign", 
                            title = "Data er ikke gratis og der trækkes 2 procentpoint fra din score for hver ekstra variabel du inkluderer i din model efter de første tre."
                          )),
                        choices = vars, 
                        multiple = TRUE
                        #options = list(`max-options` = 4)
                        ),
            sliderInput("mtry",
                        label = tags$span(
                          "max features/split", 
                          tags$i(
                            class = "glyphicon glyphicon-info-sign", 
                            title = "Dette er en forklaring........"
                          )),
                        min = 1,
                        max = 7,
                        value = 3),
            sliderInput("nodesize",
                        label = tags$span(
                          "Node size", 
                          tags$i(
                            class = "glyphicon glyphicon-info-sign", 
                            title = "Dette er en forklaring........"
                          )),
                        min = 1,
                        max = 50,
                        value = 25),
            sliderInput("maxnodes",
                        label = tags$span(
                          "Max nodes", 
                          tags$i(
                            class = "glyphicon glyphicon-info-sign", 
                            title = "Dette er en forklaring........"
                          )),
                        min = 1,
                        max = 10,
                        value = 5),
            textInput("name", "Dit navn"),
            textInput("mail", "E-mailadresse"),
            textInput("company", "Virksomhed"),
            textInput("initials", "Dine initialer (vises på leaderboard)"),
            checkboxInput("confirm", p("Ved at deltage, godkender jeg",tags$a(href="https://www.kapacity.dk/cookies/", "Kapacitys betingelser"),  "for opbevaring af mine data *"),FALSE),
            column(12,
                   actionButton("start","Start!"), 
                   actionButton("reset","Reset"),
                   align = "center",
                   style = "margin-top: 50px;")
        ),
        tabsetPanel(
            tabPanel("Hovedside",
                mainPanel(
                    h3("Instruktioner"),
                    p("I denne udfordring, skal du træne en model der forudsiger..."),
                    p("I menuen til venstre, skal du vælge hvilke features du tror bedst forudsiger.."),
                    p("Der er hjælp at hente i fanen 'Definitioner' ovenfor."),
                    hr(),
                    h3("Din score"),
                    gaugeOutput("gauge", height = "100%"),
                    fluidRow(
                      h3(textOutput("thanks",inline = TRUE),
                       align = "left",
                       style = "margin-left: 250px;"
                    )
                )
                )
            ),
            tabPanel("Definitioner",
                     h3("Variabler"),
                     p(strong("Variabel A")," - bla bla"),
                     p(strong("Variabel B")," - bla bla"),
                     p(strong("Variabel B")," - bla bla"),
                     hr()
            )
        )
    )
)

### SERVER ###

server <- function(input, output, session) {
      
  
  # Validate input
  iv <- InputValidator$new()
  iv$add_rule("name", sv_required(message = "Påkrævet"))
  iv$add_rule("company", sv_required(message = "Påkrævet"))
  iv$add_rule("initials", sv_required(message = "Påkrævet"))
  iv$add_rule("mail", sv_email(message = "Indtast venligt en gyldig email"))
  iv$add_rule("confirm", sv_equal(TRUE, 
                                  message_fmt = "Godkend venligst for at deltage i konkurrencen"))
  iv$disable()
  
  
      observeEvent(input$start, {
        
        iv$enable()
        
        # Collect user data and score
        name       <- input$name
        company    <- input$company
        mail       <- input$mail
        initials   <- input$initials
        score      <- acc()
        permission <- input$confirm
        
        data <- tibble(name,company,mail,initials,score, permission) %>%
          filter(permission==T, name != "", company!="",mail!="",initials!="")
        
        if (nrow(data) == 1) { 
          
          # write csv
          filename <- paste0(mail,".csv")
          write_csv(data, filename, col_names = F)
        
          # append to leaderboard
          storage_upload(container, src=filename, 
                       dest="leaderboard/leaderboard.csv", 
                       type="AppendBlob",
                       append=TRUE)

          # store backup file
          storage_upload(container, src=filename, dest=paste0("archive/", filename))
          
          # Send thanks
          delay(1500, 
                output$thanks <- renderText({"Tak for din deltagelse!"}))
          
          iv$disable()
          reset("vars")
          reset("mtry")
          reset("nodesize")
          reset("maxnodes")
          reset("name")
          reset("mail")
          reset("company")
          reset("initials")
          reset("confirm")
          
        } 
    })
  
    observeEvent(input$reset, {
      
      iv$disable()
      reset("vars")
      reset("mtry")
      reset("nodesize")
      reset("maxnodes")
      reset("name")
      reset("mail")
      reset("company")
      reset("initials")
      reset("confirm")
      
      output$gauge <- renderGauge({})
      
    })
      
      # train model and get accuracy
      acc <- eventReactive(input$start,{
        
        train <- train %>% select(all_of(input$vars),Attrition)
        test <- test %>% select(all_of(input$vars),Attrition)
        task = makeClassifTask(data = train, target = "Attrition")
        base_clf = makeLearner("classif.randomForest", fix.factors.prediction = FALSE)
        tuned_clf = setHyperPars(base_clf, 
                                 ntree = 200,
                                 mtry  = input$mtry, 
                                 nodesize = input$nodesize,
                                 maxnodes = input$maxnodes)
        mod = mlr::train(tuned_clf, task)
        pred = predict(mod, newdata = test)
        n_vars = length(input$vars) - 8
        
        if (n_vars > 0) {
          (round(calculateROCMeasures(pred)$measures$acc,4)*100) - (n_vars * 1)
        } else {
          round(calculateROCMeasures(pred)$measures$acc,4)*100
        }
        
    })
      
    output$gauge <- renderGauge({
    
        gauge(acc(), 
              min = 0, 
              max = 100, 
              symbol="%",
              sectors = gaugeSectors(success = c(0, 40),
                                     warning = c(40, 60),
                                     danger = c(60, 100),
                                     colors = c(gold, gold, gold)
                                     )
              )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
