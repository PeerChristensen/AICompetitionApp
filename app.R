library(shiny)
library(shinyWidgets)
library(flexdashboard)
library(bslib)
library(shinyBS)
library(shinydashboard)
library(tidyverse)
library(randomForest)
library(mlr)

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
vars  <- read_rds("vars.rds")
train <- read_csv("titanic_train.csv") %>% 
    mutate_if(is.character,factor) %>%
    mutate(Survived = factor(Survived)) %>%
    drop_na()
test  <- read_csv("titanic_test.csv") %>% 
    mutate_if(is.character,factor) %>%
    mutate(Survived = factor(Survived)) %>%
    drop_na()

### UI ###
ui <- fluidPage(theme = theme,
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
                        multiple = TRUE,
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
            column(12,actionButton("start","Start!"), 
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
                    gaugeOutput("gauge", height = "100%")
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

      acc <- eventReactive(input$start,{
        train <- train %>% select(all_of(input$vars),Survived)
        test <- test %>% select(all_of(input$vars),Survived)
        task = makeClassifTask(data = train, target = "Survived")
        base_clf = makeLearner("classif.randomForest", fix.factors.prediction = FALSE)
        tuned_clf = setHyperPars(base_clf, 
                                 ntree = 200,
                                 mtry = input$mtry, 
                                 nodesize = input$nodesize,
                                 maxnodes = input$maxnodes)
        mod = train(tuned_clf, task)
        pred = predict(mod, newdata = test)
        n_vars = length(input$vars) - 3
        if (n_vars > 0) {
          (round(calculateROCMeasures(pred)$measures$acc,4)*100) - (n_vars * 2)
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
