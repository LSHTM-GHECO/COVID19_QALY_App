
################## requried libraries
require(shiny)
require(shinythemes)
require(xlsx)
require(rsconnect)
require(dplyr)
require(tidyr)
require(data.table)

################# required data
q.male <- as.data.table(read.xlsx("Inputs/inputs.xlsx", 1))
q.female <- as.data.table(read.xlsx("Inputs/inputs.xlsx", 2)) 
qol <- as.data.table(read.xlsx("Inputs/inputs.xlsx", 3))
covid.age <- as.data.table(read.xlsx("Inputs/inputs.xlsx", 4))


# The user interface (ui) object controls the layout and appearance of your app. 
# The server function contains the instructions that your computer needs to build your app. 
# Finally the shinyApp function creates Shiny app objects from an explicit UI/server pair.

###################### BACKGROUND CODE ##############################

#######################################################################
############## USER INTERFACE ########################################
ui <- fluidPage(  
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  titlePanel("The COVID-19 QALY Loss Calculator for Associated Deaths"),
  
  sidebarPanel(h3("Key Inputs"),
              
               ## nationality
               radioButtons(inputId="country", label="Country", 
                            choices=c("UK", "US","Canada","Norway","Israel"), selected = "UK"),
               
                
               ## SMR
               numericInput("smr", em("Standardized Mortality Ratios for comorbidities (number between 1-5)"), 1, min = 1, 
                            max = 5),
              
               ##assumed reduction in QoL due to comorbidities
               numericInput("qcm", em("Comorbidity Quality of Life Adjustment Factor (0%-100%)"), 100, min = 0, 
                            max = 100),
               
               ## discount rate
               numericInput("r", em("Discount rate (0%-10%)"), 3.5, min = 0, 
                            max = 100)
               
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Main Results",
    h3("Weighted Mean Loss per Death"),
      br(),
    
    tableOutput("resultstab"),
    tags$head(tags$style("#resultstab table {background-color: lightblue; }", media="screen", type="text/css")),

    br(),
    h5("Abbreviations: LE - Life Expectancy, QALE - Quality Adjusted Life Expectancy, dQALY - Discounted Quality Adjusted Life Years"),
    h5("Definitions: Standardized mortality ratio (SMR) - the increase in mortality in the comorbidity group compared to population norms, Comorbidity Quality of Life Adjustment Factor (qCM)- Percentage of population quality of life norms in the comorbidity group"), 
    h5("Note that this calculator calculates QALY losses from excess deaths only, weighted across the frequency distribution of age at death for COVID‐19"),
    code("App & R code by N.R Naylor. For descriptions of  model code and underlying data see: https://github.com/LSHTM-CHIL/COVID19_QALY_App"),
    code("App Last updated February 2020. Latest Data Source July 2020. This code may take a few seconds to run on first loading so please be patient"),
    br(),
    strong("Based on Briggs, Andrew H., et al. Estimating (quality‐adjusted) life‐year losses associated with deaths: With application to COVID‐19 Health Economics (2020), Excel Model Version 5.0."),
    strong("This work was done as part of the Centre for Health Economics in London at the London School of Hygiene and Tropical Medicine")),           
    
    tabPanel("Results by Age Group",
    h3("Breakdown by Age Group"),
    tableOutput("agetab"),
    br(),
    h5("Abbreviations: LE - Life Expectancy, QALE - Quality Adjusted Life Expectancy, dQALY - Discounted Quality Adjusted Life Years. In this instance, these are the expected values conditional on a person being in the relevant age group"),
    h5("Definitions: Standardized mortality ratio (SMR) - the increase in mortality in the comorbidity group compared to population norms, Comorbidity Quality of Life Adjustment Factor (qCM)- Percentage of population quality of life norms in the comorbidity group"),
    )
    
    )))


######################################################
############# SERVER ###############################################
server <- function(input,output){
  
  
  # Reactive dependencies - if these change then MODEL will run again and update values
  xxchange <- reactive({
    paste(input$smr, input$country, input$qcm, input$r)
    }) 
  
  
  model <- eventReactive(xxchange(), {
   country <- input$country
   smr <- input$smr
   qcm <- input$qcm/100
   r <- input$r/100
  
   validate(
     need(input$smr <=5, "SMR input is invalid, please try another value"),
     need(input$smr >=1, "SMR input is invalid, please try another value"),
     need(input$r <=10, "Please choose a valid discount rate between 0% and 10%"),
     need(input$r >=0, "Please choose a valid discount rate between 0% and 10%"),
     need(input$qcm <=100, "Please choose a valid qCM between 0% and 100%"),
     need(input$qcm >=0, "Please choose a valid qCM between 0% and 100%")
   )
    myvector <- c("Age",country)
    
    l_x_est <- function(dt, countr, smr){
      ## dt = data table with q(x) vaues
      ## country = selected country
      ## smr = smr
      myvector <- c("Age",countr)
      
      y <- dt[, ..myvector]
      colnames(y) <- c("x","q_x")
      
      y[ , d_x := -log(1-y$q_x)]
      
      y[ 1, l_x := 100000] 
      
      for (i in 2:nrow(y)){
        y[i, l_x := y$l_x[[i-1]] * 
            exp((-y$d_x[[i-1]])*smr)] 
      }
      return(y)
    }
    
    q.male <- l_x_est(q.male, country, smr)
    q.female <- l_x_est(q.female, country, smr)
    
    q.person <- merge(q.male, q.female, by="x")
    colnames(q.person) <- c("x","q_male","d_male","l_male",
                            "q_female","d_female","l_female")
    q.person[ , p.f := l_female/(l_female+l_male)]
    q.person[ , l_person := (p.f*l_female)+
                ((1-p.f)*l_male)]
    
    for (i in 1:(nrow(q.person)-1)){
      q.person[i, bigl_x := (q.person$l_person[[i]]+ q.person$l_person[[i+1]])/2]
    }
    
    q.person[nrow(q.person), bigl_x := (q.person$l_person[[nrow(q.person)]])/2]
    
    for (i in 1:nrow(q.person)){
      q.person[i, t_x := sum(q.person$bigl_x[i:nrow(q.person)])]
    }
    
    q.person[ , LE_x := t_x/l_person]
    
    ########### calculating QALE ########
    myvector.qol <- c("low","high",country)
    
    dt.qol <- qol[, ..myvector.qol]
    colnames(dt.qol) <- c("low","high","qol_age")
    
    qale <- q.person[dt.qol, on = .(x >= low, x <= high), nomatch = 0,
                     .(x.x, l_person, bigl_x, t_x, LE_x,qol_age)]
    
    qale[ , z_x := bigl_x*qol_age*qcm]
    
    for (i in 1:nrow(qale)){
      qale[i , t_adj := sum(qale$z_x[i:nrow(qale)])]
    }
    
    qale[ , qale_x := t_adj/l_person]
    
    qaly.calc <- qale[ , c("x.x","z_x")]
    
    temp.q <- list()
    for (i in 1:nrow(qaly.calc)){
      temp.q[[i]] <- qaly.calc[i:nrow(qaly.calc),]
    }
    
    temp.q <- bind_rows(temp.q, .id = "column_label")
    temp.q %>% setDT() ## creating a copy as otherwise there is a warning
    ## message (still runs but just for "clean" code), so this stops attempts of .internal.selfref detected
    temp.q_copy <- copy(temp.q)
    temp.q_copy[ , column_label := as.numeric(column_label)-1]
    temp.q_copy[ , b_x := z_x/((1+r))^(x.x-(column_label))] ## n.b x.x = u and column_label = x in the corresponding formulae in the CodeBook
    
    
    total.b <- temp.q_copy[,.(bigb_x=sum(b_x)), by=column_label]
    colnames(total.b) <- c("x.x","bigb_x")
    qale <- merge(qale, total.b, by="x.x")
    
    qale[ , dQALY := bigb_x/l_person]
    
    ######### calculating covid19 loss #######
    myvector.cov <- c("low","high",country)
    
    dt.cov <- covid.age[, ..myvector.cov]
    colnames(dt.cov) <- c("low","high","cov_age")
    
    dt.cov[ , midpoint := ceiling((low+high)/2)]
    cov <- merge(qale, dt.cov, by.x="x.x", by.y="midpoint", all=FALSE)
    
    cov[ , weight.LE := cov_age*LE_x]
    cov[ , weight.qale := cov_age*qale_x]
    cov[ , weight.qaly := cov_age*dQALY]
    
    estimates <- colSums(cov)
    resultstab <- data.table("Weighted LE Loss"=estimates["weight.LE"],
                          "Weighted QALE Loss"=estimates["weight.qale"],
                          "Weighted dQALY loss"=estimates["weight.qaly"])
   ### ADDING AGE GROUP BREAKDOWN TABLE
    cov[,"Age Group":=paste(cov[,low],cov[,high],sep="-")]
    cov[ , "Age at Death (% of all deaths)" := cov_age*100]
    setnames(cov, old=c("LE_x","qale_x","dQALY"),
             new=c("LE","QALE","dQALY"))

    agetab <- cov[ , c("Age Group","Age at Death (% of all deaths)",
                       "LE","QALE","dQALY")]
    
    list(resultstab=resultstab, agetab=agetab)
  })


  
  output$resultstab <- renderTable(model()$resultstab, bordered = TRUE)
  output$agetab <- renderTable(model()$agetab, bordered = TRUE)

}

##################################################################
############ SHINYAPP ###########################################
shinyApp(ui = ui, server = server)