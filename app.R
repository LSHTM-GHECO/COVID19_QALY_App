
################## requried libraries
require(shiny)
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
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  titlePanel("CHiL COVID-19 QALY Calculator"),
  
  sidebarPanel(h3("Key Analytical Inputs"),
              
               ## nationality
               radioButtons(inputId="country", label="Country", 
                            choices=c("UK", "US","Canada","Norway","Israel"), selected = "UK"),
               
                
               ## SMR
               numericInput("smr", em("SMR for comorbidities"), 1, min = 0, 
                            max = 100),
              
               ##assumed reduction in QoL due to comorbidities
               numericInput("qcm", em("qCM"), 1, min = 0, 
                            max = 1),
               
               ## discount rate
               numericInput("r", em("discount rate"), 0.035, min = 0, 
                            max = 1)
               
  ),
  
  mainPanel(
    
    h3("Results"),
      br(),
    
    tableOutput("resultstab"),
     br(),
    
    h5("Abbreviations: LE - Life Expectancy, QALE - Quality Adjusted Life Expectancy, dQALY - Discounted Quality Adjusted Life Years"),
    br(),
    
    h6("App by N.R Naylor. Description of use and resources see: https://github.com/NikkiR08/covid19_QALY_calculator"),
    
    h6("Based on A. Briggs 2020 Covid19 QALY Excel Tool: https://www.lshtm.ac.uk/research/centres-projects-groups/chil#covid-19"),
    
               
    )
  )


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
   qcm <- input$qcm
   r <- input$r
    
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
    temp.q[ , column_label := as.numeric(column_label)-1]
    temp.q[ , b_x := z_x/((1+r))^(x.x-(column_label))]
    
    total.b <- temp.q[,.(bigb_x=sum(b_x)), by=column_label]
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
    
   
  })


  
  output$resultstab <- renderTable(model())

}

##################################################################
############ SHINYAPP ###########################################
shinyApp(ui = ui, server = server)