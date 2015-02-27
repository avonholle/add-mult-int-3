# ui.R

library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Demonstration of additive and multiplicative effect modification"),
  sidebarPanel(
    h2("Select parameters for simulation"),
    p("Regression coefficients for logistic regression (defaults based on parameter estimates from page 452-453. 
      Shows additive interaction but no significant multiplicative interaction).
      "),
    sliderInput("n1","Sample size", min=100, max=5000, value=600, step=1, format="###", animate=FALSE),
    sliderInput("beta0","Intercept:", min=0.01, max=0.9, value=0.03, step=0.01, format="#.#", animate=FALSE),
    sliderInput("beta1","Coefficient for x1:", min=0, max=1, value=0.036, step=0.001, format="#.#", animate=FALSE),
    sliderInput("beta2","Coefficient for x2", min=0, max=1, value=0.201, step=0.001, format="#.#", animate=FALSE),
    sliderInput("beta3","Coefficient for interaction between x1 and x2", min=-1, max=1, value=0.161, step=0.001, format="#.#", animate=FALSE),
    br()
    ),
  
  mainPanel(
    h2("Based on example in the Hosmer and Lemeshow 2013 textbook, 'Applied Logistic Regression'"),
    h2("ISBN: 9780470582473. pages 448-456"),
    tags$a(href="http://www.wiley.com/WileyCDA/WileyTitle/productCd-0470582472.html", "Link to book"),
    h3("Part 2: using example from section 10.9.2, pages 451-456"),
    withMathJax(),
    h3("Model used to simulate data with an additive interaction, p453. Table 10.27"),
    h3(uiOutput("eqn2")),
    br(),
    h3("Current selected coefficients for simulation:"),
    h3(uiOutput("textn")),
    h3(uiOutput("text0")),
    h3("Sample of simulated data"),
    tableOutput("table1"),
    br(),
    h3("Frequencies by x1 and x2 (covariates) by y (outcome)"),
    tableOutput("table2alt"),
    br(),
    h3("Additive Scale"),
    tags$hr(),
    h3("Summary of regression model with binomial distribution and linear link"),
    tableOutput("summary"),
    h4("Interaction contrast (IC) for additive model:"),
    h3(uiOutput("ic.r00")),
    h3(uiOutput("ic.r01")),
    h3(uiOutput("ic.r10")),
    h3(uiOutput("ic.r11")),
    h3(uiOutput("ic.3")),
    br(),
    h3("Multiplicative Scale"),
    tags$hr(),
    h3("Summary of regression model with binomial distribution and logit link"),
    tableOutput("summary2"),
    h3("Plot of same data using additive and multiplicative scales"),
    plotOutput("plot1"),
    p("The results of this demonstration, when set at the defaults for section 10.9.2, 
      show interaction on the additive scale (first, linear link binomial regression) and 
      no (significant) interaction on the multiplicative scale (second, logit link regression)"),
    plotOutput("plot2"),
    h3(uiOutput("eqn3")),
    h3(uiOutput("eqn4"))
  )

))