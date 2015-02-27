# server.R
# code based on the sim.e.mod.R program
# this one is with two dichtomous covariates

library(mgcv)
library(ggplot2)
library(plyr)
library(reshape2)

# Parameters for function below..................
# ns: number of people in sample
# beta.0: coefficient for intercept
# beta.1: coefficient for x term
# beta.2: coefficient for z term
# beta.3: coefficient for interaction term between x and z

# this function creates a sample for the example on page 449.
created = function(ns, beta.0, beta.1, beta.2, beta.3){
  
  ns.2 = ns
  
  # Simulate data for example on 452-453.
  # both covariates are dichtomous
  # .......................................
  
  beta.0.2 = beta.0
  beta.1.2 = beta.1
  beta.2.2 = beta.2
  beta.3.2 = beta.3
  
  # Generate logisitic regression outcome values (log odds) according to params above
  # see http://stats.stackexchange.com/questions/46523/how-to-simulate-artificial-data-for-logistic-regression
  
  dadmy = sample(x=c(0,1), size=ns.2, 
                 replace=TRUE, 
                 prob=c(0.5,0.5))
  spheq.50 = sample(x=c(0,1), size=ns.2, 
                    replace=TRUE, 
                    prob=c(0.70,0.30)) # take probabilities from table 10.26, page 452
  
  lc.2 = beta.0.2 + beta.1.2*dadmy + beta.2.2*spheq.50 + beta.3.2*dadmy*spheq.50
  y.2 = rbinom(ns.2, 1, lc.2)
  
  df.2 = data.frame(y=y.2, dadmy=dadmy, spheq.50=spheq.50)
  return(df.2)
}

expit <- function(x) 1/(1+exp(-x))

shinyServer(function(input, output) {
  
  withMathJax()
  
  # take parameters entered with the slider bar and use to simulate data
  ldata <- reactive({
    created(input$n1, input$beta0, input$beta1, input$beta2, input$beta3)
  })

  # get a summary of the simulated data
  output$table1 <- renderTable(print(ldata()[1:10,]))
  
  # get 2spheq.50 tables by strata
  # This method doesn't work in putting the object into the server.R
  # #########################################################
  
  # alternate way of getting two by two table
  output$table2alt <- renderTable ({ 
    yn = c("No", "Yes")
    t1 = ldata()
    t1 = within(t1, { y.f = factor(y, labels=yn)
                          dadmy.f = factor(dadmy, labels=yn)
                          spheq.50.f = factor(spheq.50, labels=yn)})
    
    dcast(t1, spheq.50.f + dadmy.f ~ y.f, value.var="spheq.50", fun.aggregate=length)
  })
    
  # Use simulated data to run regression
  
  # binomial distribution with linear link
  m.3 = reactive({
    glm( y ~ dadmy*spheq.50,
         data=ldata(),
         family=binomial(link="identity"),
         start=c(input$beta0, 0 , 0, 0)) # model on page 453
    }) # model on page 449 
    # original object was 'results1'


  # Output results to a table
  # ############################
  output$summary <- renderTable({
    summary(m.3())
  })
    
  # now fit data following a binomial distribution with linear link with logistic regression
  # ................................................................
  # binomial distribution with logit link
  m.4 = reactive({
    glm( y ~ dadmy*spheq.50,
             data=ldata(),
             family=binomial)
  })
  
  # Output results to a table
  # ############################
  output$summary2 <- renderTable({
    summary(m.4())
  })
  
  
  # Make plot
  # plot the log(odds) =  g(x) = beta.0 + beta.1*age + beta.2*male
  # from the logistic regression model.
  # .............................................................
      
  
  ic.calc <- reactive({
    # Get estimates of risks from regression coefficients
    # first digit following the R in the variable name represents spheq.50 variable values
    # second digit represents dadmy variable values.
    R.00 = coef(m.3())[1]
    R.10 = coef(m.3())[1] + coef(m.3())[3]
    R.01 = coef(m.3())[1] + coef(m.3())[2]
    R.11 = sum(coef(m.3()))
    
    # additive interaction contrast (IC)
    IC.3 = (R.00 - R.01) - (R.10 - R.11)
    
    data.frame(R.00, R.10, R.01, R.11, IC.3)
  })
  
  output$ic.r00 <- renderUI({
    withMathJax(
      sprintf("\\(
              R_{00} = \\beta_0 = %0.02f
              \\)",
      ic.calc()$R.00))
  })
  
  output$ic.r11 <- renderUI({
    withMathJax(
      sprintf("\\(
              R_{11} = \\beta_0 + \\beta_1 + \\beta_2 + \\beta_3 = %0.02f
              \\)",
              ic.calc()$R.11))
  })
  
  output$ic.r01 <- renderUI({
    withMathJax(
      sprintf("\\(
              R_{11} = \\beta_0 + \\beta_1 = %0.02f
              \\)",
              ic.calc()$R.01))
  })
  
  output$ic.r10 <- renderUI({
    withMathJax(
      sprintf("\\(
              R_{11} = \\beta_0 + \\beta_3 = %0.02f
              \\)",
              ic.calc()$R.10))
  })

  output$ic.3 <- renderUI({
      withMathJax(
      sprintf("\\(
                \\text{IC} = (R_{00} - R_{01}) - (R_{10} - R_{11}) = %.02f
              \\)",
              ic.calc()$IC.3))
     })
  
  output$plot1 <- renderPlot({
    
    # Get estimates of risks from regression coefficients
    # first digit following the R in the variable name represents spheq.50 variable values
    # second digit represents dadmy variable values.
    R.00 = coef(m.3())[1]
    R.10 = coef(m.3())[1] + coef(m.3())[3]
    R.01 = coef(m.3())[1] + coef(m.3())[2]
    R.11 = sum(coef(m.3()))
    
    # additive interaction contrast (IC)
    IC.3 = (R.00 - R.01) - (R.10 - R.11)
    
    # Get estimates of odds from regression coefficients
    # first digit following the R in the variable name represents spheq.50 variable values
    # second digit represents dadmy variable values.
    Odd.00 = exp(coef(m.4())[1])
    Odd.10 = exp(coef(m.4())[1] + coef(m.4())[3])
    Odd.01 = exp(coef(m.4())[1] + coef(m.4())[2])
    Odd.11 = exp(sum(coef(m.4())))
    
    # odds ratio within SPHEQ.50 variable (dad myopic vs not)
    or.high = Odd.11 / Odd.10
    or.low = Odd.01 / Odd.00
    
    # plot the predicted risks for additive and multiplicative model
    # ..............................................................
    
    df.2.plot = cbind(expand.grid(dadmy=unique(ldata()$dadmy), 
                                  spheq.50=unique(ldata()$spheq.50)), 
                      risks = c(R.01, R.00, R.11, R.10))
    df.2.plot = within(df.2.plot, {dadmy.f = factor(dadmy, labels=c("Not myopic", "Myopic"))
                                   spheq.50.f = factor(spheq.50, labels=c("High", "Low"))})
    
    plot.ex2 = 
      ggplot(aes(x=spheq.50.f, y=risks, colour=dadmy.f), data=df.2.plot) +
      geom_point(size = 5, shape = 2) +
      scale_colour_discrete("Status of dad") +
      theme_bw() +
      theme(text = element_text(size=16),
            axis.text.x  = element_text(size=16)) +
      xlab("SPHEQ, sphreical equivalent refraction") +
      ylab("Risk of myopia in child") +
      theme(legend.position="bottom")
    
    # Add annotation
    
    # Note: first digit following the R in the variable name represents spheq.50 variable values
    # second digit represents dadmy variable values.
    p.01 = df.2.plot[df.2.plot$dadmy==1 & df.2.plot$spheq.50==0,]$risks
    p.10 = df.2.plot[df.2.plot$dadmy==0 & df.2.plot$spheq.50==1,]$risks
    p.11 = df.2.plot[df.2.plot$dadmy==1 & df.2.plot$spheq.50==1,]$risks
    p.00 = df.2.plot[df.2.plot$dadmy==0 & df.2.plot$spheq.50==0,]$risks
    
    # risk difference within SPHEQ.50 variable
    rd.high = p.11 - p.10; rd.high
    rd.low = p.01 - p.00; rd.low
    
plot.ex3 =
  plot.ex2 + 
      annotate("text", y=mean(c(p.11, p.10))-0.02, x = 2+0.3, 
               label=paste("RD=", round(rd.high, digits=3)) ) +
      annotate("text", y=mean(c(p.11, p.10)), x = 2+0.3, 
               label=paste("OR=", round(or.high, digits=3)) ) +
      annotate("text", y= mean(c(p.01, p.00))-0.02, x = 1+0.3, 
               label=paste("RD=", round(rd.low, digits=3)) ) +
      annotate("text", y = mean(c(p.01, p.00)), x = 1+0.3, 
               label=paste("OR=", round(or.low, digits=3)) ) + 
      
      annotate("text", y=p.11, x = 2-0.3, 
               label=paste(round(p.11, digits=3)) ) +
      annotate("text", y=p.10, x = 2-0.3, 
               label=paste(round(p.10, digits=3)) ) +
      annotate("text", y=p.01, x = 1-0.3, 
               label=paste(round(p.01, digits=3)) ) +
      annotate("text", y=p.00, x = 1-0.3, 
               label=paste(round(p.00, digits=3)) )

return(plot.ex3)
 })
  

output$plot2 <- renderPlot({
  
  # Get estimates of risks from regression coefficients
  # first digit following the R in the variable name represents spheq.50 variable values
  # second digit represents dadmy variable values.
  p.00 = coef(m.3())[1]
  p.10 = coef(m.3())[1] + coef(m.3())[3]
  p.01 = coef(m.3())[1] + coef(m.3())[2]
  p.11 = sum(coef(m.3()))
    
  # Get estimates of odds from regression coefficients
  # first digit following the R in the variable name represents spheq.50 variable values
  # second digit represents dadmy variable values.
  Odd.00 = exp(coef(m.4())[1])
  Odd.10 = exp(coef(m.4())[1] + coef(m.4())[3])
  Odd.01 = exp(coef(m.4())[1] + coef(m.4())[2])
  Odd.11 = exp(sum(coef(m.4())))
  
  # Add an extra figure demonstrating the interaction (Super-additive, sub-additive, etc...)
  
  # checking multiplicative interaction
  
  dat.plot.add = rbind.data.frame(
    list(0, as.numeric(p.01+p.10), "Additive", "sub-"),
    list(as.numeric(p.01+p.10), 1, "Additive", "super-"),
    list(0, as.numeric(expit(coef(m.4())[2] + coef(m.4())[3])), "Multiplicative", "sub-"),
    list(as.numeric(expit(coef(m.4())[2] + coef(m.4())[3])), 1, "Multiplicative", "super-"),
    list(as.numeric(expit(sum(coef(m.4())[2:4]))), as.numeric(expit(sum(coef(m.4())[2:4]))), "Multiplicative", "Observed"),
    list(as.numeric(p.11), as.numeric(p.11), "Additive", "Observed")
  )
  
  colnames(dat.plot.add) = c("start", "end", "type", "group")

  
  
  ggplot(dat.plot.add, aes(colour=group)) + 
    geom_segment(aes(x=start, xend=end, y=type, yend=type), size=3) +
    geom_point(data=dat.plot.add[dat.plot.add$group=="Observed",], aes(x=start, y=type), shape=19, size=10, colour="black")+
    xlab("Duration") +
    scale_x_continuous(breaks=seq(0,1,0.1)) +
    #  geom_vline(x=(p.11), color="red") +
    #  geom_vline(x=expit(Odd.11), color="black") +
    scale_colour_discrete("Type of interaction") +
    ylab("Scale")+
    xlab("Risk") +
    scale_colour_manual(name="Type of interaction",
                        values=c("Observed"="black", "sub-"="blue", "super-"="purple")) +
    theme_bw()  
})



        
  # see http://shiny.rstudio.com/gallery/mathjax.html
  # have to be careful with font sizes.
  output$eqn1 <- renderUI({
    withMathJax(
      helpText('\\( \\text{logit(p) = } \\left(\\frac{p}{1-p}\\right) \\text{ = } \\beta_0 + \\beta_1 \\text{age} + \\beta_2 \\text{male} + \\beta_3 \\text{age} \\times \\text{male}\\)')
    )
    })

  # eqn for binomial distribution, linear link model
  output$eqn2 <- renderUI({
    withMathJax(
      helpText('\\( \\text{Risk=} \\pi \\text{(dadmy, spheq.50)} = \\beta_0 + \\beta_1 \\text{dadmy} + \\beta_2 \\text{spheq.50} + \\beta_3 \\text{dadmy} \\times \\text{spheq.50}\\)')
    )
  })

output$eqn3 <- renderUI({
  withMathJax(
    helpText('\\( \\text{Perfect multiplicativity} = OR_{01} \\times OR_{10} = \\text{exp}(\\beta_1 + \\beta_2). \\text{Convert to p by: }p = \\frac{OR}{1+OR} \\)')
  )
})

output$eqn4 <- renderUI({
  withMathJax(
    helpText('\\( \\text{Perfect additivity = } p_{01} + p_{10} = \\beta_1 + \\beta_2 \\)')
  )
})

  # set up text indicating the selected parameters for simulation.
  # .............................................................
  
  output$text0 <- renderUI({
    n = input$n1
    x<-input$beta0
    y<-input$beta1
    z<-input$beta2
    z2 <- input$beta3
    withMathJax(
      sprintf("\\( 
              \\beta_0 = %.02f , 
              \\beta_1 = %.02f ,  
              \\beta_2 = %.02f , 
              \\beta_3 = %.02f ,
              \\text{n} = %d 
              \\)", x, y, z, z2, n)
    )
  })
  
  output$text0i <- renderUI({
    x <- exp(input$beta0)
    y <- exp(input$beta1)
    z <- exp(input$beta2)
    z2 <- exp(input$beta3)
    withMathJax(
      sprintf("\\(
              exp(\\beta_0) = %.02f = \\text{odds of y at x=0 and z=0,}
              \\)",
              x))
    
  })
  
  output$text1i <- renderUI({
    y <- exp(input$beta1)
    withMathJax(
      sprintf("\\(
              exp(\\beta_1) = %.02f = \\text{odds ratio for x=1 vs x=0 with no interaction,}
              \\)",
              y))
    
  })
  
  output$text2i <- renderUI({
    z <- exp(input$beta2)
    withMathJax(
      sprintf("\\(
              exp(\\beta_2) = %.02f = \\text{odds ratio for z=1 vs z=0 with no interaction,}
              \\)",
              z))
    
  })
  
  output$text3i <- renderUI({
    z2 <- exp(input$beta3)
    withMathJax(
      sprintf("\\(
              exp(\\beta_3) = %.02f = \\text{interaction term}
              \\)",
              z2))
    
  })
  
})