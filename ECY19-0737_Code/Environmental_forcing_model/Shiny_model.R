
library(shiny)

ui=fluidPage(
  fluidRow(column(3,
sliderInput(inputId = "time",label="Choose the length of the simulation (in year)",
              value=5,min=5,max=1000),
sliderInput(inputId = "DMTc",label="Choose the background wave intensity (small values = high intensity)",
            value=250,min=0,max=2000),
sliderInput(inputId = "DMTe",label="Choose the intensity of hydrodynamic disturbances (small values = high intensity)",
            value=100,min=0,max=2000),
sliderInput(inputId = "freqH",label="Choose the annual probability of an hydrodynamic disturbance",
            value=0.01,min=0,max=1),
sliderInput(inputId = "freqpart",label="Choose the annual probability of a partial mortality event",
            value=0.01,min=0,max=1),
sliderInput(inputId = "partial_ratio",label="Choose the ratio of the coral population touched by partial mortality event",
            value=0.2,min=0,max=1),
sliderInput(inputId = "partial",label="Choose the area of the coral lost once touched by partial mortality event (in cm2)",
            value=250,min=0,max=10000),
submitButton("Run the simulation")
),
column(7,
plotOutput('theplot'))
))

server=function(input,output){
  
  setwd("/Users/yoaneynaud/Desktop/Travail/Post_doc_scripps/Modelling/Am_Nat/multivariate_exploration/")
  source('Model_core_hydro_function_partial_area_mu_rand_cycle_FULL_EXPLOR_window.R')
  require(doMC)
  #colnames(param_explor)=c('DMT','freq_H','rate_of_col','partial_loss','freq_Rand')
  # model_full<-function(tmax,DMTc,DMTe,frequency,plasticity,mu,part,frequency_death)
  #
  

output$theplot=renderPlot({

  simu=model_full(input$time,input$DMTc,input$DMTe,input$freqH,1,input$partial_ratio,input$partial/10000,input$freqpart)
  #plot(runif(100))
  plot(1:input$time,100*simu$proportion[1,]/25,'l',las=1,ylim=c(0,100),col='red',lwd=2,xlab='Time (year)',ylab='cover (%)',main='Benthic groups dynamic')
  lines(1:input$time,100*simu$proportion[2,]/25,'l',las=1,ylim=c(0,100),col='green',lwd=2)
  lines(1:input$time,100*simu$proportion[3,]/25,'l',las=1,ylim=c(0,100),col='blue',lwd=2)

})
#output$theplot=renderPlot({plot(input$time*runif(100)))}
#output$theplot=renderPlot({plot(runif(100))})
}

shinyApp(ui=ui,server=server)