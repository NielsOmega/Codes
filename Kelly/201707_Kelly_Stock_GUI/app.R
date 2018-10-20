library(shiny)
library(shinydashboard)
#library(ggplot2)
library(Cairo)

library(MASS)
library(StableEstim)

# colors
green2_raw=c(0, 204, 102)/255
green2=rgb(green2_raw[1],green2_raw[2],green2_raw[3]); #green
blue2_raw=c(0, 102, 204)/255
blue2=rgb(blue2_raw[1],blue2_raw[2],blue2_raw[3]); #blue
red2_raw=c(204, 0, 0)/255
red2=rgb(red2_raw[1],red2_raw[2],red2_raw[3]); #red



# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title="Kelly-DAX30"),
  
  dashboardSidebar(
    sidebarMenu(id="tabs"),
    sidebarMenuOutput("menu")
    
  ),
  
  dashboardBody(
  tabItems(
    tabItem(
      tabName = "Programm",
    
    fluidRow(  
    box(
      title="Eingabe",status = "warning", height=440, width=4,
      #sliderInput("W0",
       #           "Startvermoegen",
        #          min = 1,
         #         max = 100,
          #        value = 1),
      selectInput("sim", "Verteilungsannahme:",
                  c("Gauss/Normal" = "gaussian",
                    "Student-T" = "student",
                    "alpha-Stabil" = "stable")),
      sliderInput("f_naive",
                  "Investitionsgrad (in %)",
                  min = 1,
                  max = 200,
                  value = 10,
                  step=2),
      sliderInput("mu",
                  "Erwartete Rendite (p.a. in %)",
                  min = 0,
                  max = 20,
                  value = 5),
      sliderInput("rf",
                  "Risikofreier Zins (p.a. in %)",
                  min = -1,
                  max = 5,
                  value = 0,
                  step=1)
    ),

    box(
      title=textOutput("title1"),status = "primary", width=8, height=440,
      plotOutput("plot_growth",height="360px")
    )),
    
    fluidRow(
    
    box(
      title="Vermoegensstatistiken",status = "primary", height=400, width=4,
      dataTableOutput("plot_table")#,height="320px")
    ),
    

    box(
      title="Beispielpfad",status = "primary", height=400, width=8,
      plotOutput("plot_wealth",height="320px")
    )
    
    )),
    
    tabItem(
      tabName = "Beschreibung",
      
      box(
        title="Beschreibung", height=650, width=4,#4
        tags$div("Sie investieren mehrere Jahre in den DAX30-Index, welcher fuer die Zukunft simuliert wird (Normalverteilung/Student-t/alpha-stabil).
                 Auf Grundlage ihres Vermoegens koennen sie zwischen 0% und 200% ihres Gesamtvermoegens investieren (Investitionsgrad).
                 Bei einem Investitionsgrad kleiner (groesser) als 100% wird das risikofreie Asset gekauft (leerverkauft). 
                 Der risikofreie Zinssatz ist selbst zu waehlen.
                 Der Erwartungswert vom DAX30-Index determiniert das arithmetische Mittel der Simulationen. 
                 Die Frage ist, mit welchem Investitionsgrad das hoechste Vermoegen nach einem gewissen Zeitraum (hier 50 Jahre) erreicht wird. 
                 Die Antwort liefert, beginnend mit Kelly (1956), Merton (1969,1992) fuer die Normalverteilung, welcher den Erwartungswert des logarithmischen Vermoegens maximiert. 
                 Fuer den Fall der Normalverteilung ist die analytische Loesung fuer den Investitionsgrad f=(mu-r)/sigma2 wobei mu den Erwartungswert, sigma2 die Varianz und r den risikofreien Zinssatz darstellt.")
      ),
      
     box(
      title="Referenz",tabPanel("Reference", 
               tags$iframe(style="height:500px; width:100%; scrolling=yes", 
               src="Kelly_Foundations.pdf"))
      )
    ),
    
    tabItem(
      tabName = "Autor",
      box(title="Kontakt",
          tags$p("Niels Wesselhoefft"),
          a("kontakt@statistiker-berlin.de",href="mailto:kontakt@statistiker-berlin.de")
          #tags$iframe(seamless="seamless",src="http://www.investing.com/quotes/streaming-forex-rates-%E2%80%93-majors", height=800, width=1400)
          )
    )
  )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$menu <- renderMenu({
    sidebarMenu(
      tags$hr(),
      menuItem("Programm", tabName="Programm", icon = icon("area-chart")),
      menuItem("Beschreibung", tabName="Beschreibung", icon = icon("book")),
      #tags$img(src='logo.jpg',height='10',width='10'),
      menuItem("Autor", tabName="Autor", icon = icon("database")), #icon("database")
      tags$hr()
    )
  })
  
  
  #function(input,output,session){
  
  var_pass=reactive({ 
    # inputs
    f_naive=(input$f_naive)/100
    f_interval=seq(0,2.5,1/50)
    m=length(f_interval)
    n=10^4
    W_T=matrix(nrow=m, ncol=n)
    W0=1
    f=rbind(f_interval, 1-f_interval)
    
    # for growth plot and boxplot
    source("simulate_distribution.R")
    ret_sim_discrete = simulate_distribution(input$mu/100,input$rf/100,n,input$sim)
    print(rowMeans(ret_sim_discrete))
    
    source("wealth.R")
    W_T=wealth(W0,f,m,n,ret_sim_discrete)
    
    
    g=rowSums(log(W_T/W0))
    g[g<0]=0
    g_max_index=which.max(g)
    g_naive_index=which.min(abs(f_naive-f_interval))
    
    f_opt=f[1,g_max_index]
    
    # for trajectory
    l=51
    W_t_kelly=vector(length=l)
    W_t_naive=vector(length=l)
    W_t_kelly[1]=W0
    W_t_naive[1]=W0
    for (i in 1:50){
      W_t_kelly[1+i]=W_t_kelly[i]*(1+c(f_opt,1-f_opt)%*%ret_sim_discrete[,i])
      W_t_naive[1+i]=W_t_naive[i]*(1+c(f_naive,1-f_naive)%*%ret_sim_discrete[,i])              
    }
    
    bankrupt_kelly_index=which(W_t_kelly < 0)[1]
    bankrupt_naive_index=which(W_t_naive < 0)[1]
    if (is.na(bankrupt_kelly_index)==0){W_t_kelly[bankrupt_kelly_index:l]=0}
    if (is.na(bankrupt_naive_index)==0){W_t_naive[bankrupt_naive_index:l]=0}
    
    
    return(list(W_T,g,g_max_index,g_naive_index,f_opt,f_interval,W_t_kelly,W_t_naive))
    })
    
  #}
  
  output$title1=renderText({paste("Optimaler Investitionsgrad = ", var_pass()[[5]]*100, " % vs.",
                                  "gewaehlter Investitionsgrad", input$f_naive, "%")})
  
  output$plot_growth <- renderPlot({
      
      # code
      f_naive=input$f_naive/100
    
      var_pass = var_pass()
      W_T=var_pass[[1]]
      g=var_pass[[2]]
      g_max_index=var_pass[[3]]
      g_naive_index=var_pass[[4]]
      f_opt=var_pass[[5]]
      f_interval=var_pass[[6]]
      #print(var_pass())
      
      # plot
      par(lwd=1.5,cex=1,mar=c(5,4,2,1))
      plot(f_interval*100,g/100,"l",xlab="Investitionsgrad (in %)", ylab="Kelly Kriterium")
      points(f_opt*100,max(g)/100,col=green2,pch=21,bg=green2,cex=1.3)
      points(f_naive*100,g[g_naive_index]/100,col=blue2,bg=blue2,pch=21,cex=1.3)
      abline(h=max(g)/100,v=f_opt*100,col=green2,lty=2)
      abline(h=g[g_naive_index]/100,v=f_naive*100,col=blue2,lty=2)
      legend("bottomright", legend=c("Kelly", "Auswahl"),
              col=c(green2, blue2),pch=21, cex=1)
      
   })
   
   output$plot_wealth <- renderPlot({
     # input
     var_pass = var_pass()
     W_t_kelly=var_pass[[7]]
     W_t_naive=var_pass[[8]]
     # plot
     par(lwd=1.5,cex=1,mar=c(5,4,2,1))
     plot(2017:2067,W_t_kelly,"l",col=green2,xlab="Jahr", ylab="Vermoegen")
     lines(2017:2067,W_t_naive, col=blue2)
     legend("bottomright", legend=c("Kelly", "Auswahl"),
            col=c(green2, blue2),lty=1, cex=1)
     
   })
   
   output$plot_table <- renderDataTable(
     options=list(
     paging = FALSE,searching = FALSE,bInfo = FALSE),
     
     { #Plot
     # input
     var_pass = var_pass()
     W_T=var_pass[[1]]
     g=var_pass[[2]]
     g_max_index=var_pass[[3]]
     g_naive_index=var_pass[[4]]
     f_opt=var_pass[[5]]
     f_interval=var_pass[[6]]
     
     W_T_kelly=W_T[g_max_index, ]-1
     W_T_naive=W_T[g_naive_index, ]-1
     
     data_table=matrix(round(c(g[g_max_index]/100, g[g_naive_index]/100,
                         mean(W_T_kelly)*100,mean(W_T_naive)*100,
                         sd(W_T_kelly)*100,sd(W_T_naive)*100,
                         -quantile(W_T_kelly,0.5)*100,-quantile(W_T_naive,0.5)*100,
                         -quantile(W_T_kelly,0.05)*100,-quantile(W_T_naive,0.05)*100,
                       -quantile(W_T_kelly,0.01)*100,-quantile(W_T_naive,0.01)*100), digits=2),
                       nrow=6, ncol=2, byrow=TRUE)
     
     data_frame=data.frame(c("Geometrisches Mittel","Arithmetisches Mittel", "Standardabweichung", 
                             "Median","VaR, Konfidenz 95%","VaR, Konfindenz 99%"),data_table)
     colnames(data_frame)=c("Kennzahl (p.a. in %)","Kelly","Auswahl")
     
     return(data_frame)
     
     
     
     # plot
     
     #par(lwd=1.5,cex=1,mar=c(5,4,2,1))
     #plot(density(W_T[g_max_index, ])) #,log='y'
     #lines(density(W_T[g_naive_index,])) #,log='y'
     #boxplot(cbind(log(W_T[g_max_index,]+1),log(W_T[g_naive_index,]+1)),
             #col=(c(green2,blue2)),names=c("Kelly","Auswahl"),ylab="Logarithmus des Endvermoegens") #,log="y"
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

