library(shiny)
library(shinydashboard)
#library(ggplot2)
library(Cairo)

# colors
green2_raw=c(0, 204, 102)/255
green2=rgb(green2_raw[1],green2_raw[2],green2_raw[3]); #green
blue2_raw=c(0, 102, 204)/255
blue2=rgb(blue2_raw[1],blue2_raw[2],blue2_raw[3]); #blue
red2_raw=c(204, 0, 0)/255
red2=rgb(red2_raw[1],red2_raw[2],red2_raw[3]); #red



# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title="Kelly-Investition"),
  
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
      title="Eingabe",status = "warning", height=450, width=4,
      #sliderInput("W0",
       #           "Startvermoegen",
        #          min = 1,
         #         max = 100,
          #        value = 1),
      sliderInput("f_naive",
                  "Investitionsgrad pro Runde (in %)",
                  min = 1,
                  max = 100,
                  value = 10),
      sliderInput("p",
                  "Gewinnwahrscheinlichkeit (in %)",
                  min = 0,
                  max = 100,
                  value = 60),
      sliderInput("o",
                  "Quote",
                  min = 0,
                  max = 50,
                  value = 1),
      
      sliderInput("n",
                  "Anzahl der Spiele",
                  min = 1,
                  max = 500,
                  value = 100,
                  step=1)
      #tags$b("Kelly-optimaler Investitionsgrad"),
      #textOutput("text_fraction"),
      #tags$head(tags$style("#text_fraction{font-size: 20px;font-style: bold;
      #                     }"))
    ),

    box(
      title=textOutput("title1"),status = "primary", width=8, height=450,
      plotOutput("plot_growth",height="320px")
    )),
    
    fluidRow(
    
    box(
      title="Boxplot des Endvermoegens",status = "primary", height=400, width=4,
      plotOutput("plot_boxplot",height="320px")
    ),
    

    box(
      title="Beispielpfad",status = "primary", height=400, width=8,
      plotOutput("plot_wealth",height="320px")
    )
    
    )),
    
    tabItem(
      tabName = "Beschreibung",
      
      box(
        title="Beschreibung", height=570, width=6,#4
        tags$div("Sie spielen eine Folge von Spielen mit einer unfairen Muenze (Bernoulli-Experiment). 
        Auf Grundlage ihres Vermoegens koennen sie zwischen 0% und 100% ihres Gesamtvermoegens pro Runde investieren (Investitionsgrad). 
        Die Gewinnwahrscheinlichkeit determiniert, wie oft das gewuenschte Ereignis eintritt (in %). 
        Die Frage ist, mit welchem Investitionsgrad das hoechste Vermoegen nach der gewaehlten Anzahl der Spiele erreicht wird.
        Die Antwort liefert die Kelly (1956) Optimierung, welche den Erwartungswert des logarithmischen Vermoegens maximiert.
        Fuer den Bernoulli-Fall ist die analytische Loesung fuer den Investitionsgrad f=2*p-1 wobei p die Gewinnwahrscheinlichkeit darstellt.")
      ),
      
     box(
      title="Referenz",width=6,tabPanel("Reference", 
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




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  source("simulate_bernoulli.R")
  b = reactive({simulate_bernoulli(500,input$p/100,input$o,500)}) #m=value
  
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
  
  output$title1=renderText({paste("Optimaler Investitionsgrad = ", (2*input$p/100-1)*100, " % vs.",
                                  "gewaehlter Investitionsgrad", input$f_naive, "%")})
  
  #output$text_fraction=renderText({paste((2*input$p/100-1)*100, "%")})
  
  
   output$plot_growth <- renderPlot({
      # code
      p=(input$p)/100
      o=input$o
      f_naive=(input$f_naive)/100
      f=(o*p-(1-p))/o
      f_interval=seq(0,2*f,2*f/50)
      g=p*log(1+o*f_interval)+(1-p)*log(1-f_interval)
      # plot
      par(lwd=1.5,cex=1,mar=c(5,4,2,1))
      plot(f_interval*100,g*100,"l",xlab="Investitionsgrad (in %)", ylab="Kelly Kriterium")
      points(f*100,max(g*100,na.rm = TRUE),col=green2,pch=21,bg=green2,cex=1.3)
      g_naive=which.min(abs(f_naive-f_interval))
      points(f_naive*100,g[g_naive]*100,col=blue2,bg=blue2,pch=21,cex=1.3)
      abline(h=max(g,na.rm = TRUE)*100,v=f*100,col=green2,lty=2)
      abline(h=g[g_naive]*100,v=f_naive*100,col=blue2,lty=2)
      legend("topleft", legend=c("Kelly", "Auswahl"),
             col=c(green2, blue2),pch=21, cex=1)
      
   })
   
   output$plot_wealth <- renderPlot({
     # input
     p=(input$p)/100
     n=(input$n)
     o=input$o
     W0=100#(input$W0)
     f_naive=(input$f_naive)/100
     
     f=(o*p-(1-p))/o
     m=1
     
     source("wealth_bernoulli.R")
     W_kelly=wealth_bernoulli(W0,b(),n,m,f)
     W_naive=wealth_bernoulli(W0,b(),n,m,f_naive)
     
     # plot
     par(lwd=1.5,cex=1,mar=c(5,4,2,1))
     plot(W_kelly,,"l",col=green2,xlab="Anzahl der Spiele", ylab="Vermoegen")
     lines(W_naive, col=blue2)
     legend("topleft", legend=c("Kelly", "Auswahl"),
            col=c(green2, blue2),lty=1, cex=1)
     
   })
   
   output$plot_boxplot <- renderPlot({
     # input
     p=(input$p)/100
     n=(input$n)
     o=input$o
     W0=100#(input$W0)
     f_naive=(input$f_naive)/100
     f=(o*p-(1-p))/o
     m=500
     
     source("wealth_bernoulli.R")
     W_kelly=wealth_bernoulli(W0,b(),n,m,f)
     W_naive=wealth_bernoulli(W0,b(),n,m,f_naive)
     
     # plot
     par(lwd=1.5,cex=1,mar=c(5,4,2,1))
     boxplot(cbind(log(W_kelly[n+1,]+1),log(W_naive[n+1,]+1)),
             col=(c(green2,blue2)),names=c("Kelly","Auswahl"),ylab="Logarithmus des Endvermoegens") #,log="y"
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

