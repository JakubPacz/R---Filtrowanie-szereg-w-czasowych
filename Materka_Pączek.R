library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)

spskr <- c("acp", "ale", "alr", "cdr", "cps", "dnp", "jsw", "kgh", "kru", "kty", "lpp", "mbk", "opl", "pco", "peo", "pge", "pkn", "pko", "pzu", "spl")
sp <- c("ASSECOPOL", "ALLEGRO", "ALIOR", "CDPROJECT", "CYFRPLSAT", "DINOPL", "JSW", "KGHM", "KRUK", "KETY", "LPP", "MBANK", "ORANGEPL", "PEPCO", "PEKAO", "PGE", "PKNORLEN", "PKOBP", "PZU", "SANPL")


ui <- dashboardPage(

    dashboardHeader(title = "Filtrowanie szeregów czasowych dla spółek z indeksu WIG20"),

    dashboardSidebar(
            selectInput(
              'spolka', h3("Wybierz spółkę"),
              choices = sp, 
              multiple=FALSE
            ),
            h3("Wybierz zakres dat", style = "margin-left: 15px"),
            dateInput(
              'poczatek', "Data początkowa", min = 1994-04-16, max = Sys.Date()
            ),
            dateInput(
              'koniec', "Data końcowa", min = 1994-04-16, max = Sys.Date()
            ),
            actionButton("otworzURL", "Załaduj dane"),
            sidebarMenu(
              h3("Menu", style = "margin-left: 15px"),
              menuItem("Średnie dla miesięcy", tabName = "srm0"),
              menuItem("Średnie ruchome miesięczne", tabName = "srm1"),
              menuItem("Średnie ruchome dwumiesięczne", tabName = "sr2m"),
              menuItem("Wykresy średniej ruchomej", tabName = "wykresy12"),
              menuItem(p("Logarytmiczne dzienne stopy", br(), "zwrotu"), tabName = "stz"),
              menuItem("Średnia ruchoma miesięczna", tabName = "stz1"),
              menuItem("Średnia ruchoma dwumiesięczna", tabName = "stz2"),
              menuItem(p("Wykres średniej ruchomej", br(), "logarytmicznych stóp zwrotu"), tabName = "wykresy34")
            )
    ),
    dashboardBody(
      fluidRow(
        tabItems(
        tabItem(
          tabName = "srm0",
          h2("Średnie dla poszczególnych miesięcy", style = "margin-left: 15px"),
          tableOutput("srm")
          ),
        tabItem(
          tabName = "srm1",
          h2("Średnie ruchome miesięczne", style = "margin-left: 15px"),
          tableOutput("sr1m"),
        ),
        tabItem(
          tabName = "sr2m",
          h2("Średnie ruchome dwumiesięczne", style = "margin-left: 15px"),
          tableOutput("sr2m"),
        ),
        tabItem(
          tabName = "wykresy12",
          plotOutput("wykres1"),
          plotOutput("wykres2")
        ),
        tabItem(
          tabName = "stz",
          h2("Logarytmiczne stopy zwrotu",style = "margin-left: 15px" ),
          tableOutput("stz")
        ),
        tabItem(
          tabName = "stz1",
          h2("Średnie ruchome miesięczne logarytmicznych stóp zwrotu", style = "margin-left: 15px"),
          tableOutput("stz1"),
        ),
        tabItem(
          tabName = "stz2",
          h2("Średnie ruchome dwumiesięczne logarytmicznych stóp zwrotu", style = "margin-left: 15px"),
          tableOutput("stz2"),
        ),
        tabItem(
          tabName = "wykresy34",
          plotOutput("wykres3"),
          plotOutput("wykres4")
        )
        )
      )
    )
        

)

server <- function(input, output) {
  
  dane <- reactiveVal()
  
  observeEvent(input$otworzURL, {
    req(input$otworzURL)
    skrot <- spskr[sp == input$spolka]
    pocz <- as.integer(format(input$poczatek, "%Y%m%d")) - 1
    kon <- as.integer(format(input$koniec, "%Y%m%d")) + 1
    url <- paste0("https://stooq.pl/q/d/l/?s=", skrot, "&d1=", pocz, "&d2=", kon, "&i=d")
    x <- data.frame(read.csv(url(url)))
    x[,1] <- format(as.Date(x[,1], "%Y-%m-%d"))
    if(x[1,1] > pocz){
      showNotification(paste("Dane dostępne od", x[1,1]))
    }
    dane(x)
  })
  output$srm <- renderTable({
    if(!is.null(dane())){
    dane() %>%
      mutate(Rok_miesiąc = format(as.Date(Data), "%Y-%m")) %>%
      group_by(Rok_miesiąc) %>%
      summarize("Średnie dla miesięcy" = mean(Zamkniecie))
    }
  })
  
  output$sr1m <- renderTable({
    x <- dane()
    if(!is.null(x)){
    data.frame(cbind(Data = x$Data, Srednie = stats::filter(x$Zamkniecie, sides = 2, rep(1, 20)/20)))
    }})
  
  output$sr2m <- renderTable({
    x <- dane()
    if(!is.null(x)){
    data.frame(cbind(Data = x$Data, Srednie = stats::filter(x$Zamkniecie, sides = 2, rep(1, 40)/40)))
  }})
  
  output$wykres1 <- renderPlot({
    x <- dane()
    if(!is.null(x)){
    sred <- data.frame(cbind(Data = x$Data, Srednie = stats::filter(x$Zamkniecie, sides = 2, rep(1, 20)/20)))
    sred <- sred[!is.na(sred$Srednie),]
    ggplot(sred, aes(x = as.Date(Data), y = as.numeric(Srednie), group = 1)) +
      geom_line(col = "blue") +
      labs(
        title = "Średnie ruchome miesięczne", x= "Data", y = "Średnie"
      ) +
      theme(title = element_text(size=20))
  }})
  
  output$wykres2 <- renderPlot({
    x <- dane()
    if(!is.null(x)){
    sred <- data.frame(cbind(Data = x$Data, Srednie = stats::filter(x$Zamkniecie, sides = 2, rep(1, 40)/40)))
    sred <- sred[!is.na(sred$Srednie),]
    ggplot(sred, aes(x = as.Date(Data), y =as.numeric(Srednie), group = 1)) +
      geom_line(col = "red") +
      labs(
        title = "Średnie ruchome dwumiesięczne", x = "Data", y = "Średnie"
      )  +
      theme(title = element_text(size=20))
  }})
  
  stopyzwrotu <- reactive({
    x <- dane()
    if(!is.null(x)){
    x <- x %>% mutate(StopyZwrotu = log(Zamkniecie/Otwarcie)) %>%
      select(Data, StopyZwrotu)
    return(x)
  }})
  output$stz <- renderTable({
    x <- stopyzwrotu()
    if(!is.null(stopyzwrotu())){
    data.frame(cbind(Data = x$Data, Stopy_Zwrotu = round(x$StopyZwrotu, 5)))
  }})
  
  output$stz1 <- renderTable({
    x <- stopyzwrotu()
    if(!is.null(x)){
    data.frame(cbind(Data = x$Data, Średnie = round(stats::filter(x$StopyZwrotu, sides = 2, rep(1, 20)/20), 5)))
  }})
  
  output$stz2 <- renderTable({
    x <- stopyzwrotu()
    if(!is.null(x)){
    data.frame(cbind(Data = x$Data, Średnie = round(stats::filter(x$StopyZwrotu, sides = 2, rep(1, 40)/40), 5)))
  }})
  
  output$wykres3 <- renderPlot({
    x <- stopyzwrotu()
    if(!is.null(x)){
    sred <- data.frame(cbind(Data = x$Data, Srednie = stats::filter(x$StopyZwrotu, sides = 2, rep(1, 20)/20)))
    sred <- sred[!is.na(sred$Srednie),]
    ggplot(sred, aes(x = as.Date(Data), y = as.numeric(Srednie), group = 1)) +
      geom_line(col = "blue") +
      labs(
        title = "Średnie ruchome miesięczne logarytmów dziennych stóp zwrotu", x= "Data", y = "Średnie", 
      )  +
      theme(title = element_text(size=20))
  }})
  
  output$wykres4 <- renderPlot({
    x <- stopyzwrotu()
    if(!is.null(x)){
    sred <- data.frame(cbind(Data = x$Data, Srednie = stats::filter(x$StopyZwrotu, sides = 2, rep(1, 40)/40)))
    sred <- sred[!is.na(sred$Srednie),]
    ggplot(sred, aes(x = as.Date(Data), y = as.numeric(Srednie), group = 1)) +
      geom_line(col = "red") +
      labs(
        title = "Średnie ruchome dwumiesięczne logarytmów dziennych stóp zwrotu", x = "Data", y= "Średnie"
      )  +
      theme(title = element_text(size=20))
  }})
}
shinyApp(ui = ui, server = server)
