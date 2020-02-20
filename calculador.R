library(shiny)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("sandstone"),
  inputPanel(
  column(width = 11,
  textInput('kilo','Valor Kilo', value = 10000,width = '50%'),
  textOutput('kilos')),
  column(width = 11,
         textInput('fletexkilo','Valor Flete x Kilo', value = 1800,width = '50%'),
         textOutput('fletekilo')),
  column(width = 11,
         textInput('precioarrobaventa','Valor Arroba Venta', value = 155000,width = '50%'),
         textOutput('precioarrobaxventa')),
  column(width = 11,
         textInput('Bultosventa','Numero Bultos Venta', value = 5,width = '50%'),
         textOutput('bultosventa')),
 
  submitButton("CALCULAR"), 
  textOutput('CALCULO')
  submitButton("GANANCIA"), 
  textOutput('GANAN')
))

server <- function(input, output){
 KILO <-reactive(as.numeric({input$kilo}))
 FLETEKILO <-reactive(as.numeric({input$fletexkilo}))
 PRECIOARROBAVENTA <-reactive(as.numeric({input$precioarrobaventa})) 
 BULTOSVENTA <-reactive(as.numeric({input$Bultosventa}))
 
  output$CALCULO <- renderText({
    Valor_Arroba <- 12.5*KILO()
    Valor_Flete_X_Arroba <- (FLETEKILO())*12.5
    Kilo_Venta <-(PRECIOARROBAVENTA())/12.5
    Numero_arrobas <-(BULTOSVENTA())*4
    arroba_flete <-  Valor_Arroba +Valor_Flete_X_Arroba 
    Ganancia_X_arroba <- (PRECIOARROBAVENTA() -arroba_flete)
    Precio_Mercancia_Venta <- (PRECIOARROBAVENTA())*Numero_arrobas
    Precio_Mercancia_Compra <- arroba_flete*Numero_arrobas
   Ganancia_Total <- Precio_Mercancia_Venta-Precio_Mercancia_Compra 
     vector <- c("Valor Arroba Compra:","$", format(round(Valor_Arroba,1),big.mark = ","),
                 "Valor Flete X Arroba:","$", format(round(Valor_Flete_X_Arroba,1),big.mark = ","),
                 "Valor Flete + Arroba :","$", format(round(arroba_flete,1),big.mark = ","),
                 "Precio Kilo Venta :","$", format(round(Kilo_Venta,1),big.mark = ","),
                 "Numero Arrobas para Venta:", format(round( Numero_arrobas,1),big.mark = ","),
                 "Ganancia X Arroba:","$", format(round( Ganancia_X_arroba,1),big.mark = ","),
                 "TOTAL COMPRA:","$", format(round( Precio_Mercancia_Compra,1),big.mark = ","),
                 "TOTAL VENTA:","$", format(round(  Precio_Mercancia_Venta,1),big.mark = ","),
                 "GANANCIA:","$", format(round(  Ganancia_Total,1),big.mark = ",")
                   )
     
     })
  
  
}

shinyApp(ui, server)
