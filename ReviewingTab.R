ReviewingTab_UI <- function(id) {
  tagList(
    selectInput(NS(id,"Cat"),"Category",c("Furniture","Office Supplies","Technology"),"Furniture"),
    plotOutput(NS(id,"sales_trendline")),
    downloadButton(NS(id,"downloadChartData"),"Line Plot Data")
  )
}

ImportDataTab_Server <-  function(id,data){
  moduleServer(id,function(input, output, session){
    Months = c("Jan","Feb","Mar","Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    # browser()
    TotalSalesperMonth.4inputCategory.wrtOrderDate <- reactive({
      data() %>%
        select('Order Date',Category,Sales) %>%
        mutate('Order Date in Month' = factor(format(`Order Date`,"%b"),
                                              levels = Months, ordered = TRUE)) %>%
        filter(Category == input$Cat) %>%
        group_by(`Order Date in Month`) %>%
        summarise("Total Sales"=sum(Sales), .groups='drop')})
    output$sales_trendline <- renderPlot(
      ggplot(TotalSalesperMonth.4inputCategory.wrtOrderDate(), aes( `Order Date in Month`,`Total Sales`, group=1))+
        geom_line(color="blue", size=1)+
        geom_point(color="red", size= 3))
    output$downloadChartData  <- downloadHandler(
      download_file_name <- function(){
        "OrderMonthVsTotalSales.csv"
      },
      content <- function(file){
        write.csv(TotalSalesperMonth.4inputCategory.wrtOrderDate(),file)
      }
    )
    
  }
  )
}