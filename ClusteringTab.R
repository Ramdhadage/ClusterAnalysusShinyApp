ClusteringTab_UI <- function(id) {
  tagList(
    fluidRow(
      radioButtons(NS(id,"clstring_var"),"Clustring Based On:",
                   c("Sales"="Sales", "Profit"="Profit"),inline = TRUE)),
    fluidRow(
      column(6,
             plotOutput(NS(id,"screeplot"))
             
      ),
      column(6,
             tableOutput(NS(id,"summaryatCluster"))
      )
    ),
    
    fluidRow(
      DTOutput(NS(id,"ClasifyData2Clusters"))
  )
  )
}

ClusteringTab_Server <-  function(id,data){
  moduleServer(id,function(input, output, session){
    
    wss_and_km <- reactive( {
    
      if(input$clstring_var=="Sales")
      {
        data.input.norm <-  data() %>% select(Sales) %>% scale()
        km <- kmeans(data.input.norm,2)
      }else{
        data.input.norm <-  data() %>% select(Profit) %>% scale()
        km <- kmeans(data.input.norm,2)
      }
      
      kmax <- 6
      
      km_wss <-  function(i){
        kmeans(data.input.norm,i)$tot.withinss
      }
      wss=sapply(1:kmax, km_wss)
      
      list(wss, km)
      
    })
    
    data.clusters <- reactive(data() %>%
                                select("Order ID","Customer Name", "Category":"Profit") %>%
                                mutate("Cluster No" = factor(wss_and_km()[[2]][[1]])))
    data4scree_plot <- reactive(tibble("#Cluster"=1:6,"wss"=wss_and_km()[[1]]))
    output$screeplot <- renderPlot(
      ggplot(data4scree_plot(), aes( `#Cluster`,`wss`, group=1))+
        geom_line(color="blue", size=1)+
        geom_point(color="red", size= 3)
    )
    
    output$summaryatCluster <- renderTable(data.clusters() %>%
                                             group_by(`Cluster No`) %>%
                                             summarise("# Customers"=n_distinct(`Customer Name`),"Sales"=sum(Sales),"# Orders" = n_distinct(`Order ID`),
                                                       "# Quantity" = sum(Quantity),"# Sales per customer"=sum(Sales)/n_distinct(`Customer Name`), .groups='drop'),striped = TRUE,bordered = TRUE, hover = TRUE)
    
    
    output$ClasifyData2Clusters <- renderDT(
      data.clusters(),rownames = FALSE,  options=list(lengthMenu = c(5, 10, 20,40,80,100))
    )
    
  })
}