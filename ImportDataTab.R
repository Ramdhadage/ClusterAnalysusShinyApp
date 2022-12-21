
ImportData_UI <- function(id) {
  # ns <- NS(id)
  tagList(
    fileInput(NS(id,"upload"),label = "Upload the data")
    
  )
}

ImportData_Server <-  function(id){
  moduleServer(id,function(input, output, session){
    data  <- reactive({
      req(input$upload)
      
      if(tools::file_ext(input$upload$name ) == "xls"){
        
        
        read_xls(input$upload$datapath) %>% head(n=100)
        
      }else{
        
        validate("Invalid file type; File Extention should be xls")
        
      }
    })
    return(data)

  }
  
  )
}
