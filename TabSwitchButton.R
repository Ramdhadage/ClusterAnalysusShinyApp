TabSwitchButton_UI <- function(id,inputid,Icon) {
  tagList(actionButton(NS(id,"tab1to2"), Icon))
  
}
TabSwitchButton_Server <-   function(id,inputid, tabname){
    moduleServer(id,function(input, output, session){
      # ToggleTabs <- function(ToggleTabVal){
      #   updateTabsetPanel(session,"tabsetpanelid",ToggleTabVal )
      # }

      observeEvent(input$tab1to2,
                   # print(input$inputid)
                  updateTabsetPanel(session,"tabsetpanelid","ReviewingData")
                   )
    })
  }


