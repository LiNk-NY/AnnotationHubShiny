shinyUI(
    fluidPage(
        titlePanel("Select rows in the Data Table"),
        sidebarLayout(
            sidebarPanel(
                actionButton("btnSend", "Send"),
                width = 1
            ),
            mainPanel(
                DT::dataTableOutput('tbl')
            )
        )
    )
)