library(shinytoastr)
shinyUI(
    fluidPage(
        useToastr(),
        titlePanel("Search for Annotation Hub resources"),
        sidebarLayout(
            sidebarPanel(
                helpText("Use search box in table to filter records.  Click on rows to select for downloading the metadata."),
                textOutput("notes"),
                downloadButton("btnSend", "Download metadata on selections"),
                width = 3
            ),
            mainPanel(
                DT::dataTableOutput('tbl')
            )
        )
    )
)
