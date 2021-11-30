.getInit <- function(ahid = "click_a_row") {
    paste0(
"## Make sure BiocManager is installed
if (!require('BiocManager', quietly = TRUE))
    install.packages('BiocManager')
## Make sure AnnotationHub is installed
if (!require('AnnotationHub', quietly = TRUE))
    BiocManager::install('AnnotationHub')

## Use this code to download the resource
library(AnnotationHub)
ah <- AnnotationHub()
## Select rows in the table
",
        paste0(paste0("ah[['", ahid, "']]"), collapse = "\n")
    )
}

#' Initialize the shiny application for AnnotationHub resources
#'
#' The shiny app will allow the user to view a table of `AnnotationHub`
#' resources. The user can select a single resource for download or select
#' multiple resources to download a `data.frame` of metadata that includes
#' `AnnotationHub` identifiers used to download, i.e. in pseudocode:
#' `AH[["Identifier"]]`.
#'
#' @details Note. The code here was adapted from `interactiveDisplayBase` and
#' `?'display,Hub-method'`.
#'
#' @seealso ?`interactiveDisplayBase::display`, ?`display,Hub-method`
#'
#' @param ... Further arguments to the `runApp` function
#'
#' @md
#'
#' @import shiny AnnotationHub
#' @export
AnnotationHubApp <- function(...) {
    ui <- fluidPage(
        shinytoastr::useToastr(),
# https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler
        shinyjs::useShinyjs(),
        titlePanel(textOutput("notes"), windowTitle = "AnnotationHubShiny"),
        sidebarLayout(
          sidebarPanel(
            h3("Instructions for resource download"),
            br(),
            h4("1. Click the rows of interest in the main table"),
            br(),
            h4("2. Go to the 'Download' tab"),
            br(),
            h4("3. Copy, paste, and run the generated code in your R session"),
            br(),
            helpText(
                paste(c(
                    "To retrieve the metadata for your selection",
                    "as a date-stamped RDS file, go to the 'Download' tab and",
                    "select the 'Download metadata' button."
                ), collapse=" ")
            ),
            br(),
            actionButton("stopBtn", "Stop AnnotationHubShiny"),
            width=2
          ),
          mainPanel(
            div(
               style = "margin-right:10px",
               img(
                   src = "images/bioconductor_logo_rgb_small.png",
                   align = "right"
               )
            ),
            tabsetPanel(
              tabPanel("Resources", { DT::dataTableOutput('tbl') }),
              tabPanel("Download", {
                fluidRow(
                  column(4,
                      h3("Download resources"),
                      helpText(
                          "Select rows of interest and then run the code",
                          "in an R session."
                      ),
                      helpText(
                          strong("Tip"), ": Use the search box at the top",
                          "right of the table to filter records."
                      ),
                      br(),
                      h3("Download metadata"),
                      br(),
                      helpText(
                          "Select rows and click 'Download metadata'."
                      ),
                      br(),
                      downloadButton("btnSend", "Download metadata"),
                  ),
                  column(6,
                      shinyAce::aceEditor(
                          outputId = "code",
                          selectionId = "selection",
                          value = .getInit(),
                          mode = "r"
                      )
                  )
               )
              }),
              tabPanel("About", {
                HTML(paste0(
                  "<b>AnnotationHubShiny</b> version: ",
                  packageVersion("AnnotationHubShiny"),
                  "<br>",
                  "Last updated: 2021-11-30",
                  "<br>",
                  "Source: ",
                  "<a href='https://github.com/LiNk-NY/AnnotationHubShiny' class='fa fa-github'></a>",
                  "<br>",
                  verbatimTextOutput("sessionInfo")
                ))
                #, align = "center", style = "
                #   bottom:0; width:100%; height:80px; /* Height of footer */
                #   color: darkblue; padding: 10px; background-color: lightgray;"
              }),  # end about panel
            ) # end tabset panel
          ) # end main panel
        ) # end sidebarlayout
    ) # end fluidpage
    ## from interactiveDisplayBase:::.dataFrame3
    server <- function(input, output, session) {

        # data retrieval, massaging
        getAH <- reactive({
            # let the user know that action is ongoing during loading
            shinytoastr::toastr_info(
                "retrieving AnnotationHub...", timeOut=3000
            )
            ah <<- AnnotationHub()
            md <- mcols(ah)
            ans <- as.data.frame(md)
            ans <- as.data.frame(
                append(as.list(ans), list(AHID = rownames(ans)), 0L),
                row.names = NULL
            )

            ans <- ans[,
                -which(names(ans) %in%
                    c("rdataclass", "rdatapath", "sourceurl",
                      "sourcetype", "preparerclass"))
            ]
            ans$tags <- vapply(
                unname(unclass(md$tags)),
                base::paste,
                character(1), collapse = ", "
            )
            ans
        })

        # table rendering
        output$tbl <- DT::renderDataTable(
            {
                shinytoastr::toastr_info(
                    "loading AnnotationHub data...", timeOut=4500
                )
                on.exit({shinytoastr::toastr_info("done.", timeOut=2500)})
                obj_AH <<- getAH()
            },
            server = TRUE,
            filter = "top",
            options = list(orderClasses = TRUE)
        )

        # render title text
        output$notes = renderText({
            nrec <- nrow(obj_AH)
            nspec <- length(unique(ah$species))
            sprintf(
                "Search through %d AnnotationHub resources from %d distinct species in Bioconductor",
                nrec, nspec
            )
        })


        # prepare output selections for download
        observeEvent(
            input$stopBtn,
            {
                # could return information here
                stopApp(returnValue=NULL)
            }
        )

        observeEvent(
            input$tbl_rows_selected,
            {
                idx <- input$tbl_rows_selected
                ans <- obj_AH[idx,]
                shinyAce::updateAceEditor(
                    session,
                    "code",
                    value = .getInit(ans$AHID)
                )
            }
        )

        output$btnSend <- downloadHandler(
            filename = function() {
                msg <- "ahub_md_sel_"
                paste(
                    msg, format(Sys.time(), "%F_%Hh%Mm"), '.rds', sep=''
                )
            },
            content = function(con) {
                idx <- input$tbl_rows_selected
                ans <- obj_AH[idx,]
                saveRDS(ans, file=con)
            },
            contentType="application/octet-stream"
        )
        output$sessionInfo <- renderPrint({
            capture.output(sessionInfo())
        })
    }
    shinyApp(ui, server, ...)
}
