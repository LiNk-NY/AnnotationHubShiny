.getInit <- function(ahid = "click_a_row") {
    sprintf(
"## Use this code to download a resource
library(AnnotationHub)
ah <- AnnotationHub()
## Select a _single_ row
ah[['%s']]
", ahid
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
    ui <-
        fluidPage(
            shinytoastr::useToastr(),
            shinyjs::useShinyjs(),  # see https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler
            titlePanel(textOutput("notes")),

            DT::dataTableOutput('tbl'),

            hr(),

            fluidRow(
                column(4,

                    h3("Download instructions"),
                    helpText(
                        "To download a resource, select a single row and",
                        "run the code in an R session."
                    ),
                    helpText(
                        "Tip: Use the search box at the top right",
                        "of the table to filter records."
                    ),
                    br(),
                    helpText("Select multiple rows to download metadata."),
                    br(),
                    downloadButton("btnSend", "Download metadata"),
                    br()
                    # shinyjs::hidden(downloadButton("btnSend2", "Download resource"))

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
        )

        ## from interactiveDisplayBase:::.dataFrame3
    server <-
        function(input, output, session) {

            # data retrieval, massaging

            getAH <- reactive({  # there may be a delay so let the user know that action is ongoing
                shinytoastr::toastr_info("retrieving AnnotationHub...", timeOut=2000)
                object <- AnnotationHub()
                shinytoastr::toastr_info("done.", timeOut=500)
                object
            })
            fixAH <- function(object) {  # can filter columns here if desired
                md <- mcols(object)
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
            }

            # table rendering

            output$tbl <- DT::renderDataTable(
                {
                    obj_AH <<- getAH()
                    fixAH(obj_AH)
                },
                server = TRUE,
                filter = "top",
                options = list(orderClasses = TRUE)
            )

            output$notes = renderText({
                tab <- fixAH(obj_AH)
                nrec <- nrow(tab)
                nspec <- length(unique(tab$species))
                sprintf(
                    "Search through %d AnnotationHub resources from %d distinct species",
                    nrec, nspec
                )
            })

            # prepare output selections for download

            observeEvent(
                input$tbl_rows_selected,
                {
                    if (length(input$tbl_rows_selected) == 1) {
                        shinyjs::show("btnSend2")
                        idx <- input$tbl_rows_selected
                        ans <- fixAH(obj_AH)[idx,]
                        shinyAce::updateAceEditor(
                            session,
                            "code",
                            value = .getInit(ans$AHID)
                        )
                    } else if (length(input$tbl_rows_selected) > 1) {
                        shinyjs::hide("btnSend2")
                        shinyAce::updateAceEditor(
                            session,
                            "code",
                            value = .getInit()
                        )
                    }
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
                    ans <- fixAH(obj_AH)[idx,]
                    saveRDS(ans, file=con)
                },
                contentType="application/octet-stream"
            )

            output$btnSend2 <- downloadHandler(
                filename = function() {
                    idx <- input$tbl_rows_selected
                    ans <- fixAH(obj_AH)[idx,]
                    paste0(
                        ans[["AHID"]], "_",
                        format(Sys.time(), "%F_%Hh%Mm"), '.rds'
                    )
                },
                content = function(con) {
                    idx <- input$tbl_rows_selected
                    tag <- fixAH(obj_AH)[idx, "AHID"]
                    shinytoastr::toastr_info(paste("retrieving", tag))
                    ans <- obj_AH[[tag]]
                    attr(ans, "_ahub_app_tag") = tag
                    saveRDS(ans, file=con)
                },
                contentType="application/octet-stream"
            )
        }

    shinyApp(ui, server, ...)
}
