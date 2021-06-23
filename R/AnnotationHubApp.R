utils::globalVariables("obj_AH")

#' Initialize the shiny application for AnnotationHub resources
#'
#' The shiny app will allow the user to view a table of `AnnotationHub`
#' resources including `AnnotationHub` identifiers which are important for
#' download.
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
            titlePanel("Search for Annotation Hub resources"),
            sidebarLayout(
                sidebarPanel(
                    helpText("Use search box in table to filter records.  Click on rows to select for downloading the metadata."),
                    textOutput("notes"),
                    downloadButton("btnSend", "Download metadata on selections"),
                    helpText("To download a resource, select only one row in the table."),
                    shinyjs::hidden(downloadButton("btnSend2", "Download selection")),
                    width = 3
                ),
                mainPanel(
                    DT::dataTableOutput('tbl')
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
                ans$tags <- vapply(
                    unname(unclass(md$tags)),
                    base::paste,
                    character(1), collapse = ", "
                )
                ans
            }

            # table rendering

            output$tbl <- DT::renderDataTable({
                obj_AH <- getAH()
                fixAH(obj_AH)}, server = TRUE, filter = "top",
                options = list(orderClasses = TRUE)
            )

            output$notesBAD = renderText({
                orgs = sort(table(fixAH(obj_AH)$species), decreasing=TRUE)
                txt = sprintf("Total of %d records.  Top three species: %s (%d records),
                            %s (%d), %s (%d).", nrow(obj_AH), names(orgs)[1], as.numeric(orgs)[1],
                              names(orgs)[2], as.numeric(orgs)[2],
                              names(orgs)[3], as.numeric(orgs)[3])
                txt
            })
            output$notes = renderText({
                tab = fixAH(obj_AH)
                nrec = nrow(tab)
                nspec = length(unique(tab$species))
                sprintf("There are %d AnnotationHub resources available on
                        %d distinct species.", nrec, nspec)
            })

            # prepare output selections for download

            observeEvent( input$tbl_rows_selected, {
                if (length(input$tbl_rows_selected) != 1)
                    shinyjs::hide("btnSend2")
                else
                    shinyjs::show("btnSend2")
            })

            output$btnSend <- downloadHandler(
                filename = function() {
                    msg = "ahub_md_sel_"
                    paste(msg, Sys.Date(), '.rds', sep='')
                },
                content = function(con) {
                    idx = input$tbl_rows_selected
                    ans = fixAH(obj_AH)[idx,]
                    saveRDS(ans, file=con)
                },
                contentType="application/octet-stream"
            )

            output$btnSend2 <- downloadHandler(
                filename = function() {
                    msg = "ahub_sel_"
                    paste(msg, Sys.Date(), '.rds', sep='')
                },
                content = function(con) {
                    idx <- input$tbl_rows_selected
                    tag <- rownames(fixAH(obj_AH)[idx,])
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
