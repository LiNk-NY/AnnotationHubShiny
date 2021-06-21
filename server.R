library(AnnotationHub)
library(shiny)
options(repos = BiocManager::repositories())

## from AnnotationHub:::.display
object <- AnnotationHub()
md <- mcols(object)
df <- as.data.frame(md)
drops <- c("title", "taxonomyid", "sourceurl")
df <- df[, !(names(df) %in% drops), drop = FALSE]
df$tags <- vapply(
    unname(unclass(md$tags)), base::paste,  character(1), collapse = ", "
)
summaryMessage <- capture.output(show(object))
serverOptions <- list(
    bSortClasses = TRUE,
    aLengthMenu = c(1000, 5000, "All"),
    iDisplayLength = 1000,
    sDom = "<\"top\"i>rt<\"top\"f>lt<\"bottom\"p><\"clear\">"
)

## from interactiveDisplayBase:::.dataFrame3
shinyServer(
    function(input, output) {
            output$tbl <- DT::renderDataTable(
                df, server = TRUE, filter = "top",
                options = list(orderClasses = TRUE)
            )

            if (length(summaryMessage) != 1L) {
                output$summary <- renderUI({
                    HTML(paste0(
                        sprintf(
                            '<span class="shiny-html-output" >%s</span> ',
                            summaryMessage
                        ), "<br>"
                    ))
                })
            }

            observe({
                if (input$btnSend > 0)
                    isolate({
                        idx <- input$tbl_rows_selected
                        stopApp(returnValue = df[idx,])
                    })
            })
    }
)