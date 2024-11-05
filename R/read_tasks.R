read_tasks <-
function(file, ...) {
    tasks <- jsonlite::read_json(file)
    class(tasks) <- "taskwarrior_tasks"
    tasks
}

as.data.frame.taskwarrior_tasks <-
function(x, row.names = NULL, optional = FALSE, ...,
         fields = c("uuid", "id", "description",
                    "entry", "due", "status"),
         time.fields = c("due", "end", "entry", "modified")) {


    fetch_field <- function(list, field) {
        tmp <- list[[field]]
        if (is.null(tmp))
            NA
        else
            tmp
    }

    res <- list()
    for (field in fields) {
        res[[field]] <-
            if (field %in% time.fields) {
                c(as.POSIXct(sapply(x, fetch_field, field),
                             tz = "UTC",
                             format = "%Y%m%dT%H%M%SZ"))
        } else {
            sapply(x, fetch_field, field)
        }

    }

    attr(res, "row.names") <- .set_row_names(length(res[[1L]]))
    class(res) <- "data.frame"
    res

}

tags <- function(tasks, ...)
    sort(unique(unlist(lapply(tasks, `[[`, "tags"))))
