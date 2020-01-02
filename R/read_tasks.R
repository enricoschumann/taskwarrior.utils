read_tasks <-
function(file, ...) {
    tasks <- jsonlite::read_json(file)
    class(tasks) <- "taskwarrior_tasks"
    tasks
}

as.data.frame.taskwarrior_tasks <-
function(x, row.names = NULL, optional = FALSE, ...) {

    fetch_field <- function(list, field) {
        tmp <- list[[field]]
        if (is.null(tmp))
            NA
        else
            tmp
    }

    res <- list(uuid = sapply(x, fetch_field, "uuid"),
                id = sapply(x, fetch_field, "id"),
                description = sapply(x, fetch_field, "description"),
                entry = c(as.POSIXct(sapply(x, fetch_field, "entry"), tz = "UTC", format = "%Y%m%dT%H%M%SZ")),
                due = c(as.POSIXct(sapply(x, fetch_field, "due"), tz = "UTC", format = "%Y%m%dT%H%M%SZ")),
                status = sapply(x, fetch_field, "status")
                )
    attr(res, "row.names") <- .set_row_names(length(res[[1L]]))
    class(res) <- "data.frame"
    res

}

tags <- function(tasks, ...)
    sort(unique(unlist(lapply(tasks, `[[`, "tags"))))
