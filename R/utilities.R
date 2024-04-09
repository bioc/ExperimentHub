### =========================================================================
### Helpers for package-specific resource discovery
### -------------------------------------------------------------------------

## export resources as accessor functions
.HUB <- new.env(parent = emptyenv())

.GET_HUB <- function() get("eh", envir = .HUB)

.SET_HUB <- function(value) assign("eh", value, envir=.HUB)

.get_ExperimentHub <- function() {
     eh <- try(.GET_HUB(), silent=TRUE)
     if (inherits(eh, "try-error")) {
       eh <- ExperimentHub::ExperimentHub()
       .SET_HUB(eh)
     }
     eh
}

.hubAccessorFactory <- function(pkgname, title) {
     function(metadata=FALSE) {
         ehub <- .get_ExperimentHub()
         eh1 <- ehub[package(ehub) == pkgname & ehub$title == title]
         if (length(eh1) == 0L)
             stop("\"", title, "\" not found in ExperimentHub")
         if (length(eh1) != 1L)
             stop("\"", title, "\" matches more than 1 ExperimentHub resource")
         if (metadata) {
             eh1
         } else
             eh1[[1L]]
     }
}

createHubAccessors <- function(pkgname, titles) {
    ## Create and export accessor functions in package namespace.
    ns <- asNamespace(pkgname)
    for (title in titles) {
        assign(title, .hubAccessorFactory(pkgname, title), envir=ns)
        namespaceExport(ns, title)
    }
}

## resource discovery

.filterResources_EH <- function(package, filterBy=character()) {
    if (!is.character(filterBy))
        stop("'filterBy' must be a character vector")
    suppressMessages({eh <- ExperimentHub()})
    if (!package %in% unique(package(eh)))
        stop(paste0("'", package, "' resources were not found in ExperimentHub"))

    sub <- query(eh, package)
    if (length(filterBy))
        query(sub, filterBy)
    else
        sub
}

setMethod("listResources", "ExperimentHub",
    function(hub, package, filterBy=character()) {
        metadata <- .filterResources_EH(package, filterBy)
        mcols(metadata)$title
})

setMethod("loadResources", "ExperimentHub",
    function(hub, package, filterBy=character()) {
        metadata <- .filterResources_EH(package, filterBy)
        eh <- ExperimentHub()
        lapply(names(metadata), function(i) eh[[i]])
})
