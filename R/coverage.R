
# nocov start

# Single File Coverage -----
# Rendering =====
#' @name covr-rendering-single
#' @title Rendering for single file report
#' @description
#' These functions facilitate the creation of reports for coverage of a
#' single file.
#'
#' @param line,lines Line(s) number
#' @param source source file
#' @param coverage The number of times covered
#' @param file the file in question
#' @param report.file Where to output the HTML report.
#' @param highlight Highlight the row.
#' @param file_stats The coverage object for the file.
#' @param dir the base directory for the HTML output
#' @param libdir Where to put html dependencies?
#'
#' @family coverage
NULL



#' @rdname covr-rendering-single
.renderSourceRow <- function(line, source, coverage) {
    requireNamespace('htmltools')
    cov_type <- NULL
    if (coverage == 0) {
      cov_value <- shiny::tags$td("!")
      cov_type <- "missed"
    } else if (coverage > 0) {
      cov_value <- with(shiny::tags, td( span(coverage, class="coverage-count")
                                       , em("x")
                                       , class='coverage'
                                       ))
      cov_type <- "covered"
    } else {
      cov_type <- "never"
      cov_value <-shiny::tags$td("")
    }
    line <- shiny::tags$td( class = "num"      , line)
    src  <- shiny::tags$td( class = "col-sm-12", shiny::tags$pre(class = "language-r", source))
    htmltools::renderTags(htmltools::tags$tr( class = cov_type, line, src, cov_value))
}
#' @rdname covr-rendering-single
.renderSourceFile <- function(lines, file="source", highlight=TRUE) {
    assert_that( requireNamespace('htmltools')
               , requireNamespace('shiny')
               )
    rows <- Map(.renderSourceRow, lines$line, lines$source, lines$coverage)
    html <- shiny::tags$div( id = file
                           , class = "source-listing"
                           , shiny::tags$table( class = "table-condensed"
                                              , shiny::tag('tbody', rows )
                                              )
                           )
    if (highlight) {
        highlight.deps  <- htmltools::htmlDependency("highlight.js", "6.2",
            system.file(package = "shiny", "www/shared/highlight"),
            script = "highlight.pack.js", stylesheet = "rstudio.css")
        html <- htmltools::attachDependencies( html
                                             , c( htmltools::htmlDependencies(html)
                                                , list(highlight.deps)
                                                ))
    }
    return(htmltools::renderTags(html))
}
#' @rdname covr-rendering-single
.single_file_summary <-
function(file_stats){
    assert_that(requireNamespace('htmltools'))
    htmltools::renderTags(
        with(htmltools::tags,
             table(tbody( tr(th("Coverage:"   ), td(shiny::HTML(file_stats$Coverage)))
                        , tr(th("Total Lines:"), td(shiny::HTML(file_stats$Lines)))
                        , tr(th("Relevant:"   ), td(shiny::HTML(file_stats$Relevant)))
                        , tr(th("Covered:"    ), td(shiny::HTML(file_stats$Covered)))
                        , tr(th("Missed:"     ), td(shiny::HTML(file_stats$Missed)))
                        , tr(th("Hits / Line:"), td(shiny::HTML(file_stats$`Hits / Line`)))
                        ))
             )
    )
}
#' @rdname covr-rendering-single
.renderReport <-
function( coverage
        , report.file
        , dir = dirname(report.file)
        , libdir = file.path(dir, "lib")
        )
{
    assert_that( requireNamespace("shiny")
               , requireNamespace('covr')
               , requireNamespace('DT')
               , isNamespace(covr <- asNamespace('covr'))
               )

    shiny.data <- covr$to_report_data(coverage)

    file <- attr(coverage, 'file')
    pkg  <- attr(coverage, 'package')
    fname <- gsub(normalizePath(pkg$path, '/'), '', file, fixed = TRUE)

    shiny.summary <- DT::datatable( shiny.data$file_stats
                                  , escape = FALSE
                                  , options = list(searching = FALSE, dom = "t", paging = FALSE)
                                  , rownames = FALSE
                                  )
    shiny.source <- .renderSourceFile(shiny.data$full[[1]])
    ui <- shiny::fluidPage( shiny::includeCSS(system.file("www/report.css",package = "covr"))
                          , title = paste0("{", pkg$package, "}", fname , " Coverage")
                          , shiny::column( 8, offset=2
                                         , htmltools::tags$h1( "Coverage for file"
                                                             ,  htmltools::tags$pre(fname))
                                         , shiny::tabsetPanel( shiny::tabPanel( htmltools::tags$h2("Summary")
                                                                              , .single_file_summary(shiny.data$file_stats)
                                                                              ))
                                         , shiny::tabsetPanel( shiny::tabPanel( htmltools::tags$h2("Source")
                                                                              , shiny.source
                                                                              ))
                                         )
                          )

    ui <- htmltools::tags$body(ui, style = "background-color:white")

    ui <- htmltools::renderTags(ui)
    if (!dir.exists(libdir)) dir.create(libdir, recursive = TRUE)
    ui$dependencies <- lapply(ui$dependencies, function(dep) {
        dep <- htmltools::copyDependencyToDir(dep, libdir, FALSE)
        dep <- htmltools::makeDependencyRelative(dep, dir, FALSE)
        dep
    })

    html <- c( "<!DOCTYPE html>"
             , "<html>"
                 , "<head>"
                      , "<meta charset=\"utf-8\"/>"
                      , htmltools::renderDependencies(ui$dependencies)
                      , ui$head
                  , "</head>"
                  , ui$html
              , "</html>")
    writeLines(html, report.file, useBytes = TRUE)
}

# Computing =====
#' @name covr-single
#' @title Single File Coverage
#' @description
#' These functions extract tests, run tests and create a report of the coverage for
#' a single file.
#'
#' @param file The file to extract test from and compute coverage.
#' @param pkg The package `file` is associated with.
#' @inheritDotParams covr::file_coverage
#' @param coverage Coverage returned from `file_coverage()`.
#' @param report.file Where to save the HTML report.
#' @param show.report if the HTML report should be displayed.
NULL

#' @describeIn covr-single Extract tests and compute the coverage for the given file.
file_coverage <-
function( file = rstudioapi::getSourceEditorContext()$path
        , pkg = '.'
        , ...
        ){
    assert_that( requireNamespace('covr', quietly = TRUE)
               , requireNamespace('devtools', quietly = TRUE)
               , isNamespace(covr <- asNamespace('covr'))
               )
    rstudioapi::documentSave() %if% missing(file)

    pkg <- devtools::as.package(pkg)
    if (!isNamespaceLoaded(pkg$package)) devtools::load_all(pkg$path)
    env <- asNamespace(pkg$package)
    covr$trace_environment(env)
    on.exit({
        covr$reset_traces()
        covr$clear_counters()
    })

    tests <- .extract_tests_to_file( file, verbose=TRUE)
    testthat::test_file(attr(tests, 'test.file'), env=env)

    coverage <- structure(as.list(covr$.counters), class = "coverage")
    coverage <- covr$exclude(coverage, ..., path=pkg$path)

    pat <- paste0("^", gsub("\\.", "\\\\.", basename(file)), ":[0-9:]+$")
    coverage <- structure( coverage[grepl(pat, names(coverage))]
                         , class = 'coverage'
                         , package=pkg
                         , relative=TRUE
                         , file=file
                         )
}

#' @describeIn covr-single Create a report for a single
covr_file <-
function( coverage = file_coverage()
        , report.file = NULL
        , show.report=interactive()
        ){
    force(coverage)
    assert_that( requireNamespace("shiny")
               , requireNamespace('covr')
               , requireNamespace('DT')
               , isNamespace(covr <- asNamespace('covr'))
               )
    if (is.null(report.file))
        report.file <- file.path(tempdir(), paste0("coverage-report-", basename(attr(coverage, 'file')), ".html"))
    report.file <- normalizePath(report.file, '/', FALSE)
    .renderReport(coverage, report.file)
    if (show.report)
        rstudioapi::viewer(report.file) # nocov
    invisible(report.file)
}
if(FALSE){# Interactive testing, do not extract.
    #single file extract and coverage
    tmp.dir <- normalizePath(tempdir(), '/')
    pkg <- file.path(tmp.dir, "testExtractionTest")
    if (dir.exists(pkg)) unlink(pkg, recursive = TRUE, force = TRUE)
    package.skeleton("testExtractionTest"
                    , path=tmp.dir, force=TRUE
                    , code_files = list.files(system.file("testExtractionTest", "R", package='testextra'), full=TRUE)
                    )
    dir.create(file.path(pkg, 'tests', 'testthat'), recursive = TRUE)

    file <- file.path(pkg, 'R', 'function.R')
    expect_true(file.exists(file))

    coverage <- file_coverage(file, pkg)
    expect_is(coverage, 'coverage')
    expect_length(names(coverage), 1L)
    expect_true(file.exists(file.path(pkg, 'tests', 'testthat', 'test-function.R')))

    report.file <- file.path(pkg, 'covr', 'covr-function.html')

    expect_null(.renderReport(coverage, report.file))
    expect_true(file.exists(report.file))

    output <- covr_file(coverage, report.file, FALSE)

    expect_identical(output, report.file)

    output.lines <- readLines(output)
    output.lines <- gsub( "data-tabsetid=\"\\d+\""
                        , "data-tabsetid=\"1234\""
                        , output.lines)
    output.lines <- gsub( "\"(#?)tab-\\d+-1\""
                        , "\"\\1tab-1234-1\""
                        , output.lines)

    expected.lines <- readLines(system.file("testExtractionTest", "covr-expected", "covr-function.html", package='testextra'))
    expected.lines <- gsub( "data-tabsetid=\"\\d+\""
                          , "data-tabsetid=\"1234\""
                          , expected.lines)
    expected.lines <- gsub( "\"(#?)tab-\\d+-1\""
                          , "\"\\1tab-1234-1\""
                          , expected.lines)

    expect_identical(output.lines, expected.lines)
}


### Coverage for File Groups #####
#' Compute coverage for a group of files.
#'
#' @param filter A regular expression filter to apply to the files from `pkg`.
#' @param pkg The package to compute coverage for.
#' @param report If a report should be constructed and shown.
covr_files <-
function( filter
        , pkg = '.'
        , report = TRUE
        ){
    pkg <- devtools::as.package(pkg)
    assert_that( requireNamespace("shiny")
               , requireNamespace('covr')
               , isNamespace(covr <- asNamespace('covr'))
               , isNamespace(testthat <- asNamespace('testthat'))
               )
    rstudioapi::documentSaveAll()

    devtools::load_all(pkg)

    tests <- extract_tests(pkg, filter=filter, verbose=TRUE)
    pkg_message(length(unlist(tests)) %<<% 'test blocks extracted' %<<%
                "from" %<<% length(Filter(length, tests)) %<<<% '/' %<<<%
                length(tests) %<<% 'files.\n'
                )
    if (requireNamespace('devtools')) {
        devtools::test(pkg=pkg, filter=filter, perl=TRUE)
    }
    src.files <- names(tests)
    test.files <- as.character(purrr::compact(purrr::map(tests, attr, 'test.file')))

    env <- asNamespace(pkg$package)
    covr$trace_environment(env)
    on.exit({
        covr$reset_traces()
        covr$clear_counters()
    })

    testthat$test_files(test.files)
    coverage <- structure(as.list(covr$.counters), class = "coverage")
    coverage <- covr$exclude(coverage, path=pkg$path)

    pat <- paste0("^(", collapse(gsub("([.])", "\\\\\\1", basename(src.files)), '|'), "):[0-9:]+$")
    coverage <- structure( coverage[grepl(pat, names(coverage))]
                         , class = 'coverage'
                         , package=pkg
                         , relative=TRUE
                         , file=file
                         )
    if (report) covr::report(coverage, browse = TRUE)
    invisible(coverage)
}
# nocov end


# RStudio Addins ----------------------------------------------------------
# nocov start
#' Add-in for `covr_file`
#'
#' This allows for [covr_file] to be run from a menu in RStudio.
addin_covr_file <- function(){
    stopifnot(requireNamespace("rstudioapi"))
    pkg <- rstudioapi::getActiveProject()
    doc <- rstudioapi::getSourceEditorContext()
    rstudioapi::documentSave(doc$id)
    try(covr_file( file_coverage(doc$path, pkg = pkg)
                 , show.report = TRUE
                 ))
}

#' Add-in for Extract & Coverage
#'
#'
addin_extract_covr <- function(){
    stopifnot(requireNamespace("rstudioapi"))
    project <- rstudioapi::getActiveProject()
    if (is.null(project)) project <- getwd()
    try({
        extract_tests(project)
        covr::report(covr::package_coverage(project))
    })
}


# nocov end
