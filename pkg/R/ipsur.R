
read <- function(topic, pkg = "IPSUR") {
    topic <- as.character(substitute(topic))
    z <- paste("doc/", topic, ".pdf", sep = "")
    tmp <- list(pdf = file.path(system.file(package = pkg), z))
    f = function (x, ...){
      if (nzchar(out <- x$PDF)) {
        ext <- tools::file_ext(out)
        port <- if (tolower(ext) == "html") 
          tools::startDynamicHelp(NA)
        else 0L
        out <- if (port > 0L) 
          sprintf("http://127.0.0.1:%d/library/%s/doc/%s", 
                  port, basename(x$Dir), out)
        else file.path(x$Dir, "doc", out)
        if (tolower(ext) == "pdf") {
          pdfviewer <- getOption("pdfviewer")
          if (identical(pdfviewer, "false")) {
          }
          else if (.Platform$OS.type == "windows" && identical(pdfviewer, 
                                                               file.path(R.home("bin"), "open.exe"))) 
            shell.exec(out)
          else system2(pdfviewer, shQuote(out), wait = FALSE)
        }
        else browseURL(out)
      }
      else {
        warning(gettextf("vignette %s has no PDF/HTML", sQuote(x$Topic)), 
                call. = FALSE, domain = NA)
      }
      invisible(x)
    }
    f(tmp)
}

ipsur <- function (string, site = "ipsur.org") {
    paste0 <- function(...) paste(..., sep = "")
    string <- paste0("http://www.google.com/search?as_q=",
                     gsub(" ", "+", string))
    mpp <- paste0("as_sitesearch=", site)
    qstring <- paste(URLencode(string), mpp, sep = "&")
    browseURL(qstring)
    cat(gettext("A search query has been submitted to"), "http://www.google.com\n")
    cat(gettext("The results page should open in your browser shortly\n"))
    invisible(qstring)
}
