
#' Add single quotes to strings, useful for converting R strings into SQL formatted strings.
#'
#' @param x A string.
#' @param char_only TRUE/FALSE, if TRUE, adds quotes only if is.character(x) is TRUE.
#' @return A string, with single quotes added to match postgreSQL string formatting.
#' @examples
#' quoteText("Sample quotes.")
quoteText <- function(x, char_only = TRUE){

  if(char_only == TRUE){
    if(is.character(x) == TRUE){
      return(paste0("'", gsub("\'", "''", x), "'"))
    }else{
      return(x)
    }
  }else{
    return(paste0("'", gsub("\'", "''", x), "'"))
  }

}

#' Add double quotes to strings.
#'
#' @param x A string.
#' @param char_only TRUE/FALSE, if TRUE, adds quotes only if is.character(x) is TRUE.
#' @return A string, with double quotes added.
#' @examples
#' doubleQuoteText("Sample quotes.")
doubleQuoteText <- function(x, char_only = TRUE){

  if(char_only == TRUE){
    if(is.character(x) == TRUE){
      return(paste0('"', x, '"'))
    }else{
      return(x)
    }
  }else{
    return(paste0('"', x, '"'))
  }

}

#' Format data as a JSON object (like this: { “x”: “120” }).
#'
#' @param name A string, the name of the JSON entry
#' @param val A string, the value to associate with the JSON entry.
#' @return A string, data formatted as a JSON object.
#' @examples
#' jsonStr(name = "var1", val = "Blue")
jsonStr <- function(name, val){
  paste0("{", doubleQuoteText(name), ":", doubleQuoteText(val), "}")
}

#' Convert strings to numeric if possible, otherwise remains as is.
#'
#' @param x A string.
#' @return A string, converted to numeric if possible.
#' @examples
#' castNumeric("100")
castNumeric <- function(x){
  suppressWarnings(
    if(is.na(as.numeric(x)) == FALSE){
      x <- as.numeric(x)
    }
  )
  return(x)
}

#' Format a date string as "%Y-%m-%d" (YYYY-MM-DD), useful for converting dates selected
#' from a SQL database to a format compatible with a HTML date input value.
#'
#' @param x A string.
#' @return A string, formatted YYYY-MM-DD.
#' @examples
#' castDateString(Sys.time())
castDateString <- function(x){

  return(
    if(nchar(x) > 0){
      format(x, "%Y-%m-%d")
    }else{
      x
    }
  )
}

#' Replace NA values with "", useful for passing values to HTML tags.
#'
#' @param x A vector of length 1.
#' @return A string, if x is NA, returns "".
#' @examples
#' toInput(NA)
toInput <- function(x){

  if(NA %in% x){
    x <- ""
  }

  return(x)

}

#' Prepare values collected from HTML forms to save to a SQL database by calling quoteText. If x is "", returns "NULL".
#'
#' @param x A vector of length 1.
#' @return A string, if x is "", returns "NULL".
#' @examples
#' fromInput("Test")
#' fromInput("100")
#' fromInput(100)
#' fromInput("")
fromInput <- function(x){

  if("" %in% x){
    x <- "NULL"
  }else{
    x <- quoteText(x)
  }

  return(x)

}

#' Generates (pseudo)random strings of the specified char length.
#'
#' @param char A integer, the number of chars to include in the output string.
#' @return A string.
#' @examples
#' sampleStr(10)
sampleStr <- function(char){

  x <- c()

  for(i in 1:char){
    x <- c(x, sample(c(letters, LETTERS, 0:9), 1))
  }

  return(
    paste0(x, collapse = "")
  )
}

#' Creates HTML option tags for each position of a list of values and labels by calling html5::option(), returning a string of HTML to pass to a select tag through html5::select().
#'
#' @param x A named list, one name should refer to a vector of values, one name should refer to a vector of labels equal in length to the values.
#' @param value The name of the position in x to use as the value attribute for each option tag.
#' @param label The name of the position in x to use as the displayed content for each option tag.
#' @param selected_value A value in the vector passed as value to mark as the initially selected option in the select tag.
#' @param add_blank TRUE/FALSE, if TRUE, adds a blank ("") option tag.
#' @return A string, with an option tag each row of x.
#' @examples
#' smart_options(
#' x = list(col1 = c("1", "2", "3"), col2 = c("New York", "Los Angeles", "Chicago")),
#' value = "col1",
#' label = "col2",
#' selected_value = "3",
#' add_blank = TRUE
#' )
smart_options <- function(x, value, label, selected_value, add_blank = FALSE){

  values <- x[[value]]

  labels <- x[[label]]

  if(length(values) == length(labels)){

    options <- list()

    lapply(1:length(values), function(i){

      if(values[i] == selected_value){
        options[[i]] <<- option(value = values[i], selected = TRUE, labels[i])
      }else{
        options[[i]] <<- option(value = values[i], labels[i])
      }

    })

    if(add_blank == TRUE){

      options[[length(values) + 1]] <- option(value = "", "")

    }

    options <- paste0(unlist(options), collapse = "")

    return(options)

  }else{

    print("Warning: length of value vector must equal length of label vector.")
    return()

  }

}

#' Parse HTTP parameter strings.
#'
#' @param x A parameter string, likely accessed from req[["rook.input"]]$read_lines().
#' @param split The character to use to split the parameter string into constituent parameters.
#' @param custom_decode A named list, must consist of list(pattern = c(...), replacement = c(...)) where pattern contains characters to decode that are not included in utils::URLencode
#' and replacement contains the character to replace the character passed in the same indexed position in pattern.
#' @return A list, with names being parameter names and values being parameter values.
#' @examples
#' paramList("?param1=Test&param2=1234&param3=Example")
paramList <- function(x, split = "&", custom_decode = list(pattern = c("+"), replacement = c(" "))){

  x <- stringi::stri_replace_first_fixed(x, pattern = "?", replacement = "")

  x <- unlist(stringi::stri_split_fixed(x, split))

  x <- lapply(x, stringi::stri_split_fixed, "=")

  params <- list()

  pattern <- custom_decode[["pattern"]]
  replacement <- custom_decode[["replacement"]]

  lapply(1:length(x), function(i){

    y <- unlist(x[[i]])

    p_name <- y[1]
    p_value <- URLdecode(y[2])

    lapply(1:length(pattern), function(x){
      p_value <<- stringi::stri_replace_all_fixed(p_value, pattern[x], replacement[x])
    })

    if(length(params[[p_name]]) > 0){

      params[[p_name]] <<- c(params[[p_name]], p_value)

    }else{

      params[[p_name]] <<- p_value

    }

  })

  return(params)

}

# req_list <- list(
#   request_method = req$REQUEST_METHOD,
#   script_name = req$SCRIPT_NAME,
#   path_info = req$PATH_INFO,
#   query_string = req$QUERY_STRING,
#   server_name = req$SERVER_NAME,
#   server_port = req$SERVER_PORT,
#   headers = req$HEADERS,
#   rook_input = req[["rook.input"]]$read_lines(),
#   rook_version = req[["rook.version"]]$read_lines(),
#   rook_url_scheme = req[["rook.url_scheme"]]$read_lines(),
#   rook_error_stream = req[["rook.errors"]]$read_lines()
# )

#' Conveniently create HTTP server using httpuv::startServer() or httpuv::runServer().
#'
#' @param host A string that is a valid IPv4 or IPv6 address that is owned by this server, which the application will listen on.
#' "0.0.0.0" represents all IPv4 addresses and "::/0" represents all IPv6 addresses. Refer to host parameter of httpuv::startServer() for more details.
#' @param port The port number to listen on. Refer to port parameter of httpuv::startServer() for more details.
#' @param persistent TRUE/FALSE. If FALSE, calls httpuv::startServer(), which returns back to the R session
#' (and would therefore not work with launching a persistent server through a system service as the R session would continue and likely exit/end).
#' If TRUE, calls httpuv::runServer(), which does not return to the R session unless an error or
#' interruption occurs and is suitable for use with system services to start or stop a server.
#' @param static A named list, names should be URL paths, values should be paths to the files to be served statically
#' (such as a HTML file saved somewhere).
#' @param dynamic A named list, names should be URL paths, values should be named vectors with vector names equaling a
#' HTTP method (such as "GET" or "POST") and the values being expressions that when evaluated return a named list with valid entries
#' for status, headers, and body as specified by httpuv::startServer(). Refer to httpuv::startServer() for more details on what can be returned
#' as the response.
#' ex. list("/" = c("GET" = expression(get_function(req)), "POST" = expresssion(post_function(req))))
#' @return A HTTP web server on the specified host and port.
#' @details serveHTTP is a convenient way to start a HTTP server that works for both static and dynamically created pages.
#' It offers a simplified and organized interface to httpuv::startServer()/httpuv::runServer() that makes serving static and
#' dynamic pages easier. For dynamic pages, the expression evaluated when a browser requests a dynamically served path should
#' likely be an expression wrapping a function that has "req" as a parameter. Per the Rook specification implemented by httpuv, "req" is
#' the R environment in which browser request information is collected. Therefore, to access HTTP request headers, inputs, etc. in a function
#' served by a dynamic path, "req" should be a parameter of that function. For the dynamic parameter of serveHTTP,
#' list("/" = c("GET" = expression(get_homepage(req)))) would be a suitable way to call the function get_homepage(req) when the root path of a
#' website is requested with the GET method. The req environment has the following variables:
#' request_method = req$REQUEST_METHOD,
#' script_name = req$SCRIPT_NAME,
#' path_info = req$PATH_INFO,
#' query_string = req$QUERY_STRING,
#' server_name = req$SERVER_NAME,
#' server_port = req$SERVER_PORT,
#' headers = req$HEADERS,
#' rook_input = req[["rook.input"]]$read_lines(),
#' rook_version = req[["rook.version"]]$read_lines(),
#' rook_url_scheme = req[["rook.url_scheme"]]$read_lines(),
#' rook_error_stream = req[["rook.errors"]]$read_lines()
#'
#' @examples
#' # Run both functions and go to http://127.0.0.1:5001/ in a web browser
#' get_example <- function(req){
#'
#' html <- html_doc(
#' head(),
#' body(
#' h1("Hello"),
#' p("Here is a list of some of the variables included in the req environment
#' that were associated with this request:"),
#' ul(
#' li(paste0("req$REQUEST_METHOD = ", req$REQUEST_METHOD)),
#' li(paste0("req$SCRIPT_NAME = ", req$SCRIPT_NAME)),
#' li(paste0("req$PATH_INFO = ", req$PATH_INFO)),
#' li(paste0("req$QUERY_STRING = ", req$QUERY_STRING)),
#' li(paste0("req$SERVER_NAME = ", req$SERVER_NAME)),
#' li(paste0("req$SERVER_PORT = ", req$SERVER_PORT))
#' ),
#' p("You can use paramList() to deal with inputs passed through query strings as
#' well as passed through the input stream."),
#' p("params <- paramList(req[[\"rook.input\"]]$read_lines()) will give you a
#' named list of parameters.")
#' )
#' )
#' return(
#' list(
#' status = 200L,
#' headers = list('Content-Type' = 'text/html'),
#' body = html
#' )
#' )
#' }
#'
#' serveHTTP(
#' host = "127.0.0.1",
#' port = 5001,
#' persistent = FALSE,
#' static = list(),
#' dynamic = list(
#' "/" = c(
#' "GET" = expression(get_example(req))
#' )
#' )
#' )
serveHTTP <- function(
  host = "127.0.0.1",
  port = 5001,
  persistent = FALSE,
  static = list(),
  dynamic = list()
){

  if(length(static) > 0){

    static <- lapply(static, staticPath, indexhtml = FALSE)

  }

  if(length(dynamic) > 0){

    for(i in names(dynamic)){

      static[[i]] <- excludeStaticPath()

    }

  }

  valid_dynamic_paths <- names(dynamic)

  if(persistent == TRUE){
    return(
      runServer(
        host,
        port,
        app = list(
          call = function(req) {

            if(req$PATH_INFO %in% valid_dynamic_paths){

              x <- eval(dynamic[[req$PATH_INFO]][req$REQUEST_METHOD])

              list(
                status = x[["status"]],
                headers = x[["headers"]],
                body = x[["body"]]
              )

            }else{

              list(
                status = 404,
                headers = list(
                  'Content-Type' = 'text/html'
                ),
                body = "404. Page not found."
              )

            }

          },
          staticPaths = static
        )
      )
    )
  }else{
    return(
      startServer(
        host,
        port,
        app = list(
          call = function(req) {

            if(req$PATH_INFO %in% valid_dynamic_paths){

              x <- eval(dynamic[[req$PATH_INFO]][req$REQUEST_METHOD])

              list(
                status = x[["status"]],
                headers = x[["headers"]],
                body = x[["body"]]
              )

            }else{

              list(
                status = 404,
                headers = list(
                  'Content-Type' = 'text/html'
                ),
                body = "404. Page not found."
              )

            }

          },
          staticPaths = static
        )
      )
    )
  }

}

#' Stop HTTP server(s) by calling httpuv::stopServer() or httpuv::stopAllServers().
#'
#' @param x A server object that was previously returned from serveHTTP.
#' @param all TRUE/FALSE, if TRUE, calls httpuv::stopAllServers.
#' @return Nothing.
#' @examples
#' endServer(all = TRUE)
endServer <- function(x = NULL, all = FALSE){

  if(all == TRUE){
    return(stopAllServers())
  }else{
    return(stopServer(x))
  }

}
