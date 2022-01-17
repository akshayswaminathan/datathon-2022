library(tidyverse)
library(glue)
library(DBI)
library(dbplyr)
library(odbc)
library(stringr)
library(prettydoc)
library(htmltools)
library(lubridate)
library(purrr)
library(tibble)
library(gtsummary)
library(data.table)

make_attrition_table <- function(data, ie_list, first_row_name = "Before filtering") {
  
  datasets <- list()
  cohort_selection <- data.frame(step = c(first_row_name, names(ie_list)),
                                 count = 1:(length(ie_list) + 1))
  
  datasets[[1]] <- data
  cohort_selection$count[1] <- nrow(data)
  
  for (i in 1:length(ie_list)) {
    data <- data %>% 
      filter(!!ie_list[[i]])
    
    datasets[[i + 1]] <- data
    cohort_selection$count[i + 1] <- nrow(data)
  }
  
  out <- list("filtered_datasets" = datasets %>% 
                setNames(cohort_selection$step),
              "cohort_selection" = cohort_selection)
  
  return(out)
  
}

render_markdown_output <- function(results_object, 
                                   other_objects = NULL, 
                                   max_tab_level = Inf, 
                                   markdown_title, 
                                   output_file,
                                   fig_height = 6,
                                   fig_width = 14,
                                   rmd_template = "~/unstructured-data/generic_rmd2.Rmd") {
  # Renders an html Markdown object from a results object.
  # Examine  "~/code/flatiron/analyses/cg/generic_rmd2.Rmd" for more information on what this does.
  # Params:
  #   @results_object: a list (or list of lists, or list of lists of lists... etc.) to display
  #     in the markdown
  #   @max_tab_level: the maximum nesting of the tabbed headings (for instance, 1 would only nest
  #     the first tab level. 0 would result in no tabbed nesting).
  #   @markdown_title: Character string to give a title to Markdown
  #   @file_name: Character string of a location and filename for the html Markdown output
  # Returns:
  #   Nothing, but writes an html markdown to a file in a specified location
  
  markdown_envir <- new.env()
  markdown_envir$results_object <- results_object
  markdown_envir$other_objects <- other_objects
  
  attr(markdown_envir$results_object, "max_tab_level") <- max_tab_level
  
  rmarkdown::render(input = rmd_template,
                    output_file = output_file,
                    params = list(set_title = markdown_title,
                                  fig_height = fig_height,
                                  fig_width = fig_width),
                    envir = markdown_envir)
  
}

make_baseline_table <- function(data, 
                                table_vars_and_names,
                                grouping_var,
                                grouping_var_name,
                                show_all = T,
                                show_p = T) {
  
  table_data <- data %>% 
    select_at(c(names(table_vars_and_names), grouping_var)) %>% 
    setNames(c(table_vars_and_names, grouping_var_name))
  
  cg_formula <- ifelse(is.null(grouping_var_name), 
                       "~.", 
                       glue::glue("{grouping_var_name} ~ ."))
  
  cg_summary <- compareGroups::compareGroups(as.formula(cg_formula),
                                             method = 2,
                                             max.ylev = 10,
                                             data = table_data,
                                             simplify = F)
  
  cg_table <- compareGroups::createTable(cg_summary, 
                                         show.all = show_all, 
                                         show.p.overall = show_p, 
                                         show.n = F)
  
  return(cg_table)
  
}

WriteTablesToExcelUsingOpenxlsx <- function(tables_list,
                                            file_path) {
  # Writes a list of CompareGroups tables to Excel
  # Args:
  #    tables_list: a named list containing objects of class "createTable", created by the
  #      compareGroups package, or a data.frame. Names should be less than 30 characters to fit in
  #      the worksheet tab.
  #      You can assign an attribute to the createTable element of "full_desc_name" if you would
  #      like to give it a longer name in the worksheet.
  #    file_path: the path of the file to write
  #  Returns:
  #    an Excel workbook with a worksheet for each element
  
  
  table_classes <- map(tables_list, class)
  
  if (!any(table_classes %in% c("createTable", "data.frame"))) {
    warning(paste("Following objects cannot be written to excel:",
                  paste(names(tables_list)[which(!table_classes %in% c("createTable", "data.frame"))],
                        collapse = ", ")))
  }
  
  full_desc_names <- map(tables_list, ~attr(.x, "full_desc_name"))
  
  tables_list <- keep(tables_list, table_classes %in% c("createTable", "data.frame")) %>%
    imap(function(x, i) {
      if (table_classes[i] == "createTable") {
        return(ConvertCreateTableToDataframe(x))
      } else {
        return(x)
      }
    })
  
  openxlsx::write.xlsx(x = tables_list,
                       file = file_path)
  
  # Set column sizes and rename first cell
  wb <- openxlsx::loadWorkbook(file_path)
  map(1:length(tables_list), function(i){
    
    table_name <- if_else(is.null(full_desc_names[[i]]), names(tables_list)[i], full_desc_names[[i]])
    
    openxlsx::writeData(wb = wb,
                        sheet = i,
                        xy = c(1, 1),
                        x = table_name)
    openxlsx::setColWidths(wb, i, cols = 1:ncol(tables_list[[i]]), widths = "auto")
  }
  )
  
  # Save wb
  openxlsx::saveWorkbook(wb, file_path, overwrite = T)
  
}

ConvertCreateTableToDataframe <- function(create_table, nmax = T, header.labels = c()){
  # Converts the 'desc' element of a "createTable" object to a data.frame
  # The bulk of this function is adapted from createTable::export2xls
  # Input
  #   create_table: an object of class "createTable"
  #   nmax: logical, indicating whether to show the number of subjects
  #         with at least one valid value across all row-variables.
  #   header.labels: see the 'header.labels' argument from createTable.
  # Output
  #   a data.frame corresponding to create_table$desc
  
  # From: https://rdrr.io/cran/compareGroups/src/R/trim.R
  trim <- function(x){
    x <- gsub("^[ ]+","",x)
    x <- gsub("[ ]+$","",x)
    x
  }
  pp <- compareGroups:::prepare(create_table, nmax = T, header.labels)
  table1 <- compareGroups:::prepare(create_table, nmax = nmax, header.labels)[[1]]
  cc <- unlist(attr(pp, "cc"))
  ii <- ifelse(rownames(table1)[2] == "", 2, 1)
  table1 <- cbind(rownames(table1), table1)
  if (!is.null(attr(create_table, "caption")))
    table1[, 1] <- paste("    ", table1[, 1])
  aux <- NULL
  for (i in (ii + 1):nrow(table1)) {
    if (!is.null(cc) && cc[i - ii] != "") {
      aux <- rbind(aux, c(cc[i - ii], rep("", ncol(table1) -
                                            1)))
      aux <- rbind(aux, table1[i, ])
    }
    else {
      aux <- rbind(aux, table1[i, ])
    }
  }
  table1 <- rbind(table1[1:ii, ], aux)
  
  if (nrow(table1) > 1 && length(grep("^N=", trim(table1[2, 2])))) {
    wn <- grep("^N=", trim(table1[2, ]))
    nn <- paste(trim(table1[1, wn]), " ", trim(table1[2, wn]))
    table1[1, wn] <- nn
    table1 <- table1[-2, ]
  }
  
  table1[1, 1] <- " "
  colnames(table1) <- table1[1, ]
  table1 <- table1[-1, , drop = FALSE]
  # table1 <- rbind(colnames(table1), table1)
  cn <- colnames(table1)
  table1 <- as.data.frame(table1)
  names(table1) <- c(" ", cn[-1])
  rownames(table1) <- NULL
  
  return(table1)
  
}

DisplayObjectCorrectly <- function(object) {
  # Displays an object correctly in RMarkdown
  # Args:
  #   object: an object you want to display in markdown, of the following class:
  #     data.frame, createTable, ggplot, survplot, character, kableExtra
  # Returns: Nothing, but prints out an object of the above type in a markdown-friendly way
  
  if ("data.frame" %in% class(object)) {
    object %>%
      kable(digits = 2, format = "pipe") %>%     
      kable_styling(c("striped", "bordered")) %>% 
      print()
    cat("\n \n")
  } else if ("createTable" %in% class(object)) {
    cat("\n\n")
    print(export2md(object))
    cat("\n \n")
  } else if ("character" %in% class(object)) {
    cat("\n")
    cat(object)
    cat("\n \n")
  } else if ("gtsummary" %in% class(object)) {
    cat("\n\n")
    object %>% 
      as_gt() %>% 
      gt::as_raw_html() %>% 
      print()
    cat("\n \n")
  } else if ("plotly" %in% class(object) |
             "datatables" %in% class(object)) {
    cat("\n")
    htmltools::tagList(object) %>% 
      print()
    # cat("\n \n")
  } else if ("ggplot" %in% class(object) | "ggsurvplot" %in% class(object) |
             "character" %in% class(object) | "kableExtra" %in% class(object)) {
    cat("\n")
    print(object)
    cat("\n \n")
  } else if ("summaryDefault" %in% class(object)) {
    cat("\n")
    object %>% 
      broom::tidy() %>% 
      as_data_frame() %>% 
      kable %>%
      print
    cat("\n \n")
  } else if ("grob" %in% class(object)) {
    cat("\n")
    grid::grid.newpage()
    gridExtra::grid.arrange(object)
    cat("\n \n")
  } else if ("recordedplot" %in% class(object)) {
    grid::grid.newpage()
    print(object)
  } else if ("grViz" %in% class(object)) {
    cat("\n ")
    sink()
    object %>%
      export_svg %>%
      charToRaw %>% 
      rsvg_png(file = "/TMP/chart.png")
    sink(file = NULL)
    grid::grid.newpage()
    print(grid::grid.raster(png::readPNG("/TMP/chart.png")))
    grid::grid.newpage()
    
    cat("\n \n \n ")
  } else {
    cat("\n Object cannot be printed \n \n")
  }
}

PrintMarkdownResults <- function(results_object, 
                                 level = 1, 
                                 max_tab_level = attr(results_object, "max_tab_level"),
                                 force_tabs = attr(results_object, "force_tabs"),
                                 suppress_tabs = attr(results_object, "suppress_tabs")) {
  # Recursively, for all objects in list (or list of lists), prints to a markdown doc the names of
  #    lists, or the actual nicely-formatted objects in a list (if the object is not a list).
  # Args:
  #   results_object: the list (or list of lists, or list of lists of lists... etc.) used to produce 
  #     the markdown
  #   level: the initial level of section headings (1 is default, recursion will increase this number)
  #   max_tab_level: the maximum nesting of the tabbed headings
  # Returns:
  #   Nothing, but prints all content to a markdown document
  
  # if (is.null(max_tab_level)) {
  #   max_tab_level <- Inf
  # }
  # if (is.null(force_tabs)) {
  #   force_tabs <- F
  # }
  # if (is.null(suppress_tabs)) {
  #   suppress_tabs <- F
  # }
  # browser()
  if ("list" %in% class(results_object)) {
    invisible(imap(results_object, function(result, object_name, result_level) {
      max_tab_level <- attr(result, "max_tab_level")
      force_tabs <- attr(result, "force_tabs")
      suppress_tabs <- attr(result, "suppress_tabs")
      if (is.null(max_tab_level)) {
        max_tab_level <- Inf
      }
      if (is.null(force_tabs)) {
        force_tabs <- F
      }
      if (is.null(suppress_tabs)) {
        suppress_tabs <- F
      }
      if (object_name == "no_tab") {
        cat(paste0("\n"))
      } else if (any(max_tab_level >= result_level, force_tabs) & !suppress_tabs) {
        cat(paste0("\n", glue_collapse(rep("#", level)), " ", object_name, "{.tabset} \n"))
      } else {
        cat(paste0("\n", glue_collapse(rep("#", level)), " ", object_name, " \n"))
      } 
      if (!is.null(attributes(results_object)$description)) {
        cat(attributes(results_object)$description)
      }
      new_max_tab_level <- ifelse(is.null(attr(results_object, "max_tab_level")),
                                  max_tab_level,
                                  attr(results_object, "max_tab_level"))
      PrintMarkdownResults(result, level = level + 1, max_tab_level = new_max_tab_level)
    }
    , result_level = level))
  } else {
    DisplayObjectCorrectly(results_object)
  }
}