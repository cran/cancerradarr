#' Create a template file to be filled by cancer registry partners
#'
#' @param filename file path, the name of the template file to be created
#' @param verbose logical, shall progress message be printed
#'
#' @return
#'  a template .xlsx file is created on the hard drive.
#' @export
#'
#' @import openxlsx
#'
#' @concept main
#'
#' @examples
#' \donttest{
#'   file.in <- 'input_file_test.xlsx'
#'   create_registry_input_file(file.in)
#'   ## remove the file to pass package computation tests
#'   unlink(file.in)
#' }
create_registry_input_file <-
  function(filename = 'cancerRADAR_input.xlsx', verbose = TRUE){
    ageg.list <- c('total', paste0(sprintf('%02d', seq(0, 80, 5)), '_', sprintf('%02d', seq(4, 84, 5))), '85')
    qual.list <- c('DCO', 'MV') ## number of cases based on death certificate only (DCO) and microscopically verified (MV)
    item.list <- c('py', 'nallC', 'ncx', 'nbrea', 'nliv', 'nstm', 'ncolo', 'nlun')
    sex.list <- c('male', 'female')

    dat.cob <- NULL
    utils::data('dat.cob', envir = environment())

    dat.list <- list()
    
    dat.list[['data_info']] <-
      dplyr::tribble(
        ~ `DATA INFORMATIONS`,
        paste0('Country:'),
        paste0('Cancer registry designation'),
        paste0('Period over which this data was collected (2018-2022, 2013-2017, 2008-2012, 2003-2007):'),
        paste0('Data source of birth-country in registry (e.g. statistical Agency, hospital data, ...):'),
        paste0('Data source of population-at-risk (e.g. statistical Agency, national survey data, ...):')
      )

    for(sex_ in sex.list){
      for(item_ in item.list){
        if(!(sex_ == 'male' & (item_ == 'ncx' | item_ == 'nbrea'))){
          dat.list[[paste(item_, sex_, sep = '_')]] <-
            cbind(
              dat.cob,
              paste(item_, sex_, if(item_ == 'py') ageg.list else c(ageg.list, qual.list), sep = '_') |> 
                purrr::map_dfc(stats::setNames, object = list(character(1)))
            )
        }
      }
    }
    if(!file.exists(filename)){
      headerStyle <- createStyle(textDecoration = 'bold', border = 'bottom', halign = 'center', valign = 'center', wrapText = TRUE, locked = TRUE)
      casesStyle <- createStyle(numFmt = 'COMMA', locked = FALSE)
      # percentStyle <- createStyle(numFmt = 'PERCENTAGE', locked = FALSE)
      # labelStyle <- createStyle(wrapText = TRUE)

      readme.file <- system.file("extdata", "readme_cancerradarr_input_file.xlsx", package = "cancerradarr")
      wb <- loadWorkbook(readme.file, na.convert = FALSE) # create a workbook using predefined README
      for(sn_ in names(dat.list)){
        addWorksheet(wb, sn_)
        writeDataTable(wb, sn_, dat.list[[sn_]], headerStyle = headerStyle)
        protectWorksheet(wb, sn_, protect = TRUE, lockSorting = FALSE, lockFormattingCells = FALSE, lockFormattingColumns = FALSE, lockInsertingColumns = TRUE, lockDeletingColumns = TRUE)
        ## sheets custom formatting
        if(sn_ == 'data_info'){
          # mergeCells(wb, sn_, rows = 1, cols = 1:2)
          setColWidths(wb, sn_, cols = 1, widths = 80)
          addStyle(wb, sn_, createStyle(locked = FALSE, border = 'TopBottomLeftRight'), cols = 2, rows = 2:6)
          suppressWarnings(dataValidation(wb, sn_, cols = 2, rows = 2, type = "list", value = "'py_male'!$A$2:$A$252"))
        } else {
          setColWidths(wb, sn_, cols = 1, widths = 30)
          setColWidths(wb, sn_, cols = 2:3, widths = 14)
          setColWidths(wb, sn_, cols = 4:ncol(dat.list[[sn_]]), widths = 20)
          freezePane(wb, sn_, firstActiveRow = 2, firstActiveCol = 4)
          for(i in 4:ncol(dat.list[[sn_]])){
            addStyle(wb, sn_, casesStyle, cols = i, rows = 2:(nrow(dat.list[[sn_]]) + 1))
          }
        }
      }
      saveWorkbook(wb, filename) # save workbook
      if(verbose) cat('\n>', filename, 'has been created, please fill it with cancer registry data')
    } else {
      warning(
        paste0(
          filename, ' already exists on your hard drive. Nothing has been done. ',
          'If you are sure to want to overwrite this file please type unlink("', filename, '") ',
          'and rerun the create_registry_input_file() function.'
        )
      )
    }
  }
