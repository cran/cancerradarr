## -----------------------------------------------------------------------------
#| eval: false
# # Check if the remotes package is installed and install it if not.
# if(!('remotes' %in% rownames(installed.packages()))) install.packages('remotes', dependencies = TRUE)
# 
# # Install the cancerradarr package
# # - from GitLab (latest devel version) - recommended
# remotes::install_gitlab("cancerradar/cancerradarr", dependencies = TRUE)
# # - or from CRAN (latest stable version)
# # install.packages("cancerradarr", dependencies = TRUE)


## -----------------------------------------------------------------------------
#| eval: false
# # Load the cancerradarr package
# library(cancerradarr)
# 
# # Create the working directory
# # Please add here the working directory from which you want to work
# # Note that the Microsoft environment uses the backslash (\) as a path
# # separator. For R to interpret the path correctly, forward slashes (/)
# # need to be replaced with backslashes (\).
# # The use of normalizePath should insure this path is correctly written
# workdir <- normalizePath('~/Documents/241107_cancerRADAR_getting_started')
# 
# dir.create(workdir, showWarnings = FALSE, recursive = TRUE)
# 
# # Create the Cancer RADAR input file
# create_registry_input_file(file.path(workdir, 'cancerRADAR_input.xlsx'))
# openxlsx::openXL(file.path(workdir, 'cancerRADAR_input.xlsx'))


## -----------------------------------------------------------------------------
#| eval: false
# # Open the input file in Excel
# openxlsx::openXL(file.path(workdir, 'cancerRADAR_input.xlsx'))


## -----------------------------------------------------------------------------
#| eval: false
# # Get an example of a filled input file
# file.copy(
#   file.path(path.package('cancerradarr'), 'extdata/ex_cancerRADAR_input_filled.xlsx'),
#   file.path(workdir, 'ex_cancerRADAR_input_filled.xlsx'),
#   overwrite = TRUE
# )
# 
# # Open the example filled input file in Excel
# openxlsx::openXL(file.path(workdir, 'ex_cancerRADAR_input_filled.xlsx'))


## -----------------------------------------------------------------------------
#| eval: false
# # Compute summary statistics including country of birth aggregation
# create_canradar_summary_file(
#   filename.in = file.path(workdir, 'ex_cancerRADAR_input_filled.xlsx'),
#   filename.out = file.path(workdir, 'ex_cancerRADAR_output_with_cob_01.xlsx'),
#   ncan.min = 5,
#   py.min = 100,
#   include.by.cob.stat = TRUE
# )
# 
# # Or without country of birth aggregation
# create_canradar_summary_file(
#   filename.in = file.path(workdir, 'ex_cancerRADAR_input_filled.xlsx'),
#   filename.out = file.path(workdir, 'ex_cancerRADAR_output_without_cob_01.xlsx'),
#   ncan.min = 5,
#   py.min = 100,
#   include.by.cob.stat = FALSE
# )


## -----------------------------------------------------------------------------
#| eval: false
# cancerradarr::open_canradar_dictionary()


## -----------------------------------------------------------------------------
#| eval: false
# # Create a report of the entered data
# create_static_report(file.path(workdir, 'ex_cancerRADAR_output_with_cob_01.xlsx'))
# 
# # The report can be opened with the following command or by double-clicking on the filename
# browseURL(file.path(workdir, 'ex_cancerRADAR_output_with_cob_01.html'))


## -----------------------------------------------------------------------------
#| eval: false
# # Create a dynamic report of the entered data
# run_dynamic_report(file.path(workdir, 'ex_cancerRADAR_output_with_cob_01.xlsx'))

