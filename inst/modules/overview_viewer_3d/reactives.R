local_env = new.env(parent = emptyenv())
input = getDefaultReactiveInput()
session = getDefaultReactiveDomain()
local_data = reactiveValues()

local_env$tables = list()


find_csv <- function( project_dir, scodes ){
    scodes = '_project_data/3dviewer' #c('_project_data/3dviewer', scodes)
    # find all csv files within project folder
    res = lapply( scodes, function(scode){
        root_dir = file.path(project_dir, scode)
        fs = list.files(root_dir, pattern = '\\.[cC][sS][vV]$', all.files = TRUE, recursive = TRUE)
        fs
    })
    
    unlist(res)
}

observeEvent(local_data$current_project, {
    local_env$tables = list()
})

# observeEvent(input$viewer_result_btn2, {
#   local_data$viewer_result_btn = input$viewer_result_btn2
# })

observeEvent(local_data$data_files_needUpdate, {
    s = local_env$data_files_more
    sfs = unique(c(s, shiny::isolate(input$data_files)))
    fs = unique(c(sfs, local_env$csv_files))
    local_env$data_files_more = NULL
    updateSelectInput(session, 'data_files', choices = fs, selected = sfs)
})





observeEvent(input$show_example, {
    s = shiny::isolate(local_data$show_example)
    s = !isTRUE(s)
    local_data$show_example = s
})

file_check <- function(){
    
    show_example = get_val(local_data$show_example, default = FALSE)
    
    if( show_example ){
        tbl = data.frame(
            Project = subject$project_name,
            Subject = subject$subject_code,
            Electrode = c('1','1','2','3', '...'),
            Time = c('0','0.2','0','1', '...'),
            Value1 = c(sprintf('%.2f', rnorm(4)), '...'),
            Value2 = c(letters[1:4], '...')
        )
        xtbl = knitr::kable(
            tbl, format = 'html', digits = 2, table.attr = "class='table-striped', style='width:100%'",
            caption = 'An example of csv file. "Subject" and "Electrode" are mandatory. Column names are case sensitive.'
        )
        
        tagList(
            actionLink(ns('show_example'), label = 'Hide example'),
            hr(),
            div(
                style = 'overflow-x: scroll;',
                htmltools::HTML(xtbl)
            )
        )
    }else{
        actionLink(ns('show_example'), label = 'Show example')
    }
    
}

observeEvent(input$csv_file, {
    print(input$csv_file)
    files = input$csv_file
    # Read in csv file
    lapply(seq_len(nrow(files)), function(ii){
        file_info = files[ii, ]
        
        notif = p('Cannot read ', htmltools::strong(file_info$name), '. Invalid csv table file.')
        
        tryCatch({
            dat = read.csv(file_info$datapath, stringsAsFactors = FALSE)
            if(!all( c('Electrode', 'Subject') %in% names(dat) )){
                notif = p('Table ', htmltools::strong(file_info$name), ' MUST has columns ', htmltools::strong('"Subject"'), '(character) and ', htmltools::strong('"Electrode"'), '(integer), case sensitive.')
                stop()
            }
            
            dest_dir = file.path(subject$dirs$rave_dir, '../../_project_data/3dviewer/')
            dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
            
            fname = file_info$name
            save_path = file.path(dest_dir, file_info$name)
            if( file.exists(save_path) ){
                fname = stringr::str_to_lower(file_info$name)
                fname = stringr::str_replace(fname, '\\.csv$', strftime(Sys.time(), '[%y%m%d-%H%M%S].csv'))
                save_path = file.path(dest_dir, fname)
            }
            
            utils::write.csv(dat, file = save_path, row.names = FALSE)
            
            # s = file.path('_project_data', '3dviewer', save_path)
            # Force update selected data files
            local_env$data_files_more = fname
            
            local_data$data_files_needUpdate = Sys.time()
        }, error = function(e){
            showNotification(notif, type = 'error')
        })
    })
    
})
