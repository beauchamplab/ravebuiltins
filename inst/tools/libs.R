.packageName = 'ravebuiltins'

# Functions from third-party packages
read_yaml <- function(...){
  yaml::read_yaml(...)
}


cat2 <- function(...){
  rutabaga::cat2(...)
}

check_installed_packages <- function(...){
  rutabaga::check_installed_packages(...)
}

parse_svec <- function(...){
  rutabaga::parse_svec(...)
}

package_installed <- function(...){
  rutabaga:::package_installed(...)
}


rstudio_viewer <- function(...){
  rstudioapi::viewer(...)
}

#' Get project root dir
get_root_dir <- function(){
  d = rstudioapi::getActiveProject()
  if(length(d) == 1 && grepl(paste0('/', .packageName, '$'), d)){
    # package developer
    return(d)
  }else{
    # package user
    return(system.file('/', package = .packageName))
  }
}

verify_rstudio_version <- function(){
  rstudioapi::verifyAvailable(version_needed = '1.2')
}

select_path <- function(is_directory = TRUE){
  if(is_directory){
    path = rstudioapi::selectDirectory()
  }else{
    path = rstudioapi::selectFile()
  }
  path
}

#' Get yes or no answer
ask_question <- function(title, message, ok = 'Yes', cancel = 'No', 
                         use_console = FALSE, level = 'WARNING'){
  if(use_console){
    rutabaga::cat2(title, ' - ', message, ' [yes or no]?', level = level, sep = '')
    v = readline(prompt = 'y or N: ')
    if(!v %in% c('y', 'N')){
      stop('Please enter "y" or "N", case sensitive.')
    }else if (v == 'y'){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    rstudioapi::showQuestion(
      title = title,
      message = message,
      ok = ok,
      cancel = cancel
    )
  }
  
}

`%?<-%` <- rave::`%?<-%`
