#' @export
viewer_brain = rave_context_generics('viewer_brain', function(results, ...){})

#' @export
viewer_brain.default = viewer_brain.rave_running_local = function(results, ...){
    brain = results$get_value('brain')
    return(brain$plot(...))
}

#' @export
viewer_brain.rave_running <- function(...){
    ctx = rave_context()
    client_size = ctx$instance$wrapper_env$get_client_size()
    client_height = client_size$available_size[[2]] - 200
    client_height = sprintf('%.0fpx', client_height)
    re = htmltools::div(
        style = 'margin:-10px;',
        define_output(
            threeBrain::threejsBrainOutput('viewer_result_out', width = '100%', height = client_height)
        )
    )
    re
}
