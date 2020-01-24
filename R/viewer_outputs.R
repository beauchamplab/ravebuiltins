# brain_viewer = rave_context_generics('brain_viewer', function(results, ...){})
# 
# brain_viewer.default = brain_viewer.rave_running_local = function(results, ...){
#     brain = results$get_value('brain')
#     return(brain$plot(...))
# }
# 
# 
# brain_viewer.rave_running <- function(results, ...){
#     brain = results$get_value('brain')
#     proxy = results$get_value('proxy')
#     shiny::validate(shiny::need(length(brain), message = 'Brain not initialized'))
#     
#     # get session
#     brain
# }
# 
# 
# 
