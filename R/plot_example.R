if(FALSE) {
  
  
  a = function(a, c, ...){
    print(a)
  }
  
  b = function(a, b, ...){
    print(c(a,b))
  }
  
  ab = function(a,b,...){
    if(length(c(...))){
      stop('c is not allowed')
    }else{
      b(a,b,...)
    }
  }
  ba = function(a, ...){
    args = list(...)
    args['c'] = NULL
    
    whitelist = formalArgs(plot.default)
    
    do.call(ab, c(list(a=a), args))
  }
  
  
  a(a = 1, b = 2, c = 3)
  b(a = 1, b = 2, c = 3)
  ab(a = 1, b = 2, c = 3)
  ba(a = 1, b = 2, c = 3)
  
  
  
  
  aa = function(a,b) {
    FN <- function(a, ...) {
      list2env(list(...),environment())
      print(a);print(b)
    }
    if(missing(a)) {
      return(FN)
    }
    FN(a,b)
  }
  
  
  aa(b=10)(a=2)
  a = 1
  rm(a)
  
  aa = quote({
    a = a + 1;a
  })
  bb = quote({
    a = a*2;a
  })
  
  quo = rlang::quo({
    !!bb
    !!aa
    
  })
  
  a = 10
  .env$a = 50
  
  
  plot.ruta <- function(x, ...){
    # do sth to x$quo
    rlang::eval_tidy(quo, data = data)
  }
  # print.aaa =
  
  dipsaus::eval_dirty(quo, env=.env)
  
  make_graph_options <- function(...) {
    vals <- list(
      tcl=0,
      lwd=1,
      pch=19,
      col='gray30',
      ylab='', xlab=''
    )
    
    ll <- list(...)
    for(n in names(ll)) {
      vals[[n]] <- ll[[n]]
    }
    
    return(vals)
  }
  
  filter_args <- function(ll, FUN) {
    ll[names(ll) %in% names(formals(FUN))]
  }
  
  do_plot <- function(x, graph_opts=make_graph_options()) {
    graph_opts$x = x
    
    do.call(plot, args=filter_args(graph_opts, plot.default))
  }
  
  do_plot(rnorm(10), graph_opts = make_graph_options(type='o', ylab='Hi', junk='junk'))

}
