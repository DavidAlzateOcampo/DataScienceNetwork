dyn.print.selectors.1<-function(){
  print.dimension.selector(NULL#input$dimension.2
                          ,"dimension.1"
                          ,label.choose.first.dim)
}


dyn.print.selectors.2<-function(input){
  if (is.null(input$dimension.1)){
   return()}else{}
  if(input$dimension > 1){
    print.dimension.selector(input$dimension.1
                             ,"dimension.2"
                             ,label.choose.second.dim)
  }
}

dyn.sel.df<-function(input){
  cat("\n paso por aqui :( ")
  return(data)
  #cat(" Anho seleccionado = ",input$survey.year)
  #return(input$survey.year)
}

dyn.print.cat.X.select<-function(input){
  inputSelectCheckfunc(data,
                       input$dimension.1,
                       "catX",
                       input$dimension.1)
}
