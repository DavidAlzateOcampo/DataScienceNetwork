print.Plot.ini<-function(){
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
}

print.dashboard.main.side<-function(){
  dashboardSidebar(
    sidebarMenu(
      menuItem(label.Stats.Title, icon = icon(icon.Stats),
               menuSubItem(label.Stats.subItem1
                           , tabName = "stats"
                           ,icon=icon.Stats.pie),
               menuSubItem(label.Stats.subItem2
                           , tabName = "subitem2")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  )
}

print.dashboard.Body<-function(){
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "stats"
              ,print.tab.Stats.col1()
              ,hr()
              ,print.tab.Stats.col2(input)
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
}

print.tab.Stats.col1<-function(){
  fluidPage(
    fluidRow(infoBoxOutput("max.category.value",width = 3),
             infoBoxOutput("max.category.perc.value", width = 3),
             infoBoxOutput("min.category.value", width = 3),
             infoBoxOutput("min.category.perc.value", width = 3))
    ,fluidRow(
      box(plotlyOutput("distPlot")
          ,width = 10)
      ,uiOutput("dim.X.category.selectors",width=12)
      )
  ,fluidRow(
    box(
    print.selector.unique.val("survery"
                            ,"radioButtons"
                            ,label.rdb.survey.year
                            ,"get.choices.survey.year"
                            ,inline = TRUE
                            ),width = 12)
    ,width = 12
  ))
}

print.tab.Stats.col2<-function(inputt){
  fluidRow(
    tabBox(
      title = label.tabBox.Title
      ,id = "tabset1", height = "300px"
      ,tabPanel(label.tab1.Title, "_",print.box.Variables())
      ,tabPanel(label.tab2.Title, "_",print.box.Variable.X(input))
      ,width = 12
    )
  )
}

print.box.Variables<-function(){
  fluidRow(
  box(
     status = "primary"
    ,print.selector.unique.val("dimension"
                              ,"radioButtons"
                              ,label.nbr.dim
                              ,"get.choices.dimensions")
    ,width = 6
  ),
  box(
     dyn.print.selectors.1()
    #uiOutput("dimensions.selectors.1"),
    ,uiOutput("dimensions.selectors.2")
    ,width = 6
  )
  
  )
}

print.box.Variable.X<-function(input){
  box(
    title = label.controls
    ,solidHeader = TRUE
    ,status = "primary"
    
    
  )
}

print.selector.unique.val<-function(valName,Type,labelVal,functionName,...){
  FUN <- match.fun(functionName) 
  switch(Type,
         "SelectInput"= selectInput(valName, label = labelVal, 
              choices = FUN()
             ,selected = 1)
        ,"radioButtons" = radioButtons(valName, label = labelVal,
                                       choices = FUN(),
                                       selected = 1
                                       ,...)
        )
}

print.select.input.based.dim<-function(dimSelected){
  dimSelected
}

plotly.Histogram.1.dim<-function(input,dataIn){
  cat("\nthis crap has changed")
  g<- ggplot(dataIn, aes_string(x=input$dimension.1,color = "Result_of_last_cytology"))+ 
      geom_histogram(alpha=0.5, position="identity")
  ggplotly(g) 
}

plotly.Bar.1.dim<-function(input,dataIn){
  cat("\n Entro a bar 1 ")
  # #if(glbl.last.dimension.1.stat != input$dimension.1){
  #   cat("\n valor de dimension ",input$dimension.1)
  #   cat("\n valor anterior de dimension ",glbl.last.dimension.1.stat)
  #   glbl.last.dimension.1.stat <- input$dimension.1
  #   cat("\n valor despues de conversion de dimension ",glbl.last.dimension.1.stat)
  # }else{
    
    if(!is.null(paste0(list(input$catX)))){
      source<-paste0(list(input$catX))
      cat("\n valores de lista : ",source," valor de dimension "
          ,sum(sapply(input$catX, length)))
      
      sentence <- paste(input$dimension.1, "%in%", list(input$catX) )
      
      dataIn <- dataIn %>%
        dplyr::filter_(sentence) 
    }
      cat("\n valores de lista seleccionado ",paste0(list(input$catX)) )
      
  # }
  
  
  g<-ggplot(dataIn,aes_string(x=input$dimension.1,fill=string.variable.objetivo))+
    geom_bar(stat="count")
  ggplotly(g)
}

plotly.Bar.2.dim<-function(input,dataIn){
  g<-ggplot(dataIn,aes_string(x=input$dimension.1,fill="Result_of_last_cytology"))+
    geom_bar(stat="count") + facet_wrap(c(input$dimension.2))
  ggplotly(g)
}

print.dimension.selector<-function(valSelect.dim,variableName,labelValue){
  listNames<-names(data)
  if(is.null(valSelect.dim)){
    cat("\n Esta nulo la variable ",variableName)
  }else{
    cat("\n La variable ",variableName, " es igual a : ",valSelect.dim)
    listNames<-listNames[!listNames %in% list(valSelect.dim)]
  }
  
  selectInput(variableName
              ,label = labelValue#label.choose.first.dim, 
              ,choices = listNames
              )
}

get.Var.Type<-function(variableName,dataIn){
  dataCat<-get.df.column.class(dataIn)
  a<-dataCat[dataCat$Variable==variableName,2]
  a<-as.character(a[1])
  return(a)
}

get.data.frm.csv<-function(fileName){
  cat(fileName)
  df<-read.csv(fileName,header = T,na.strings = c('?','n.a','NA'))
  for(i in 1:ncol(df)){
    if(class(df[,i])=="integer"){
      a<-base::unique(df[,i])
      if(is.element(0,a) & is.element(1,a) & length(a) <= 3){
        df[,i]<-as.factor(df[,i])
        levels(df[,i])<-c(string.No,string.Yes)
      }
    }
  }
  return(df)
}

get.df.column.class<-function(dataIn){
  max<-ncol(dataIn);
  dfOut<-data.frame(Variable=NULL
                   ,Class=NULL
                   ,Position=NULL
                   ,RangeMin=NULL
                   ,RangeMax=NULL)
  RangeMin<-NULL;
  RangeMax<-NULL;
  for(i in 1:max){
    RangeMin<-NULL;
    RangeMax<-NULL;
    RangeMin<-1#getRangeMin(dataIn[,i]);
    RangeMax<-1#getRangeMax(dataIn[,i]);
    Variable<-names(dataIn)[i];
    Class<-class(dataIn[,i]);
    
    dfOut<-rbind(dfOut,
           data.frame(
             Variable=Variable
            ,Class=Class
            ,Position=i
            ,RangeMin=RangeMin
            ,RangeMax=RangeMax
           ))
  }
  return(dfOut)
}

getRangeMin.Var.data<-function(dataIn,survey,dimensionVal){
  dataIn<-get.selected.data(survey)
  vars<-c(dimensionVal,string.variable.objetivo)
  df<-dataIn[,vars]
  return(getRangeMin2(df))
}

getRangeMin<-function(colVal){
  valMin<-NULL
  if(class(colVal)=="factor"){
    #valMin<-sort(levels(colVal))[1]
    tmp<-table(colVal)[which.min(table(colVal))]
    valMin<-paste(names(tmp)," = ",tmp)
  }else{
    valMin<-(min(colVal,na.rm = NA))
  }
  return(valMin)
}

getRangeMax<-function(colVal){
  valMax<-NULL
  if(class(colVal)=="factor"){
    tmp<-table(colVal)[which.max(table(colVal))]
    valMin<-paste(names(tmp)," = ",tmp)
  }
  else{
    valMax<-(max(colVal,na.rm = NA))
  }
  return(valMax)
}

getRangeMax.Var.data<-function(dataIn,survey,dimensionVal){
  dataIn<-get.selected.data(survey)
  vars<-c(dimensionVal,string.variable.objetivo)
  df<-dataIn[,vars]
  return(getRangeMax2(df))
}

getRangeMax2<-function(colVal){
  valMax<-NULL
  if(class(colVal[,1])=="factor"){
    tmp<-prop.table(table(colVal[,1],colVal[,2]),1)
    tmp2<-table(colVal[,1],colVal[,2])
    valMax<-paste(row.names(tmp)[which.max(tmp[,2])]
                  ," = "
                  ,tmp2[which.max(tmp[,2]),2])
  }
  else{
    valMax<-(max(colVal,na.rm = NA))
  }
  return(valMax)
}

getPercMax.Var.data<-function(dataIn,survey,dimensionVal){
  dataIn<-get.selected.data(survey)
  vars<-c(dimensionVal,string.variable.objetivo)
  df<-dataIn[,vars]
  return(getPercMax2(df))
}

getPercMax2<-function(colVal){
  valMax<-NULL
  if(class(colVal[,1])=="factor"){
    
    tmp<-prop.table(table(colVal[,1],colVal[,2]),1)
    tmp2<-table(colVal[,1],colVal[,2])
    valPerc<-round(tmp[which.max(tmp[,2]),2]*100,2)
    valMax<-paste(row.names(tmp)[which.max(tmp[,2])]
                  ," = "
                  ,valPerc,"%")
  }
  else{
    valMax<-(max(colVal,na.rm = NA))
  }
  return(valMax)
}

getPercMin.Var.data<-function(dataIn,survey,dimensionVal){
  dataIn<-get.selected.data(survey)
  vars<-c(dimensionVal,string.variable.objetivo)
  df<-dataIn[,vars]
  return(getPercMin(df))
}

getPercMin<-function(colVal){
  valMin<-NULL
  if(class(colVal[,1])=="factor"){
    
    tmp<-prop.table(table(colVal[,1],colVal[,2]),1)
    tmp2<-table(colVal[,1],colVal[,2])
    valPerc<-round(tmp[which.min(tmp[,2]),2]*100,2)
    valMin<-paste(row.names(tmp)[which.min(tmp[,2])]
                  ," = "
                  ,valPerc,"%")
  }
  else{
    #valMin<-(max(colVal,na.rm = NA))
  }
  return(valMin)
}

getRangeMin2<-function(colVal){
  valMin<-NULL
  if(class(colVal[,1])=="factor"){
    
    tmp<-prop.table(table(colVal[,1],colVal[,2]),1)
    tmp2<-table(colVal[,1],colVal[,2])
    valMin<-paste(row.names(tmp)[which.min(tmp[,2])]
                  ," = "
                  ,tmp2[which.min(tmp[,2]),2])
  }
  else{
   # valMin<-(max(colVal,na.rm = NA))
  }
  return(valMin)
}

get.choices.dimensions<-function(){
  a<-list();
  a[[label.1.dim]]=1;
  a[[label.2.dim]]=2;
  return(a)
}

get.choices.survey.year<-function(){
  a<-list();
  a[[label.1.2010]]=1;
  a[[label.2.2015]]=2;
  a[[label.3.all]]=3;
  return(a)
}

get.selected.data<-function(survey.val){
 tmp<-as.numeric(survey.val)
 df<-switch(tmp,
     data.2010
    ,data.2015
    ,data.ALL
  )
  return(df)
}

inputSelectCheckfunc<-function(DF,fieldname,varInput,varLabel){ 
  if(!is.null(fieldname)){
    seller <- DF %>% distinct_(fieldname)
  checkboxGroupInput(varInput, varLabel,
                     choices = unlist(seller,use.names = F),
                     selected = unlist(seller,use.names = F),
                     inline = T  )
  }
}

get.name.from.dimension<-function(strVar){
  cat("\ Valor de strVar",strVar)
  strRename<-  gsub("_"," ",strVar)
  return(strRename)
}