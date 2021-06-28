function(input, output, session) {
  
#variables needed within the tool
  index<-reactiveValues()
  #data measure selected for habitat
  index$hab<-NULL
  #data measure selected for wq
  index$wq<-NULL
  #data measure selected for lcmr
  index$lcmr<-NULL
  #data measure selected for cl
  index$cl<-NULL
  #data measure selected for eco
  index$eco<-NULL
  #redefine from regionwide to state boundary
  index$state<-NULL
  #load in the script for custom css for switch button
  source("./Rsource/SwitchButton.R")
#Initial blank map
  output$myMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$CartoDB.DarkMatter)%>%
            setView(-90.4369, 28.9072,5)%>%
            addFullscreenControl()%>%
      addEsriTiledMapLayer(
            url = "https://services1.arcgis.com/cYEfxjk21j8UlsTQ/arcgis/rest/services/PADUS2_Fee_Easement_Designation/MapServer",
            options = providerTileOptions(opacity = 0.5),group = "PAD-US")%>%
      addEsriDynamicMapLayer(
            url = "https://gis.usgs.gov/sciencebase2/rest/services/Catalog/5da9e701e4b09fd3b0c9cb6a/MapServer",
            options = dynamicMapLayerOptions(opacity = 0.5),group = "SECAS")%>%
      addLegend(position = "bottomright",colors = c("#4FAFEE","#0B316B"),
                labels = c("Medium conservation value","High conservation value"),opacity = 0.5,group = "SECAS")%>%
      addLayersControl(overlayGroups = c("PAD-US","SECAS"),
                      position="bottomleft")%>%
      hideGroup("PAD-US")%>%
      hideGroup("SECAS")
  })

#finish the first select panel
  observeEvent(input$nextto2,{
    if(input$geography=="region"){
      #data1<<-data0
      updateCollapse(session, "collapseExample", open = "Panel 2")
    }else if(input$geography=="states"){
      if(is.null(index$state)){
        showModal(modalDialog(
          title = "At least one state needed",footer = modalButton("Ok"),
          "Please select some states to analyze."
        ))
      }
      else{
        datastate_final<-datastate[datastate$Name %in% index$state]
        #data1<<-st_join(data0, datastate_final, join = st_intersects)
        #data1<<-data0
        updateCollapse(session, "collapseExample", open = "Panel 2")
        showModal(modalDialog(
          title = "Under development",footer = modalButton("Ok"),
          "State visualization is still under development, the result would still show up as a regionwide visualization."
        ))
      }
    }
  })
#Whenever state is selected, store the selection into index$state
  observeEvent(input$states,{
    index$state<-input$states
  })
#Table for all measure selected  
  output$measureselected<-renderTable({
    Measures<-c(hab_measure_name[which(nameshab %in% index$hab)],
                   wq_measure_name[which(nameswq %in% index$wq)],
                   lcmr_measure_name[which(nameslcmr %in% index$lcmr)],
                   cl_measure_name[which(namescl %in% index$cl)],
                   eco_measure_name[which(nameseco %in% index$eco)]
                   )
    weighthab<-c()
    utilityhab<-c()
    if(!is.null(index$hab)){
      
      for(i in 1:length(index$hab)){
        weighthab<-c(weighthab,weightindex[as.numeric(eval(parse(text = paste0("input$weight",index$hab[i]))))+1])
        if(eval(parse(text = paste0("input$switch",index$hab[i])))){
          utilityhab<-c(utilityhab,"Positive")
        }else{
          utilityhab<-c(utilityhab,"Negative")
        }
      }
    }
    weightwq<-c()
    utilitywq<-c()
    if(!is.null(index$wq)){
    
      for(i in 1:length(index$wq)){
        weightwq<-c(weightwq,weightindex[as.numeric(eval(parse(text = paste0("input$weight",index$wq[i]))))+1])
        if(eval(parse(text = paste0("input$switch",index$wq[i])))){
          utilitywq<-c(utilitywq,"Positive")
        }else{
          utilitywq<-c(utilitywq,"Negative")
        }
      }
    }
    weightlcmr<-c()
    utilitylcmr<-c()
    if(!is.null(index$lcmr)){
      for(i in 1:length(index$lcmr)){
        
        weightlcmr<-c(weightlcmr,weightindex[as.numeric(eval(parse(text = paste0("input$weight",index$lcmr[i]))))+1])
        if(eval(parse(text = paste0("input$switch",index$lcmr[i])))){
          utilitylcmr<-c(utilitylcmr,"Positive")
        }else{
          utilitylcmr<-c(utilitylcmr,"Negative")
        }
      }
    }
    weightcl<-c()
    utilitycl<-c()
    if(!is.null(index$cl)){
      for(i in 1:length(index$cl)){
        weightcl<-c(weightcl,weightindex[as.numeric(eval(parse(text = paste0("input$weight",index$cl[i]))))+1])
        if(eval(parse(text = paste0("input$switch",index$cl[i])))){
          utilitycl<-c(utilitycl,"Positive")
        }else{
          utilitycl<-c(utilitycl,"Negative")
        }
      }
    }
    weighteco<-c()
    utilityeco<-c()
    if(!is.null(index$eco)){
      for(i in 1:length(index$eco)){
        weighteco<-c(weighteco,weightindex[as.numeric(eval(parse(text = paste0("input$weight",index$eco[i]))))+1])
        if(eval(parse(text = paste0("input$switch",index$eco[i])))){
          utilityeco<-c(utilityeco,"Positive")
        }else{
          utilityeco<-c(utilityeco,"Negative")
        }
      }
    }
    Weights<-c( weighthab, weightwq, weightlcmr, weightcl, weighteco)
    Utility<-c(utilityhab,utilitywq,utilitylcmr,utilitycl,utilityeco)
    data.frame(Measures,Weights,Utility)
  })
#onclick event for going back to goal weights selection
  observeEvent(input$adjustgoal,{
    updateCollapse(session, "collapseExample", open = "Panel 3")
  })
#onclick event for going back to data measure selection  
  observeEvent(input$adjustmeasure,{
    updateCollapse(session, "collapseExample", open = "Panel 2")
  })
  #finish the second select panel  
  observeEvent(input$nextto3,{
    if(is.null(index$hab) &
       is.null(index$wq) &
       is.null(index$lcmr ) &
       is.null(index$cl) &
       is.null(index$eco)){
      showModal(modalDialog(
        title = "At least one data measure needed",footer = modalButton("Ok"),
        "Please select some data measures."
      ))

    }else{
      updateCollapse(session, "collapseExample", open = "Panel 3")
      
    }
  })
#render table for goal weights selection
  output$goalweight <- renderTable({
    data.frame(Goal = c("Habitat","Water Quality & Quantity","Living Coastal & Marine Resource","Community Resilience","Gulf Economy"),
               Weights=c(input$habslide,input$wqslide,input$lcmrslide,input$clslide,input$ecoslide))
  })

#finish the thrid select panel
  observeEvent(input$nextto4,{
    if((input$habslide+input$wqslide+input$lcmrslide+input$clslide+input$ecoslide)!=100){
      showModal(modalDialog(
        title = "The weights should sum up to 100.",footer = modalButton("Ok"),
        "Please go back and review the weights."
      ))
    }
    else{
      updateCollapse(session, "collapseExample", open = "Panel 4")
      #print(head(data1$hab_f))
      #print(class(data1$hab_f))
      #print(input$habslide)
      #print(class(input$habslide))

      

    }
  })
#finish the last select panel
  observeEvent(input$nextto1,{
    index$hab1<-input$habslide
    index$wq1<-input$wqslide
    index$lcmr1<-input$lcmrslide
    index$cl1<-input$clslide
    index$eco1<-input$ecoslide
    if(is.null(index$hab) &
       is.null(index$wq) &
       is.null(index$lcmr ) &
       is.null(index$cl) &
       is.null(index$eco)){
      showModal(modalDialog(
        title = "At least one data measure needed",footer = modalButton("Ok"),
        "Please select some data measures."
      ))
      
    }else if((index$hab1+index$wq1+index$lcmr1+index$cl1+index$eco1)!=100){
        showModal(modalDialog(
          title = "The weights should sum up to 100.",footer = modalButton("Ok"),
          "Please go back and review the weights."
        ))
    }else{
    #updateCollapse(session, "collapseExample", open = "Panel 1")
      if(!is.null(index$hab)){
        print(index$hab[1])
        print(eval(parse(text = paste0("input$weight",index$hab[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$hab)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",nameshab1[which(nameshab==index$hab[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$hab[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$hab[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$hab[i])))==FALSE){
            tmp1 <-1-tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$hab_f<<-tmp/length(index$hab)
      }
      if(!is.null(index$wq)){
        print(index$wq[1])
        print(eval(parse(text = paste0("input$weight",index$wq[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$wq)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",nameswq1[which(nameswq==index$wq[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$wq[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$wq[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$wq[i])))==FALSE){
            tmp1 <-1-1*tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$wq_f<<-tmp/length(index$wq)
      }
      if(!is.null(index$lcmr)){
        print(index$lcmr[1])
        print(eval(parse(text = paste0("input$weight",index$lcmr[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$lcmr)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",nameslcmr1[which(nameslcmr==index$lcmr[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$lcmr[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$lcmr[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$lcmr[i])))==FALSE){
            tmp1 <-1-1*tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$lcmr_f<<-tmp/length(index$lcmr)
      }
      if(!is.null(index$cl)){
        print(index$cl[1])
        print(eval(parse(text = paste0("input$weight",index$cl[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$cl)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",namescl1[which(namescl==index$cl[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$cl[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$cl[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$cl[i])))==FALSE){
            tmp1 <-1-1*tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$cl_f<<-tmp/length(index$cl)
      }
      if(!is.null(index$eco)){
        print(index$eco[1])
        print(eval(parse(text = paste0("input$weight",index$eco[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$eco)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",nameseco1[which(nameseco==index$eco[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$eco[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$eco[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$eco[i])))==FALSE){
            tmp1 <-1-1*tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$eco_f<<-tmp/length(index$eco)
      }
      if(is.null(index$eco)){
        data1$eco_f<<-data1$padus*0
      }
      if(is.null(index$cl)){
        data1$cl_f<<-data1$padus*0
      }
      if(is.null(index$wq)){
        data1$wq_f<<-data1$padus*0
      }
      if(is.null(index$lcmr)){
        data1$lcmr_f<<-data1$padus*0
      }
      if(is.null(index$hab)){
        data1$hab_f<<-data1$padus*0
      }
      
      data1$weight<<-(data1$hab_f*index$hab1+data1$wq_f*index$wq1+data1$lcmr_f*index$lcmr1+data1$cl_f*index$cl1+data1$eco_f*index$eco1)/100
     
     tmpmax = max(data1$weight)
     data1$weight<- data1$weight/tmpmax
     data1$weight<-round(data1$weight,2)
 #"ylorrd"    
    cols = colour_values_rgb(.bincode(data1$weight, b, FALSE, TRUE)/6, palette="ylorrd", include_alpha = FALSE) / 255 ### #
    showModal(modalDialog(
      title = "The map is on your way",footer = modalButton("Ok"),
      "Based on the large volume of computation, please allow up to 25 seconds for the map to load."
    ))

    leafletProxy("myMap")%>%
      clearGroup("weight")%>% 
      clearControls()%>%
      addGlPolygons(data = data1, color = cols,group = "weight", popup = "weight")%>%
      addLegend(title = "Score",labels=c("0","0.01-0.25","0.25-0.5","0.5-0.75","0.75-0.99","1") , colors = c("#FFFFB2", "#FED976", "#FEB24C","#FD8D3C","#F03B20","#BD0026"), opacity = 0.7,
              position = "bottomright")
    }
    #c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C","#FC4E2A","#E31A1C","#B10026","#800026") 9 classes
    #c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C","#FC4E2A","#E31A1C","#B10026") 8 classes   
    #c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A","#E31A1C","#B10026") 7 classes
    #c("#FFFFB2", "#FED976", "#FEB24C","#FD8D3C","#F03B20","#BD0026") 6 classes
  })
 
  
#finish the last select panel with the screening method  
  observeEvent(input$screening,{
    index$hab1<-input$habslide
    index$wq1<-input$wqslide
    index$lcmr1<-input$lcmrslide
    index$cl1<-input$clslide
    index$eco1<-input$ecoslide
    if(is.null(index$hab) &
       is.null(index$wq) &
       is.null(index$lcmr ) &
       is.null(index$cl) &
       is.null(index$eco)){
      showModal(modalDialog(
        title = "At least one data measure needed",footer = modalButton("Ok"),
        "Please select some data measures."
      ))
      
    }else if((index$hab1+index$wq1+index$lcmr1+index$cl1+index$eco1)!=100){
      showModal(modalDialog(
        title = "The weights should sum up to 100.",footer = modalButton("Ok"),
        "Please go back and review the weights."
      ))
    }else{
    #updateCollapse(session, "collapseExample", open = "Panel 1")
      if(!is.null(index$hab)){
        print(index$hab[1])
        print(eval(parse(text = paste0("input$weight",index$hab[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$hab)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",nameshab1[which(nameshab==index$hab[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$hab[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$hab[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$hab[i])))==FALSE){
            tmp1 <-1-tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$hab_f<<-tmp/length(index$hab)
      }
      if(!is.null(index$wq)){
        print(index$wq[1])
        print(eval(parse(text = paste0("input$weight",index$wq[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$wq)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",nameswq1[which(nameswq==index$wq[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$wq[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$wq[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$wq[i])))==FALSE){
            tmp1 <-1-1*tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$wq_f<<-tmp/length(index$wq)
      }
      if(!is.null(index$lcmr)){
        print(index$lcmr[1])
        print(eval(parse(text = paste0("input$weight",index$lcmr[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$lcmr)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",nameslcmr1[which(nameslcmr==index$lcmr[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$lcmr[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$lcmr[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$lcmr[i])))==FALSE){
            tmp1 <-1-1*tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$lcmr_f<<-tmp/length(index$lcmr)
      }
      if(!is.null(index$cl)){
        print(index$cl[1])
        print(eval(parse(text = paste0("input$weight",index$cl[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$cl)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",namescl1[which(namescl==index$cl[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$cl[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$cl[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$cl[i])))==FALSE){
            tmp1 <-1-1*tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$cl_f<<-tmp/length(index$cl)
      }
      if(!is.null(index$eco)){
        print(index$eco[1])
        print(eval(parse(text = paste0("input$weight",index$eco[1]))))
        tmp<-NULL
        tmp2<-NULL
        for(i in 1:length(index$eco)){
          tmp2<-as.numeric(eval(parse(text = paste0("data1$",nameseco1[which(nameseco==index$eco[i])]))))
          tmp1<-tmp2*as.numeric(eval(parse(text = paste0("input$weight",index$eco[i]))))/3
          if(eval(parse(text = paste0("input$switch",index$eco[i])))){
            tmp1 <-tmp1
          }else if(eval(parse(text = paste0("input$switch",index$eco[i])))==FALSE){
            tmp1 <-1-1*tmp1
          }
          if(is.null(tmp)){
            tmp<-tmp1
          }
          else{
            tmp<-tmp+tmp1
          }
        }
        data1$eco_f<<-tmp/length(index$eco)
      }
      if(is.null(index$eco)){
        data1$eco_f<<-data1$padus*0
      }
      if(is.null(index$cl)){
        data1$cl_f<<-data1$padus*0
      }
      if(is.null(index$wq)){
        data1$wq_f<<-data1$padus*0
      }
      if(is.null(index$lcmr)){
        data1$lcmr_f<<-data1$padus*0
      }
      if(is.null(index$hab)){
        data1$hab_f<<-data1$padus*0
      }
    index$hab1<-input$habslide
    index$wq1<-input$wqslide
    index$lcmr1<-input$lcmrslide
    index$cl1<-input$clslide
    index$eco1<-input$ecoslide
    data1$weight<<-(data1$hab_f*index$hab1+data1$wq_f*index$wq1+data1$lcmr_f*index$lcmr1+data1$cl_f*index$cl1+data1$eco_f*index$eco1)/100
    
    data1$weight1<-0
    data1$weight1[data1$weight>0.5]<-1
    cols = colour_values_rgb(data1$weight1, include_alpha = FALSE) / 255
    showModal(modalDialog(
      title = "The map is on your way",footer = modalButton("Ok"),
      "Based on the large volume of computation, please allow up to 25 seconds for the map to load."
    ))
    leafletProxy("myMap")%>%
      clearGroup("weight")%>% 
      clearControls()%>%
      addGlPolygons(data = data1, color = cols,group = "weight", popup = "weight1")%>%
      addLegend(title = "Result",labels =c("No passing the screening","Passed the screening") , colors = c("purple","yellow"), opacity = 0.7,
                position = "bottomright")
    
    
    }
  })

#future event for bookmark feature
  observeEvent(input$loadbookmark,{
    showModal(modalDialog(
      title = "Input from an existing model",footer = modalButton("Ok"),
      textInput("bookmark", "Type your bookmark here!",
                placeholder = 'Should be something looks like: "http://blank"'
      )
    ))

  })
  
  output$sumgoalweight<-renderText(sum(input$habslide+input$wqslide+input$lcmrslide+input$clslide+input$ecoslide))


#data measure selected for habitat
  observeEvent(input$habitat,{
    print(input$habitat)
    tmp<-setdiff(input$habitat,index$hab)
    tmp1<-setdiff(index$hab,input$habitat)
    print(tmp)
    print(tmp1)
    if(length(tmp)>0){
      for (i in 1:length(tmp)) {
        tmpp<-strsplit(tmp,"")[[i]]
        print(tmpp)
        insertUI(
          selector = "#nextto3",
          where='beforeBegin',
          ui = div(id=paste0("id",tmp[i]),
                   box(width = 12,title = hab_measure_name[as.numeric(tmpp[length(tmpp)])],
                       div(style="display: inline-block;vertical-align:top; width: 150px;",
                           switchButton(inputId = paste0("switch",tmp[i]),
                                        label = "Utility",
                                        value = TRUE, col = "GB", type = "TF")),
                       div(style="display: inline-block;vertical-align:top; width: 250px;",
                           selectInput(inputId = paste0("weight",tmp[i]),label = "Weight",choices = c("Low"=1,"Medium"=2,"High"=3),selected = 3)
                       )
                      )
              )


        )
      }
    }
    if(length(tmp1)>0){
      for (i in 1:length(tmp1)){
        removeUI(
          selector = paste0("#","id",tmp1[i])
        )

      }
    }
    index$hab<-input$habitat


  }, ignoreNULL = FALSE)
#data measure selected for water quality
  observeEvent(input$wq,{
    tmp<-setdiff(input$wq,index$wq)
    tmp1<-setdiff(index$wq,input$wq)
    if(length(tmp)>0){
      for (i in 1:length(tmp)) {
        tmpp<-strsplit(tmp,"")[[i]]
        insertUI(
          selector = "#nextto3",
          where='beforeBegin',
          ui = div(id=paste0("id",tmp[i]),
                   box(width = 12,title = wq_measure_name[as.numeric(tmpp[length(tmpp)])],
                       div(style="display: inline-block;vertical-align:top; width: 150px;",
                           switchButton(inputId = paste0("switch",tmp[i]),
                                        label = "Utility",
                                        value = TRUE, col = "GB", type = "TF")),
                       div(style="display: inline-block;vertical-align:top; width: 250px;",
                           selectInput(inputId = paste0("weight",tmp[i]),label = "Weights",choices = c("Low"=1,"Medium"=2,"High"=3),selected = 3)
                       )
                   )
          )


        )
      }
    }
    if(length(tmp1)>0){
      for (i in 1:length(tmp1)){
        removeUI(
          selector = paste0("#","id",tmp1[i])
        )

      }
    }
    index$wq<-input$wq


  }, ignoreNULL = FALSE)
#data measure selected for lcmr
  observeEvent(input$lcmr,{
    tmp<-setdiff(input$lcmr,index$lcmr)
    tmp1<-setdiff(index$lcmr,input$lcmr)
    if(length(tmp)>0){
      for (i in 1:length(tmp)) {
        tmpp<-strsplit(tmp,"")[[i]]
        insertUI(
          selector = "#nextto3",
          where='beforeBegin',
          ui = div(id=paste0("id",tmp[i]),
                   box(width = 12,title = lcmr_measure_name[as.numeric(tmpp[length(tmpp)])],
                       div(style="display: inline-block;vertical-align:top; width: 150px;",
                           switchButton(inputId = paste0("switch",tmp[i]),
                                        label = "Utility",
                                        value = TRUE, col = "GB", type = "TF")),
                       div(style="display: inline-block;vertical-align:top; width: 250px;",
                           selectInput(inputId = paste0("weight",tmp[i]),label = "Weights",choices = c("Low"=1,"Medium"=2,"High"=3),selected = 3)
                       )
                   )
          )


        )
      }
    }
    if(length(tmp1)>0){
      for (i in 1:length(tmp1)){
        removeUI(
          selector = paste0("#","id",tmp1[i])
        )

      }
    }
    index$lcmr<-input$lcmr


  }, ignoreNULL = FALSE)
#data measure selected for cl
  observeEvent(input$cl,{
    tmp<-setdiff(input$cl,index$cl)
    tmp1<-setdiff(index$cl,input$cl)
    if(length(tmp)>0){
      for (i in 1:length(tmp)) {
        tmpp<-strsplit(tmp,"")[[i]]
        insertUI(
          selector = "#nextto3",
          where='beforeBegin',
          ui = div(id=paste0("id",tmp[i]),
                   box(width = 12,title = cl_measure_name[as.numeric(tmpp[length(tmpp)])],
                       div(style="display: inline-block;vertical-align:top; width: 150px;",
                           switchButton(inputId = paste0("switch",tmp[i]),
                                        label = "Utility",
                                        value = TRUE, col = "GB", type = "TF")),
                       div(style="display: inline-block;vertical-align:top; width: 250px;",
                           selectInput(inputId = paste0("weight",tmp[i]),label = "Weights",choices = c("Low"=1,"Medium"=2,"High"=3),selected = 3)
                       )
                   )
          )


        )
      }
    }
    if(length(tmp1)>0){
      for (i in 1:length(tmp1)){
        removeUI(
          selector = paste0("#","id",tmp1[i])
        )

      }
    }
    index$cl<-input$cl

  }, ignoreNULL = FALSE)
#data measure selected for eco
  observeEvent(input$eco,{
    tmp<-setdiff(input$eco,index$eco)
    tmp1<-setdiff(index$eco,input$eco)
    if(length(tmp)>0){
      for (i in 1:length(tmp)) {
        tmpp<-strsplit(tmp,"")[[i]]
        insertUI(
          selector = "#nextto3",
          where='beforeBegin',
          ui = div(id=paste0("id",tmp[i]),
                   box(width = 12,title = eco_measure_name[as.numeric(tmpp[length(tmpp)])],
                       div(style="display: inline-block;vertical-align:top; width: 150px;",
                           switchButton(inputId = paste0("switch",tmp[i]),
                                        label = "Utility",
                                        value = TRUE, col = "GB", type = "TF")),
                       div(style="display: inline-block;vertical-align:top; width: 250px;",
                           selectInput(inputId = paste0("weight",tmp[i]),label = "Weights",choices = c("Low"=1,"Medium"=2,"High"=3),selected = 3)
                       )
                   )
          )


        )
      }
    }
    if(length(tmp1)>0){
      for (i in 1:length(tmp1)){
        removeUI(
          selector = paste0("#","id",tmp1[i])
        )

      }
    }
    index$eco<-input$eco

  }, ignoreNULL = FALSE)
  
  observe({
    shinyjs::hide("habslide")
    shinyjs::hide("wqslide")
    shinyjs::hide("lcmrslide")
    shinyjs::hide("clslide")
    shinyjs::hide("ecoslide")
    if(!is.null(index$hab)){
      shinyjs::show("habslide")
    }else{
      updateSliderInput(session,"habslide",value=0)
    }
    if(!is.null(index$wq)){
      shinyjs::show("wqslide")
    }else{
      updateSliderInput(session,"wqslide",value=0)
    }
    if(!is.null(index$lcmr)){
      shinyjs::show("lcmrslide")
    }else{
      updateSliderInput(session,"lcmrslide",value=0)
    }
    if(!is.null(index$cl)){
      shinyjs::show("clslide")
    }else{
      updateSliderInput(session,"clslide",value=0)
    }
    if(!is.null(index$eco)){
      shinyjs::show("ecoslide")
    }else{
      updateSliderInput(session,"ecoslide",value=0)
    }
  })















}
