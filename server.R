library(shiny)
library(ggvis)
library(dplyr)
#library(DT)
library(rdrop2)

#if (FALSE) library(RSQLite)



shinyServer(function(input, output, session) {
  
  #  output$Box1 = renderUI(selectInput("aos", "Area of Specialty", c("All", aos_lev)
  #                                     , selected = "ALL")
  # )
  output$Box2 = renderUI(if (input$aos != "All"){
    selectInput("primaos", "Primary AOS", 
                c("All", levels(factor(all$primaryAoS[which(all$AOS ==input$aos)]))),
                "All")
  }
  )
  
  
  # Filter the graduates, returning a data frame
  graduates <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    #Gender <- input$Gender
    #Phdfrom <- factor(input$Phdfrom)
    #aos <- input$aos 
    #minyear <- factor(input$Graduation_year[1])
    Legend <- input$Legend
    minyear <- input$Graduation_year[1]
    maxyear <- input$Graduation_year[2]
    #minboxoffice <- input$boxoffice[1] * 1e6
    #maxboxoffice <- input$boxoffice[2] * 1e6
    
    # Apply filters
    m <- all %>%
      filter(
        #gender == Gender,
        graduation_year >= minyear,
        graduation_year <= maxyear
      ) %>%
      arrange(graduation_year)
    
    n<-all #[1,]
    # Optional: filter by AOS
    if (input$aos != "All") {
      # aos <- paste0("%", input$aos, "%")
      m <- filter(m, AOS == input$aos)
    }
    if (!is.null(input$primaos) && input$primaos != "All"){
      m <- filter(m, primaryAoS == input$primaos)
    }
    
    # Optional: filter by placement type
    if (input$Position != "All") {
      #Position <- paste0("%", input$Position, "%")
      m <- filter(m, position == input$Position)
    }
    
    # Optional: filter by genre
    if (input$Phdfrom != "All") {
      #   Phdfrom <- paste0("%", input$aos, "%")
      m <- filter(m, university_name == input$Phdfrom)
    }
    
    
    #    # Optional: filter by school
    #    if (!is.null(input$Phdfrom) && input$Phdfrom != "") {
    #      Phdfrom <- paste0("%", input$Phdfrom, "%")
    #      m <- m %>% filter(university_name %like% Phdfrom)
    #    }
    #    if(nrow(m) == 0){
    
    if(nrow(m) == 0){
      createAlert(session, "alert2", "Alert", title = "Oops:",
                  content = "Data is not available for the criteria you have selected. Please broaden your search.", append = FALSE)
      m<-n
    }
    else {
      closeAlert(session, "Alert")
    }
    if(minyear < 2012) {
      createAlert(session, "alert", "2012Alert", title = "Note:",
                  content = "Data for years prior to 2012 may be incomplete.", append = FALSE)
    }
    else {
      closeAlert(session, "2012Alert")
    }
    
    # m <- data.frame(person_id = "10000000", gender="3",  ethicity="1", race="1", AOS="", 
    #  primaryAoS="", graduation_year="", university_id="", university_name="", placement_year="",
    #  placement_university="", position="")
    #  m <- m[1,]
    #     m <-n
    #    }
    
    
    m <- as.data.frame(m)
    
    # Add column for gender
    # Be a little careful in case we have a zero-row data frame
    m$has_gender <- character(nrow(m))
    m$has_gender[m$gender == 1] <- "Men"
    m$has_gender[m$gender == 2] <- "Women"
    m$has_gender[m$gender >  2] <- "Unknown"
    #levels(m$has_gender) <- list(Men = "Men", Women = "Women", Unknown = "Unkown")
    #m$color <- c("blue","green","purple")
    #m$has_gender <- factor(m$has_gender, levels = rev(levels(m$has_gender)))
    if (input$legendvar == "has_gender") {
      m$legendvar <- factor(m$has_gender, levels= c("Men", "Women", "Unknown"))
    }
    else {
      m$legendvar <- factor(m$position,levels= c("Tenure-Track", "Other Permanent Position", "Fellowship/Postdoc",
                                                 "Visiting Position", "Other Temporary Position", "Non-academic Position", "Not Placed or Unknown Placement"))
      
    }
    #    m$has_position <- character(nrow(m))
    #    m$has_position[m$position == 1] <- "Men"
    #    m$has_position[m$position == 2] <- "Women"
    #    m$has_position[m$position >  2] <- "Unknown"
    
    m
  })
  
  vis_names <- function(x) {
    if(is.null(x)) return(NULL)
    paste0(names(x),collapse="<br />")
  }
  
  bar_info <- function(x) {
    if(is.null(x)) return(NULL) 
    
    paste(
      names(x)[1],
      x[1,1],
      names(x)[2],
      x[1,2],
      names(x)[3],
      x[1,3],
      names(x)[4],
      x[1,4],
      sep=" : ",
      collapse="<br />")
  }
  
  
  
  graduate_tooltip <- function(x) {
    if(is.null(x)) return(NULL)
    xvar2 <- input$xvar
    legendvar <- input$legendvar
    
    data <- isolate(graduates())
    my_data <-aggregate(data["person_id"], by=c(data[legendvar], data[xvar2]), FUN=length)
    bar_obj <- my_data %>% 
      #  group_by(legendvar)%>% 
      ggvis(x = ~graduation_year) %>%
      layer_bars(fillOpacity := 1, fillOpacity.hover := .5, width = .5,
                 #      stroke = ~has_gender, 
                 stack=FALSE
      )
    #   bar_obj <- get("bar_obj",.GlobalEnv)
    
    input_data <- get_data(bar_obj)[[1]]
    bar_data <- get_data(bar_obj)[[3]]
    xmin_col <- input_data[,2]
    #row <- which.min(abs(x[,2]-xmin_col))
    row <- abs(x[1,2]-xmin_col)
    #gen <- abs(x[,2]-xmin_col)
    row_idx <- which(row == min(row))
    #  input_data$newcol <- input_data[,1]
    #      input_data$var <- input_data[,1]
    #     input_data[,var]<- input_data[,get("legendvar")]
    paste0(input_data[row_idx,1], 
           ": ","<br>",
           input_data$person_id[row_idx],
           collapse = "<br />"
    )
  }
  # A reactive expression with the ggvis plot
  vis <- reactive({
    
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    #    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    xvar2 <- input$xvar
    
    legend_name <- names(legend_vars)[legend_vars == input$legendvar]
    legendvar <- input$legendvar
    
    #   yvar <- prop("y", as.symbol(input$yvar))
    #   graduates <- graduates %>% mutate(position = factor(position))
    #    graduates %>%
    #        group_by(has_gender)%>%

    #bar_obj %>% add_tooltip(graduate,"hover") 
    graduates %>%
      #my_data %>%
      group_by(legendvar)%>%
      ggvis(x=xvar, fill = ~legendvar)%>%#~has_gender)%>%
      #            , y = yvar) %>%
      layer_bars(fillOpacity := 1, fillOpacity.hover := .5, width = .5,
                 #      stroke = ~has_gender, 
                 stack=FALSE
      )%>%
      
      #size.hover := 200, 
      #                       fillOpacity := .75, fillOpacity.hover := 1,
      #                        width = .5,
      #                       stack=FALSE) %>%
      
      #  layer_points(size := 50, size.hover := 200,
      
      #, key := ~graduation_year) %>%
      
      
    add_tooltip(graduate_tooltip, "hover") %>%
      
      add_axis("x", title = xvar_name, title_offset = 100, format='d',
               properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
      add_axis("y", title = "# of Placements") %>%
      add_legend("fill", title = legend_name) %>%
      #                     properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
      #        add_legend("fill", title = "Gender", values = c("Men", "Women", "Unknown")) %>%
      
      #    scale_nominal("stroke", domain = c("Uknown","Men", "Women"),
      #                  range = c("blue","orange", "#aaa")) %>%
      
      #            , y = yvar) %>%
      #    layer_points(size := 50, size.hover := 200,
      #                 fillOpacity := 0.2, fillOpacity.hover := 0.5,
      #                 stroke = ~gender, key := ~person_id) %>%
      #    add_tooltip(graduate_tooltip, "hover") %>%
    #    add_axis("x", title = xvar_name) %>%
    #    add_axis("y", title = yvar_name) %>%
    #    add_legend("stroke", title = "Gender", values = c("Men", "Women")) %>%
    scale_nominal("fill", domain = levels(legendvar), sort = FALSE,
                  range = c("cornflowerblue", "lightseagreen", "#aaa",
                            "orange","purple","turquoise", "red")) %>%
      set_options(width = 1000, height = 500)
  })
  
  vis %>% bind_shiny("plot1")
  #  output$mytable <- renderTable({graduates()})
  output$n_graduates <- renderText({ nrow(graduates()) })
  output$text1 <- renderText({ 
    "*Data for years prior to 2012 may be incomplete. "
  })
  output$text2 <- renderText({ 
    "*Data for certain universities may be incomplete. "
  })
  #my_data2 <- reactive({
   # xvar2 <- input$xvar
    #legendvar <- input$legendvar
    #data1 <- graduates()
   # data2 <- as.data.frame(aggregate(data1, by=c(data1[legendvar],data1[xvar2]), 
    #                             FUN=length))
    #xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)
    #t <-ftable(xtabs(~ has_gender+graduation_year, data = data1))
   # t
  #})
  t <- reactive({
  xtabs(as.formula(paste0("~",input$legendvar,"+",input$xvar)), graduates())
  })
  n <- reactive({
    legend_name <- names(legend_vars)[legend_vars == input$legendvar]
    tf <- c(rownames(t()))
    tf2 <- c(rownames(t()))
    tf3<-cbind(tf,tf2)
   colnames(tf3)<-c(legend_name, legend_name)
    tf4 <-cbind(tf3,t())
    tf4
  })
     output$mytable <- renderDataTable({ n()[,-1]
       #my_data2()
       #data.frame(x=my_data2())
     })
  #  output$Position <- renderText({ input$position })
})

