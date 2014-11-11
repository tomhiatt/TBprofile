###########################
# SERVER
# Tom Hiatt
# MDR-TB tab of TB profile
# Updated: 26 Sep 2014
###########################

# setwd("D:/Users/hiattt/Dropbox/Code/Shiny/TBprofile")

# library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)
library(gridExtra)
library(googleVis)

# Functions

sum0 <- function(x) sum(x, na.rm = !all(is.na(x)))

rowSumsNA <- function(x,...) {
  # remove NAs unless all are NA
  rowMeans(x, na.rm=TRUE) * rowSums(!is.na(x))
}

# Theme for plots
theme_bw2 <- function(base_size=12, base_family="") {
  require(ggthemes)
  colors <- ggthemes_data$few
  gray <- colors$medium['gray']
  black <- colors$dark['black'] # I don't know why these last 3 parts are needed, but they are. List is here: http://docs.ggplot2.org/0.9.2.1/theme.html
  theme_bw(base_size=base_size, base_family=base_family) +
    theme(
      line = element_line(colour = gray),
      rect = element_rect(fill = "white", colour = NA),
      text = element_text(colour = black),
      axis.ticks.x = element_line(colour = gray),
      axis.ticks.y = element_blank(),
      legend.key = element_rect(colour = NA),
      ## Examples do not use grid lines
      panel.border = element_rect(colour = gray),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill="white", colour=NA),
      strip.text = element_text(hjust=0),
      legend.position = "bottom", 
      legend.direction = "vertical", 
      legend.text.align = 0,
      plot.title = element_text(hjust=0),
      legend.title = element_blank()
    )
}

# Prepare dataset
if(FALSE){
  runprofile()
  dta <- select(merge(tb, f, all.x=T), country, iso3, g_whoregion, year, c_newinc, mdr, 
                c_notified, c_ret, new_ep, ret_rel_ep, 
                dst_rlt_new, dst_rlt_ret, dst_rlt_unk,
                dr_r_nh_ret, dr_r_nh_new, dr_r_nh_unk, 
                mdr_new,mdr_ret,mdr_unk,
                xpert_new, xpert_ret, xpert_unk, 
                xpert_dr_r_new, xpert_dr_r_ret, xpert_dr_r_unk,
                rapid_dx_dr_r, conf_mdr_tx, unconf_mdr_tx,
                mdr_coh, mdr_cur, mdr_cmplt, mdr_died, 
                mdr_fail, mdr_def, mdr_succ,
                exp_sld, exp_mdrmgt)
  dta$admin0 <- "Global"
  num.idvars <- 4 
  names(dta)[1:num.idvars] <- c("admin2", "admin2code", "admin1", "year")
  dta <- dta[c(ncol(dta), 1:(ncol(dta)-1))]
  save(dta, num.idvars, file="dta.Rdata")
  
  input <- data.frame(a2select=c("VNM", "WPR"))
  load("dta.Rdata")
  dta1 <- dta
}

load("dta.Rdata")

template <- dta
# template[(num.idvars+1):ncol(dta)] <- NA

shinyServer(function(input, output, session) {
  
  output$template <- downloadHandler(
    filename = function() "template.csv",
    content = function(file) {
      write.csv(template, file, row.names = FALSE, na="")
    }
  )
  
  dta1 <- reactive({
    inFile <- input$filled.template
    
    if (is.null(inFile))
      return(dta)
    
    df.raw <- read.csv(inFile$datapath, header=TRUE, colClasses=c(rep("character",4), rep("numeric",34)))
    
    #     if(identical(names(dta), names(df.raw))) return(df.raw)
    #       else(warning("Uploaded template does not match. Please make sure the first row is identical to the template and try again.")) 
    
  })
  
  
  observe({
    
    
    dtao <- dta1() %>% # dtao <- dta1 %>%
      filter(year==max(year)) %>%
      select(admin0, admin1, admin2, admin2code)
    
    #   subset(dta, year==max(dta$year), c(admin0, admin1, admin2, admin2code))
    dtan <- rbind(data.frame(admin2=unique(dtao$admin0), 
                             admin2code=unique(dtao$admin0)), 
                  data.frame(admin2=sort(unique(dtao$admin1)), 
                             admin2code=sort(unique(dtao$admin1))), 
                  dtao[c("admin2", "admin2code")])
    Encoding(dtan$admin2) <- "latin1"
    ls.code <- as.list(dtan$admin2code)
    names(ls.code) <- sapply(ls.code, function(x) dtan[dtan$admin2code==x,"admin2"])
    ls.admin2 <- as.list(dtan$admin2)
    names(ls.admin2) <- sapply(ls.admin2, function(x) dtan[dtan$admin2==x,"admin2code"])
    
    
    output$areaSelect <- renderUI({
      selectInput(inputId = "a2select", 
                  label = "Select areas to combine and compare:", 
                  choices = ls.code, 
                  selected=ls.code[c(7, 109, 45, 114, 162, 160, 137, 217)], 
                  width="400px", 
                  multiple=TRUE)
      
    })
  })
  
  output$test <- renderText({
    "Coming soon!"
  })
  
  output$profileplots <- renderPlot({
    #         par( mfrow =c(2,2)) # mar=c(3,4,4,2)+0.2, , cex=1.1, oma=c(1,1,2,1)
    
    # f.dst.trend --------------------------------------------------
    
    plot1 <- dta1() %>% # dta2 <- dta1 %>%
      filter(year >= 2007, admin2code %in% input$a2select | admin1 %in% input$a2select | admin0 %in% input$a2select
      ) %>%
      
      select(admin2, admin2code, admin1, year, 
             c_notified, c_ret, new_ep, ret_rel_ep, 
             dst_rlt_new, dst_rlt_ret, dst_rlt_unk,
             dr_r_nh_ret, dr_r_nh_new, dr_r_nh_unk, 
             mdr_new,mdr_ret,mdr_unk,
             xpert_new, xpert_ret, xpert_unk, 
             xpert_dr_r_new, xpert_dr_r_ret, xpert_dr_r_unk
      ) %>%
      
      mutate(dstx = rowSumsNA(cbind(dst_rlt_ret, xpert_ret)),
             mdrr = rowSumsNA(cbind(mdr_ret, dr_r_nh_ret, xpert_dr_r_ret)),
             
             dstx.nmr = ifelse(is.na(c_ret), NA, dstx),
             dstx.dmr = ifelse(is.na(dstx), NA, c_ret),
             
             mdrr.nmr = ifelse(is.na(dstx), NA, mdrr),
             mdrr.dmr = ifelse(is.na(mdrr), NA, dstx)
      ) %>%
      group_by(year) %>%
      summarize(dstx_pct = sum0(dstx.nmr) / sum0(dstx.dmr) * 100, 
                mdrr_pct = sum0(mdrr.nmr) /sum0( mdrr.dmr) * 100
      ) %>%
      
      melt(id=1) %>%
      
      ggplot(aes(year, value, color=variable)) + geom_point(alpha=.5) + geom_line(size=1, alpha=.5) + scale_color_brewer(type="qual", palette=6, breaks=c("dstx_pct", "mdrr_pct"), labels=c(expression("% DST among pulmonary cases"^a), "% MDR-TB or RR-TB among tested")) + scale_x_continuous("") + scale_y_continuous("Percent")+ guides(fill = guide_legend(reverse = TRUE)) + theme_bw2() + expand_limits(y=0) + ggtitle("DST coverage and MDR-TB positivity")
    #     + geom_text(aes(label=variable), subset(dta2, year==2013), hjust=1.25, vjust=0.3, size=4)
    
    # f.alignment ------------------------------------------------------
    
    plot2 <- dta1() %>% # dta2 <- dta1 %>%
      filter(year >= 2007, admin2code %in% input$a2select | admin1 %in% input$a2select | admin0 %in% input$a2select
      ) %>%
      
      select(admin2, admin2code, admin1, year, 
             mdr, rapid_dx_dr_r, conf_mdr_tx, unconf_mdr_tx
      ) %>%
      
      mutate(rrconf = rowSumsNA(cbind(mdr, rapid_dx_dr_r)),
             enrolled = rowSumsNA(cbind(conf_mdr_tx, unconf_mdr_tx))
      ) %>%
      
      group_by(year) %>%
      
      summarize(`Cases confirmed` = as.integer(sum0(rrconf)), 
                `Patients enrolled on treatment` = as.integer(sum0(enrolled))
      ) %>%
      
      melt(id=1) %>%
      
      ggplot(aes(year, value, color=variable)) + geom_point(alpha=.5) + geom_line(size=1, alpha=.5) + scale_color_brewer("", type="qual", palette=6) + scale_x_continuous("") + scale_y_continuous("MDR-TB cases")+ guides(fill = guide_legend(reverse = TRUE)) + theme_bw2() + expand_limits(y=0) + ggtitle("Notification and enrolment")
    #     + geom_text(aes(label=variable), subset(dta2, year==2013), hjust=1.25, vjust=0.3, size=4)
    
    # f.tx.out --------------------------------------------
    
    plot3 <- dta1() %>% # dta2 <- dta1 %>%
      filter(year %in% 2007:2011, admin2code %in% input$a2select | admin1 %in% input$a2select | admin0 %in% input$a2select
      ) %>%
      
      select(admin2, admin2code, admin1, year, 
             mdr_coh, mdr_cur, mdr_cmplt, mdr_died, mdr_fail, mdr_def, mdr_succ
      ) %>%
      
      mutate(mdr_succ = ifelse(year>=2011, mdr_succ, rowSumsNA(cbind(mdr_cur, mdr_cmplt)))
      ) %>%
      
      group_by(year) %>%
      
      summarize(Success = sum0(mdr_succ) / sum0(mdr_coh) * 100,
                Died = sum0(mdr_died) / sum0(mdr_coh) * 100,
                Failed = sum0(mdr_fail) / sum0(mdr_coh) * 100,
                `Lost to follow-up` = sum0(mdr_def) / sum0(mdr_coh) * 100,
                `Not evaluated` = (sum0(mdr_coh) - sum0(c(mdr_succ, mdr_died, mdr_fail, mdr_def))) / sum0(mdr_coh) * 100
      ) %>%
      
      melt(id=1) %>%
      
      #       if(all(is.na(value))) mutate(, value = 0) %>%
      
      ggplot(aes(year, value, fill=variable)) + geom_bar(stat="identity", position="stack") + theme_bw2() + scale_fill_brewer(type="qual", palette=6) + scale_x_continuous("", limits=c(2006.5, 2011.5)) + scale_y_continuous("Percent of cohort") + coord_cartesian(ylim=c(0,100)) + guides(fill = guide_legend(reverse = TRUE)) + ggtitle("Treatment outcomes")
    
    # f.expend ------------------------------------------------------
    plot4 <- dta1() %>% # dta2 <- dta1 %>%
      filter(year >= 2007, admin2code %in% input$a2select | admin1 %in% input$a2select | admin0 %in% input$a2select
      ) %>%
      
      select(admin2, admin2code, admin1, year, 
             exp_sld, exp_mdrmgt
      ) %>%
      
      mutate(exp_pmdt = rowSumsNA(cbind(exp_sld, exp_mdrmgt))
      ) %>%
      
      group_by(year) %>%
      
      summarize(exp_pmdt = sum0(exp_pmdt) / 1000
      ) %>%
      
      ggplot(aes(year, exp_pmdt)) + geom_point(alpha=.5) + geom_line(size=1, alpha=.5) + scale_x_continuous("") + scale_y_continuous("US$ (thousands)") + theme_bw2() + expand_limits(y=0) + ggtitle(
        "Expenditure on MDR-TB")
    
    grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
    
  })
  
  # Compare plot -------------------------------------------------
  output$compareplot <- renderGvis({
    
    # calculate vars first
    
    dta6 <- dta1() %>% # dta6 <- dta1 %>%
#       filter(area %in% input$a2select) %>%
      
      mutate(dstx = rowSumsNA(cbind(dst_rlt_ret, xpert_ret)),
             mdrr = rowSumsNA(cbind(mdr_ret, dr_r_nh_ret, xpert_dr_r_ret)),
             
             dstx.nmr = ifelse(is.na(c_ret), NA, dstx),
             dstx.dmr = ifelse(is.na(dstx), NA, c_ret),
             
             mdrr.nmr = ifelse(is.na(dstx), NA, mdrr),
             mdrr.dmr = ifelse(is.na(mdrr), NA, dstx)
      ) %>%
      mutate(rrconf = rowSumsNA(cbind(mdr, rapid_dx_dr_r)),
             enrolled = rowSumsNA(cbind(conf_mdr_tx, unconf_mdr_tx))
      ) %>%
      mutate(mdr_succ = ifelse(year>=2011, mdr_succ, rowSumsNA(cbind(mdr_cur, mdr_cmplt)))
      ) %>%
      mutate(exp_pmdt = rowSumsNA(cbind(exp_sld, exp_mdrmgt))
      ) %>%
      
      mutate(dstx_pct = dstx.nmr / dstx.dmr * 100, 
             mdrr_pct = mdrr.nmr / mdrr.dmr * 100,
             
             `Cases confirmed` = as.integer(rrconf), 
             `Patients enrolled on treatment` = as.integer(enrolled),
             
             Success = mdr_succ / (mdr_coh) * 100,
             Died = (mdr_died) / (mdr_coh) * 100,
             Failed = (mdr_fail) / (mdr_coh) * 100,
             `Lost to follow-up` = (mdr_def) / (mdr_coh) * 100,
             `Not evaluated` = ((mdr_coh) - rowSumsNA(cbind(mdr_succ, mdr_died, mdr_fail, mdr_def))) / (mdr_coh) * 100,
             
             exp_pmdt = (exp_pmdt) / 1000
      )
      
    # reshape the data so admin1 and admin0 aggregates are added as rows.
    
    #admin0
    dta3 <- dta6 %>% # dta3 <- dta6 %>%
      group_by(year, admin0) %>%
      select(-contains("admin")) %>%
      summarise_each(funs(sum0))%>% 
      rename(c(admin0="area"))
    
    #admin1
    dta4 <- dta6 %>% # dta4 <- dta6 %>%
      group_by(year, admin1) %>%
      select(-contains("admin")) %>%
      summarise_each(funs(sum0)) %>%
      rename(c(admin1="area"))
    
    #admin2
    dta5 <- dta6 %>% # dta5 <- dta6 %>%
      rename(c(admin2code="area")) %>%
      select(-contains("admin"))
    
    dtb <- rbind(dta5, dta4, dta3)
    
    # save the initial state
    
    prestate <- '
{"sizeOption":"_UNISIZE","time":"2013","dimensions":{"iconDimensions":["dim0"]},"yAxisOption":"3","yZoomedDataMin":0,"orderedByY":false,"yLambda":1,"iconKeySettings":[],"uniColorForNonSelected":false,"xAxisOption":"_TIME","showTrails":false,"duration":{"timeUnit":"Y","multiplier":1},"xZoomedIn":false,"iconType":"LINE","xLambda":1,"colorOption":"_UNIQUE_COLOR","nonSelectedAlpha":0.4,"yZoomedDataMax":130,"orderedByX":false,"yZoomedIn":false,"xZoomedDataMax":1356998400000,"playDuration":15000,"xZoomedDataMin":788918400000}    
    '
    
    
    # calculate all the MDR indicators
    
    dtb %>% 
      filter(area %in% input$a2select) %>%
#       
#       mutate(dstx = rowSumsNA(cbind(dst_rlt_ret, xpert_ret)),
#              mdrr = rowSumsNA(cbind(mdr_ret, dr_r_nh_ret, xpert_dr_r_ret)),
#              
#              dstx.nmr = ifelse(is.na(c_ret), NA, dstx),
#              dstx.dmr = ifelse(is.na(dstx), NA, c_ret),
#              
#              mdrr.nmr = ifelse(is.na(dstx), NA, mdrr),
#              mdrr.dmr = ifelse(is.na(mdrr), NA, dstx)
#       ) %>%
#       mutate(rrconf = rowSumsNA(cbind(mdr, rapid_dx_dr_r)),
#              enrolled = rowSumsNA(cbind(conf_mdr_tx, unconf_mdr_tx))
#       ) %>%
#       mutate(mdr_succ = ifelse(year>=2011, mdr_succ, rowSumsNA(cbind(mdr_cur, mdr_cmplt)))
#       ) %>%
#       mutate(exp_pmdt = rowSumsNA(cbind(exp_sld, exp_mdrmgt))
#       ) %>%
#       
#       mutate(dstx_pct = dstx.nmr / dstx.dmr * 100, 
#              mdrr_pct = mdrr.nmr / mdrr.dmr * 100,
#              
#              `Cases confirmed` = as.integer(rrconf), 
#              `Patients enrolled on treatment` = as.integer(enrolled),
#              
#              Success = mdr_succ / (mdr_coh) * 100,
#              Died = (mdr_died) / (mdr_coh) * 100,
#              Failed = (mdr_fail) / (mdr_coh) * 100,
#              `Lost to follow-up` = (mdr_def) / (mdr_coh) * 100,
#              `Not evaluated` = ((mdr_coh) - rowSumsNA(cbind(mdr_succ, mdr_died, mdr_fail, mdr_def))) / (mdr_coh) * 100,
#              
#              exp_pmdt = (exp_pmdt) / 1000
#       ) %>%
      
      # Make a big google bubble chart with the data and these indicators
      
      select(Area=area, Year=year, `DST among pulmonary cases (%)`=dstx_pct, `MDR-TB or RR-TB among tested (%)`=mdrr_pct, `Confirmed MDR/RR-TB Cases`=`Cases confirmed`, `Patients enrolled on MDR-TB treatment`=`Patients enrolled on treatment`, `Treatment success (%)`=Success, `Died (%)`=Died, `Failed (%)`=Failed, `Lost to follow-up (%)`=`Lost to follow-up`, `Not evaluated (%)`=`Not evaluated`, `Expenditure on PMDT (thousands)`=exp_pmdt) %>%
      
      gvisMotionChart(idvar="Area", timevar="Year", options=list(state=prestate))  # %>%
    
    #       plot()
    
  })
  
  # output$view <- renderGvis({
  #   gvisScatterChart(cars)
  # })
  
})





# library(ggplot2)
# 
# tb %>%
#   filter(g_whoregion=="WPR") %>%
#   filter(iso3=="KHM") %>%
#   select(year,c_notified) %>%
# #  ggplot(aes(year, c_notified)) + geom_line() %>%
#   function(data){qplot(year,c_notified, data=data)}
# 
# 
# %>% 
#   sum(na.rm=T)





