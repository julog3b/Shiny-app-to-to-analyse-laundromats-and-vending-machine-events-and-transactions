shinyServer(function(input, output) {
  
  react <- reactiveValues(cleanData1=NULL, cleanData2=NULL,
                          recipe=NULL, model=NULL)
  
  onSessionEnded(function() {
    stopApp()
  })
  
  
  
  
  
  
  
  
  observeEvent(input$Go1,
               {getdata()})
  
  
  
  
  # -----------------------------------------------------
  # CONNECT TO BIGQUERY
  # ----------------------------------------------------
  
  
  getdata <- reactive({
    req(input$Go1)
    showNotification(id = "Loading", ui = "Data is loading. Please wait...",
                     duration = NULL, type = "error")
    
    # --------------------------------------------------------------------------
    #
    #---------------------------------------------------------------------------
    start_date = input$daterange[1] - hours(13)
    end_date = input$daterange[2] + hours(11)
    
    # Get the user's local timezone
     local_tz <- Sys.timezone()
     
     start_date <- as.character(with_tz(ymd(input$daterange[1]), local_tz))
     end_date <- as.character(with_tz(ymd(input$daterange[2]), local_tz))
     
     
 
 
 
     con <- dbConnect(
       bigrquery::bigquery(),
       project = "payments-cloud-test",
       dataset = "live_vending_data_snapshot_20230202",
       billing = "payments-cloud-test")
 
 
 
 
 
 
 
     sql <- "SELECT DISTINCT
 transaction_datetime,
 (SELECT t0.value FROM UNNEST (tags) AS t0 WHERE t0.key = 'siteName') as siteName,
 (SELECT t1.value FROM UNNEST (tags) AS t1 WHERE t1.key = 'machineId')
 as machineId,
 (SELECT t2.value FROM UNNEST (tags) AS t2 WHERE t2.key = 'siteId') as siteId,
 (SELECT t3.value FROM UNNEST (tags) AS t3 WHERE t3.key = 'contractName')
 as contractName,
 (SELECT items[SAFE_OFFSET(0)].appliance_name) as appliance_name,
 (total_cost_cents/100)	as total_cost_dollars,
 cell_provider				,
 modem_model				,
 card_type	,
 dex_ok		,
 total_qty	,
 vmc_serial_number			,
 transaction_number		,
 gateway_serial_number			,
 terminal_result_string		,
 cell_ip_address			,
 terminal_result_code,
 vmc_manufacturer		,
 terminal_transaction_type		,
 terminal_id,
 
 card_number		,
 vmc_model			,
 receipt				,
 gateway_mac_address				,
 sim_number				,
 gateway_software_version	,
 modem_imei			,
 vend_result			,
 credit_amount_cents		,
 credit_authorized_by		,
 credit_reason,
 
 
 
 FROM `payments-cloud-test.live_vending_data_snapshot_20230202.transaction`
 WHERE
 terminal_transaction_type in ('purchase')
 and
 transaction_datetime BETWEEN 'DATE1' AND 'DATE2'
 
 ORDER BY transaction_datetime DESC"
 
 
 
 
     sql <- sub("DATE1", start_date,sql);
     sql <- sub("DATE2", end_date,sql)
 
     transactions <- strings2factors(dbGetQuery(con, sql))
 

    # --------------------------------------------------------------------------
    removeNotification(id = "Loading")
    
    
    
    
    
    
    
    
    
    
    
    
    # --------------------------------------------------------------------------
    #                                 CLEANING THE DATA
    # --------------------------------------------------------------------------
    
    # 1. Combining siteName and contractName 
    transactions$siteName_contractName <- paste(transactions$siteName, 
                                                transactions$contractName)
    
    
    
    # 2. Update the choices lists
    allowedG20 <- unique(transactions$siteName_contractName)
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG2",
                         choices = allowedG20, selected = allowedG20[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG4", 
                         choices = allowedG20, selected = allowedG20[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG6", 
                         choices = allowedG20, selected = allowedG20[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG7", 
                         choices = allowedG20, selected = allowedG20[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG20", 
                         choices = allowedG20, selected = allowedG20[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG21", 
                         choices = allowedG20, selected = allowedG20[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG22", 
                         choices = allowedG20, selected = allowedG20[1])
    
    
    
    
    
    # --------------------------------------------------------------------------
    # 3. Creating datetime vars for UTC and NZ & an NZ date var used for plots
    # --------------------------------------------------------------------------
    transactions$transaction_datetimeUTC <-as.POSIXct(
      transactions$transaction_datetime)
    transactions$transaction_datetimeNZ <-format(
      transactions$transaction_datetimeUTC, 
      tz = "Pacific/Auckland", usetz = TRUE)
    transactions$transaction_datetime <-as.Date(
      transactions$transaction_datetimeNZ)
    
    # 4. Ordering the dataframe by date
    transactions <- transactions[order(transactions$transaction_datetime, 
                                       decreasing = TRUE),]
    
    
    # 5. Combining Denied/Declined, Cancelled/CANCELLED, Terminal busy/Busy
    transactions$terminal_result_string[transactions$terminal_result_string ==
                                          'DENIED'] <- 'DECLINED'
    transactions$terminal_result_string[transactions$terminal_result_string ==
                                          'Cancelled'] <- 'CANCELLED'
    transactions$terminal_result_string[transactions$terminal_result_string ==
                                          'TERMINAL BUSY'] <- 'Busy'
    
    # 6.Creating a column 'by_week' for weekly time series and bar charts
    transactions$by_week <- ceiling_date(transactions$transaction_datetime,
                                         "week")
    
    
    showNotification(id = "Done",
                     ui = "Data is loaded. Proceed to visualisations", 
                     duration = 5, type = "message")
    
    
    
    
    
    transactions
    
    
    
    
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # --------------------------------------------
  # HEADINGS WITH DATES
  # --------------------------------------------
  
  output$heading1 <- renderPrint({
    cat(
      paste0("Summaries for the period ",
             getdata()$transaction_datetime[nrow(getdata())],
             " to ", getdata()$transaction_datetime[1])
    )
  })
  
  output$heading2 <- renderPrint({
    cat(
      paste0("Visualisations for the period ", 
             getdata()$transaction_datetime[nrow(getdata())],
             " to ", getdata()$transaction_datetime[1])
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # -----------------------------------
  #     TABLES AND SUMMARIES
  # -----------------------------------
  
  output$datatable1 <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(getdata()),
                  options = list(scrollX = TRUE))
  })
  
  
  
  output$SummaryA2 <- renderPrint({
    str(getdata())
  })
  
  
  
  # ---------------------------------------------------
  #     Terminal result string summary
  # ---------------------------------------------------
  
  
  output$SummaryB0plot <- renderHighchart({
    all_approved_and_not_approved <- getdata() %>%
      group_by(terminal_result_string) %>%
      dplyr::summarise(Amount = sum(total_cost_dollars)) %>%
      arrange(desc(Amount))
    as.data.frame(all_approved_and_not_approved)
    plotSummary <- hchart(all_approved_and_not_approved,
                          "column", hcaes(x = terminal_result_string,
                                          y = Amount)) %>%
      hc_title(text = paste0("Terminal result strings bar chart")) %>%
      hc_yAxis(title = list(text = "Amount ($)"),
               labels = list(format = "${value} "))
  })
  
  
  
  output$SummaryB0 <- renderPrint({
    all_approved_and_not_approved <- getdata() %>%
      group_by(terminal_result_string) %>%
      dplyr::summarise(Amount = sum(total_cost_dollars)) %>%
      arrange(desc(Amount))
    as.data.frame(all_approved_and_not_approved)
    
    
    
  })
  
  #--------------------------------------------------
  #   Note for the terminal result string summary
  # ------------------------------------------------
  
  output$stringNotes1 <- renderPrint({
    cat(paste0(
      "
      If APPROVED transactions are less than the sum of the other strings, we
      have problems that need to be investigated.
      
      A look into the 'Terminal result string details' table and 
      'Approved transactions as a percentage' table can give us
      some light into the appliances with issues that need to be checked.
      "
    ))
  })
  
  
  
  
  
  
  
  
  # ---------------------------------------------------------- 
  # All Appliances with less than 50 % approved transactions
  # ---------------------------------------------------------
  output$approved_less50 <- DT::renderDataTable({
    
    
    
    
    total_transactions <- as.data.frame(getdata() %>%
                                          filter(is.na(
                                            getdata()$appliance_name) == 
                                              FALSE) %>%
                                          group_by(appliance_name, 
                                                   gateway_serial_number,
                                                   machineId, 
                                                   siteName_contractName, 
                                                   gateway_software_version) %>%
                                          dplyr::summarise(
                                            total_transactions = n(), 
                                            .groups = "drop")) 
    
    
    approved_transactions <- as.data.frame(getdata() %>%
                                             filter(terminal_result_string ==
                                                      "APPROVED") %>%
                                             group_by(appliance_name,
                                                      gateway_serial_number,
                                                      siteName_contractName) %>%
                                             dplyr::summarise(
                                               approved_transactions = n(),
                                               .groups = "drop")) # %>%
    
    
    combined <-  merge(total_transactions, approved_transactions,
                       all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions, 
                                             is.na(approved_transactions), 0))
    
    # Adding a column with the calculated percentages
    combined$percentage_approved <- round(
      combined$approved_transactions/combined$total_transactions, 2)
    
    # Filtering maximum percentages approved
    combined_less50 <- combined %>%
      filter(percentage_approved <= input$unapproved) 
    
    
    
    
    
    
    
    cancelled_transactions <- as.data.frame(getdata() %>%
                                              
                                              filter(terminal_result_string ==
                                                       "CANCELLED") %>%
                                              group_by(appliance_name, 
                                                       gateway_serial_number, 
                                                       siteName_contractName) %>%
                                              dplyr::summarise(
                                                cancelled_transactions = n(),
                                                .groups = "drop")) # %>%
    
    
    combinedCancelled <-  merge(total_transactions, cancelled_transactions,
                                all.x = TRUE) %>%
      mutate(cancelled_transactions = replace(cancelled_transactions, 
                                              is.na(cancelled_transactions), 0))
    
    # Adding a column with the calculated percentages
    combinedCancelled$percentage_cancelled <- round(
      combinedCancelled$cancelled_transactions/combinedCancelled$total_transactions,
      2)  
    
    
    
    
    
    
    
    declined_transactions <- as.data.frame(getdata() %>%
                                             
                                             
                                             filter(terminal_result_string == 
                                                      "DECLINED") %>%
                                             group_by(appliance_name,
                                                      gateway_serial_number,
                                                      siteName_contractName) %>%
                                             dplyr::summarise(declined = n(),
                                                              .groups = "drop")) 
    
    
    combinedDeclined <-  merge(total_transactions, declined_transactions, 
                               all.x = TRUE)  %>%
      mutate(declined = replace(declined, is.na(declined), 0))
    
    # Adding a column with the calculated percentages
    combinedDeclined$percentage_declined <- round(
      combinedDeclined$declined/combinedDeclined$total_transactions, 2)
    
    
    
    
    
    
    
    approved_cancelled <- merge(combined_less50, combinedCancelled, 
                                all.x = TRUE) %>%
      mutate(cancelled_transactions = replace(cancelled_transactions,
                                              is.na(cancelled_transactions), 0)) %>%
      mutate(approved_transactions = replace(approved_transactions,
                                             is.na(approved_transactions), 0)) %>%
      mutate(percentage_approved = replace(percentage_approved, 
                                           is.na(percentage_approved), 0)) %>%
      mutate(percentage_cancelled = replace(percentage_cancelled, 
                                            is.na(percentage_cancelled), 0)) 
    
    all_combined <- merge(approved_cancelled, combinedDeclined, all.x = TRUE) %>%
      mutate(declined = replace(declined, is.na(declined), 0)) %>%
      mutate(percentage_declined = replace(percentage_declined, 
                                           is.na(percentage_declined), 0))
    
    
    
    
    
    url1 = paste0("https://payments-cloud.web.app/machine/" , 
                  all_combined$machineId)
    all_combined$machine_url <- paste0("<a href='",url1,"'>link</a>")
    
    all_combined$machineId <- NULL       # Removing the machineId 
    
    DT::datatable(data = as.data.frame(all_combined),
                  escape = FALSE, extensions = 'Buttons', 
                  options = list(
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = list('copy', 'csv', 'excel', 'print', list(
                      extend = 'pdf',
                      pageSize = 'A2', # Choosing a page size that fits on  screen
                      orientation = 'landscape'
                    ))
                  )) %>%
      formatStyle(c("percentage_approved", "total_transactions",
                    "approved_transactions"), 
                  color = "red", backgroundColor = "lightblue") %>%
      formatPercentage(c("percentage_approved", "percentage_cancelled", 
                         "percentage_declined"),2)
  }, escape = FALSE)
  
  
  
  
  
  # ----------------------------------------------------------------------------
  # Note below the table for Appliances with less than 50% Approved transactions
  # ----------------------------------------------------------------------------
  
  output$approved_less50notes <- renderPrint({
    cat(paste0(
      "
      Appliances with less than 50 % approved transactions need to be checked
      in the web app and reported to the technical team, so they look to see why
      they have a high percentage of unapproved transactions.
     
      When clicking on the link, choose 'Open in new window' so you can continue
      working on this page.
      "
    ))
  })
  
  
  
  
  
  
  # --------------------------------- 
  # All Terminal result strings
  # --------------------------------
  output$approved_per_appliance <- DT::renderDataTable({
    
    
    SITE8 = input$VariablesG4
    
    
    
    total_transactions <- as.data.frame(getdata() %>%
                                          filter(siteName_contractName == SITE8,
                                                 is.na(appliance_name) == FALSE) %>%
                                          group_by(appliance_name, machineId) %>%
                                          dplyr::summarise(
                                            total_transactions = n(),
                                            .groups = "drop")) # %>%
    
    
    approved_transactions <- as.data.frame(getdata() %>%
                                             filter(terminal_result_string ==
                                                      "APPROVED",
                                                    siteName_contractName == 
                                                      SITE8) %>%
                                             group_by(appliance_name) %>%
                                             dplyr::summarise(
                                               approved_transactions = n())) 
    
    
    
    cancelled_transactions <- as.data.frame(getdata() %>%
                                              
                                              filter(siteName_contractName ==
                                                       SITE8) %>%    
                                              filter(terminal_result_string ==
                                                       "CANCELLED") %>%
                                              group_by(appliance_name) %>%
                                              dplyr::summarise(cancelled = n()))
    
    
    
    declined_transactions <- as.data.frame(getdata() %>%
                                             
                                             filter(siteName_contractName == 
                                                      SITE8) %>%    
                                             filter(terminal_result_string == 
                                                      "DECLINED") %>%
                                             group_by(appliance_name) %>%
                                             dplyr::summarise(declined = n()))   
    
    
    
    failed_transactions <- as.data.frame(getdata() %>%
                                           
                                           filter(siteName_contractName ==
                                                    SITE8) %>%    
                                           filter(terminal_result_string %in% 
                                                    c("FAILED")) %>%
                                           group_by(appliance_name) %>%
                                           dplyr::summarise(failed = n()))
    
    
    
    
    busy_transactions <- as.data.frame(getdata() %>%
                                         
                                         filter(siteName_contractName ==
                                                  SITE8) %>%    
                                         filter(terminal_result_string ==
                                                  "Busy") %>%
                                         group_by(appliance_name) %>%
                                         dplyr::summarise(busy = n())) 
    
    
    
    
    
    unavailable_transactions <- as.data.frame(getdata() %>%
                                                
                                                filter(siteName_contractName ==
                                                         SITE8) %>%    
                                                filter(terminal_result_string ==
                                                         "HOST UNAVAILABLE") %>%
                                                group_by(appliance_name) %>%
                                                dplyr::summarise(unavailable = n())) 
    
    
    
    # --------------------------
    # Approved Transactions
    # --------------------------
    
    combined0 <-  merge(total_transactions, approved_transactions,
                        all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions,
                                             is.na(approved_transactions), 0))
    
    # Adding a column with the calculated percentages
    combined0$approved_percentage <- round(
      combined0$approved_transactions/combined0$total_transactions, 3)
    combined0 <- combined0[order(combined0$approved_percentage, 
                                 decreasing = TRUE),]
    
    
    
    
    # --------------------------
    # Cancelled Transactions
    # --------------------------
    
    combined1 <-  merge(total_transactions, cancelled_transactions,
                        all.x = TRUE)  %>%
      mutate(cancelled = replace(cancelled, is.na(cancelled), 0))
    
    # Adding a column with the calculated percentages
    combined1$cancelled_percentage <- round(
      combined1$cancelled/combined1$total_transactions, 3)
    combined1 <- combined1[order(combined1$cancelled_percentage, 
                                 decreasing = TRUE),]
    
    
    
    
    # --------------------------
    # Declined Transactions
    # --------------------------   
    
    combined2 <-  merge(total_transactions, declined_transactions,
                        all.x = TRUE)  %>%
      mutate(declined = replace(declined, is.na(declined), 0))
    
    # Adding a column with the calculated percentages
    combined2$declined_percentage <- round(
      combined2$declined/combined2$total_transactions, 3)
    combined2 <- combined2[order(combined2$declined_percentage,
                                 decreasing = TRUE),]
    
    
    
    
    
    
    
    # --------------------------
    # Failed Transactions
    # --------------------------
    
    
    combined3 <-  merge(total_transactions, failed_transactions, 
                        all.x = TRUE)  %>%
      mutate(failed = replace(failed, is.na(failed), 0))
    
    # Adding a column with the calculated percentages
    combined3$failed_percentage <- round(
      combined3$failed/combined3$total_transactions, 3)
    combined3 <- combined3[order(combined3$failed_percentage, 
                                 decreasing = TRUE),]
    
    
    
    
    
    
    # ---------------------------------
    # Busy Transactions
    # ---------------------------------
    
    
    combined4 <-  merge(total_transactions, busy_transactions, all.x = TRUE) %>%
      mutate(busy = replace(busy, is.na(busy), 0))
    
    # Adding a column with the calculated percentages
    combined4$busy_percentage <- round(
      combined4$busy/combined4$total_transactions, 3)
    combined4 <- combined4[order(combined4$busy_percentage, 
                                 decreasing = TRUE),]
    
    
    
    # ---------------------------------
    # Host Unavailable Transactions
    # ---------------------------------
    
    
    
    combined5 <-  merge(total_transactions, unavailable_transactions,
                        all.x = TRUE)  %>%
      mutate(unavailable = replace(unavailable, is.na(unavailable), 0))
    
    # Adding a column with the calculated percentages
    combined5$unavailable_percentage <- round(
      combined5$unavailable/combined5$total_transactions, 3)
    combined5 <- combined5[order(combined5$unavailable_percentage, 
                                 decreasing = TRUE),]
    
    
    
    
    
    major1 <- merge(combined0, combined1, all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions, 
                                             is.na(approved_transactions), 0)) %>%
      mutate(cancelled = replace(cancelled, is.na(cancelled), 0))
    
    major2 <- merge(combined2, combined3, all.x = TRUE) %>%
      mutate(declined = replace(declined, is.na(declined), 0)) %>%
      mutate(failed = replace(failed, is.na(failed), 0))
    
    major3 <- merge(combined4, combined5, all.x = TRUE) %>%
      mutate(busy = replace(busy, is.na(busy), 0)) %>%
      mutate(unavailable = replace(unavailable, is.na(unavailable), 0))
    
    
    
    major4 <- merge(major1, major2, all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions,
                                             is.na(approved_transactions), 0)) %>%
      mutate(cancelled = replace(cancelled, is.na(cancelled), 0)) %>%
      mutate(declined = replace(declined, is.na(declined), 0)) %>%
      mutate(failed = replace(failed, is.na(failed), 0))
    
    major5 <- merge(major4, major3, all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions, 
                                             is.na(approved_transactions), 0)) %>%
      mutate(cancelled = replace(cancelled, is.na(cancelled), 0)) %>%
      mutate(declined = replace(declined, is.na(declined), 0)) %>%
      mutate(failed = replace(failed, is.na(failed), 0)) %>%
      mutate(busy = replace(busy, is.na(busy), 0)) %>%
      mutate(unavailable = replace(unavailable, is.na(unavailable), 0)) 
    
    
    
    url1 = paste0("https://payments-cloud.web.app/machine/" , major5$machineId)
    major5$machine_url <- paste0("<a href='",url1,"'>link</a>")
    
    major5$machineId <- NULL       # Removing the machineId column 
    
    DT::datatable(data = as.data.frame(major5), escape = FALSE, 
                  extensions = 'Buttons', 
                  options = list(
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = list('copy', 'csv', 'excel', 'print', list(
                      extend = 'pdf',
                      pageSize = 'A2',           # Choosing a page size 
                      orientation = 'landscape'
                    ))
                  )) %>%
      formatStyle(c("approved_percentage", "cancelled_percentage",
                    "declined_percentage", 
                    "busy_percentage", "failed_percentage", 
                    "unavailable_percentage"), 
                  color = "red", backgroundColor = "lightblue") %>%
      formatPercentage(c("approved_percentage", "cancelled_percentage",
                         "declined_percentage",
                         "busy_percentage", "failed_percentage",
                         "unavailable_percentage"),2)
    
    
    
  }, escape = FALSE)
  
  
  
  
  # --------------------------------------
  # Notes below the All strings table
  # --------------------------------------
  
  output$allStringsNotes <- renderPrint({
    cat(paste0(
      "
      This is a breakdown of all the terminal result strings. 
      NOTE: All percentages should add to 100%.
      A 'good' appliance should have more than 50% of its transactions approved.
      A 'perfect' appliance should have more than 90% of its transactions approved.
      
      ALL sites with no appliance names will give an ERROR.
      ALL sites with NAs (no sitename) will give an ERROR.
      
      When clicking on the link, choose 'Open in new window' so you can continue
      working on this page. 
      "
    ))
  })
  
  
  
  #------------------------------------------------------------------
  # Calculation of the average approved transactions for each site
  # -----------------------------------------------------------------
  
  output$SummaryB6 <- renderPrint({
    SITE4 = input$VariablesG4
    total_transactions <- as.data.frame(getdata() %>%
                                          filter(siteName_contractName ==
                                                   SITE4) %>%
                                          group_by(appliance_name) %>%
                                          dplyr::summarise(total_transactions = n()))
    
    
    approved_transactions <- as.data.frame(getdata() %>%
                                             filter(terminal_result_string ==
                                                      "APPROVED", 
                                                    siteName_contractName == 
                                                      SITE4) %>%
                                             group_by(appliance_name) %>%
                                             dplyr::summarise(approved_transactions = n()))
    
    
    combined <-  merge(total_transactions, approved_transactions, 
                       all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions, 
                                             is.na(approved_transactions), 0))
    
    
    
    DT::datatable(data = as.data.frame(combined), escape = FALSE)
    
    sum_all <- sum(combined$total_transactions)
    sum_approved = sum(combined$approved_transactions)
    overall_average = mean(combined$approved_transactions / combined$total_transactions *100)
    std_dev = sd(combined$approved_transactions / combined$total_transactions *100)
    overall_average
    two_std_dev = std_dev * 2
    cat(paste0( "Average Percentage of Approved Transactions in ",
                input$VariablesG4, " = ", round(overall_average, 2),"%"))
    
  })
  
  
  
  
  
  
  
  
  
  
  # ------------------------------------------------------------- 
  #                           PLOTS
  # -------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ----------------------------------------------------------------------------  
  # Bar chart for looking at the total sales from each site
  # --------------------------------------------------------------------------
  
  
  filedata3 <- reactive({
    
    infile3 <- input$file3
    if (is.null(infile3)) {
      return(NULL)
    }
    myDF3 <- fread(infile3$datapath)
    return(myDF3)
  })
  
  
  
  hchart1 <- reactive({
    
    hchartBarAll <- as.data.frame(getdata() %>%
                                    filter(terminal_result_string == 'APPROVED') %>%
                                    group_by(siteName_contractName, siteId) %>%
                                    summarise(sales = sum(total_cost_dollars),
                                              .groups = 'drop')) 
    
    hchart(hchartBarAll,
           "column", hcaes(x = siteName_contractName, y = sales, key = siteId)) %>%
      hc_title(text = paste0("Total sales for each site")) %>%
      hc_plotOptions(
        column = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = 
                JS( "function (self) {window.open('https://payments-cloud.web.app/site/' + this.options.key); }"))))) %>%
      hc_yAxis(title = list(text = "Sales ($)"),
               labels = list(format = "${value} ")) 
  })
  
  
  
  hchart2 <- reactive({ 
    hchartBarAll_sortBar <- as.data.frame(getdata() %>%
                                            filter(terminal_result_string ==
                                                     'APPROVED') %>%
                                            group_by(siteName_contractName, 
                                                     siteId) %>%
                                            summarise(sales = sum(
                                              total_cost_dollars),
                                              .groups = 'drop')) %>%
      
      
      arrange(desc(sales))  
    
    hchart(hchartBarAll_sortBar,
           "column", hcaes(x = siteName_contractName, y = sales, 
                           key = siteId)) %>%
      hc_title(text = paste0("Total sales for each site")) %>%
      hc_plotOptions(
        column = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = 
                JS( "function (self) {window.open('https://payments-cloud.web.app/site/' + this.options.key); }"))))) %>%
      hc_yAxis(title = list(text = "Sales ($)"),
               labels = list(format = "${value} ")) 
  }) 
  
  
  
  
  
  
  graphInput1 <- reactive({
    switch(input$graph3,
           "By sitename (A-Z)" = hchart1(),
           "By sales descending" = hchart2()
           
    )
  })
  
  
  output$selected_sort <- renderHighchart({ 
    graphInput1()
  })
  
  
  
  
  
  
  
  
  # ---------------------------------------
  #  Notes on the Total Sales plot
  #---------------------------------------
  output$totalSalesNotes <- renderPrint({
    cat(paste0(
      "
      The bar plot shows the total sales from each site.
      
      Click on any bar to be directed to the sitename in Payments Cloud.
      "
    ))
  })
  
  
  
  
  
  # -----------------------------------------------------------
  #     Time Series plot for daily and weekly sales
  # -----------------------------------------------------------
  
  
  filedata <- reactive({
    
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    myDF <- fread(infile$datapath)
    return(myDF)
  })
  
  
  daily_ts <- reactive({
    SITE1 = input$VariablesG20 
    
    mydate2 <- data.frame(
      seq(as.Date(getdata()$transaction_datetime[nrow(getdata())]), 
          as.Date(getdata()$transaction_datetime[1]),
          "day"))  #dataframe of dates from the 1st to the last day of reporting
    
    colnames(mydate2) <- "transaction_datetime" #Renaming the column to match that in getdata()
    
    
    
    #    Calculating the total sales for the site
    site_total <-   getdata() %>%
      filter(terminal_result_string == "APPROVED") %>%
      filter(siteName_contractName == SITE1) %>%
      dplyr::summarise(total_cost_dollars = sum(total_cost_dollars)) %>%
      pull()
    
    
    
    
    hcharTS1 <- getdata() %>%
      filter(terminal_result_string == "APPROVED") %>%
      group_by(siteName_contractName, transaction_datetime) %>%
      dplyr::summarise(total_income = sum(total_cost_dollars),
                       .groups = 'drop') %>%
      filter(siteName_contractName == SITE1)
    
    #merging the two 
    myMerged2a <- merge(mydate2, hcharTS1, all = TRUE) 
    myMerged2a[is.na(myMerged2a)] <- 0                  # assigning zero to NAs
    
    
    hchart(myMerged2a,
           input$ts_line_bar,
           hcaes(x = transaction_datetime, y = total_income)) %>%
      
      hc_title(text = paste0("Total sales from all machines at Location: ", SITE1, 
                             " =  $", format(round(site_total, 2), nsmall = 2))) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Sales ($)"),
               labels = list(format = "${value} ")) %>%
      hc_plotOptions(
        column = list(
          groupPadding = 0,
          pointPadding = 0,
          borderWidth = 0.5,
          borderColor = "#ccc"))
    
    
    
  })
  
  
  weekly_ts <- reactive({
    
    SITE1 = input$VariablesG20
    
    #    Calculating the total sales for the site
    site_total <-   getdata() %>%
      filter(terminal_result_string == "APPROVED") %>%
      filter(siteName_contractName == SITE1) %>%
      dplyr::summarise(total_cost_dollars = sum(total_cost_dollars)) %>%
      pull()
    
    
    
    
    hchart(as.data.frame(getdata()) %>%
             filter(terminal_result_string == "APPROVED") %>%
             group_by(siteName_contractName, by_week) %>%
             dplyr::summarise(total_income = sum(total_cost_dollars), 
                              .groups = 'drop') %>%
             filter(siteName_contractName == SITE1),
           
           
           input$ts_line_bar, color = "red", hcaes(x = by_week,
                                                   y = total_income)) %>%
      hc_title(text = paste0("Weekly sales from all machines at Location: ",
                             SITE1,
                             " =  $", format(round(site_total, 2), 
                                             nsmall = 2))) %>%
      hc_xAxis(title = list(text = "Date of the end of week")) %>%
      hc_yAxis(title = list(text = "Sales ($)"),
               labels = list(format = "${value} ")) %>%
      hc_plotOptions(
        column = list(
          groupPadding = 0,
          pointPadding = 0,
          borderWidth = 0.5,
          borderColor = "#ccc"))
  })
  
  
  
  
  
  # Return the requested graph
  graphInput <- reactive({
    switch(input$graph,
           "Daily" = daily_ts(),
           "Weekly" = weekly_ts()
           
    )
  })
  
  output$selected_graph <- renderHighchart({ 
    graphInput()
  })
  
  
  # -----------------------------------------------------------
  #       A note under the Daily and Weekly time series plots
  # ----------------------------------------------------------
  
  output$timeseriesNotes <- renderPrint({
    SITE1 = input$VariablesG20
    cat(paste0(
      "
     This plot shows the daily and weekly sales for ", SITE1,".
     Note the peaks and troughs for each site to see if they are following a cycle. 
     As a company, we need to think of strategies we can use to increase the
     sales on those days where sales are usually low and to get more sales on
     days where sales are usually high.
     
     Possible strategies would be:
     1. Give discounted prices on the days when sales are usually low.
     2. Create raffles where a minimum amount is required to enter.
     3. Increase the price on those days when sales are high.
     The line graph allows us to easily see the trend for sales, especially 
     over longer periods.
     "
    ))
  }) 
  
  
  
  
  
  
  
  
  
  
  
  # ------------------------------------
  # TIME SERIES DECOMPOSITION
  # -------------------------------------
  
  output$ts_decompose <- renderPlot({
    
    mydate2 <- data.frame(seq(
      as.Date(getdata()$transaction_datetime[nrow(getdata())]),
      as.Date(getdata()$transaction_datetime[1]), "day"))
    
    colnames(mydate2) <- "transaction_datetime" #Renaming the column to match that in getdata()
    
    
    SITE1 = input$VariablesG22
    hcharTS1<- getdata() %>%
      filter(terminal_result_string == "APPROVED") %>%
      group_by(siteName_contractName, transaction_datetime) %>%
      dplyr::summarise(total_income = sum(total_cost_dollars), .groups = 'drop') %>%
      filter(siteName_contractName == SITE1)
    
    #merging the two 
    myMerged2a <- merge(mydate2, hcharTS1, all = TRUE) 
    myMerged2a[is.na(myMerged2a)] <- 0                  # assigning zero to NAs
    
     
    
    
    
    myTS <- ts(myMerged2a$total_income, 
               start = myMerged2a$transaction_datetime[1],
               end = myMerged2a$transaction_datetime[nrow(myMerged2a)/2],
               frequency = 2)
    decompose1 <- stl(myTS, s.window = 3, t.window = input$window)
    plot(decompose1, col = "blue", 
         main = paste0("Time series decomposition for sales at site: ", SITE1))
    
  })
  
  output$ts_decomposeNOTES <- renderPrint({
    SITE1 = input$VariablesG22
    cat(paste0(
      "
     The daily sales for site ", SITE1, " have been decomposed to show
     the actual plot (data),
     the seasonal component,
     the trend component and
     the remainder (residuals) component.
     
     Use the slider above to smoothen the trend.
     "
    ))
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ------------------------------------------------------
  # Bar plot for total sales from each pi/serial number
  # ------------------------------------------------------
  
  
  
  
  #  output$bar <- renderHighchart({
  
  machines <- reactive({ 
    
    SITE2 = input$VariablesG2
    
    
    serialNum_count <- getdata() %>%
      filter(terminal_result_string == "APPROVED") %>%
      filter(siteName_contractName == SITE2) %>%
      group_by(gateway_serial_number)   %>%
      dplyr::summarise() %>%
      nrow()
    
    
    site_totalCost = getdata() %>%
      filter(terminal_result_string == "APPROVED") %>%
      filter(siteName_contractName == SITE2) %>%
      dplyr::summarise(total_cost_dollars = sum(total_cost_dollars)) %>%
      pull()  
    
    
    
    hchartbarPi <- as.data.frame(getdata() %>%
                                   filter(terminal_result_string ==
                                            "APPROVED") %>%
                                   group_by(siteName_contractName, 
                                            gateway_serial_number, machineId) %>%
                                   dplyr::summarise(total_income = 
                                                      sum(total_cost_dollars),
                                                    .groups = 'drop') %>%
                                   filter(siteName_contractName == SITE2))
    
    
    hchart(hchartbarPi,
           "column", color = "red" ,  hcaes(x = gateway_serial_number, 
                                            y = total_income, 
                                            key = machineId)) %>%
      hc_title(text = paste0("Total sales from all machines at Location: ",
                             SITE2, " =  $", 
                             format(round(site_totalCost, 2), 
                                    nsmall = 2))) %>%
      hc_subtitle(text = paste0( "Number of Machines that transacted at ",
                                 SITE2, " = ", serialNum_count)) %>%
      hc_plotOptions(
        column = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = 
                JS( "function (self) {window.open('https://payments-cloud.web.app/machine/' + this.options.key); }"))))) %>%
      hc_xAxis(title = list(text = "Machine serial number")) %>%
      hc_yAxis(title = list(text = "Sales ($)"),
               labels = list(format = "${value} "))
  })
  
  
  
  
  
  
  
  
  
  
  # --------------------------------------------- 
  # Bar chart for sales per appliance per site
  # -------------------------------------------
  
  
  
  appliances <- reactive({ 
    SITE7 = input$VariablesG2
    
    
    
    total_transactions <- as.data.frame(getdata() %>%
                                          filter(siteName_contractName == 
                                                   SITE7) %>%
                                          group_by(gateway_serial_number,
                                                   appliance_name) %>%
                                          dplyr::summarise(total_transactions = n(),
                                                           .groups = 'drop')) 
    
    
    approved_transactions <- as.data.frame(getdata() %>%
                                             filter(terminal_result_string == 
                                                      "APPROVED", 
                                                    siteName_contractName == SITE7) %>%
                                             group_by(appliance_name) %>%
                                             dplyr::summarise(
                                               approved_transactions = n(), 
                                               income = sum(total_cost_dollars)))  
    
    
    
    combined <- merge(total_transactions, approved_transactions, all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions,
                                             is.na(approved_transactions), 0)) %>%
      mutate(income = replace(income, is.na(income), 0))
    
    all_appl = combined %>%
      group_by(appliance_name)   %>%
      dplyr::summarise() %>%
      nrow()
    
    
    
    
    
    
    
    
    total_transactions <- as.data.frame(getdata() %>%
                                          filter(siteName_contractName == 
                                                   SITE7) %>%
                                          group_by(gateway_serial_number, 
                                                   appliance_name, machineId) %>%
                                          dplyr::summarise(
                                            total_transactions = n(),
                                            .groups = 'drop')) # %>%
    
    
    approved_transactions <- as.data.frame(getdata() %>%
                                             filter(terminal_result_string == 
                                                      "APPROVED", 
                                                    siteName_contractName == SITE7) %>%
                                             group_by(appliance_name) %>%
                                             dplyr::summarise(
                                               approved_transactions = n(),
                                               income = sum(total_cost_dollars)))  
    
    
    
    combined <- merge(total_transactions, approved_transactions, all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions,
                                             is.na(approved_transactions), 0)) %>%
      mutate(income = replace(income, is.na(income), 0))
    
    
    
    
    
    
    site_totalCost <-   getdata() %>%
      filter(terminal_result_string == "APPROVED") %>%
      filter(siteName_contractName == SITE7) %>%
      dplyr::summarise(total_cost_dollars = sum(total_cost_dollars)) %>%
      pull()
    
    
    
    all_appl = combined %>%
      group_by(appliance_name) %>%
      dplyr::summarise() %>%
      NROW()
    
    ave_sales = format(round((site_totalCost / all_appl), 2), nsmall = 2) # 2d.p
    
    
    
    
    hchart(combined,
           "column", color = "green", hcaes(x = appliance_name, y = income,
                                            key = machineId)) %>%
      hc_title(text = paste0("Total sales from each appliance at Location: ",
                             SITE7, " =  $",
                             format(round(site_totalCost, 2), nsmall = 2))) %>%
      hc_subtitle(text = paste0("Number of appliances that transacted at siteName ", 
                                SITE7, " = ", all_appl)) %>%
      hc_plotOptions(
        column = list(
          cursor = "pointer",
          point = list(
            events = list(
              click =
                JS( "function (self) {window.open('https://payments-cloud.web.app/machine/' + this.options.key); }"))))) %>%
      hc_yAxis(title = list(text = "Sales ($)"),
               labels = list(format = "${value} "),
               plotLines = list(list(value = ave_sales, color = "red", 
                                     width = 2, dashStyle = "shortdash",
                                     zIndex = 100,
                                     label = list(text = paste0(
                                       "Average sales = $", ave_sales),
                                       style = 
                                         list(color = "purple", 
                                              fontSize = "16px",
                                              fontWeight = "bold"))                      
               )))
  })
  
  
  
  # Return the requested graph
  graphInput4 <- reactive({
    switch(input$graph4,
           "machines" = machines(),
           "appliances" = appliances()
           
    )
  })
  
  output$selected_graph4 <- renderHighchart({ 
    graphInput4()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ----------------------------------------------------------
  # Note under the Bar chart for sales per appliance per site
  # ----------------------------------------------------------
  
  output$applianceNotes <- renderPrint({
    cat(paste0(
      "
      Here we can switch to see the total sales from each pi as well as the total
      sales from each appliance at a site. 
      
      The Pi's with meagre sales need to be investigated if they have been 
      reporting errors or whatever the cause for low sales might be.
    
      The appliances whose sales are way lower than the average will need to be
      investigated.
      From here, go to the bubble plot, to see other appliances that are
      connected together with the ones performing poorly in order to see if it is
      just one appliance or it is all of them in the pi.
      
      Click on the bar to be directed to the Payments Cloud for detailed transactions.
      "
    ))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ----------------------------------------   
  # Bubble plot 
  # --------------------------------------
  
  output$appliance_serialNo <- renderHighchart({
    SITE6 = input$VariablesG6
    
    total_transactions <- as.data.frame(getdata() %>%
                                          filter(siteName_contractName == 
                                                   SITE6) %>%
                                          group_by(gateway_serial_number,
                                                   appliance_name, machineId) %>%
                                          dplyr::summarise(
                                            total_transactions = n(),
                                            .groups = 'drop')) # %>%
    
    
    approved_transactions <- as.data.frame(getdata() %>%
                                             filter(terminal_result_string ==
                                                      "APPROVED",
                                                    siteName_contractName == 
                                                      SITE6) %>%
                                             group_by(
                                               appliance_name, machineId) %>%
                                             dplyr::summarise(
                                               approved_transactions = n(), 
                                               income = sum(
                                                 total_cost_dollars),
                                               .groups = "drop"))  
    
    
    
    combined <- merge(total_transactions, approved_transactions, 
                      all.x = TRUE) %>%
      mutate(approved_transactions = replace(approved_transactions, 
                                             is.na(approved_transactions), 0)) %>%
      mutate(income = replace(income, is.na(income), 0))
    
    
    
    hcbubble <- hchart(combined, "packedbubble",
                       hcaes(name = appliance_name, value = income,
                             group = 
                               gateway_serial_number, key = machineId))
    q95 <- as.numeric(quantile(combined$income, .0))
    
    hcbubble %>% 
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "<b>{point.name}:</b> ${point.value}"
      ) %>% 
      hc_legend(enabled = input$hclegend) %>%
      hc_plotOptions(
        packedbubble = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = JS( "function (self) {window.open('https://payments-cloud.web.app/machine/' + this.options.key); }"))),
          useSimulation = TRUE,
          maxSize = "100%",
          zMin = 0,
          layoutAlgorithm = list(
            gravitationalConstant =  0.05,
            splitSeries =  TRUE, # TRUE to group points
            seriesInteraction = FALSE,
            dragBetweenSeries = FALSE,
            parentNodeLimit = TRUE
          ),
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}",
           # filter = list(
           #   property = "y",
           #   operator = ">",
           #   value = q95
          #  ),
            style = list(
              color = "black",
              textOutline = "none",
              fontWeight = "bold"
            )
          )
        )
      )
  }) 
  
  # ----------------------------------------------------------------------------
  #     Note below the Bubble plot
  # ----------------------------------------------------------------------------
  output$bubbleSummary <- renderPrint({
    cat(paste0(
      "
    This bubble plot shows the appliances that are connected together on the same pi.
    The big circle is the pi, and the small circles inside are the appliances.
    The size of the small circles is proportional to the sales received from 
    each appliance, whilst the size of the big circle is proportional to the 
    total sales from all appliances in the pi.
   
    
    Click on the appliances with abnormally small circles to be directed to the
    Payments Cloud so you can see possible reasons for the low sales.
    "
    ))
  })    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
})
