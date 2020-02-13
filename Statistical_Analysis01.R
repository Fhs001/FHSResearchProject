###### Statistical analysis (regression) - Laura 

### Read files into objects ----

    Final_Merge_02 <- read.csv("~/Desktop/FHS_Data01/Merge/Final_Merge_02.csv", row.names=1, stringsAsFactors = FALSE, na.strings = c("","NULL","NA", " "))

    
    table(Final_Merge_02$Cohort)
    
### Install packages -----
    
    # install.packages("Amelia")
    library(Amelia)
    
    #install.packages("dplyr")
    library(dplyr)
    
### Check for missing values -----
    
    sapply(Final_Merge_02,function(x) sum(is.na(x)))
    sapply(Final_Merge_02, function(x) length(unique(x)))
    
    ### Visualise missing values via Missingness Map
    
    # missmap(Final_Merge_02, main = "Missingness Map of Merged Data", 
        #   col = c("red","blue"), rank.order = FALSE)
    
      ### Remove VICTOR AND QUASAR2
    
        #  table(Final_Merge_02$Cohort)
        #  Scort_Cohorts <- Final_Merge_02[Final_Merge_02$Cohort == "COPERNICUS"
             #                                | Final_Merge_02$Cohort == "FOCUS"
              #                               | Final_Merge_02$Cohort == "FOCUS_4C"
               #                              | Final_Merge_02$Cohort == "FOCUS_IRINOTECAN"
                 #                            | Final_Merge_02$Cohort == "GRAMPIAN"
                #                             | Final_Merge_02$Cohort == "FOXTROT"
                   #                          | Final_Merge_02$Cohort == "NEW EPOC"
                     #                        | Final_Merge_02$Cohort == "POLYPS"
                  #  #                         | Final_Merge_02$Cohort == "PT1_BELFAST"
               #                              | Final_Merge_02$Cohort == "QC_SET"
                   #                          | Final_Merge_02$Cohort == "SPINAL"
                       #                      | Final_Merge_02$Cohort == "TREC",]
           
          # table(Scort_Cohorts$Cohort)
           
          #  missmap(Scort_Cohorts, main = "Missingness Map of Merged Data (Scort Only)", 
              #     col = c("red","blue"), rank.order = FALSE)
      
### 
###### ----- EDITING DATAFRAME FOR ANALYSIS ----- #######
### Remove samples with no metastasis data to lung, liver, nodes, or peritoneum ----
    
    table(Final_Merge_02$Cohort)
    
    ### Summarise metastases
           
        table(Final_Merge_02$Liver_Met)
        table(Final_Merge_02$Lung_Met)
        table(Final_Merge_02$Node_Met)
        table(Final_Merge_02$Peritoneum_Met)
    
    ### Create additional column and add "Yes" if met in selected location     
    
        Final_Merge_02$Met_For_Analysis[Final_Merge_02$Liver_Met == "Yes"
                                        |Final_Merge_02$Liver_Met == "No"] <- "Yes"
        table(Final_Merge_02$Met_For_Analysis)
        
        Final_Merge_02$Met_For_Analysis[Final_Merge_02$Lung_Met == "Yes"
                                        |Final_Merge_02$Liver_Met == "No"] <- "Yes"
        table(Final_Merge_02$Met_For_Analysis)
        
        Final_Merge_02$Met_For_Analysis[Final_Merge_02$Node_Met == "Yes"
                                        |Final_Merge_02$Liver_Met == "No"] <- "Yes"
        table(Final_Merge_02$Met_For_Analysis)
        
        Final_Merge_02$Met_For_Analysis[Final_Merge_02$Peritoneum_Met == "Yes"
                                        |Final_Merge_02$Liver_Met == "No"] <- "Yes"
        table(Final_Merge_02$Met_For_Analysis)
    
    ### Only keep rows with metastasis to lung, liver, nodes, or peritoneum
        
        Final_Merge_02$Met_For_Analysis[is.na(Final_Merge_02$Met_For_Analysis)] <- "No"
        
        table(Final_Merge_02$Met_For_Analysis)
        
        Final_Merge_Mets <- Final_Merge_02[Final_Merge_02$Met_For_Analysis == "Yes",]
        
        table(Final_Merge_Mets$Met_For_Analysis)
        Final_Merge_Mets$Met_For_Analysis <- NULL
        
    ### Re-check Mets values
        
        table(Final_Merge_Mets$Liver_Met)
        table(Final_Merge_Mets$Lung_Met)
        table(Final_Merge_Mets$Node_Met)
        table(Final_Merge_Mets$Peritoneum_Met)

        
        
### Binarise variables ----
        
        #Gender
        
        table(Final_Merge_Mets$Gender)
        Final_Merge_Mets$Gender <- gsub("Female", "F", Final_Merge_Mets$Gender)
        Final_Merge_Mets$Gender <- gsub("Male", "M", Final_Merge_Mets$Gender)
        Final_Merge_Mets$Gender <- gsub("M", "1", Final_Merge_Mets$Gender)
        Final_Merge_Mets$Gender <- gsub("F", "0", Final_Merge_Mets$Gender)
        table(Final_Merge_Mets$Gender)
        
        
        #Metastases
        
        Final_Merge_Mets$Liver_Met <- gsub("Yes", "1", Final_Merge_Mets$Liver_Met)
        Final_Merge_Mets$Liver_Met <- gsub("No", "0", Final_Merge_Mets$Liver_Met)
        table(Final_Merge_Mets$Liver_Met)
        
        Final_Merge_Mets$Node_Met <- gsub("Yes", "1", Final_Merge_Mets$Node_Met)
        Final_Merge_Mets$Node_Met <- gsub("No", "0", Final_Merge_Mets$Node_Met)
        table(Final_Merge_Mets$Node_Met)
        
        Final_Merge_Mets$Lung_Met <- gsub("Yes", "1", Final_Merge_Mets$Lung_Met)
        Final_Merge_Mets$Lung_Met <- gsub("No", "0", Final_Merge_Mets$Lung_Met)
        table(Final_Merge_Mets$Lung_Met)
        
        Final_Merge_Mets$Brain_Met <- gsub("Yes", "1", Final_Merge_Mets$Brain_Met)
        Final_Merge_Mets$Brain_Met <- gsub("No", "0", Final_Merge_Mets$Brain_Met)
        table(Final_Merge_Mets$Brain_Met)
        
        Final_Merge_Mets$Peritoneum_Met <- gsub("Yes", "1", Final_Merge_Mets$Peritoneum_Met)
        Final_Merge_Mets$Peritoneum_Met <- gsub("No", "0", Final_Merge_Mets$Peritoneum_Met)
        table(Final_Merge_Mets$Peritoneum_Met)
        
        Final_Merge_Mets$Other_Met <- gsub("Yes", "1", Final_Merge_Mets$Other_Met)
        Final_Merge_Mets$Other_Met <- gsub("No", "0", Final_Merge_Mets$Other_Met)
        
        # OS_Status 
        
        table(Final_Merge_Mets$OS_Status)
        
        
        
        ### Replace all  with 0/1 (may be done by creating new columns)
        
        #Wt/Mut
        Final_Merge_Mets[] <- lapply(Final_Merge_Mets, gsub, pattern = "Wt", replacement = "0", fixed = T)
        Final_Merge_Mets[] <- lapply(Final_Merge_Mets, gsub, pattern = "Mut", replacement = "1", fixed = T)
        
        #MSI
        table(Final_Merge_Mets$MSI)
        Final_Merge_Mets$MSI_Binary[Final_Merge_Mets$MSI=="MSS"] <- "0"
        Final_Merge_Mets$MSI_Binary[Final_Merge_Mets$MSI=="MSI"] <- "1"
        table(Final_Merge_Mets$MSI_Binary)
        
        
        #Sidedness
        Final_Merge_Mets$Sidedness_Binary[Final_Merge_Mets$Sidedness=="Right"] <- "0"
        Final_Merge_Mets$Sidedness_Binary[Final_Merge_Mets$Sidedness=="Left"] <- "1"
        
        #APC - wt/mut so now 0/1
        #KRAS - wt/mut so now  0/1
        #BRAF - wt/mut so now 0/1
        
        #BRAF_Signature
        Final_Merge_Mets$BRAF_Signature_Binary[Final_Merge_Mets$BRAF_Signature=="pred-BRAFwt"] <- "0"
        Final_Merge_Mets$BRAF_Signature_Binary[Final_Merge_Mets$BRAF_Signature=="BRAFm-like"] <- "1"
  
        
        
        ###  TNM staging 
        
              # N_Stage (N0 -> 0 ; N1,2,3 -> 1)
              table(Final_Merge_Mets$N_Stage)
              Final_Merge_Mets$N_Stage_Binary[Final_Merge_Mets$N_Stage == "0"] <- "0"
              Final_Merge_Mets$N_Stage_Binary[Final_Merge_Mets$N_Stage == "1 = Absent (0)"] <- "0"
              Final_Merge_Mets$N_Stage_Binary[Final_Merge_Mets$N_Stage == "1"] <- "1"
              Final_Merge_Mets$N_Stage_Binary[Final_Merge_Mets$N_Stage == "2"] <- "1"
              Final_Merge_Mets$N_Stage_Binary[Final_Merge_Mets$N_Stage == "2 = Present (1 or 2)"] <- "1"
              Final_Merge_Mets$N_Stage_Binary[Final_Merge_Mets$N_Stage == "3"] <- "1"
              table(Final_Merge_Mets$N_Stage_Binary)
              
              
              # T_Stage (T0,1,2 -> 0 ; T3,T4 -> 1)
              table(Final_Merge_Mets$T_Stage)
              Final_Merge_Mets$T_Stage_Binary[Final_Merge_Mets$T_Stage == "T0"] <- "0"
              Final_Merge_Mets$T_Stage_Binary[Final_Merge_Mets$T_Stage == "0"] <- "0"
              Final_Merge_Mets$T_Stage_Binary[Final_Merge_Mets$T_Stage == "1"] <- "0"
              Final_Merge_Mets$T_Stage_Binary[Final_Merge_Mets$T_Stage == "2"] <- "0"
              Final_Merge_Mets$T_Stage_Binary[Final_Merge_Mets$T_Stage == "1 = Absent (1 or 2)"] <- "0"
              Final_Merge_Mets$T_Stage_Binary[Final_Merge_Mets$T_Stage == "3"] <- "1"
              Final_Merge_Mets$T_Stage_Binary[Final_Merge_Mets$T_Stage == "4"] <- "1"
              Final_Merge_Mets$T_Stage_Binary[Final_Merge_Mets$T_Stage == "2 = Present (3 or 4)"] <- "1"
              table(Final_Merge_Mets$T_Stage_Binary)
              
              # M_Stage
              table(Final_Merge_Mets$M_Stage)
              Final_Merge_Mets$M_Stage_Binary[Final_Merge_Mets$M_Stage == "0"] <- "0"
              Final_Merge_Mets$M_Stage_Binary[Final_Merge_Mets$M_Stage == "x"] <- "0"
              Final_Merge_Mets$M_Stage_Binary[Final_Merge_Mets$M_Stage == "1"] <- "1"
              table(Final_Merge_Mets$M_Stage_Binary)
              
        # CIN_DNN
              
              table(Final_Merge_Mets$CIN_DNN)
              
              Final_Merge_Mets$CIN_DNN_Binary[Final_Merge_Mets$CIN_DNN == "CIN-"] <- "0"
              Final_Merge_Mets$CIN_DNN_Binary[Final_Merge_Mets$CIN_DNN == "CIN+"] <- "1"
              
              table(Final_Merge_Mets$CIN_DNN_Binary)
              

### Binarise cateogrical variables ----
        
        # Cohorts
              
              table(Final_Merge_Mets$Cohort)
              
              # Focus (Focus, Focus1_Irinotecan, and Focus_4C)
              Final_Merge_Mets$Focus[Final_Merge_Mets$Cohort == "FOCUS" | Final_Merge_Mets$Cohort == "FOCUS_IRINOTECAN" | Final_Merge_Mets$Cohort == "FOCUS_4C"] <- "1"
              Final_Merge_Mets$Focus[Final_Merge_Mets$Cohort != "FOCUS" & Final_Merge_Mets$Cohort != "FOCUS_IRINOTECAN"  & Final_Merge_Mets$Cohort != "FOCUS_4C"] <- "0"
              
              table(Final_Merge_Mets$Focus)
              
              
              # New Epoc
              Final_Merge_Mets$NewEpoc[Final_Merge_Mets$Cohort == "NEW EPOC"] <- "1"
              Final_Merge_Mets$NewEpoc[Final_Merge_Mets$Cohort != "NEW EPOC"] <- "0"
              
              table(Final_Merge_Mets$NewEpoc)
              
              # Quasar 2
              Final_Merge_Mets$Quasar2[Final_Merge_Mets$Cohort == "QUASAR2"] <- "1"
              Final_Merge_Mets$Quasar2[Final_Merge_Mets$Cohort != "QUASAR2"] <- "0"
              
              table(Final_Merge_Mets$Quasar2)
              
              # Spinal
              Final_Merge_Mets$Spinal[Final_Merge_Mets$Cohort == "SPINAL"] <- "1"
              Final_Merge_Mets$Spinal[Final_Merge_Mets$Cohort != "SPINAL"] <- "0"
              
              table(Final_Merge_Mets$Spinal)
              
              # Victor
              Final_Merge_Mets$Victor[Final_Merge_Mets$Cohort == "VICTOR"] <- "1"
              Final_Merge_Mets$Victor[Final_Merge_Mets$Cohort != "VICTOR"] <- "0"
              
              table(Final_Merge_Mets$Victor)
        
              
        
        ## CRIS (A,B,C,D,E)
        
              table(Final_Merge_Mets$CRIS)
              
                    # A
                    Final_Merge_Mets$CRIS_A[Final_Merge_Mets$CRIS == "CRIS-A"] <- "1"
                    Final_Merge_Mets$CRIS_A[Final_Merge_Mets$CRIS == "CRIS-B"
                                             |Final_Merge_Mets$CRIS == "CRIS-C"
                                             |Final_Merge_Mets$CRIS == "CRIS-D"
                                            |Final_Merge_Mets$CRIS == "Unclassified"
                                             |Final_Merge_Mets$CRIS == "CRIS-E" ] <- "0"
                    
                    # B
                    Final_Merge_Mets$CRIS_B[Final_Merge_Mets$CRIS == "CRIS-B"] <- "1"
                    Final_Merge_Mets$CRIS_B[Final_Merge_Mets$CRIS == "CRIS-A"
                                             |Final_Merge_Mets$CRIS == "CRIS-C"
                                             |Final_Merge_Mets$CRIS == "CRIS-D"
                                            |Final_Merge_Mets$CRIS == "Unclassified"
                                             |Final_Merge_Mets$CRIS == "CRIS-E"] <- "0"
                    
                    # C
                    Final_Merge_Mets$CRIS_C[Final_Merge_Mets$CRIS == "CRIS-C"] <- "1"
                    Final_Merge_Mets$CRIS_C[Final_Merge_Mets$CRIS == "CRIS-B"
                                             |Final_Merge_Mets$CRIS == "CRIS-A"
                                             |Final_Merge_Mets$CRIS == "CRIS-D"
                                            |Final_Merge_Mets$CRIS == "Unclassified"
                                             |Final_Merge_Mets$CRIS == "CRIS-E"] <- "0"
                    
                    # D
                    Final_Merge_Mets$CRIS_D[Final_Merge_Mets$CRIS == "CRIS-D"] <- "1"
                    Final_Merge_Mets$CRIS_D[Final_Merge_Mets$CRIS == "CRIS-B"
                                             |Final_Merge_Mets$CRIS == "CRIS-C"
                                             |Final_Merge_Mets$CRIS == "CRIS-A"
                                            |Final_Merge_Mets$CRIS == "Unclassified"
                                             |Final_Merge_Mets$CRIS == "CRIS-E"] <- "0"
                    
                    
                    # E
                    Final_Merge_Mets$CRIS_E[Final_Merge_Mets$CRIS == "CRIS-E"] <- "1"
                    Final_Merge_Mets$CRIS_E[Final_Merge_Mets$CRIS == "CRIS-B"
                                             |Final_Merge_Mets$CRIS == "CRIS-C"
                                             |Final_Merge_Mets$CRIS == "CRIS-D"
                                            |Final_Merge_Mets$CRIS == "Unclassified"
                                             |Final_Merge_Mets$CRIS == "CRIS-A"] <- "0"
              
       
        table(Final_Merge_Mets$CRIS_A)
        table(Final_Merge_Mets$CRIS_B)
        table(Final_Merge_Mets$CRIS_C)
        table(Final_Merge_Mets$CRIS_D)
        table(Final_Merge_Mets$CRIS_E)
        
        #CMS (1,2,3,4)
        
        table(Final_Merge_Mets$CMS)
        
              # 1
              Final_Merge_Mets$CMS1[Final_Merge_Mets$CMS == "CMS1"] <- "1"
              Final_Merge_Mets$CMS1[Final_Merge_Mets$CMS == "CMS2"
                                    |Final_Merge_Mets$CMS == "Unclassified"
                                     |Final_Merge_Mets$CMS == "CMS3"
                                     |Final_Merge_Mets$CMS == "CMS4"] <- "0"
              
              # 2
              Final_Merge_Mets$CMS2[Final_Merge_Mets$CMS == "CMS2"] <- "1"
              Final_Merge_Mets$CMS2[Final_Merge_Mets$CMS == "CMS1"
                                    |Final_Merge_Mets$CMS == "Unclassified"
                                     |Final_Merge_Mets$CMS == "CMS3"
                                     |Final_Merge_Mets$CMS == "CMS4"] <- "0"
              
              # 3
              Final_Merge_Mets$CMS3[Final_Merge_Mets$CMS == "CMS3"] <- "1"
              Final_Merge_Mets$CMS3[Final_Merge_Mets$CMS == "CMS2"
                                    |Final_Merge_Mets$CMS == "Unclassified"
                                     |Final_Merge_Mets$CMS == "CMS1"
                                     |Final_Merge_Mets$CMS == "CMS4"] <- "0"
              
              # 4
              Final_Merge_Mets$CMS4[Final_Merge_Mets$CMS == "CMS4"] <- "1"
              Final_Merge_Mets$CMS4[Final_Merge_Mets$CMS == "CMS2"
                                    |Final_Merge_Mets$CMS == "Unclassified"
                                     |Final_Merge_Mets$CMS == "CMS3"
                                     |Final_Merge_Mets$CMS == "CMS1"] <- "0"
          
        table(Final_Merge_Mets$CMS1)
        table(Final_Merge_Mets$CMS2)
        table(Final_Merge_Mets$CMS3)
        table(Final_Merge_Mets$CMS4)
        
        ## Copy number data
        
            #CN_SMAD4_isLoss (Loss (1) = -2, -1; No_Loss (0) = 0, 1, 2)
            
        
            Final_Merge_Mets$CN_SMAD4_isLoss[Final_Merge_Mets$CN_SMAD4 == "-2" |
                                                Final_Merge_Mets$CN_SMAD4 == "-1"] <- "1"
            
            Final_Merge_Mets$CN_SMAD4_isLoss[Final_Merge_Mets$CN_SMAD4 == "2" |
                                                Final_Merge_Mets$CN_SMAD4 == "1" |
                                                Final_Merge_Mets$CN_SMAD4 == "0"] <- "0"
            
            
            #CN_TP53_isLoss (Loss (1) = -2, -1; No_Loss (0) = 0, 1, 2)
            
            Final_Merge_Mets$CN_TP53_isLoss[Final_Merge_Mets$CN_TP53 == "-2" |
                                               Final_Merge_Mets$CN_TP53 == "-1"] <- "1"
            
            Final_Merge_Mets$CN_TP53_isLoss[Final_Merge_Mets$CN_TP53 == "2" |
                                               Final_Merge_Mets$CN_TP53 == "1" |
                                               Final_Merge_Mets$CN_TP53 == "0"] <- "0"
        
            
            
            ## CN_08_p_isLoss (Loss (1) = "Loss"; No loss (0) = "Neutral", "Gain")
            
            table(Final_Merge_Mets$CN_08_p)
            
            Final_Merge_Mets$CN_08_p_isLoss[Final_Merge_Mets$CN_08_p == "Loss"] <- "1"
            
            Final_Merge_Mets$CN_08_p_isLoss[Final_Merge_Mets$CN_08_p == "Neutral" |
                                              Final_Merge_Mets$CN_08_p == "Gain"] <- "0"
            
            table(Final_Merge_Mets$CN_08_p_isLoss)
            
            ## CN_08_q_isGain (Gain (1) = "Gain"; No loss (0) = "Neutral", "Loss")
            
            table(Final_Merge_Mets$CN_08_q)
            
            
            Final_Merge_Mets$CN_08_q_isGain[Final_Merge_Mets$CN_08_q == "Gain"] <- "1"
            
            Final_Merge_Mets$CN_08_q_isGain[Final_Merge_Mets$CN_08_q == "Neutral" |
                                              Final_Merge_Mets$CN_08_q == "Loss"] <- "0"
            
            table(Final_Merge_Mets$CN_08_q_isGain)
            
            
            ## CN_APC
            
            table(Final_Merge_Mets$CN_APC)
            
            Final_Merge_Mets$CN_APC_isLoss[Final_Merge_Mets$CN_APC == "-2"
                                           | Final_Merge_Mets$CN_APC == "-1"] <- "1"
            
            Final_Merge_Mets$CN_APC_isLoss[Final_Merge_Mets$CN_APC == "0" |
                                              Final_Merge_Mets$CN_APC == "1"] <- "0"
            
            table(Final_Merge_Mets$CN_APC_isLoss)
            
            Final_Merge_Mets$CN_APC_isGain[Final_Merge_Mets$CN_APC == "1"] <- "1"
            
            Final_Merge_Mets$CN_APC_isGain[Final_Merge_Mets$CN_APC == "-1" |
                                             Final_Merge_Mets$CN_APC == "-2"
                                           | Final_Merge_Mets$CN_APC == "0"] <- "0"
            
            table(Final_Merge_Mets$CN_APC_isGain)
            
            
            ## CN_KRAS
            
            table(Final_Merge_Mets$CN_KRAS)
            
            Final_Merge_Mets$CN_KRAS_isGain[Final_Merge_Mets$CN_KRAS == "1"
                                            | Final_Merge_Mets$CN_KRAS == "2"] <- "1"
            
            Final_Merge_Mets$CN_KRAS_isGain[Final_Merge_Mets$CN_KRAS== "-1" |
                                             Final_Merge_Mets$CN_KRAS == "-2"
                                           | Final_Merge_Mets$CN_KRAS == "0"] <- "0"
            
            table(Final_Merge_Mets$CN_KRAS_isGain)
          
            
### Write object into csv file ----
    
            
      write.csv(Final_Merge_Mets, "/Users/laurahudson/Documents/Merge_Binary.csv")
  
            
### Look for binary variables with frequency below 5% ----
            
            table(Final_Merge_Mets$APC)
            
            # Check whether variables are binary, creating dataframe
            
            Is_binary <- apply(Final_Merge_Mets, 2, function(x) {all(na.omit(x) %in% 0:1)})
            
            Is_binary_DF <- as.data.frame(cbind(names(Final_Merge_Mets), Is_binary))
            
            # Split dataframe into one for binary and one for non-binary variables
            
            Binary_variables <- Is_binary_DF[Is_binary_DF$Is_binary == TRUE,]
            Non_Binary_variables <- Is_binary_DF[Is_binary_DF$Is_binary == FALSE,]
            
            # List whether binary (and non-binary) variables as characters, and then tranform into DFs
            
            Binary_variables <- as.character(Binary_variables[,-2])
            Non_Binary_variables <- as.character(Non_Binary_variables[,-2])
            
            Binary_variables <- dplyr::select(Final_Merge_Mets, one_of(Binary_variables))
            Non_Binary_variables <- dplyr::select(Final_Merge_Mets, one_of(Non_Binary_variables))
            
            
            # Remove variables that aren't mutation data (i.e. clinical data)
            
            rownames(Binary_variables)
            Binary_variables <- dplyr::select(Binary_variables, -one_of("Liver_Met", "Lung_Met", "Node_Met", "Brain_Met", "Peritoneum_Met", "pEMVI", 
                                                                        "Response_Curated_None_Moderate_Marked_Complete", 
                                                                        "Response_Regression_minimal_partial_good_partial_complete", 
                                                                        "Response_Regression_no_minimal_moderate_good_complete",
                                                                        "Response_Regression_no_mild_partial_moderate_good_complete", 
                                                                        "Response_Regression_no_mild_moderate_marked_complete", 
                                                                        "CIBERSORT_B_cells_memory", "Met_Surgery", 
                                                                         "Gender", "OS_Status", "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                        "Sidedness_Binary", "Focus", "Spinal", "Victor", "Quasar2",
                                                                        "BRAF", "PIK3CA_ex9", "KRAS_c1213", "CN_APC_isLoss"))
            
            # Calculate frequencies of variables
            
            Binary_variables %>% mutate_if(is.character, as.numeric) -> Binary_variables2
            
            Total_Observations <- apply(Binary_variables2, 2, function(x) { length(which(!is.na(x)))})
            
            Model_Binary <- apply(Binary_variables2, 2, sum, na.rm = TRUE) #Total number of mutations
            
            FreqMutOutofNonMissingData <- Model_Binary/Total_Observations*100 
            
            Model_Binary <- data.frame(names(Binary_variables), Model_Binary, Total_Observations, FreqMutOutofNonMissingData)
            #Total_Observations <- data.frame(names(Binary_variables), Total_Observations)
            
            
            Model_Binary$FreqMutOutofAllData <- Model_Binary$Model_Binary/nrow(Binary_variables2)*100
            names(Model_Binary)[1:2] <- c("Binary_variable", "Mutation_Count")
            Model_Binary$Binary_variable <- as.character(Model_Binary$Binary_variable)
            
            
            # No mutation (ie. Wt)
            Model_Binary$NoMutationCount <- colSums(Binary_variables2 == 0, na.rm = TRUE)
            Model_Binary$NoMutation_Frequency <- (Model_Binary$NoMutationCount/nrow(Binary_variables2))*100
            
            # No data (ie. NA)
            Model_Binary$Missing <- colSums(apply(Binary_variables2, 2, is.na))
            Model_Binary$Missing_Frequency <- (Model_Binary$Missing/nrow(Final_Merge_Mets))*100
            
          # Arrange by frequency of mutation, and keep only those with mut frequency >5%
            
            Model_Binary <- dplyr::arrange(Model_Binary, FreqMutOutofNonMissingData)
            Model_Binary_Freq_Over_5 <- subset(Model_Binary, FreqMutOutofNonMissingData>5)
            
            
          # Visualise
            
            par(las=2)
            par(mar=c(5,8,4,2))
            barplot(Model_Binary_Freq_Over_5$FreqMutOutofNonMissingData, names.arg = Model_Binary_Freq_Over_5$Binary_variable, horiz = TRUE,
                    xlim = c(0,100), xlab = "Mutation Frequency (%)",
                    main = "Frequency of genetic mutations or copy number changes") 
            
            
           # Write code for variables of frequency >5%
            
              List_Var_f_over5 <- Model_Binary_Freq_Over_5$Binary_variable
              print(List_Var_f_over5 )
            
            
        
### *** COMPARING 3 ANALYSES (All v Scort Only v Non-Scort only) *** -----
            
      ### Select cohorts for analysis (only use 1 line at once)
      ### and then rename (to work with rest of code)
            
          ## For all cohorts use neither of lines below (keep as Final_Merge_Mets)
            
            table(Final_Merge_Mets$Cohort) 
            
          ## For SCORT Cohorts only: 
              
             #  Final_Merge_Mets_Scort <- Final_Merge_Mets[Final_Merge_Mets$Cohort != "VICTOR" & Final_Merge_Mets$Cohort != "QUASAR2",]
             #  Final_Merge_Mets <- Final_Merge_Mets_Scort
            
          ## For Victor and Quasar 2: 
            
             #  Final_Merge_Mets_QnV <- Final_Merge_Mets[Final_Merge_Mets$Cohort == "VICTOR" | Final_Merge_Mets$Cohort == "QUASAR2",]
             #  Final_Merge_Mets <- Final_Merge_Mets_QnV

### Keep only rows with a single site of metastasis -----
            
            ### Remove rows with data that is NA for metastasis locations (liver, lung, node, peritoneum)
            
            
            ## For Liver mets
            Final_Merge_Liver_Met <- Final_Merge_Mets[!is.na(Final_Merge_Mets$Liver_Met),]
            
            ## For Lung mets
            Final_Merge_Lung_Met <- Final_Merge_Mets[!is.na(Final_Merge_Mets$Lung_Met),]
            
            ## For Node mets
            Final_Merge_Node_Met <- Final_Merge_Mets[!is.na(Final_Merge_Mets$Node_Met),]
            
            ## For Peritoneum mets
            Final_Merge_Peritoneum_Met <- Final_Merge_Mets[!is.na(Final_Merge_Mets$Peritoneum_Met),]

### Further data visualisation
            
       ## Select data for metastasis present
            
            Final_Merge_Liver_Met_Yes <-  Final_Merge_Liver_Met[Final_Merge_Liver_Met$Liver_Met == "1",]
            Final_Merge_Lung_Met_Yes <-  Final_Merge_Lung_Met[Final_Merge_Lung_Met$Lung_Met == "1",]
            Final_Merge_Node_Met_Yes <-  Final_Merge_Node_Met[Final_Merge_Node_Met$Node_Met == "1",]
            Final_Merge_Peritoneum_Met_Yes <- Final_Merge_Peritoneum_Met[ Final_Merge_Peritoneum_Met$Peritoneum_Met == "1",]
            
            
##### -------- UNIVARIATE ANALYSIS --------- ########
### Select variables for univariate analysis ----
            
            library(dplyr)
            
            
            
  ## SELECT VARIABELS FOR COMPARING 3 ANALYSES (All cohorts, SCORT, non-SCORT)
            
          # Final_Merge_Selected_Liver <- dplyr::select(Final_Merge_Liver_Met, "Scort_ID", "Liver_Met", "T_Stage_Binary", "N_Stage_Binary", 
           #                                              "M_Stage_Binary",
            #                                            "Sidedness_Binary", "Gender", "Age", "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor")
            
        #    Final_Merge_Selected_Lung <- dplyr::select(Final_Merge_Lung_Met, "Scort_ID", "Lung_Met", "T_Stage_Binary", "N_Stage_Binary", 
         #                                              "M_Stage_Binary",
          #                                             "Sidedness_Binary", "Gender", "Age", "Focus", "NewEpoc", "Quasar2", "Spinal", "Victor")
            
           # Final_Merge_Selected_Nodal <- dplyr::select(Final_Merge_Node_Met, "Scort_ID", "Node_Met", "T_Stage_Binary", "N_Stage_Binary",
                                                        #"M_Stage_Binary",
               #                                         "Sidedness_Binary", "Gender", "Age", "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor")
            
            # Final_Merge_Selected_Peritoneal <- dplyr::select(Final_Merge_Peritoneum_Met, "Scort_ID", "Peritoneum_Met", "T_Stage_Binary", "N_Stage_Binary",  
                                                             #"M_Stage_Binary",
                #                                             "Sidedness_Binary", "Gender", "Age", "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor")
    
  ## SELECT VARIABLES FOR ACTUAL REGESSION
            
            Final_Merge_Selected_Liver <- dplyr::select(Final_Merge_Liver_Met, "Scort_ID", "Liver_Met", 
                                                        "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                        "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor", 
                                                       "Sidedness_Binary", "Age",
                                                        List_Var_f_over5, "Hypoxia_Buffa", "TGFb_Fibroblast", 
                                                        "ESTIMATE_TumorPurity", "MCP_CD8_T_cells", "MCP_Cytotoxic_lymphocytes", 
                                                        "Methyl_Phen_Index",  "Total_Mutations")
            
           
            Final_Merge_Selected_Lung <- dplyr::select(Final_Merge_Lung_Met, "Scort_ID", "Lung_Met", 
                                                       "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                       "Sidedness_Binary", "Age",
                                                       "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor",
                                                       List_Var_f_over5, "Hypoxia_Buffa", "TGFb_Fibroblast", 
                                                       "ESTIMATE_TumorPurity", "MCP_CD8_T_cells", "MCP_Cytotoxic_lymphocytes", 
                                                       "Methyl_Phen_Index",  "Total_Mutations")
                                                        
            Final_Merge_Selected_Nodal <- dplyr::select(Final_Merge_Node_Met, "Scort_ID", "Node_Met", 
                                                        "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                        "Sidedness_Binary", "Age",
                                                        "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor",
                                                        List_Var_f_over5, "Hypoxia_Buffa", "TGFb_Fibroblast", 
                                                        "ESTIMATE_TumorPurity", "MCP_CD8_T_cells", "MCP_Cytotoxic_lymphocytes", 
                                                        "Methyl_Phen_Index",  "Total_Mutations")
            
            Final_Merge_Selected_Peritoneal <- dplyr::select(Final_Merge_Peritoneum_Met, "Scort_ID", "Peritoneum_Met", 
                                                             "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                             "Sidedness_Binary", "Age", 
                                                             "Focus",  "NewEpoc", "Quasar2", "Spinal", "Victor",
                                                             List_Var_f_over5, "Hypoxia_Buffa", "TGFb_Fibroblast", 
                                                             "ESTIMATE_TumorPurity", "MCP_CD8_T_cells", "MCP_Cytotoxic_lymphocytes", 
                                                             "Methyl_Phen_Index",  "Total_Mutations")
          
### Make all numeric (exc. Scort_ID) -----
            
            Final_Merge_Selected_Liver[,-1] <- lapply(Final_Merge_Selected_Liver[,-1], function(x) as.numeric(as.character(x)))
            Final_Merge_Selected_Lung[,-1] <- lapply(Final_Merge_Selected_Lung[,-1], function(x) as.numeric(as.character(x)))
            Final_Merge_Selected_Nodal[,-1] <- lapply(Final_Merge_Selected_Nodal[,-1], function(x) as.numeric(as.character(x)))
            Final_Merge_Selected_Peritoneal[,-1] <- lapply(Final_Merge_Selected_Peritoneal[,-1], function(x) as.numeric(as.character(x))) 
            
### Run regression for each dependent variable ----
            
        ### For Liver
            
            Independent_Liver <- dplyr::select(Final_Merge_Selected_Liver, one_of("Scort_ID", "Liver_Met"))
            Dependent_Liver <- dplyr::select(Final_Merge_Selected_Liver, -one_of("Scort_ID", "Liver_Met"))
            
            All_dependent_cols_Liver <- ncol(Dependent_Liver)
            Test_Regression_Liver <- lapply(1:All_dependent_cols_Liver, function(x) glm(Independent_Liver[,2] ~ Dependent_Liver[,x], family = binomial))
            
            View(Test_Regression_Liver)
            Summaries_Liver <- lapply(Test_Regression_Liver, summary)
            
         ### For Lung
            
            Independent_Lung <- dplyr::select(Final_Merge_Selected_Lung, one_of("Scort_ID", "Lung_Met"))
            Dependent_Lung <- dplyr::select(Final_Merge_Selected_Lung, -one_of("Scort_ID", "Lung_Met"))
            
            All_dependent_cols_Lung <- ncol(Dependent_Lung)
            Test_Regression_Lung <- lapply(1:All_dependent_cols_Lung, function(x) glm(Independent_Lung[,2] ~ Dependent_Lung[,x], family = binomial))
            
            View(Test_Regression_Lung)
            Summaries_Lung <- lapply(Test_Regression_Lung, summary)
            
            
        ### For Nodal
            
            Independent_Nodal <- dplyr::select(Final_Merge_Selected_Nodal, one_of("Scort_ID", "Node_Met"))
            Dependent_Nodal <- dplyr::select(Final_Merge_Selected_Nodal, -one_of("Scort_ID", "Node_Met"))
            
            All_dependent_cols_Nodal <- ncol(Dependent_Nodal)
            Test_Regression_Nodal <- lapply(1:All_dependent_cols_Nodal, function(x) glm(Independent_Nodal[,2] ~ Dependent_Nodal[,x], family = binomial))
            
            View(Test_Regression_Nodal)
            Summaries_Nodal <- lapply(Test_Regression_Nodal, summary)
            
         ### For Peritoneal
            
            Independent_Peritoneal <- dplyr::select(Final_Merge_Selected_Peritoneal, one_of("Scort_ID", "Peritoneum_Met"))
            Dependent_Peritoneal <- dplyr::select(Final_Merge_Selected_Peritoneal, -one_of("Scort_ID", "Peritoneum_Met"))
            
            All_dependent_cols_Peritoneal <- ncol(Dependent_Peritoneal)
            Test_Regression_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal, function(x) glm(Independent_Peritoneal[,2] ~ Dependent_Peritoneal[,x], family = binomial))
            
            View(Test_Regression_Peritoneal)
            Summaries_Peritoneal <- lapply(Test_Regression_Peritoneal, summary)
            
            
### Extract co-efficients and p-values -----
            
            Regression_Results_Liver <- lapply(Summaries_Liver, function(x) x$coefficients[,])
            Regression_Results_Lung <- lapply(Summaries_Lung, function(x) x$coefficients[,])
            Regression_Results_Nodal <- lapply(Summaries_Nodal, function(x) x$coefficients[,])
            Regression_Results_Peritoneal <- lapply(Summaries_Peritoneal, function(x) x$coefficients[,])
            
            Regression_Results_Liver2 <- do.call(rbind.data.frame, Regression_Results_Liver)
            Regression_Results_Lung2 <- do.call(rbind.data.frame, Regression_Results_Lung)
            Regression_Results_Nodal2 <- do.call(rbind.data.frame, Regression_Results_Nodal)
            Regression_Results_Peritoneal2 <- do.call(rbind.data.frame, Regression_Results_Peritoneal)
            
            
            
            ## Remove odd rows, leaving even rows (i.e. filter out intercept and keep dependent variable)
            
            Regression_Results_Liver2 <- Regression_Results_Liver2[ c(FALSE,TRUE),]
            Regression_Results_Lung2 <- Regression_Results_Lung2[ c(FALSE,TRUE),]
            Regression_Results_Nodal2 <- Regression_Results_Nodal2[ c(FALSE,TRUE),]
            Regression_Results_Peritoneal2 <- Regression_Results_Peritoneal2[ c(FALSE,TRUE),]
            
            
            ## Create dataframe with OR, 95%CI and p-value for each dependent variable
            
            Regression_Results_Liver3 <- as.data.frame(cbind(colnames(Dependent_Liver), exp(Regression_Results_Liver2$Estimate), exp(Regression_Results_Liver2$Estimate-(1.96*Regression_Results_Liver2$`Std. Error`)), exp(Regression_Results_Liver2$Estimate+(1.96*Regression_Results_Liver2$`Std. Error`)), Regression_Results_Liver2$`Pr(>|z|)`))
            colnames(Regression_Results_Liver3) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
            Regression_Results_Liver3[,-1] <- lapply(Regression_Results_Liver3[,-1], function(x) as.numeric(as.character(x)))
            
            Regression_Results_Lung3 <- as.data.frame(cbind(colnames(Dependent_Lung), exp(Regression_Results_Lung2$Estimate), exp(Regression_Results_Lung2$Estimate-(1.96*Regression_Results_Lung2$`Std. Error`)), exp(Regression_Results_Lung2$Estimate+(1.96*Regression_Results_Lung2$`Std. Error`)), Regression_Results_Lung2$`Pr(>|z|)`))
            colnames(Regression_Results_Lung3) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
            Regression_Results_Lung3[,-1] <- lapply(Regression_Results_Lung3[,-1], function(x) as.numeric(as.character(x)))
            
            Regression_Results_Nodal3 <- as.data.frame(cbind(colnames(Dependent_Nodal), exp(Regression_Results_Nodal2$Estimate), exp(Regression_Results_Nodal2$Estimate-(1.96*Regression_Results_Nodal2$`Std. Error`)), exp(Regression_Results_Nodal2$Estimate+(1.96*Regression_Results_Nodal2$`Std. Error`)), Regression_Results_Nodal2$`Pr(>|z|)`))
            colnames(Regression_Results_Nodal3) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
            Regression_Results_Nodal3[,-1] <- lapply(Regression_Results_Nodal3[,-1], function(x) as.numeric(as.character(x)))
            
            Regression_Results_Peritoneal3 <- as.data.frame(cbind(colnames(Dependent_Peritoneal), exp(Regression_Results_Peritoneal2$Estimate), exp(Regression_Results_Peritoneal2$Estimate-(1.96*Regression_Results_Peritoneal2$`Std. Error`)), exp(Regression_Results_Peritoneal2$Estimate+(1.96*Regression_Results_Peritoneal2$`Std. Error`)), Regression_Results_Peritoneal2$`Pr(>|z|)`))
            colnames(Regression_Results_Peritoneal3) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
            Regression_Results_Peritoneal3[,-1] <- lapply(Regression_Results_Peritoneal3[,-1], function(x) as.numeric(as.character(x)))
            
            
### Adjust p-values for FDR ---- 
            
            Regression_Results_Liver3$Adjusted_PVal <- p.adjust(Regression_Results_Liver3$P_value, method = "fdr")
            Regression_Results_Lung3$Adjusted_PVal <- p.adjust(Regression_Results_Lung3$P_value, method = "fdr")
            Regression_Results_Nodal3$Adjusted_PVal <- p.adjust(Regression_Results_Nodal3$P_value, method = "fdr")
            Regression_Results_Peritoneal3$Adjusted_PVal <- p.adjust(Regression_Results_Peritoneal3$P_value, method = "fdr") 
            
### Format regression results ----
            
            #install.packages("formattable")
            
            library(formattable)
            
            #Liver
            as.datatable(
              formattable(Regression_Results_Liver3, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(Adjusted_PVal <= 0.25, "green", "red")))))
            )
            
            #Lung
            as.datatable(
              formattable(Regression_Results_Lung3, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(Adjusted_PVal <= 0.25, "green", "red")))))
            )
            
            #Nodal
            as.datatable(
              formattable(Regression_Results_Nodal3, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(Adjusted_PVal <= 0.25, "green", "red")))))
            )
            
            #Peritoneal 
            as.datatable(
              formattable(Regression_Results_Peritoneal3, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(Adjusted_PVal <= 0.25, "green", "red")))))
            )
            
###### ------------ MULTIVARIATE ANALYSIS -------- #########
    
    Merge_Binary <- read.csv("/Users/laurahudson/Documents/Merge_Binary.csv", row.names = 1, stringsAsFactors = FALSE, na.strings = c("","NULL","NA", " "))
            
### LIVER ----
        
        ## Subset significant variables from univariate analysis 
            
            Liver_Sig_Variables <- subset(Regression_Results_Liver3, Adjusted_PVal<=1000)
            
            #Remove clinical variables for adjustment
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "T_Stage_Binary")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "N_Stage_Binary")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "M_Stage_Binary")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Age")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Sidedness_Binary")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Focus")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "NewEpoc")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Spinal")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Victor")
            Liver_Sig_Variables <- subset(Liver_Sig_Variables, Variables != "Quasar2")
            
            Liver_Sig_Variables <- as.character(Liver_Sig_Variables$Variables)
            
        
        ## Select these variables from Binary Merge 
            
            Final_Merge_Selected_Liver_MV <- dplyr::select(Merge_Binary, "Scort_ID", "Liver_Met", Liver_Sig_Variables, 
                                                           "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                           "Age", "Sidedness_Binary", "Focus", "NewEpoc", "Spinal",
                                                           "Victor", "Quasar2")
            
          
      ### Run regression for each dependent variable    
            
            Dependent_Liver_MV <- dplyr::select(Final_Merge_Selected_Liver_MV, -one_of("Scort_ID", "Liver_Met", "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                                       "Age", "Sidedness_Binary", "Focus", "NewEpoc", "Spinal",
                                                                                       "Victor", "Quasar2"))
            All_dependent_cols_Liver_MV <- ncol(Dependent_Liver_MV)
            
             
            
            
            
            
            Model_Liver_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) glm(Liver_Met ~ Dependent_Liver_MV[,x]
                                                                                             + T_Stage_Binary + N_Stage_Binary
                                                                                             + M_Stage_Binary + Age + Sidedness_Binary
                                                                                             + Focus + Spinal + NewEpoc + Victor + Quasar2,
                                                                                             data = Final_Merge_Selected_Liver_MV, family = binomial))
            
            
            
            Summary_Model_Liver_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) summary(Model_Liver_MV[[x]]))
            
            
            
            ## Extract coefficients 
            
            Regression_Results_Liver_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) Summary_Model_Liver_MV[[x]]$coefficients)
            
            Regression_Results_Liver2_MV <- lapply(1:All_dependent_cols_Liver_MV, function(x) as.data.frame(Regression_Results_Liver_MV[[x]]))
            
            n <- lapply(1:All_dependent_cols_Liver_MV, function(x) nrow(Regression_Results_Liver2_MV[[x]]))
            
            ## Get P-values
            
            P_values_MV_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) unname(coef(Summary_Model_Liver_MV[[x]])[,'Pr(>|z|)'])[2:n[[x]]])
            
            Adjusted_PVal_MV_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) p.adjust(P_values_MV_Liver[[x]], method = "fdr"))
            
            
            ### Get names of coefficients
            
            CoefNames_MV_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) names(coef(summary(Model_Liver_MV[[x]]))[,'Pr(>|z|)'])[-1])
            
            
            
            #### Create dataframe with OR, 95%CI and p-value for each dependent variable
            
            
            OR_MV_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) exp(Summary_Model_Liver_MV[[x]]$coefficients[-1,1]))
            
            OR_MV_Liver[[1]]
            
            CI_MV_Low_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) exp(Summary_Model_Liver_MV[[x]]$coefficients[-1,1] + (qnorm(0.025) * Summary_Model_Liver_MV[[x]]$coefficients[-1,2])))
            
            
            CI_MV_High_Liver <- lapply(1:All_dependent_cols_Liver_MV, function(x) exp(Summary_Model_Liver_MV[[x]]$coefficients[-1,1] + (qnorm(0.975) * Summary_Model_Liver_MV[[x]]$coefficients[-1,2])))
            
            
            Liver_MV_Regression_Results <- lapply(1:All_dependent_cols_Liver_MV, function(x) cbind(CoefNames_MV_Liver[[x]], OR_MV_Liver[[x]], CI_MV_Low_Liver[[x]], CI_MV_High_Liver[[x]], P_values_MV_Liver[[x]], Adjusted_PVal_MV_Liver[[x]]))
            
            
            Liver_MV_Regression_Results <- lapply(1:All_dependent_cols_Liver_MV, function(x) as.data.frame(Liver_MV_Regression_Results[[x]]))
            
           
            ### Subset dependent variables
            Liver_MV_Regression_Results2 <- lapply(1:All_dependent_cols_Liver_MV, function(x) subset(Liver_MV_Regression_Results[[x]], V1 == "Dependent_Liver_MV[, x]"))
            
            ### Create dataframe of dependent variables
            Liver_MV_Regression_Results3 <- do.call('rbind',Liver_MV_Regression_Results2) 
            
            ### Change column names
            colnames(Liver_MV_Regression_Results3) <- c("Variables", "OR", "95%CI_Low", "95%CI_High", "P_value", "Adjusted_PVal")
            
            
            ### Fill variable column with names of actual variables
            
            Dependent_Liver_MV_Names <- colnames(Dependent_Liver_MV)
            
            Liver_MV_Regression_Results3$Variables <- c(Dependent_Liver_MV_Names)
            
            Liver_MV_Regression_Results3$Adjusted_PVal <- as.numeric(levels(Liver_MV_Regression_Results3$Adjusted_PVal))[Liver_MV_Regression_Results3$Adjusted_PVal]
            
            ### Format output table
            
            library(formattable)
            
    
            as.datatable(
              formattable(Liver_MV_Regression_Results3, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
            )
            
            
           
           
### LUNG ----
            
            ## Subset significant variables from univariate analysis 
            
            Lung_Sig_Variables <- subset(Regression_Results_Lung3, Adjusted_PVal<=1000)
            
            #Remove clinical variables for adjustment
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "T_Stage_Binary")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "N_Stage_Binary")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "M_Stage_Binary")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Age")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Sidedness_Binary")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Focus")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "NewEpoc")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Spinal")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Victor")
            Lung_Sig_Variables <- subset(Lung_Sig_Variables, Variables != "Quasar2")
            
            Lung_Sig_Variables <- as.character(Lung_Sig_Variables$Variables)
            
            
            ## Select these variables from Binary Merge 
            
            Final_Merge_Selected_Lung_MV <- dplyr::select(Merge_Binary, "Scort_ID", "Lung_Met", Lung_Sig_Variables, 
                                                          "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                          "Age", "Sidedness_Binary", "Focus", "NewEpoc", "Spinal",
                                                          "Victor", "Quasar2")
            
            
            ### Run regression for each dependent variable    
            
            Dependent_Lung_MV <- dplyr::select(Final_Merge_Selected_Lung_MV, -one_of("Scort_ID", "Lung_Met", "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                                     "Age", "Sidedness_Binary", "Focus", "NewEpoc", "Spinal",
                                                                                     "Victor", "Quasar2"))
            All_dependent_cols_Lung_MV <- ncol(Dependent_Lung_MV)
            
            
            
            
            
            
            Model_Lung_MV <- lapply(1:All_dependent_cols_Lung_MV, function(x) glm(Lung_Met ~ Dependent_Lung_MV[,x]
                                                                                  + T_Stage_Binary + N_Stage_Binary
                                                                                  + M_Stage_Binary + Age + Sidedness_Binary
                                                                                  + Focus + Spinal + NewEpoc + Victor + Quasar2,
                                                                                  data = Final_Merge_Selected_Lung_MV, family = binomial))
            
            
            
            Summary_Model_Lung_MV <- lapply(1:All_dependent_cols_Lung_MV, function(x) summary(Model_Lung_MV[[x]]))
            
            
            
            ## Extract coefficients 
            
            Regression_Results_Lung_MV <- lapply(1:All_dependent_cols_Lung_MV, function(x) Summary_Model_Lung_MV[[x]]$coefficients)
            
            Regression_Results_Lung2_MV <- lapply(1:All_dependent_cols_Lung_MV, function(x) as.data.frame(Regression_Results_Lung_MV[[x]]))
            
            n <- lapply(1:All_dependent_cols_Lung_MV, function(x) nrow(Regression_Results_Lung2_MV[[x]]))
            
            ## Get P-values
            
            P_values_MV_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) unname(coef(Summary_Model_Lung_MV[[x]])[,'Pr(>|z|)'])[2:n[[x]]])
            
            Adjusted_PVal_MV_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) p.adjust(P_values_MV_Lung[[x]], method = "fdr"))
            
            
            ### Get names of coefficients
            
            CoefNames_MV_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) names(coef(summary(Model_Lung_MV[[x]]))[,'Pr(>|z|)'])[-1])
            
            
            
            #### Create dataframe with OR, 95%CI and p-value for each dependent variable
            
            
            OR_MV_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) exp(Summary_Model_Lung_MV[[x]]$coefficients[-1,1]))
            
            OR_MV_Lung[[1]]
            
            CI_MV_Low_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) exp(Summary_Model_Lung_MV[[x]]$coefficients[-1,1] + (qnorm(0.025) * Summary_Model_Lung_MV[[x]]$coefficients[-1,2])))
            
            
            CI_MV_High_Lung <- lapply(1:All_dependent_cols_Lung_MV, function(x) exp(Summary_Model_Lung_MV[[x]]$coefficients[-1,1] + (qnorm(0.975) * Summary_Model_Lung_MV[[x]]$coefficients[-1,2])))
            
            
            Lung_MV_Regression_Results <- lapply(1:All_dependent_cols_Lung_MV, function(x) cbind(CoefNames_MV_Lung[[x]], OR_MV_Lung[[x]], CI_MV_Low_Lung[[x]], CI_MV_High_Lung[[x]], P_values_MV_Lung[[x]], Adjusted_PVal_MV_Lung[[x]]))
            
            
            Lung_MV_Regression_Results <- lapply(1:All_dependent_cols_Lung_MV, function(x) as.data.frame(Lung_MV_Regression_Results[[x]]))
            
            
            ### Subset dependent variables
            Lung_MV_Regression_Results2 <- lapply(1:All_dependent_cols_Lung_MV, function(x) subset(Lung_MV_Regression_Results[[x]], V1 == "Dependent_Lung_MV[, x]"))
            
            ### Create dataframe of dependent variables
            Lung_MV_Regression_Results3 <- do.call('rbind',Lung_MV_Regression_Results2) 
            
            ### Change column names
            colnames(Lung_MV_Regression_Results3) <- c("Variables", "OR", "95%CI_Low", "95%CI_High", "P_value", "Adjusted_PVal")
            
            
            ### Fill variable column with names of actual variables
            
            Dependent_Lung_MV_Names <- colnames(Dependent_Lung_MV)
            
            Lung_MV_Regression_Results3$Variables <- c(Dependent_Lung_MV_Names)
            
            Lung_MV_Regression_Results3$Adjusted_PVal <- as.numeric(levels(Lung_MV_Regression_Results3$Adjusted_PVal))[Lung_MV_Regression_Results3$Adjusted_PVal]
            
            ### Format output table
            
            library(formattable)
            
            
            as.datatable(
              formattable(Lung_MV_Regression_Results3, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
            )
            
            
### NODE  ----
            
            ## Subset significant variables from univariate analysis 
            
            Nodal_Sig_Variables <- subset(Regression_Results_Nodal3, Adjusted_PVal<=1000)
            
            #Remove clinical variables for adjustment
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "T_Stage_Binary")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "N_Stage_Binary")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "M_Stage_Binary")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Age")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Sidedness_Binary")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Focus")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "NewEpoc")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Spinal")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Victor")
            Nodal_Sig_Variables <- subset(Nodal_Sig_Variables, Variables != "Quasar2")
            
            Nodal_Sig_Variables <- as.character(Nodal_Sig_Variables$Variables)
            
            
            ## Select these variables from Binary Merge 
            
            Final_Merge_Selected_Nodal_MV <- dplyr::select(Merge_Binary, "Scort_ID", "Node_Met", Nodal_Sig_Variables, 
                                                           "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                           "Age", "Sidedness_Binary", "Focus", "NewEpoc", "Spinal",
                                                           "Victor", "Quasar2")
            
            
            ### Run regression for each dependent variable    
            
            Dependent_Nodal_MV <- dplyr::select(Final_Merge_Selected_Nodal_MV, -one_of("Scort_ID", "Node_Met", "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                                       "Age", "Sidedness_Binary", "Focus", "NewEpoc", "Spinal",
                                                                                       "Victor", "Quasar2"))
            All_dependent_cols_Nodal_MV <- ncol(Dependent_Nodal_MV)
            
            
            
            
            
            
            Model_Nodal_MV <- lapply(1:All_dependent_cols_Nodal_MV, function(x) glm(Node_Met ~ Dependent_Nodal_MV[,x]
                                                                                    + T_Stage_Binary + N_Stage_Binary
                                                                                    + M_Stage_Binary + Age + Sidedness_Binary
                                                                                    + Focus + Spinal + NewEpoc + Victor + Quasar2,
                                                                                    data = Final_Merge_Selected_Nodal_MV, family = binomial))
            
            
            
            Summary_Model_Nodal_MV <- lapply(1:All_dependent_cols_Nodal_MV, function(x) summary(Model_Nodal_MV[[x]]))
            
            
            
            ## Extract coefficients 
            
            Regression_Results_Nodal_MV <- lapply(1:All_dependent_cols_Nodal_MV, function(x) Summary_Model_Nodal_MV[[x]]$coefficients)
            Regression_Results_Nodal2_MV <- lapply(1:All_dependent_cols_Nodal_MV, function(x) as.data.frame(Regression_Results_Nodal_MV[[x]]))
            
            n <- lapply(1:All_dependent_cols_Nodal_MV, function(x) nrow(Regression_Results_Nodal2_MV[[x]]))
            
            ## Get P-values
            
            P_values_MV_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) unname(coef(Summary_Model_Nodal_MV[[x]])[,'Pr(>|z|)'])[2:n[[x]]])
            
            Adjusted_PVal_MV_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) p.adjust(P_values_MV_Nodal[[x]], method = "fdr"))
            
            
            ### Get names of coefficients
            
            CoefNames_MV_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) names(coef(summary(Model_Nodal_MV[[x]]))[,'Pr(>|z|)'])[-1])
            
            
            
            #### Create dataframe with OR, 95%CI and p-value for each dependent variable
            
            
            OR_MV_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) exp(Summary_Model_Nodal_MV[[x]]$coefficients[-1,1]))
            
            CI_MV_Low_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) exp(Summary_Model_Nodal_MV[[x]]$coefficients[-1,1] + (qnorm(0.025) * Summary_Model_Nodal_MV[[x]]$coefficients[-1,2])))
            
            
            CI_MV_High_Nodal <- lapply(1:All_dependent_cols_Nodal_MV, function(x) exp(Summary_Model_Nodal_MV[[x]]$coefficients[-1,1] + (qnorm(0.975) * Summary_Model_Nodal_MV[[x]]$coefficients[-1,2])))
            
            
            Nodal_MV_Regression_Results <- lapply(1:All_dependent_cols_Nodal_MV, function(x) cbind(CoefNames_MV_Nodal[[x]], OR_MV_Nodal[[x]], CI_MV_Low_Nodal[[x]], CI_MV_High_Nodal[[x]], P_values_MV_Nodal[[x]], Adjusted_PVal_MV_Nodal[[x]]))
            
            
            Nodal_MV_Regression_Results <- lapply(1:All_dependent_cols_Nodal_MV, function(x) as.data.frame(Nodal_MV_Regression_Results[[x]]))
            
            
            
            
            Nodal_MV_Regression_Results[[1]]
            
            ### Adjust by fdr 
            
            
            ### Subset dependent variables
            Nodal_MV_Regression_Results2 <- lapply(1:All_dependent_cols_Nodal_MV, function(x) subset(Nodal_MV_Regression_Results[[x]], V1 == "Dependent_Nodal_MV[, x]"))
            
            ### Create dataframe of dependent variables
            Nodal_MV_Regression_Results3 <- do.call('rbind',Nodal_MV_Regression_Results2) 
            
            ### Change column names
            colnames(Nodal_MV_Regression_Results3) <- c("Variables", "OR", "95%CI_Low", "95%CI_High", "P_value", "Adjusted_PVal")
            
            
            ### Fill variable column with names of actual variables
            
            Dependent_Nodal_MV_Names <- colnames(Dependent_Nodal_MV)
            
            Nodal_MV_Regression_Results3$Variables <- c(Dependent_Nodal_MV_Names)
            
            Nodal_MV_Regression_Results3$Adjusted_PVal <- as.numeric(levels(Nodal_MV_Regression_Results3$Adjusted_PVal))[Nodal_MV_Regression_Results3$Adjusted_PVal]
            
            ### Format output table
            
            library(formattable)
            
            
            as.datatable(
              formattable(Nodal_MV_Regression_Results3, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
            )
            
            
### PERITONEAL ----
            
            ## Subset significant variables from univariate analysis 
            
            Peritoneal_Sig_Variables <- subset(Regression_Results_Peritoneal3, Adjusted_PVal<=1000)
            
            #Remove clinical variables for adjustment
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "T_Stage_Binary")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "N_Stage_Binary")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "M_Stage_Binary")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Age")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Sidedness_Binary")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Focus")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "NewEpoc")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Spinal")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Victor")
            Peritoneal_Sig_Variables <- subset(Peritoneal_Sig_Variables, Variables != "Quasar2")
            
            Peritoneal_Sig_Variables <- as.character(Peritoneal_Sig_Variables$Variables)
            
            
            ## Select these variables from Binary Merge 
            
            Final_Merge_Selected_Peritoneal_MV <- dplyr::select(Merge_Binary, "Scort_ID", "Peritoneum_Met", Peritoneal_Sig_Variables, 
                                                                "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                "Age", "Sidedness_Binary", "Focus", "NewEpoc", "Spinal",
                                                                "Victor", "Quasar2")
            
            
            ### Run regression for each dependent variable    
            
            Dependent_Peritoneal_MV <- dplyr::select(Final_Merge_Selected_Peritoneal_MV, -one_of("Scort_ID", "Peritoneum_Met", "T_Stage_Binary", "N_Stage_Binary", "M_Stage_Binary",
                                                                                                 "Age", "Sidedness_Binary", "Focus", "NewEpoc", "Spinal",
                                                                                                 "Victor", "Quasar2"))
            All_dependent_cols_Peritoneal_MV <- ncol(Dependent_Peritoneal_MV)
            
            
            
            
            
            
            Model_Peritoneal_MV <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) glm(Peritoneum_Met ~ Dependent_Peritoneal_MV[,x]
                                                                                              + T_Stage_Binary + N_Stage_Binary
                                                                                              + M_Stage_Binary + Age + Sidedness_Binary
                                                                                              + Focus + Spinal + NewEpoc + Victor + Quasar2,
                                                                                              data = Final_Merge_Selected_Peritoneal_MV, family = binomial))
            
            
            
            Summary_Model_Peritoneal_MV <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) summary(Model_Peritoneal_MV[[x]]))
            
            
            
            ## Extract coefficients 
            
            Regression_Results_Peritoneal_MV <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) Summary_Model_Peritoneal_MV[[x]]$coefficients)
            Regression_Results_Peritoneal2_MV <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) as.data.frame(Regression_Results_Peritoneal_MV[[x]]))
            
            n <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) nrow(Regression_Results_Peritoneal2_MV[[x]]))
            
            ## Get P-values
            
            P_values_MV_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) unname(coef(Summary_Model_Peritoneal_MV[[x]])[,'Pr(>|z|)'])[2:n[[x]]])
            
            Adjusted_PVal_MV_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) p.adjust(P_values_MV_Peritoneal[[x]], method = "fdr"))
            
            
            ### Get names of coefficients
            
            CoefNames_MV_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) names(coef(summary(Model_Peritoneal_MV[[x]]))[,'Pr(>|z|)'])[-1])
            
            
            
            #### Create dataframe with OR, 95%CI and p-value for each dependent variable
            
            
            OR_MV_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) exp(Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,1]))
            
            CI_MV_Low_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) exp(Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,1] + (qnorm(0.025) * Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,2])))
            
            
            CI_MV_High_Peritoneal <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) exp(Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,1] + (qnorm(0.975) * Summary_Model_Peritoneal_MV[[x]]$coefficients[-1,2])))
            
            
            Peritoneal_MV_Regression_Results <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) cbind(CoefNames_MV_Peritoneal[[x]], OR_MV_Peritoneal[[x]], CI_MV_Low_Peritoneal[[x]], CI_MV_High_Peritoneal[[x]], P_values_MV_Peritoneal[[x]], Adjusted_PVal_MV_Peritoneal[[x]]))
            
            
            Peritoneal_MV_Regression_Results <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) as.data.frame(Peritoneal_MV_Regression_Results[[x]]))
            
            
            
            
            Peritoneal_MV_Regression_Results[[1]]
            
            ### Adjust by fdr 
            
            
            ### Subset dependent variables
            Peritoneal_MV_Regression_Results2 <- lapply(1:All_dependent_cols_Peritoneal_MV, function(x) subset(Peritoneal_MV_Regression_Results[[x]], V1 == "Dependent_Peritoneal_MV[, x]"))
            
            ### Create dataframe of dependent variables
            Peritoneal_MV_Regression_Results3 <- do.call('rbind',Peritoneal_MV_Regression_Results2) 
            
            ### Change column names
            colnames(Peritoneal_MV_Regression_Results3) <- c("Variables", "OR", "95%CI_Low", "95%CI_High", "P_value", "Adjusted_PVal")
            
            
            ### Fill variable column with names of actual variables
            
            Dependent_Peritoneal_MV_Names <- colnames(Dependent_Peritoneal_MV)
            
            Peritoneal_MV_Regression_Results3$Variables <- c(Dependent_Peritoneal_MV_Names)
            
            Peritoneal_MV_Regression_Results3$Adjusted_PVal <- as.numeric(levels(Peritoneal_MV_Regression_Results3$Adjusted_PVal))[Peritoneal_MV_Regression_Results3$Adjusted_PVal]
            
            ### Format output table
            
            library(formattable)
            
            
            as.datatable(
              formattable(Peritoneal_MV_Regression_Results3, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(Adjusted_PVal <= 0.05, "green", "red")))))
            )
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
### ** Write csv for all variables  ----
            
            write.csv(Lung_MV_Regression_Results3, "/Users/laurahudson/Documents/MV_Lung_All_Var.csv")
            write.csv(Lung_MV_Regression_Results3, "/Users/laurahudson/Documents/MV_Lung_All_Var.csv")
            write.csv(Nodal_MV_Regression_Results3, "/Users/laurahudson/Documents/MV_Nodal_All_Var.csv")
            write.csv(Peritoneal_MV_Regression_Results3, "/Users/laurahudson/Documents/MV_Peritoneal_All_Var.csv")
            
### ** Write objects into .csv files ----
            
            write.csv(Lung_MV_Regression_Results3, "/Users/laurahudson/Documents/MV_Lung.csv")
            write.csv(Lung_MV_Regression_Results3, "/Users/laurahudson/Documents/MV_Lung.csv")
            write.csv(Nodal_MV_Regression_Results3, "/Users/laurahudson/Documents/MV_Nodal.csv")
            write.csv(Peritoneal_MV_Regression_Results3, "/Users/laurahudson/Documents/MV_Peritoneal.csv")
            

### ***** MODEL AND RUN BACKWARDS STEPWISE REGRESSION  ***** -----
        
    Merge_Binary <- read.csv("/Users/laurahudson/Documents/Merge_Binary.csv", row.names=1, stringsAsFactors = FALSE, na.strings = c("","NULL","NA", " "))
            
### LIVER ----
       
    ## CRIS_A, CRIS_B, APC, CRIS_D, MSI_Binary, CMS1   
            
      Model_Liver_Mv <- glm(Liver_Met ~ T_Stage_Binary + N_Stage_Binary + M_Stage_Binary 
                            + Age + Sidedness_Binary
                            + Cohort
                            + CRIS_D
                            + APC 
                                  , data = Merge_Binary, family = binomial)
            
            Summary_Model_Liver_Mv <- summary(Model_Liver_Mv)
            
            Summary_Model_Liver_Mv$coefficients
            
            n <- nrow(Summary_Model_Liver_Mv$coefficients)
            
            #### Get p-values from model summary 
            
            P_values <- unname(coef(summary(Model_Liver_Mv))[,'Pr(>|z|)'])[2:n]
            P_values
            
            #### Get names of coefficients
            
            names(coef(summary(Model_Liver_Mv))[,'Pr(>|z|)'])[2:n]
            
            #### Create dataframe with OR, 95%CI and p-value for each dependent variable
            
            Liver_Mv_Regression_Results <- as.data.frame(cbind(names(coef(summary(Model_Liver_Mv))[,'Pr(>|z|)'])[2:n], exp(summary(Model_Liver_Mv)$coefficients[2:n,1]), exp(summary(Model_Liver_Mv)$coefficients[2:n,1] + (qnorm(0.025) * summary(Model_Liver_Mv)$coefficients[2:n,2])), exp(summary(Model_Liver_Mv)$coefficients[2:n,1] + (qnorm(0.975) * summary(Model_Liver_Mv)$coefficients[2:n,2])), summary(Model_Liver_Mv)$coefficients[2:n,4]))
            
            exp(summary(Model_Liver_Mv)$coefficients[-1,1])
            
            
            colnames(Liver_Mv_Regression_Results) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
            Liver_Mv_Regression_Results[,-1] <- lapply(Liver_Mv_Regression_Results[,-1], function(x) as.numeric(as.character(x)))
            
            Liver_Mv_Regression_Results
            
            ## Format results
            
            library(formattable)
            
            as.datatable(
              formattable(Liver_Mv_Regression_Results, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(P_value <= 0.05, "green", "red")))))
            ) 
    
            
### LUNG ----
            
            # KRAS, MSI_Binary, Hypoxia_Buffa, TGFb_Fibroblast, MCP_Cytotoxic_lymphocytes, CMS2
            
            Model_Lung_Mv <- glm(Lung_Met ~ T_Stage_Binary + N_Stage_Binary + M_Stage_Binary 
                                + Age + Sidedness_Binary
                                + Cohort
                                + KRAS
                                + Hypoxia_Buffa
                                 , data = Merge_Binary, family = binomial)
            
            Summary_Model_Lung_Mv <- summary(Model_Lung_Mv)
            

          
            Summary_Model_Lung_Mv$coefficients
            
            
              
            
            n <- nrow(Summary_Model_Lung_Mv$coefficients)
            
            #### Get p-values from model summary 
            
            P_values <- unname(Summary_Model_Lung_Mv$coefficients[,'Pr(>|z|)'])[2:n]
            P_values
            
            #### Get names of coefficients
            
            names(Summary_Model_Lung_Mv$coefficients[,'Pr(>|z|)'])[2:n]
            
            #### Create dataframe with OR, 95%CI and p-value for each dependent variable
            
            Lung_Mv_Regression_Results <- as.data.frame(cbind(names(coef(summary(Model_Lung_Mv))[,'Pr(>|z|)'])[2:n],
                                                              exp(Summary_Model_Lung_Mv$coefficients[2:n,1]),
                                                              exp(Summary_Model_Lung_Mv$coefficients[2:n,1] + (qnorm(0.025) * Summary_Model_Lung_Mv$coefficients[2:n,2])),
                                                              exp(Summary_Model_Lung_Mv$coefficients[2:n,1] + (qnorm(0.975) * Summary_Model_Lung_Mv$coefficients[2:n,2])),
                                                              Summary_Model_Lung_Mv$coefficients[2:n,4]))
            
            exp(Summary_Model_Lung_Mv$coefficients[-1,1])
            
            
            colnames(Lung_Mv_Regression_Results) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
            Lung_Mv_Regression_Results[,-1] <- lapply(Lung_Mv_Regression_Results[,-1], function(x) as.numeric(as.character(x)))
            
            Lung_Mv_Regression_Results
            
            ## Format results
            
            library(formattable)
            
            as.datatable(
              formattable(Lung_Mv_Regression_Results, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(P_value <= 0.05, "green", "red")))))
            ) 
            
            
            
### NODE ----
            
          # APC, BRAF_V600E  

            
            Model_Node_Mv <- glm(Node_Met ~ T_Stage_Binary + N_Stage_Binary + M_Stage_Binary 
                                 + Age + Sidedness_Binary
                                 + Cohort
                                 + BRAF_V600E
                                 , data = Merge_Binary, family = binomial)
            
            Summary_Model_Node_Mv <- summary(Model_Node_Mv)
            
            Summary_Model_Node_Mv$coefficients
            
            n <- nrow(Summary_Model_Node_Mv$coefficients)
            
            #### Get p-values from model summary 
            
            P_values <- unname(coef(summary(Model_Node_Mv))[,'Pr(>|z|)'])[2:n]
            P_values
            
            #### Get names of coefficients
            
            names(coef(summary(Model_Node_Mv))[,'Pr(>|z|)'])[2:n]
            
            #### Create dataframe with OR, 95%CI and p-value for each dependent variable
            
            Node_Mv_Regression_Results <- as.data.frame(cbind(names(coef(summary(Model_Node_Mv))[,'Pr(>|z|)'])[2:n], exp(summary(Model_Node_Mv)$coefficients[2:n,1]), exp(summary(Model_Node_Mv)$coefficients[2:n,1] + (qnorm(0.025) * summary(Model_Node_Mv)$coefficients[2:n,2])), exp(summary(Model_Node_Mv)$coefficients[2:n,1] + (qnorm(0.975) * summary(Model_Node_Mv)$coefficients[2:n,2])), summary(Model_Node_Mv)$coefficients[2:n,4]))
            
            exp(summary(Model_Node_Mv)$coefficients[-1,1])
            
            
            colnames(Node_Mv_Regression_Results) <- c("Variables", "OR", "95%CI_Lower", "95%CI_Upper", "P_value")
            Node_Mv_Regression_Results[,-1] <- lapply(Node_Mv_Regression_Results[,-1], function(x) as.numeric(as.character(x)))
            
            Node_Mv_Regression_Results
            
            ## Format results
            
            library(formattable)
            
            as.datatable(
              formattable(Node_Mv_Regression_Results, list(
                Variables = formatter("span",
                                      style = ~ style(color = ifelse(P_value <= 0.05, "green", "red")))))
            ) 
            

### Write csv files ----
    
    write.csv(Liver_Mv_Regression_Results, "/Users/laurahudson/Documents/MV_Liver_BSR.csv")
    write.csv(Lung_Mv_Regression_Results, "/Users/laurahudson/Documents/MV_Lung_BSR.csv")
    write.csv(Node_Mv_Regression_Results, "/Users/laurahudson/Documents/MV_Nodal_BSR.csv")
    
