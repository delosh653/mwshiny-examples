# Tiny ENCORE Campfire Demo for useR!
# By Hannah De los Santos
# Originated on: 7/5/19

# load libraries and data ----

library(mwshiny)
library(r2d3)

# neurospora library
# library(AnnotationHub)

# libraries for ontology explorer
library(topGO)
library(STRINGdb)
library(r2d3)
library(data.table)
library(jsonlite)
library(ggplot2)
library(igraph)
library(rstudioapi)

# libraries for ontology enrichment
library(topGO)
library(stringr)
library(mygene)
library(data.table)
library(STRINGdb)

# ontology libraries;
library(AnnotationDbi)
# library(org.Ag.eg.db)
# library(org.Dm.eg.db)
# library(org.Hs.eg.db)
library(org.Mm.eg.db)
# library(org.EcK12.eg.db)
# library(org.Sc.sgd.db)

# load the data and such, etc

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# preallocate links
# THESE FILES CANNOT BE LOADED TO GITHUB - FOR MORE INFO, CHECK OUT THE ENCORE GITHUB
links <- fread(paste0("10090.protein.links.v11.0.txt"))
load("Hughes_liver_ENCORE.RData")

bg <- read.csv(paste0("10090_background.csv"), stringsAsFactors = F)

# maximum interactions
max_interact <- 100

# color bar for replicates
color_bar <- c("Rep. 1"="red","Rep. 2"="blue","Rep. 3"="green",
               "Rep. 4"="yellow","Rep. 5"="purple","Rep. 6"="pink",
               "Rep. 7"="orange","Rep. 8"="magenta","Fit"="black",
               "Original"="grey")

all_ont_parents <- c("BP"="GO:0008150",
                     "CC"="GO:0005575",
                     "MF"="GO:0003674")

# function for json conversion for go.df
data_to_json <- function(data) {
  jsonlite::toJSON(data, dataframe = "rows", auto_unbox = FALSE, rownames = TRUE, digits = NA)
}

# campfire
color_pal <-  c("Repressed"="#7EBBD0","Damped"="#6F81C1","Harmonic"="#856BBD", "Forced"="#B46FC2", "Overexpressed"="#C16F84",
                "All.Circ.wo.OE.RE" = "#00D055", "All.Circ" = "#00D055")
color_pal_dark <-  c("Repressed"="#5D92C0","Damped"="#4B4FAE","Harmonic"="#7D4FB2", "Forced"="#B354B7", "Overexpressed"="#B65388",
                     "All.Circ.wo.OE.RE" = "#00A044", "All.Circ" = "#00A044")

ont_map <- c("BP" = "Biological Process",
             "CC" =  "Cellular Component",
             "MF" = "Molecular Function")

prev_parents <- c("")
prev_sig <- c("")
curr <- ""
go_name_curr <- ""
path_trace <- c("")

end_num <- 16
if (user_input$run_conf){
  end_num <- 26
}

# UI ----

win_titles <- c("Controller", "Wall", "Floor")

ui_win <- list()

ui_win[["Controller"]] <- fluidPage(
  h2("Explore the function of circadian rhythms using the campfire!"),
  sidebarLayout(
    sidebarPanel(
      selectInput("new_path", "Which GO Term would you like to examine?",
                  choices = c("metabolic process" = "GO:0008152",
                              "circadian regulation of gene expression" = "GO:0032922",
                              "translation" = "GO:0006412",
                              "cell cycle" = "GO:0007049"))
    ),
    mainPanel(
    )
  )
)

ui_win[["Wall"]] <- div(
  d3Output("wall", height = "663px")
)

ui_win[["Floor"]] <- div(
  d3Output("floor", height = "895px")
)

# CALC ----

serv_calc <- list() 

serv_calc[[1]] <- function(serverValues, sess){
  observeEvent(serverValues$new_path, {
    withProgress(message = "Calculating!", value = 0, {
      if(!serverValues$new_path %in% all_ont_parents){
        incProgress(1/3, detail = paste("Getting information for Ontology Explorer. Started on:",Sys.time()))
        
        serverValues$fc_map <- "Damped"
        serverValues$fc_groups <- c(serverValues$fc_map, "Harmonic", "Forced")
        if (serverValues$new_path == "GO:0032922"){
          serverValues$fc_map <- "Harmonic"
          serverValues$fc_groups <- c("Damped", "Harmonic", "Forced")
        }
        
        serverValues[["ont"]] <- "BP"
        #get example graph
        ex_graph <- fc_results[[serverValues$fc_map]][[serverValues$ont]][["go_data"]]@graph
        care_group <- fc_results[[serverValues$fc_map]][[serverValues$ont]][["go_results"]]
        child <- serverValues$new_path # child of interest -- it is a go id
        # child <- care_group[which(child==care_group$Term)[1],"GO.ID"]
        
        print(child)
        
        # get subgraph
        sg <- inducedGraph(ex_graph,child)
        
        # get shortest path
        ont_parent <- all_ont_parents[serverValues$ont]
        shortest_path <- names(rev(shortest_paths(igraph.from.graphNEL(sg), serverValues$new_path, to=ont_parent, weights = NA)$vpath[[1]]))
        
        orig_group <- c("Repressed","Damped", "Harmonic","Forced", "Overexpressed","All.Circ.wo.OE.RE","All.Circ")
        
        if (length(serverValues$fc_groups)==0){
          serverValues$fc_groups <- c(serverValues$fc_map)
        } else {
          if (!serverValues$fc_map %in% serverValues$fc_groups){
            serverValues$fc_groups <- c(serverValues$fc_groups, serverValues$fc_map)
          }
          
          if ("All.Circ.wo.OE.RE" %in% serverValues$fc_groups ){ # overwrites all other groups
            if (serverValues$fc_map != "All.Circ.wo.OE.RE"){
              serverValues$fc_groups <- serverValues$fc_groups[serverValues$fc_groups != "All.Circ.wo.OE.RE"]
            } else {
              serverValues$fc_groups <- c("All.Circ.wo.OE.RE")
            }
          } else if ("All.Circ" %in% serverValues$fc_groups) { # overwrites all other groups except wo
            if (serverValues$fc_map != "All.Circ"){
              serverValues$fc_groups <- serverValues$fc_groups[serverValues$fc_groups != "All.Circ"]
            } else {
              serverValues$fc_groups <- c("All.Circ")
            }
          }
          
          serverValues$fc_groups <- serverValues$fc_groups[order(match(serverValues$fc_groups,orig_group))]
          
        }
        
        # remove if in the "no significant"
        serverValues$fc_groups <- base::setdiff(serverValues$fc_groups,no_sig[[serverValues$ont]])
        
        sig_vect <- c()
        for (ch in shortest_path[-1]){
          if (care_group$classicFisher[care_group$GO.ID==ch] < user_input_ont$ont_sig_level){
            sig_vect[length(sig_vect)+1] <- "Significant"
          } else {
            sig_vect[length(sig_vect)+1] <- "Not Significant"
          }
        }
        # child is the last one in path
        serverValues[["sig"]] <- sig_vect[length(sig_vect)]
        
        # make stuff for the floor
        # groups of interest, provided by shiny app
        # currently fixed, will be updated
        
        # get the table and such
        #serverValues <- get_child_table("",serverValues, serverValues$sig)
        
        serverValues[["color_pal"]] <- color_pal[serverValues$fc_groups]
        serverValues[["color_pal_dark"]] <- color_pal_dark[serverValues$fc_groups]
        
        curr <- child
        
        go_term <- curr
        go_name <- character(0)
        for (f in serverValues$fc_groups){
          if (length(go_name) == 0){
            go_name <- fc_results[[f]][[serverValues$ont]][["go_results"]][fc_results[[f]][[serverValues$ont]][["go_results"]]$GO.ID == go_term,"Term"]
          }
        }
        
        serverValues[["go_name"]] <- go_name
        
        incProgress(1/3, detail = paste("Getting information for Chord Diagram. Started on:",Sys.time()))
        
        if (curr != ""){
          # then we also have to get everything for the chord diagram and such
          go_term <- curr
          go_name <- character(0)
          
          for (f in serverValues$fc_groups){
            if (length(go_name) == 0){
              go_name <- fc_results[[f]][[serverValues$ont]][["go_results"]][fc_results[[f]][[serverValues$ont]][["go_results"]]$GO.ID == go_term,"Term"]
            }
          }
          
          int_genes <- c()
          fc_cat_genes <- c()
          for (f in serverValues$fc_groups){
            go_data <- fc_results[[f]][[serverValues$ont]][["go_data"]]
            go_results <- fc_results[[f]][[serverValues$ont]][["go_results"]]
            
            # grab the genes connected to the term
            if (go_term %in% go_results$GO.ID) {
              go_genes <- genesInTerm(go_data, go_term)
              int_genes <- c(int_genes,go_genes[[go_term]])
            }
          }
          
          
          # int_genes <- int_genes[!duplicated(int_genes)]
          
          # string db ----
          
          # grab gene stringdb ids, original names
          
          gene_map.df <- data.frame("int_genes"=int_genes, stringsAsFactors = F)#, "fc_cat_genes" = fc_cat_genes)
          rownames(map_sub.df) <- map_sub.df$entrezgene
          gene_map.df$fc_cat_genes <- map_sub.df[gene_map.df$int_genes, "Osc.Type"]
          gene_map.df$STRING_id <- map_sub.df[gene_map.df$int_genes,"STRING_id"]
          gene_map.df$int_genes_orig <- map_sub.df[gene_map.df$int_genes,"query"]
          gene_map.df$period <- map_sub.df[gene_map.df$int_genes,"Period"]
          gene_map.df$pval <- map_sub.df[gene_map.df$int_genes,user_input_ont$pval_cat]
          # no factors!
          i <- sapply(gene_map.df, is.factor)
          gene_map.df[i] <- lapply(gene_map.df[i], as.character)
          
          # remove any mappings not found
          gene_map.df <- gene_map.df[!is.na(gene_map.df$int_genes),]
          gene_map.df <- gene_map.df[!is.na(gene_map.df$fc_cat_genes),]
          
          # remove not selected groups
          # need to change names for if we're looking at all circadian
          if ("All.Circ" %in% serverValues$fc_groups){
            gene_map.df$fc_cat_genes[gene_map.df$fc_cat_genes %in% c("Overexpressed","Repressed","Harmonic","Damped","Forced")] <- "All.Circ"
          } else if ("All.Circ.wo.OE.RE" %in% serverValues$fc_groups){
            gene_map.df$fc_cat_genes[gene_map.df$fc_cat_genes %in% c("Harmonic","Damped","Forced")] <- "All.Circ.wo.OE.RE"
          }
          gene_map.df <- gene_map.df[gene_map.df$fc_cat_genes %in% serverValues$fc_groups,]
          # remove not selected periods if not all range
          if (!is_all_range){
            gene_map.df <- gene_map.df[gene_map.df$period <= high_range & gene_map.df$period >= low_range,]
          }
          gene_map.df <- gene_map.df[gene_map.df$pval < user_input_ont$sig_level,]
          
          gene_map.df <- gene_map.df[!is.na(gene_map.df$STRING_id),]
          gene_map.df <- gene_map.df[order(gene_map.df$fc_cat_genes),]
          gene_map.df <- gene_map.df[!duplicated(gene_map.df$STRING_id),]
          
          # if we toggle that we only want to see specified signifiance of group, remove others
          keep <- c()
          for (f in serverValues$fc_groups){
            go_results <- fc_results[[f]][[serverValues$ont]][["go_results"]]
            pval <- go_results[go_results$GO.ID == go_term,"classicFisher"]
            if (length(pval) > 0){
              if (pval < user_input_ont$ont_sig_level){
                keep <- c(keep,f)
              }
            }
          }
          
          gene_map.df <- gene_map.df[gene_map.df$fc_cat_genes %in% keep,]
          
          # get connections - hmmmm
          # these are connections that only appear in the gene ontology
          temp <- links[links$protein1 %in% gene_map.df$STRING_id & links$protein2 %in% gene_map.df$STRING_id,]
          temp <- temp[order(-combined_score),]
          num_chord <- 100
          if (nrow(temp) > num_chord){
            temp <- temp[1:num_chord,]
          }
          interacts <- as.data.frame(temp)
          colnames(interacts)[1:2] <- c("from","to") # the square matrix for chords
          #interacts <- interacts[!is.na(interacts$from) & !is.na(interacts$to),]
          
          # map interacts back
          # remove duplicates
          rownames(gene_map.df) <- gene_map.df$STRING_id
          interacts$from <- as.character(gene_map.df[interacts$from,"int_genes_orig"])
          interacts$to <- as.character(gene_map.df[interacts$to,"int_genes_orig"])
          
          # now add the type of gene, based on the from
          rownames(gene_map.df) <- gene_map.df$int_genes_orig
          interacts$fc_type <- gene_map.df[interacts$from, "fc_cat_genes"]
          # sort interacts by type
          interacts <- interacts[order(interacts$fc_type),]
          
          # now make a matrix
          connect.df <- data.frame(matrix(0,length(gene_map.df$int_genes_orig),length(gene_map.df$int_genes_orig)))
          colnames(connect.df) <- rownames(connect.df) <- gene_map.df$int_genes_orig
          
          
          if (nrow(interacts) > 0){
            for (i in 1:nrow(interacts)){
              connect.df[interacts$from[i] , interacts$to[i]] <- 1
            }
            # remove rows and columns with no connections
            connect.df <- connect.df[rowSums(connect.df) > 0 | colSums(connect.df) > 0,
                                     rowSums(connect.df) > 0 | colSums(connect.df) > 0]
          }
          # make everything 1 to 1 - THIS CAN BE FASTER
          # for (i in 1:nrow(connect.df)){
          #   for (j in 1:nrow(connect.df)){
          #     if (connect.df[i,j] > 0 & connect.df[j,i] == 0){
          #       connect.df[j,i] <- 1
          #     }
          #     if (connect.df[j,i] > 0 & connect.df[i,j] == 0){
          #       connect.df[i,j] <- 1
          #     }
          #   }
          # }
          
          connect.df <- (t(connect.df)>0 & connect.df==0)+connect.df
          
          # heat maps ----
          
          int_genes_care <- rownames(connect.df)
          
          int_tr <- total_results[total_results$`Gene Name` %in% int_genes_care,]
          # need to change names for if we're looking at all circadian
          if ("All.Circ" %in% serverValues$fc_groups){
            int_tr$`Oscillation Type`[int_tr$`Oscillation Type` %in% c("Overexpressed","Repressed","Harmonic","Damped","Forced")] <- "All.Circ"
          } else if ("All.Circ.wo.OE.RE" %in% serverValues$fc_groups){
            int_tr$`Oscillation Type`[int_tr$`Oscillation Type` %in% c("Harmonic","Damped","Forced")] <- "All.Circ.wo.OE.RE"
          }
          
          hm_total <- matrix(0,nrow(int_tr),length((end_num+1):(end_num+length(timen)*1)))
          hm_order <- rep(0,nrow(hm_total))
          hm_names <- rep("",nrow(hm_total))
          hm_hours_shifted <- rep(0,nrow(hm_total))
          hm_fc <- rep("",nrow(hm_total))
          count_next <- 0
          
          for (f in serverValues$fc_groups){
            int_sub_tr <- int_tr[int_tr$`Oscillation Type` == f & !is.na(int_tr$`Oscillation Type`),]
            if (nrow(int_sub_tr) > 0){
              # there should be no na rows in this data, by default
              # adjust phase
              int_sub_tr$`Phase Shift`[int_sub_tr$Initial.Amplitude < 0] <- int_sub_tr$`Phase Shift`[int_sub_tr$Initial.Amplitude < 0]+pi
              int_sub_tr$Initial.Amplitude[int_sub_tr$Initial.Amplitude < 0] <- -1*int_sub_tr$Initial.Amplitude[int_sub_tr$Initial.Amplitude < 0]
              
              # fixing the phase shift
              int_sub_tr$`Phase Shift`[int_sub_tr$`Phase Shift` > 2*pi] <- int_sub_tr$`Phase Shift`[int_sub_tr$`Phase Shift` > 2*pi]-2*pi
              int_sub_tr$`Phase Shift`[int_sub_tr$`Phase Shift` <0] <- int_sub_tr$`Phase Shift`[int_sub_tr$`Phase Shift` < 0]+2*pi
              
              # HEAT MAP STUFF
              
              #get matrix of just the relative expression over time
              hm_mat <- as.matrix(int_sub_tr[,(end_num+1):(end_num+length(timen)*num_reps)])
              
              #if there are replicates, average the relative expression for each replicate
              mtx_reps <- list() # to store actual matrix
              mtx_count <- list() # to store how many are NA
              for (i in 1:num_reps){
                mtx_reps[[i]] <- hm_mat[, seq(i,ncol(hm_mat), by=num_reps)]
                mtx_count[[i]] <- is.na(mtx_reps[[i]])
                mtx_reps[[i]][is.na(mtx_reps[[i]])] <- 0
              }
              repmtx <- matrix(0L,ncol = length(timen),nrow = nrow(hm_mat))+num_reps # to store how many we should divide by
              hm_mat <- matrix(0L,ncol = length(timen),nrow = nrow(hm_mat)) # to store the final result
              for (i in 1:num_reps){
                hm_mat <- hm_mat + mtx_reps[[i]] # sum the replicates
                repmtx <- repmtx - mtx_count[[i]] # how many replicates are available for each time point
              }
              repmtx[repmtx==0] <- NA # to avoid division by 0 and induce NAs if there are no time points available
              hm_mat <- hm_mat/repmtx
              
              # center rows around mean
              # vector of row means
              all_row_mean <- rowMeans(hm_mat, na.rm = TRUE)
              hm_mat <- hm_mat - all_row_mean
              
              
              #normalize each row to be between -1 and 1
              for (i in 1:nrow(int_sub_tr)){
                gene_max <- max(abs((hm_mat[i,])),na.rm = TRUE)
                hm_mat[i,] <- hm_mat[i,]/gene_max 
              }
              
              
              #sort by phase shift, if more than 1 gene
              if (nrow(hm_mat) > 1){
                ord <- order(int_sub_tr$`Phase Shift`)
                hm_mat <- hm_mat[ord,]
              } else {
                ord <- 1
              }
              
              hm_total[(count_next+1):(count_next+nrow(hm_mat)),] <- hm_mat
              hm_order[(count_next+1):(count_next+nrow(hm_mat))] <- which(int_tr$`Oscillation Type` == f)[ord]
              hm_names[(count_next+1):(count_next+nrow(hm_mat))] <- int_sub_tr$`Gene Name`[ord]
              hm_hours_shifted[(count_next+1):(count_next+nrow(hm_mat))] <- int_sub_tr$`Hours Shifted`[ord]
              hm_fc[(count_next+1):(count_next+nrow(hm_mat))] <- f
              count_next <- count_next+nrow(hm_mat)
            }
          }
          
          # making the heat map a data frame
          heat.df <- as.data.frame(hm_total)
          colnames(heat.df) <- paste0("TP_",timen)
          if (nrow(heat.df) > 0){
            heat.df <- heat.df[seq(nrow(heat.df),1,-1),]
          }
          
          # order the connections to the heatmap
          if (nrow(connect.df)>1){
            connect.df <- connect.df[hm_names,hm_names]
          }
          
          # lowest you can have is one connection
          all_width <- colSums(connect.df)
          all_width[all_width==0] <- .1
          tot_width <- sum(all_width)
          heat.df$Perc_Wid <- all_width/tot_width
          
          # push everything i need to the serverValues
          # stacked_heatmap.js
          serverValues[["time_points"]] <- paste0("TP_",timen)
          serverValues[["genes"]] <- rownames(connect.df)
          serverValues[["heights"]] <- 0:length(timen)
          serverValues[["heat.df"]] <- heat.df
          serverValues[["hm_hours_shifted"]] <- hm_hours_shifted
          
          # circ.db.js
          serverValues[["from_fc_cat"]] <- gene_map.df$fc_cat_genes
          serverValues[["from_genes"]] <- gene_map.df$int_genes_orig
          
          temp_tab <-data.frame(table(gene_map.df$fc_cat_genes[gene_map.df$int_genes_orig %in% rownames(connect.df)]))
          if (nrow(temp_tab) > 0){
            ind <- match(serverValues$fc_groups, temp_tab$Var1, nomatch = 0)
            not_ind <- setdiff(c(1:nrow(temp_tab)),ind)
            temp_tab[,] <- temp_tab[c(ind,not_ind),]
          }
          
          serverValues[["tot_fc_cats"]] <- as.numeric(temp_tab$Freq)
          serverValues[["chord_dat"]] <- connect.df
          
          serverValues[["gene.df"]] <- data.frame("Gene.Name"=rownames(connect.df),
                                                  "Osc.Type"=hm_fc,
                                                  "Hours.Shifted"=hm_hours_shifted)
          
          
          serverValues[["fc_rep"]] <- temp_tab$Var1[temp_tab$Freq!=0]
          serverValues[["color_pal_chord"]] <- color_pal[as.character(temp_tab$Var1[temp_tab$Freq!=0])]
          serverValues[["color_pal_dark_chord"]] <- color_pal_dark[as.character(temp_tab$Var1[temp_tab$Freq!=0])]
          
          if (!grepl("\\(", path_trace[length(path_trace)])){
            path_trace[length(path_trace)] <<- paste0(path_trace[length(path_trace)]," (",nrow(connect.df)," genes, ",sum(serverValues$chord_dat)/2," chords)")
          }
          
          # go_chord_heatmap.js
        }
        incProgress(1/3, detail = paste("Finished! Started on:",Sys.time()))
      }
    })
  })
}

serv_calc[[2]] <- function(serverValues, sess){
  observeEvent(serverValues$dark, {
    serverValues[["darken"]] <- TRUE
    serverValues[["darken_except"]] <- serverValues$dark
    
    print(paste("DARK",serverValues$dark))
  })
}

serv_calc[[3]] <- function(serverValues, sess){
  observeEvent(serverValues$undark, {
    serverValues[["darken"]] <- FALSE
    
    print(paste("LIGHT", serverValues$undark))
  })
}

# OUTPUT ----

serv_out <- list()

serv_out[["wall"]] <- function(serverValues, sess){
  renderD3({
    r2d3(data=serverValues$heat.df, 
         script = "demo_wall.js",
         d3_version = 5,
         options(r2d3.theme = list(
           background = "#000000",
           foreground = "#000")),
         options = list(color_pal = unname(serverValues$color_pal),
                        color_pal_dark = unname(serverValues$color_pal_dark),
                        darken = serverValues$darken,
                        darken_except = serverValues$darken_except,
                        
                        time_points = serverValues$time_points,
                        genes = serverValues$genes,
                        heights = serverValues$heights,
                        fc_hours_shifted = serverValues$hm_hours_shifted
         )
    )
  })
}

serv_out[["floor"]] <- function(serverValues, sess){
  renderD3({
    r2d3(data=serverValues$heat.df, script = "demo_floor.js", d3_version = 5,
         options(r2d3.theme = list(
           background = serverValues$col_bg,
           foreground = serverValues$textcol_bg)),
         options = list(color_pal = unname(serverValues$color_pal),
                        color_pal_dark = unname(serverValues$color_pal_dark),
                        color_pal_chord = unname(serverValues$color_pal_chord),
                        color_pal_dark_chord = unname(serverValues$color_pal_dark_chord),
                        
                        time_points = serverValues$time_points,
                        genes = serverValues$genes,
                        heights = serverValues$heights,
                        chord_dat = serverValues$chord_dat,
                        fc_cats = serverValues$fc_groups,
                        fc_rep = serverValues$fc_rep,
                        from_fc_cat = serverValues$from_fc_cat,
                        from_genes = serverValues$from_genes,
                        tot_fc_cats = serverValues$tot_fc_cats,
                        go_name = serverValues$go_name,
                        fc_hours_shifted = serverValues$hm_hours_shifted,
                        tot_chords = sum(serverValues$chord_dat)/2,
                        
                        curr = curr
         )
    )
  })
}

# DEPEND ----

depend <- list()
depend[["htmlwidgets"]] <- c("www/htmlwidgets.js")
depend[["shiny"]] <- c("www/shared/selectize/js/selectize.min.js",
                       "www/shared/selectize/css/selectize.bootstrap3.css",
                       "www/shared/fontawesome/css/all.min.css",
                       "www/shared/fontawesome/css/v4-shims.min.css",
                       "www/shared/datatables/js/jquery.dataTables.min.js",
                       "www/shared/datatables/js/dataTables.bootstrap.js",
                       "www/shared/datatables/css/dataTables.bootstrap.css",
                       "www/shared/datatables/css/dataTables.extra.css")
depend[["r2d3"]] <- c("htmlwidgets/lib/r2d3/r2d3-render.js",
                      "htmlwidgets/lib/webcomponents/webcomponents.js",
                      "htmlwidgets/r2d3.js",
                      "www/d3/5.0.0/d3.min.js")

# RUN ----

mwsApp(win_titles, ui_win, serv_calc, serv_out, depend)