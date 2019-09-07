######################################################################
######## calculate Hopkins Statistic in james data
df_james_seq_data <- fread(james_sequence_cluster_result)
df_james_context_data<- fread(james_context_cluster_result)
df_james_action_data <- fread(james_action_cluster_result)
HopkinsDelegate(df_james_seq_data)
HopkinsDelegate(df_james_context_data)
HopkinsDelegate(df_james_action_data)

######## calculate Hopkins Statistic in openmrs data
df_openmrs_seq_data <- fread(openmrs_sequence_cluster_result)
df_openmrs_context_data<- fread(openmrs_context_cluster_result)
df_openmrs_action_data<- fread(openmrs_action_cluster_result)
HopkinsDelegate(df_openmrs_seq_data)
HopkinsDelegate(df_openmrs_context_data)
HopkinsDelegate(df_openmrs_action_data)

######## calculate Hopkins Statistic in google data
df_google_seq_data <- fread(google_sequence_cluster_result)
df_google_context_data<- fread(google_context_cluster_result)
df_google_action_data<- fread(google_action_cluster_result)
HopkinsDelegate(df_google_seq_data)
HopkinsDelegate(df_google_context_data)
HopkinsDelegate(df_google_action_data)

####### calculate hopkins
HopkinsDelegate <- function(df){
  drop <- c("V1","cluster")
  df <- dplyr::select(df,-drop)
  n_generated <- nrow(df)/10
  hopkins_value <- hopkins(df, n=n_generated)
  return(1-hopkins_value$H)
}