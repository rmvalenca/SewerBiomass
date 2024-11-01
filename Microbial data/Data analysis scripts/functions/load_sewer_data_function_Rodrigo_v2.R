#  Load data  #

## Goal 
  ### Order metadata 
  ### Make new dataframes
  ### Load old dataframes for fast analysis

## Output 
 ### Dataset for analysis
 ### Dataset for control 
 ### T/F for generaltion of new


## Define function ad varibles for making new
make_sewer_data <- function(
    save_unclassified = F,
    WD = "C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_sewer_microbial_communities/",
    DataPath = paste0("C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_sewer_microbial_communities/", "data/"),
    OutputPath = paste0("C:/Users/HD95LP/OneDrive - Aalborg Universitet/visual_studio_code/2023_sewer_microbial_communities/", "output/")
){
  

##################################################################################
##     Make metadata                                                            ##
##################################################################################  
  
print("Make metadata and filter to the samples used in study")
  
## Import full metadata sheet 
metadata_l <- readxl::read_xlsx(paste0(DataPath, "GiuliaMeta.xlsx")) %>% 
    select(-MiSeqID, -ProjectName, -Investigator, -Supervisor, -LibraryID, -LibraryConc, -LibraryMethod, 
           -ExtractionMethod, -Primer, -Barcode, -TubeName, -SubmittedBy, -Comments, -Season) 
  

metadata <- metadata_l %>% 
  filter(Old != "old" | is.na(Old)) %>%    # Remove samples from old homogenisation 
  mutate(
    #Season = getSeason(SampleDate),
    Municipalty = case_when(SampleSite %in% 
                              c("Skyttegarden", "Vennebjerg", "SkagenLandevej", "Hjørring") ~
                              "Hjorring", .default = "Aalborg"), 
    Month = month(SampleDate), 
    Date_month = format(SampleDate, "%d-%m"), 
    SampleNote = case_when(SampleSite == "Ferslev" & SampleContent == "biofilm" ~ "suspended sand in the SWW", 
                           .default = PrimarySettler),
    PrimarySettler = ifelse(PrimarySettler == "first_flush", "after", PrimarySettler),
    SampleSite = str_replace(SampleSite, "AAW", "AalborgWest"),
    SampleContent = if_else(SampleName == "PCRpos", "control_pos", SampleContent),
    SampleContent = if_else(SampleName == "PCRneg", "control_neg", SampleContent),
    SampleContent = if_else(SampleName == "ExtractionControl", "control_neg", SampleContent),
    SampleDate_numeric = as.numeric(SampleDate), 
    SampleEnvironment = if_else(str_detect(SampleContent, "control"), "control", "mistake"), 
    SampleEnvironment = if_else(str_detect(SampleName, "biofilm") | 
                                  str_detect(SampleName, "sediment") | 
                                  str_detect(SampleName, "bio") |
                                  str_detect(SampleName, "cocio") | 
                                  str_detect(SampleName, "SWW_RG")
                                , "sewer_wet_solids", SampleEnvironment), 
    SampleName = case_when(str_detect(SampleName, "cocio") ~ "biofilm_RG",
                           str_detect(SampleName, "SWW_RG") ~ "biofilm_RG", 
                           str_detect(SampleName, "bio") & 
                             (str_detect(SampleSite, "Sejlflod") | 
                                str_detect(SampleSite, "Doctorvej")) ~ "biofilm_end_of_pressure",    
                           str_detect(SampleName, "bio") & str_detect(SampleSite, "Vennebjerg") ~ "biofilm_RG",
                           str_detect(SampleName, "bio") & str_detect(SampleSite, "Skyttegarden") ~ "biofilm_RG",
                           str_detect(SampleName, "bio") & str_detect(SampleNote, "pump") ~ "biofilm_pre_RG",
                           .default = SampleName),
    SampleEnvironment = case_when(str_detect(SampleName, "biofilm_dry") ~ "biofilm_dry",
                                  str_detect(SampleName, "AS") ~ "AS", 
                                  SampleName =="SWW" | str_detect(SampleName, "IWW") ~ "wastewater",
                                  .default = SampleEnvironment), 
    SampleContent = if_else(str_detect(SampleName, "_RG"), "biofilm_RG", SampleContent),
    SampleContent2 = case_when(
      SampleSite %in% c("Doctorvej", "Sejlflod") & str_detect(SampleContent, "biofilm") ~ "biofilm_end_pressure",
      .default = SampleContent),
    Configuration = case_when(
      SampleSite %in% c("Skyttegarden") ~ "wide_pressure_pipes", 
      SampleSite %in% c("Vennebjerg", "SkagenLandevej") ~ "small_pressure_pipes", 
      SampleSite %in% c("AAU") ~ "control", 
      SampleSite %in% c("AalborgØst", "AalborgWest", "Hjørring", "AAW") ~ "WWTP", 
      SampleSite %in% c("Doctorvej", "Sejlflod") ~ "end_of_pressure",
      .default = "gravity_pipes"),
    SewerType = case_when(SampleSite %in% c("Frejlev", "Tvaesgade", "Visse", "Roden") ~ "combined", 
                          SampleSite %in% c("Doctorvej","Stadionvej","Sejlflod") ~ "seperated",
                          SampleSite %in% c("Skolesti") ~ "combined", 
                          SampleSite %in% c("Skyttegarden") ~ "mixed", 
                          SampleSite %in% c("Vennebjerg", "SkagenLandevej") ~ "seperated", 
                          SampleSite %in% c("AalborgWest", "AalborgØst", "Hjørring") ~ "WWTP"
    ),
    SampleSite = ifelse(Configuration == "WWTP" & SampleContent == "IWW", 
                        paste0(SampleSite, "_", PrimarySettler), SampleSite),
    SampleName = ifelse(Configuration == "WWTP"& SampleContent == "IWW", 
                        paste0(SampleName, "_", PrimarySettler), SampleName),
    #SampleName_Season = paste0(SampleName, "_", Season), 
    #SampleSite_Season = paste0(SampleSite, "_", Season),
    SampleContent_SampleDate = paste0(SampleContent, "_", as.character(SampleDate)),
    SampleContent_SampleSite_SampleDate = paste0(SampleContent, "_", SampleSite,"_", as.character(SampleDate)),
    SampleContent_SampleSite = paste0(SampleContent, "_", SampleSite),
    SampleContent2_SampleSite = paste0(SampleContent2, "_", SampleSite),
    #SampleContent2_Season = paste0(SampleContent2, "_", Season),
    Catchment_WWTP = case_when(SampleSite %in% c("Roden","Doctorvej","Stadionvej",
                                                 "Sejlflod", "Tvaesgade", "Visse", "Skolesti") ~ "AalborgEast",
                               SampleSite %in% c("Frejlev") ~ "AalborgWest"),
    SampleSite = recode(SampleSite, "Tvaesgade" = "Tvaergade"), 
    SampleName = factor(SampleName, levels = c("AS", "IWW_before", "IWW_after", "SWW", 
                                               "biofilm_wet", "biofilm_end_of_pressure",
                                               "biofilm_RG", "biofilm_pre_RG", 
                                               "sediment", "biofilm_dry" ,"PCRpos", "PCRneg", "ExtractionControl")),
    SampleContent = factor(SampleContent, 
                           levels = c("AS", "IWW", "sewage", "biofilm", "biofilm_RG","sediment","biod","control_pos", "control_neg"), 
                           labels = c("AS", "IWW", "SWW", "biofilm", "biofilm_RG","sediment","biofilm_dry","control_pos", "control_neg")), 
    Configuration = factor(Configuration, 
                           levels = c("WWTP", "gravity_pipes", "end_of_pressure", 
                                      "wide_pressure_pipes", "small_pressure_pipes","control")),
    System = ifelse(SampleContent2 %in% c("AS", "IWW"), "WWTP", "Sewer system") 
    ) %>%
  filter(SampleSite != "Ferslev") %>%     ### Remove Ferslev 
  filter(SampleContent != "biofilm_dry") %>%        ### Remove biofilm dry
  filter(SampleID != "MQ220419-206") %>%    ### Remove biofilm dry from Roden which were wrongly labled
  filter(!SampleID %in% c("MQ220419-208", "MQ220419-210")) %>% # Remove biofilms taken other places than the pipe
  filter(!SampleID %in% c("MQ230313-117", "MQ230313-124")) %>% # Remove outliers    
  filter(SampleContent != "biofilm_RG") %>%                    # Remove rensegris samples
  filter(SampleSite != "Hjørring") %>%                    # Remove Hjørring AS samples
  #filter(SampleSite != "AalborgWest_before") %>%          # Remove before primary settling 
  filter(!str_detect(SampleID, "220816")) %>%             # Remove samples where sequences has been deleted from server
  filter(!SampleSite %in% c("AalborgØst", "AalborgØst_after", "AalborgWest_after")) %>% ## Only include AAW before PS
  filter(SampleContent2 != "Sediment")                    # Remove sediment samples

 
# Merge dublicates 
dublicates <- metadata %>% 
  #filter(Old == "rep") %>%  ### tabbed out to merged all replicates
  group_by(SampleSite, SampleContent, SampleDate, SampleNote) %>% 
  mutate(SampleID2 = paste0(unique(str_sub(SampleID, 1, 9)), paste0(str_sub(SampleID, 10, 12), collapse = ";"))) %>% 
  relocate(SampleID2, .after = "SampleID")

metadata <- metadata %>% 
  left_join(., dublicates, by = join_by(SampleID, SampleName, SampleContent, 
                                        SampleContent2, SampleSite, SampleDate, 
                                        PrimarySettler, SamplingTime, Old, Municipalty, Month,
                                        SampleNote, SampleDate_numeric, SampleEnvironment, Configuration, 
                                        System,SewerType, SampleContent_SampleDate,
                                        SampleContent_SampleSite_SampleDate, SampleContent_SampleSite, SampleContent2_SampleSite, Catchment_WWTP, 
                                        Date_month
                                        )) %>% 
  mutate(SampleID2 = if_else(is.na(SampleID2), SampleID, SampleID2))


print("Load ampvis object and merge replicate samples") 
data <- amp_load(otutable = paste0(DataPath, "ASVtable_notax.tsv"),
                 taxonomy = paste0(DataPath, "ASVs_0.6_cut_off_MIDAS_5.2.R1.sintax"),
                 metadata = metadata)



data <- amp_merge_replicates(data, merge_var = "SampleID2", round = "up")

##################################################################################
##      Update taxonomic names to MiDAS 5.3                                     ##
##################################################################################

# Data was generated using Midas 5.1 
# Some taxa has been renamed in the version changes of the MiDAS 5 database
source(paste0(WD, "/scripts/functions/change_tax_names.R"))   # change to midas 5.3 names (2023-11-03)
print("Update taxonomic names to MiDAS 5.3")  
data$tax <- data$tax %>% change_tax_names()


##################################################################################
##      REMOVE  CONTROLS                                                        ##
##################################################################################

print("Remove controls and make a seperate dataframe including the control samples")
# Make control dataframe
data_control <- data
amp_diversity_control_df <- 
  data_control %>% amp_alpha_diversity()

## REMOVE  CONTROLS                                                        ##
data <- data %>% 
  amp_subset_samples(str_detect(SampleContent, "control_",negate = T), 
                     minreads = 5000) 
amp_diversity_df <-
  data %>% amp_alpha_diversity()

##################################################################################
##      Rarefy IWW and AS to correspond with the median of sewer samples        ##
##################################################################################

amp_diversity_df %>%
  group_by(System) %>%
  summarise(median(Reads),
            max(Reads),
            quantile(Reads, 0.75))
  
print(paste0("AS and and IWW is rarefied to the median read counts of the sewer samples: median = 11214 reads (biofilm (e.p.))"))

data_sewer <- data %>% 
  amp_subset_samples(System == "Sewer system")

data_wwtp <- data %>% 
  amp_subset_samples(System == "WWTP", rarefy = 14728, removeAbsentOTUs = F)

data <- amp_merge_ampvis2(data_sewer, data_wwtp, by_refseq = F)

##################################################################################
##     Convert ampvis object to long_format                                     ##
##################################################################################

print("Make long DF")
# Create new tidy_long object

source(paste0({{WD}}, "scripts/functions/tidy_long_function.R"))
data_long <-
  ampvis_to_long_data(data, SampleID_column_name = "SampleID2")

# Make column for total count 
  tot_count <- data_long %>%  
    mutate(samples = 
             map(.x = samples, ~
                   distinct(., tot_count))
    ) %>% 
    unnest(samples) %>% select(SampleID, tot_count)
  
data_long <- data_long %>% 
    mutate(samples = 
             map(.x = samples, ~
                   select(., -tot_count))
    ) %>% 
    left_join(., tot_count, by = join_by(SampleID)) %>% relocate(tot_count, .after = "SampleDate")
  
# Save unclassified ASV
if (save_unclassified) {
  print("Saving a new unclassified species txt file (for usearch)")
  unclassified_specieds <- data_long %>% 
    sample_n(1) %>% 
    unnest(samples) %>% 
    distinct(Species, OTU) %>% 
    filter(str_detect(Species, "unclassified")) %>% 
    select(OTU) %>% 
    unlist()
  write(unclassified_specieds, file=paste0(OutputPath, "tables/", "20230623_unclassified_species.txt"))
}

## GUT: import the gut species and ASV (details is provided in the script: setup_groups_gut_species.R)
  source(paste0(WD, "scripts/functions/setup_groups_gut_species.R"))
  gut_species <- species_level_classification_gut_microbes(ampvis_data = data)
  gut_OTU <- including_unclassified_ASV_in_gut(gut_species_df = gut_species, 
                                               data_long_format = data_long)
  print(paste0(unlist(length(unique(gut_OTU$Species))), " species/ASVs are identified as gut species using the 16S databases"))

## DATAFRAMES TO LOAD 

writeLines("\n
1 -> data: Ampvis object witout controls
2 -> data_long: Data in long and nested format without control
3 -> growth_df_usearch: DF with all OTU and growth group 
4 -> gut_OTU: DF with gut OTUs and the corresponding species (including unclassified gut species from usearch) 
5 -> MIDAS_metabolic_df: Metabolic classifications from MIDAS
6 -> amp_diversity_df: Alpha diversity
7 -> amp_diversity_control_df: Alpha diversity with controls
8-> data_control: Ampvis with control
9 <- DMI_rain_df    
           ")

list(
  data, # ampvis no control
  data_long, # data long format without control
  #growth_df_usearch, # DF with all OTU and growth group 
  gut_OTU # DF with gut species 
  #MIDAS_metabolic_df, # Metabolic classifications from MIDAS
  #amp_diversity_df, # Alpha diversity 
  #amp_diversity_control_df, # Alpha diversity with controls
  #data_control,  # Ampvis with control
  #DMI_rain_df  #DMI_rain_df 
  ) 


}

