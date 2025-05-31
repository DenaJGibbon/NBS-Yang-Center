# Load required libraries
library(stringr)
library(tuneR)
library(seewave)

n.segments <- 300
min.confidence <- 0.1
newfilepath <- '/Volumes/FC HDD 8/NBS PAM-KSP_PELANDUK BADAK/01-Full Scale Deploy Maintenance_20230516-20230927/Rugged SwiftOne_Pelanduk Badak_20230415-20231101/'

# Read in selection tables ------------------------------------------------
FolderList <- '/Volumes/FC HDD 8/BirdNET Results/BirdNET_KSP Species_list (241218)/01-Full Scale_Deploy Maintenance_20230516-20230927/'

KSPFolders <- list.files(FolderList, full.names=T)

for(z in 1:length(KSPFolders)){

KSPFiles <- list.files(KSPFolders[z],
           full.names = T,pattern = '.txt',recursive = T)

KSPFilesCombined <- data.frame()

for(a in 1:length(KSPFiles) ){
  print(a)
  TempFile <- read.delim(KSPFiles[a])
  TempName <- basename(KSPFiles[a])
  TempName <-str_split_fixed(TempName,pattern='.BirdNET',n=2)[,1]
  TempFile$TempName <- TempName
  KSPFilesCombined <- rbind.data.frame(KSPFilesCombined,TempFile)
}

write.csv(KSPFilesCombined,paste('data/',basename(KSPFolders[z]), '_KSPFilesCombined.csv'),row.names = F)

}


# Code to segment ---------------------------------------------------------
# NEED TO ADD LOOP OVER ALL LOCATION CSVS
UniqueSpecies <- unique(KSPFilesCombined$Common.Name)

for(b in 1:length(UniqueSpecies)){

# Define output directory for WAV files
output_dir <- paste("data/segments/",UniqueSpecies[b], '/',sep='')

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


KSPFilesCombinedSubset <- subset(KSPFilesCombined,Common.Name == UniqueSpecies[b] &
                                 Confidence >= min.confidence)

random.sample <- sample( 1:nrow(KSPFilesCombinedSubset),n.segments)

# Loop through each selection table
for (x in random.sample ) {

  # Read selection table
  tempselection <-KSPFilesCombinedSubset[x,]
  TempPath <- tempselection$`Begin.Path`
  FilePath <- str_split_fixed(TempPath,'E:/Test Folder',n=2)[,2]

  TempPath <- str_replace_all(FilePath, pattern = "\\\\", replacement = "//")
  TempPathFull <-paste(newfilepath,TempPath,sep='/')
  paste('processing', basename(TempPathFull))

  # Process each Begin.Path

  output_file <- "temp.wav"
  # Windows specifics
  # exe <- "C:/Users/djc426/Downloads/flac-1.5.0-win/flac-1.5.0-win/Win64/flac.exe"
  #
  # e <- system(paste(shQuote(exe), "-d", shQuote(TempPath, type = "cmd"),
  #                   "-o", shQuote(output_file, type = "cmd"),sep=" "),
  #             ignore.stderr = TRUE)

  # Mac specific
  system(paste(
    "flac", "-d", shQuote(TempPathFull), "-o", shQuote(output_file), "-f"
  ))


  TempWav <- readWave(output_file)

  ShortWav <-  cutw(TempWav,
                    from= as.numeric(tempselection$`Begin.Time..s.`) - 2  ,
                    to= as.numeric(tempselection$`End.Time..s.` )+2,
                    output = 'Wave')

  ShortWavName <- paste(output_dir,tempselection$Confidence,'_',
                        tempselection$Species.Code,'_',
                        tempselection$Begin.Time..s.,'_',
                        str_replace(basename(TempPath), '.flac', '.wav'),
                        sep='')

  writeWave(ShortWav,ShortWavName,extensible = FALSE)

  # Remove existing WAV file if it already exists
  if (file.exists(output_file)) {
    file.remove(output_file)
  }


}

}


