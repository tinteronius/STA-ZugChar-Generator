# # # # # # # # # # # # # #
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #

RESULT_VERSION = '112'

# Configuration Of *relative* paths

STAMMDATEN_FILEPATH = "2013-Stammdaten_ST-STTriebfahrzeugbaureihen121-komplett.xml"
FAHRLAGEN_FILEPATH = "./2013_Fahrlagen/Fahrlagen_14.11.2013_final_v11.csv"
FINVEBTS_FILEPATH = "./2013_Fahrlagen/FinVeBTS.csv"

# Configuration for parallel computing

NUMBER_OF_CORES = 7 # Note: It is strongly recommended to keep at least one core unccupied, since RStudio and OS

# Check if everything works by including the heart script:

source("Helper.R")

# # # # # # # # # # # # # #
# 1 # Mapping Fahrlagen -> STA
# # #

# Input:
#STA_FOLDER = "../ZugChar_Untersuchungen/STAs_/"
WOLKEN_FILEPATH = "2013_Fahrlagen/WolkenBST.csv"
STA_FOLDER = "./2013_Fahrlagen/ERIKA_STA_181025"
STA_MAPPING_FILE = "./2013_Fahrlagen/mapping_STAaltneu.csv"
# Output:
FAHRLAGEN_STAFIT_FILEPATH = "Fahrlagen_14.11.2013_final_v12_STAFIT.csv"
BTS2STA_FILEPATH = "bts2sta.csv"
#STAGROUPS_FILEPATH = "STA_GROUPS_tweaked200.csv"
STAGROUPS_FILEPATH = "STAGROUPS_v08.csv"
STA_RESULT_FOLDER = "STAs/"
# Options:
DO_OVERLAPPING = F # overlapping currently bugged
DO_STAFIT = T
DO_MAPPING_STA_BTS = T

# Execute:
source("newA-V-MitLaufwegen.R")


# # # # # # # # # # # # # #
# 2 #
# # #

# Output:
TEMP_TFZ_FRAME_FILEPATH = "TFZ_Frame.csv"
# Execute:
source("analyzeTFZ+WEIGHT.R")

# # # # # # # # # # # # # #
# 3 # bottomUp_new_a(v)
# # #

TFZ_LIST_FOR_A_FRAME_FILEPATH = "TFZ_Frame_for_a_frames.csv"
BOTTOMUP_RESULT_FOLDER = "all90/"
A_FRAME_RESULT_FOLDER = "a_frame/"

source("bottomUp_new_a(v).R")

# # # # # # # # # # # # # #
# 4a # setCoveringOptimizer
# # ##

BOTTOMUP_REDUCED_RESULT_FOLDER = "all90/reduced/"
COVERING_RESULT_FOLDER = "bottomup/merge_av/"
SELECTION_RESULT_FOLDER = "SelectedFiles_Opti_v01.csv"

source("setCoveringOptimizer.R")

# # # # # # # # # # # # # #
# 4b # Combining two 90% Characteristics
# # ##

# coming soon

# # # # # # # # # # # # # #
# 5 # GainMaximizer.R
# # #
OPTIMIZATION_RESULT_FOLDER = "bottomup/merge_a(v)_v12/optimizedTrains/"
source("GainMaximizer.R")

# # # # # # # # # # # # # #
# 6 # SystemtrassenAssigner.R
# # #

source("SystemtrassenAssigner.R")

# # # # # # # # # # # # # #
# 7 # Tagesgang.R
# # #

CHOSEN_REM = "13" # String!
source("Tagesgang.R")
