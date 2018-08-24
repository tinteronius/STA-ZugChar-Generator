# # # # # # # # # # # # # # 
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #

RESULT_VERSION = '100'

# Configuration Of *relative* paths

STAMMDATEN_FILEPATH = "2013-Stammdaten_ST-STTriebfahrzeugbaureihen121-komplett.xml"
FAHRLAGEN_FILEPATH = "./2013_Fahrlagen/Fahrlagen_14.11.2013_final_v07_STAFIT.csv"
FINVEBTS_FILEPATH = "./2013_Fahrlagen/FinVeBTS.csv"

# Check if everything works by including the heart script:

source("Helper.R")
source("a-v-calculations.R")

# # # # # # # # # # # # # # 
# 1 # Mapping Fahrlagen -> STA
# # #

# Input:
STA_FOLDER = "./2013_Fahrlagen/ERIKA_STA"
# Output:
FAHRLAGEN_STAFIT_FILEPATH = "Fahrlagen_14.11.2013_final_v07_STAFIT.csv"
BTS2STA_FILEPATH = "bts2sta.csv"
STAGROUPS_FILEPATH = "STAGROUPS_v06.csv"
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

source("bottomUp_new_a(v).R")