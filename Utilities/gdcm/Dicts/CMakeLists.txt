#-----------------------------------------------------------------------------
# DICOM dictionary stuff


INSTALL_FILES(/include/InsightToolkit/gdcm/ .dic
  dicomV3
  dicomVR
  dicomTS
  DicomDir
)

# Nice trick to avoid creating a .h file each time we compile gdcm
# Since CONFIGURE_FILE do a 'copy if different'
# We have to write the file here since is contains 'DICOM_DICTIONARY'
# Which is a variable afterward...

# Following code contributing by Andy Cedilnik (Kitware)
FILE(READ "dicomV3.dic" ENT)
STRING(REGEX REPLACE "\r?\n" ";" ENT "${ENT}")
SET(DICOM_DATA_DICTIONARY "")

FOREACH(line ${ENT})
  STRING(REGEX REPLACE
    "^([0-9a-f][0-9a-f][0-9a-f][0-9a-f]) ([0-9a-f][0-9a-f][0-9a-f][0-9a-f]) ([A-Z]+) ([A-Z?]+) (.*)$"
    "   {0x\\1, 0x\\2, \"\\3\" , \"\\4\" , \"\\5\"}, " nline "${line}")
  SET(DICOM_DATA_DICTIONARY "${DICOM_DATA_DICTIONARY}\n${nline}")
ENDFOREACH(line)

SET(DICOM_DATA_DICTIONARY "${DICOM_DATA_DICTIONARY} \n {0,0,0,0,0}")

FILE(READ "dicomTS.dic" ENT_TS)
STRING(REGEX REPLACE "\r?\n" ";" ENT_TS "${ENT_TS}")
SET(DICOM_TS_DICTIONARY "")

FOREACH(line ${ENT_TS})
  STRING(REGEX REPLACE
    "^([0-9.]+) +(.*)$"
    "  ts[\"\\1\"] = \"\\2\"; " nline "${line}")
  SET(DICOM_TS_DICTIONARY "${DICOM_TS_DICTIONARY}\n${nline}")
ENDFOREACH(line)

FILE(READ "dicomVR.dic" ENT_VR)
STRING(REGEX REPLACE ";" "/" ENT_VR "${ENT_VR}") # CMake doesn't like ';'
STRING(REGEX REPLACE "\n" ";" ENT_VR "${ENT_VR}")
SET(DICOM_VR_DICTIONARY "")

FOREACH(line ${ENT_VR})
  STRING(REGEX REPLACE
    "^([A-Z][A-Z]) (.*)/ +//.*$"
    "  vr[\"\\1\"] = \"\\2\"; " nline "${line}")
  SET(DICOM_VR_DICTIONARY "${DICOM_VR_DICTIONARY}\n${nline}")
ENDFOREACH(line)

FILE(READ "DicomDir.dic" ENT_DIR)
STRING(REGEX REPLACE "\\\\" "\\\\\\\\" ENT_DIR "${ENT_DIR}")
STRING(REGEX REPLACE ";" "/" ENT_DIR "${ENT_DIR}") # CMake doesn't like ';'
STRING(REGEX REPLACE "\r?\n" ";" ENT_DIR "${ENT_DIR}")
SET(DICOM_DIR_DICTIONARY "")

FOREACH(line ${ENT_DIR})
  STRING(REGEX REPLACE
    "^(metaElem|patientElem|studyElem|serieElem|imageElem) *([a-f0-9]+) ([a-f0-9]+) [\"](.*)[\"](.*)$"
    "  {\"\\1\" , 0x\\2 , 0x\\3 , \"\\4\"}, \\5" nline "${line}")
  SET(DICOM_DIR_DICTIONARY "${DICOM_DIR_DICTIONARY}\n${nline}")
ENDFOREACH(line)

SET(DICOM_DIR_DICTIONARY "${DICOM_DIR_DICTIONARY} \n {0,0,0,0}")

# See above
FOREACH(file dicomV3.dic  dicomTS.dic dicomVR.dic DicomDir.dic)
  CONFIGURE_FILE("${GDCM_SOURCE_DIR}/Dicts/${file}"
    "${GDCM_BINARY_DIR}/Dicts/${file}.out")
ENDFOREACH(file)
CONFIGURE_FILE("${GDCM_SOURCE_DIR}/src/gdcmDefaultDicts.cxx.in" "${GDCM_BINARY_DIR}/src/gdcmDefaultDicts.cxx" IMMEDIATE)

