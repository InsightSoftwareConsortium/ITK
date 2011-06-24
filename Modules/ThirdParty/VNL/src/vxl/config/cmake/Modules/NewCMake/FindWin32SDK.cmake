# Find Win32 SDK Installation on Windows

# WIN32SDK_INCLUDE_DIR   - Directories to include to use WIN32SDK
# WIN32SDK_LIBRARIES     - Files to link against to use WIN32SDK
# WIN32SDK_FOUND - Was Win32 SDK support found

# Assume no Win32 SDK support at first
SET( WIN32SDK_FOUND "NO" )

# Add Win32 SDK support if Win32 SDK installation is found
IF( WIN32 )
  FIND_PATH(WIN32SDK_INCLUDE_DIR 
    NAMES windows.h winresrc.h
    PATHS
      "c:/Program Files/Microsoft SDKs/Windows"
	  "c:/Program Files (x86)/Microsoft SDKs/Windows"
    PATH_SUFFIXES
      "v6.0a/include"
      "v6.1/include"
	  "v7.0A/include"
  )

  FIND_LIBRARY(WIN32SDK_LIBRARIES 
    NAMES kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib
    PATHS
      "c:/Program Files/Microsoft SDKs/Windows"
	  "c:/Program Files (x86)/Microsoft SDKs/Windows"
    PATH_SUFFIXES
      "v6.0a/lib"
      "v6.1/lib"
	  "v7.0A/lib"
  )
  
  IF( WIN32SDK_INCLUDE_DIR )
    IF(WIN32SDK_LIBRARIES)
      SET( WIN32SDK_FOUND "YES" )
    ENDIF(WIN32SDK_LIBRARIES)
  ENDIF( WIN32SDK_INCLUDE_DIR )

  MARK_AS_ADVANCED(
    WIN32SDK_INCLUDE_DIR
    WIN32SDK_LIBRARIES
  )
  
ENDIF( WIN32 )


