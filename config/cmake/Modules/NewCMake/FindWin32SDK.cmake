# Find Win32 SDK Installation on Windows

# WIN32SDK_INCLUDE_DIR   - Directories to include to use WIN32SDK
# WIN32SDK_LIBRARIES     - Files to link against to use WIN32SDK
# WIN32SDK_FOUND - Was Win32 SDK support found

# Assume no Win32 SDK support at first
set( WIN32SDK_FOUND "NO" )

# Add Win32 SDK support if Win32 SDK installation is found
if( WIN32 )
  find_path(WIN32SDK_INCLUDE_DIR
    NAMES windows.h winresrc.h
    PATHS
      "c:/Program Files/Microsoft SDKs/Windows"
  "c:/Program Files (x86)/Microsoft SDKs/Windows"
  "C:/Program Files (x86)/Windows Kits/"
    PATH_SUFFIXES
      "v6.0a/include"
      "v6.1/include"
  "v7.0A/include"
       "v7.1A/include"
       "8.0/Include/um"
)

  find_library(WIN32SDK_LIBRARIES
    NAMES kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib
    PATHS
      "c:/Program Files/Microsoft SDKs/Windows"
  "c:/Program Files (x86)/Microsoft SDKs/Windows"
    "C:/Program Files (x86)/Windows Kits/"
    PATH_SUFFIXES
      "v6.0a/lib"
      "v6.1/lib"
  "v7.0A/lib"
        "v7.1A/Lib"
        "8.0/Lib/win8/um/x86"

)

  if( WIN32SDK_INCLUDE_DIR )
    if(WIN32SDK_LIBRARIES)
      set( WIN32SDK_FOUND "YES" )
    endif()
  endif()

  mark_as_advanced(
    WIN32SDK_INCLUDE_DIR
    WIN32SDK_LIBRARIES
  )

endif()


