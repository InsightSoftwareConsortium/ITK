# Set GIT tag here for use in multiple CMakeLists.txt
#
set(DCMTK_GIT_REPOSITORY "https://github.com/InsightSoftwareConsortium/DCMTK.git")
# 2024.03.11 patched
# Patch list:

 #Francois Budin (1):
       #COMP: Uses ITK PNG instead of system PNG

 #Hans Johnson (1):
       #COMP: Mods to allow using ITK libs

 #Jean-Christophe Fillion-Robin (1):
       #COMP: Set 3rd party package CMake variables only if needed

 #Matt McCormick (1):
       #Support CMAKE_CROSSCOMPILING_EMULATOR # pushed upstream in https://github.com/DCMTK/dcmtk/pull/94

 #Shreeraj Jadhav (1):
       #ENH: Replace HTML_HYPERLINK_PREFIX_FOR_CGI with std::string argument

set(DCMTK_GIT_TAG "8197ee21a2e17be65b72508dca9a91d4d60725aa") # 20240311_DCMTK_PATCHES_FOR_ITK-1
