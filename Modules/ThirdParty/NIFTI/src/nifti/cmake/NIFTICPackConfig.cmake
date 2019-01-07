####################################
### Define information necessary for packaging with CPACK (https://gitlab.kitware.com/cmake/community/wikis/home#cpack)
### The last section is concerned with installing the binaries and making distributions.

set_if_not_defined(CPACK_SOURCE_GENERATOR "TGZ;TZ;STGZ;TBZ2;ZIP")

include(InstallRequiredSystemLibraries)

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${NIFTI_PROJECT_DESCRIPTION}")
set(CPACK_PACKAGE_VENDOR "NIFTI DFWG")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${NIFTI_SOURCE_DIR}/README")

set(CPACK_RESOURCE_FILE_LICENSE "${NIFTI_SOURCE_DIR}/README")

set(CPACK_PACKAGE_NAME "${PROJECT_NAME}")
set(CPACK_PACKAGE_VERSION_MAJOR "${NIFTI_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${NIFTI_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${NIFTI_VERSION_PATCH}")

set(CPACK_PACKAGE_INSTALL_DIRECTORY "NIFTI_${NIFTI_VERSION_MAJOR}.${NIFTI_VERSION_MINOR}.${NIFTI_VERSION_PATCH}")

if(WIN32 AND NOT UNIX)
  set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY
      "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  set(CPACK_PACKAGE_ICON
      "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
  set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\MyExecutable.exe")
  set(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} NIFTI Project")
  set(CPACK_NSIS_HELP_LINK "https:\\\\\\\\www.nitrc.org")
  set(CPACK_NSIS_URL_INFO_ABOUT "https:\\\\\\\\www.nitrc.org")
  set(CPACK_NSIS_CONTACT "xyz@domain.edu")
  set(CPACK_NSIS_MODIFY_PATH ON)
else()
  set(CPACK_STRIP_FILES OFF)
  set(CPACK_SOURCE_STRIP_FILES OFF)
endif()
set(CPACK_PACKAGE_EXECUTABLES "nifti_tool;NIFTI")

set(CPACK_PACKAGING_INSTALL_PREFIX ".")

set(CPACK_SOURCE_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-dev")
set(CPACK_PACKAGE_DEFAULT_LOCATION "/opt/${CPACK_PACKAGE_NAME}")
set(CPACK_SET_DESTDIR ON)

set(CPACK_SOURCE_IGNORE_FILES
        "/.git/"
        ".gitignore$"
        ".*.swp$"
        ".*~"
        "Makefile\\\\.in$"
)

include(CPack)
