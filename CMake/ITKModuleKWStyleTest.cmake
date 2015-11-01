# Provides itk_module_kwstyle_test macro for the internal modular api
#
# This macro adds an additional test to each module to perform KWStyle
#check on the code. Each module can provide the following file in the
#module root to override the default:
#  ITKKWStyleOverwrite.txt
#       - a KWSyle file to over write the default rules
#  ITKKWStyleFiles.txt.in
#       - a KWSyle file to be configured specifying files
#       - Lines should be of the form:
#           @ITK_MODULE_BINARY_DIR@/include/itk*.h
#         where ITK_MODULE in the name of the project specified by the
#         module.
#
# Globally the ITK.kws.xml file is used to configure the expected
#style.
option(ITK_USE_KWSTYLE "Enable the use of KWStyle for checking coding style." ${BUILD_TESTING})
mark_as_advanced(ITK_USE_KWSTYLE)
find_package(KWStyle 1.0.1 QUIET)

if(NOT KWSTYLE_FOUND AND BUILD_TESTING AND ITK_USE_KWSTYLE AND NOT CMAKE_CROSSCOMPILING)
  include(${ITK_CMAKE_DIR}/../Utilities/KWStyle/BuildKWStyle.cmake)
elseif(NOT KWSTYLE_FOUND)
  set(ITK_USE_KWSTYLE OFF)
endif()

macro(itk_module_kwstyle_test _name)

  set(_kwstyle_itk_configuration_file "${ITK_CMAKE_DIR}/../Utilities/KWStyle/ITK.kws.xml"  )

  if(EXISTS "${${itk-module}_SOURCE_DIR}/ITKKWStyleOverwrite.txt")
    set(_kwstyle_itk_overwrite_file "${${itk-module}_SOURCE_DIR}/ITKKWStyleOverwrite.txt" )
  else()
    set(_kwstyle_itk_overwrite_file "${ITK_CMAKE_DIR}/../Utilities/KWStyle/ITKOverwrite.txt" )
  endif()

  if(EXISTS "${${itk-module}_SOURCE_DIR}/ITKKWStyleFiles.txt.in")
    set(_kwstyle_itk_module_files_list_file "${${itk-module}_BINARY_DIR}/ITKKWStyleFiles.txt")
    # KWStyle requires that the files list be absolute paths
    configure_file(
      ${${itk-module}_SOURCE_DIR}/ITKKWStyleFiles.txt.in
      ${_kwstyle_itk_module_files_list_file}
      )
  else()
    set(_kwstyle_itk_module_files_list_file  "${${itk-module}_BINARY_DIR}/ITKKWStyleFiles.txt")
    set(_kwstyle_file_list "${${itk-module}_SOURCE_DIR}/include/itk*.h"
                            "${${itk-module}_SOURCE_DIR}/include/itk*.hxx"
                            "${${itk-module}_SOURCE_DIR}/src/*.cxx"
                            "${${itk-module}_SOURCE_DIR}/src/*.h"
                            "${${itk-module}_SOURCE_DIR}/src/*.hxx"
                            "${${itk-module}_SOURCE_DIR}/test/*.cxx")
    file(WRITE ${_kwstyle_itk_module_files_list_file} "")
    foreach(item ${_kwstyle_file_list})
      file(APPEND ${_kwstyle_itk_module_files_list_file} "${item}\n")
    endforeach()
  endif()

  if (NOT KWSTYLE_EXECUTABLE)
    message(WARNING "KWSTYLE_EXECUTABLE is not set!")
  else()
    itk_add_test(NAME ${itk-module}KWStyleTest
      COMMAND ${KWSTYLE_EXECUTABLE}
        -xml ${_kwstyle_itk_configuration_file}
        -v
        -o ${_kwstyle_itk_overwrite_file}
        -D ${_kwstyle_itk_module_files_list_file}
        -gcc
      WORKING_DIRECTORY ${ITK_CMAKE_DIR}/..
      )
  endif()
endmacro()
