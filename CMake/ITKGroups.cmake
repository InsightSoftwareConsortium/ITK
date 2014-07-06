# Set a list of group names
set(group_list
  Core
  IO
  Filtering
  Registration
  Segmentation
  Numerics
  Video
  ThirdParty
  Bridge
  Nonunit
  Compatibility
  Remote
  )

set(Core_documentation "This group of modules contain the toolkit framework used
by other modules.  There are common base classes for data objects and process
objects, basic data structures such as Image, Mesh, QuadEdgeMesh, and
SpatialObjects, and common functionality for operations such as finite
differences, image adaptors, or image transforms.")

set(Compatibility_documentation "This group contains modules that ease the transition to ITKv4 and Deprecated classes.")

set(IO_documentation "This group of modules contains classes for reading and
writing images and other data objects.")

set(Filtering_documentation "This group of modules are filters that modify data
in the ITK pipeline framework.  These filters take an input object, such as an
Image, and modify it to create an output.  Filters can be chained together to
create a processing pipeline.")

set(Registration_documentation "This group of modules address the registration
problem: find the spatial transformation between two images.  This is a high
level group that makes use of many lower level modules such as \\ref
ITKTransform, \\ref ITKOptimizers, \\ref ITKFiniteDifference, and \\ref
ITKFEM.")

set(Segmentation_documentation "This group of modules address the segmentation
problem: partition the image into classified regions (labels).  This is a high
level group that makes use of many lower level modules such as \\ref
ITKQuadEdgeMesh and \\ref ITKNarrowBand.")

set(Numerics_documentation "This group of modules are basic numerical tools and
algorithms that have general applications outside of imaging.")

set(Video_documentation "This group of modules handles input, output and processing
of static and real-time data with temporal components. It also handles communications
to OpenCV and VXL external libraires.")

set(Bridge_documentation "This group of modules are intended to bridge ITK to
other toolkits as libraries such as visualization toolkits.")

set(ThirdParty_documentation "This group of modules are third party libraries
used by other ITK modules.")

set(Nonunit_documentation "This group of modules are intended to make use of an
extensive set of the toolkit modules.")

set(Remote_documentation "This group of modules is for ITK based code that have
additional third-party dependencies not bundled with the toolkit,
new algorithms or implementations seeking greater exposure and adoption,
algorithms that hope to eventually be integrated into the toolkit,
niche algorithms with limited application, and Modules in progress that do not
yet have the test coverage and cross-platform standards required by the main toolkit.
The modules are OFF by default in ITK's CMake configuration.
Note that these modules do get the same level of support and backwards
compatibility as other modules in the toolkit.")

#------------------------------------------------
# Find the modules in each group and the module name line in itk-module.cmake
foreach( group ${group_list} )
  set( _${group}_module_list )
  file( GLOB_RECURSE _${group}_module_files ${ITK_SOURCE_DIR}/Modules/${group}/itk-module.cmake )
  foreach( _module_file ${_${group}_module_files} )
    file( STRINGS ${_module_file} _module_line REGEX "itk_module[ \n]*\\([ \n]*[A-Za-z0-9]*" )
    string( REGEX MATCH "(\\([ \n]*)([A-Za-z0-9]*)" _module_name ${_module_line} )
    set( _module_name ${CMAKE_MATCH_2} )
    set( _${_module_name}_module_line ${_module_line} )
    list( APPEND _${group}_module_list ${_module_name} )
  endforeach()
endforeach()

#------------------------------------------------
# Set up Doxygen Group descriptions

set( group_list_dox )
foreach(group ${group_list} )
  set( group_list_dox
"${group_list_dox}
// -----------------------------------------------
// Group ${group}
/** \\defgroup Group-${group} Group ${group}
${${group}_documentation} */\n"
    )

  foreach(mod ${_${group}_module_list} )
    set( group_list_dox
"${group_list_dox}
/** \\defgroup ${mod} Module ${mod}
\\ingroup Group-${group} */\n"
      )
  endforeach()
endforeach()

set( _content ${group_list_dox} )
configure_file(
  "${ITK_SOURCE_DIR}/Utilities/Doxygen/Module.dox.in"
  "${ITK_BINARY_DIR}/Utilities/Doxygen/Modules/ITK-AllGroups.dox"
  )

#------------------------------------------------
# Turn on the ITK_BUILD option for each group

# Set a module name list for each group and exclude
# Modules that should be OFF
foreach( group ${group_list} )
  set( _${group}_on_module_list )
  list( LENGTH _${group}_module_list _num_modules )
  set( _current_module 0 )
  while( ${_current_module} LESS ${_num_modules} )
    list( GET _${group}_module_list ${_current_module} _module_name )
    if( NOT ITK_MODULE_${_module_name}_EXCLUDE_FROM_DEFAULT )
      list( APPEND _${group}_on_module_list ${_module_name} )
    endif()
  math( EXPR _current_module "${_current_module} + 1" )
  endwhile()
endforeach()

if("$ENV{DASHBOARD_TEST_FROM_CTEST}" STREQUAL "")
  # developer build
  option(ITKGroup_Core "Request building core modules" ON)
endif()
foreach( group ${group_list})
    option(ITKGroup_${group} "Request building ${group} modules" OFF)
    if (ITKGroup_${group})
      foreach (itk-module ${_${group}_on_module_list} )
         list(APPEND ITK_MODULE_${itk-module}_REQUEST_BY ITKGroup_${group})
      endforeach()
    endif()
    # Hide group options if building all modules anyway.
    if(ITK_BUILD_DEFAULT_MODULES)
      set_property(CACHE ITKGroup_${group} PROPERTY TYPE INTERNAL)
    else()
      set_property(CACHE ITKGroup_${group} PROPERTY TYPE BOOL)
    endif()
endforeach()
