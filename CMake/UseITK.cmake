# This file sets up include directories, link directories, and
# compiler settings for a project to use ITK.  It should not be
# included directly, but rather through the ITK_USE_FILE setting
# obtained from ITKConfig.cmake.

# Add compiler flags needed to use ITK.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ITK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ITK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${ITK_REQUIRED_LINK_FLAGS}")


# Add include directories needed to use ITK.
include_directories(BEFORE ${ITK_INCLUDE_DIRS})

# Add link directories needed to use ITK.
link_directories(${ITK_LIBRARY_DIRS})

macro(ADD_FACTORY_REGISTRATION _registration_list_var _names_list_var _module_name _factory_name)
  list(FIND ITK_MODULES_REQUESTED ${_module_name} _module_was_requested)
  if(NOT ${_module_was_requested} EQUAL -1)
    # note: this is an internal CMake variable and should not be used outside ITK
    set(_abi)
    if(${_module_name}_ENABLE_SHARED AND BUILD_SHARED_LIBS)
      set(_abi "ITK_ABI_IMPORT")
    endif()
    set(${_registration_list_var}
      "${${_registration_list_var}}void ${_abi} ${_factory_name}FactoryRegister__Private();")
    set(${_names_list_var} "${${_names_list_var}}${_factory_name}FactoryRegister__Private,")
  endif()
endmacro()

function(_configure_IOFactoryRegisterManager factory_type formats)
  set(LIST_OF_FACTORIES_REGISTRATION "")
  set(LIST_OF_FACTORY_NAMES "")

  string(TOLOWER ${factory_type} factory_type_lc)
  set(_qualifier "_${factory_type_lc}")

  # See below for explanation.
  if("${factory_type}" STREQUAL "Image")
    set(_qualifier "")
  endif()

  foreach (format ${formats})
    set(_module_name ${${format}${_qualifier}_module_name})
    set(_factory_name ${${format}${_qualifier}_factory_name})
    ADD_FACTORY_REGISTRATION("LIST_OF_FACTORIES_REGISTRATION" "LIST_OF_FACTORY_NAMES"
      ${_module_name} ${_factory_name})
  endforeach()

  get_filename_component(_selfdir "${CMAKE_CURRENT_LIST_FILE}" PATH)
  configure_file(${_selfdir}/itk${factory_type}IOFactoryRegisterManager.h.in
   "${CMAKE_CURRENT_BINARY_DIR}/ITKIOFactoryRegistration/itk${factory_type}IOFactoryRegisterManager.h" @ONLY)

endfunction()

#
# Infrastructure for registering automatically the factories of commonly used IO formats
#

# Set each IO format's module name and factory name
# Most IO modules have consistent string characters between their module names
# and their factory class names, except the one listed below.

#-----------------------------------------------------------------------------
# ImageIO
#-----------------------------------------------------------------------------

# a list of image IOs to be registered when the corresponding modules are enabled
set(LIST_OF_IMAGEIO_FORMATS
    Nifti Nrrd Gipl HDF5 JPEG GDCM BMP LSM PNG TIFF VTK Stimulate BioRad Meta MRC GE4 GE5
    MINC
    MGH SCIFIO FDF
    PhilipsREC
    )

# For backward compatibility, ImageIO exceptions are set as
# "<format>_(module|factory)_name" instead of "<format>_image_(module|factory)_name".

# Exceptions:

set(Nifti_module_name  ITKIONIFTI)

set(Nrrd_module_name ITKIONRRD)

set(Gipl_module_name ITKIOGIPL)

set(MGH_module_name MGHIO)

set(GE4_module_name ITKIOGE)
set(GE5_module_name ITKIOGE)

set(SCIFIO_module_name SCIFIO)

set(FDF_module_name IOFDF)

foreach(ImageFormat ${LIST_OF_IMAGEIO_FORMATS})
  if (NOT ${ImageFormat}_module_name )
     set(${ImageFormat}_module_name ITKIO${ImageFormat})
  endif()
  if (NOT ${ImageFormat}_factory_name)
     set(${ImageFormat}_factory_name ${ImageFormat}ImageIO)
  endif()
endforeach()

if(NOT ITK_NO_IO_FACTORY_REGISTER_MANAGER)
  _configure_IOFactoryRegisterManager("Image" "${LIST_OF_IMAGEIO_FORMATS}")
endif()

#-----------------------------------------------------------------------------
# TransformIO
#-----------------------------------------------------------------------------

# a list of transform IOs to be registered when the corresponding modules are enabled
set(LIST_OF_TRANSFORMIO_FORMATS
  HDF5
  Matlab
  MINC
  Txt
  )

# Exceptions:

set(Txt_transform_module_name ITKIOTransformInsightLegacy)
set(Txt_transform_factory_name TxtTransformIO)

foreach(TransformFormat ${LIST_OF_TRANSFORMIO_FORMATS})
  if (NOT ${TransformFormat}_transform_module_name )
    set(${TransformFormat}_transform_module_name ITKIOTransform${TransformFormat})
  endif()
  if (NOT ${TransformFormat}_transform_factory_name)
    set(${TransformFormat}_transform_factory_name ${TransformFormat}TransformIO)
  endif()
endforeach()

if(NOT ITK_NO_IO_FACTORY_REGISTER_MANAGER)
  _configure_IOFactoryRegisterManager("Transform" "${LIST_OF_TRANSFORMIO_FORMATS}")
endif()


#-----------------------------------------------------------------------------
if(NOT ITK_NO_IO_FACTORY_REGISTER_MANAGER)

  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS ITK_IO_FACTORY_REGISTER_MANAGER)
  include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/ITKIOFactoryRegistration)

endif()
