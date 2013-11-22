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
  if(${_module_name}_LOADED)
    # note: this is an internal CMake variable and should not be used outside ITK
    set(_abi)
    if(ITK_MODULE_${_module_name}_ENABLE_SHARED AND BUILD_SHARED_LIBS)
      set(_abi "ITK_ABI_IMPORT")
    endif()
    set(${_registration_list_var}
      "${${_registration_list_var}}void ${_abi} ${_factory_name}FactoryRegister__Private(void);")
    set(${_names_list_var} "${${_names_list_var}}${_factory_name}FactoryRegister__Private,")
  endif()
endmacro()

if(NOT ITK_NO_IO_FACTORY_REGISTER_MANAGER)
  #
  # Infrastructure for registering automatically the factories of commonly used IO formats
  #

  #for Image IO
  set(LIST_OF_FACTORIES_REGISTRATION "")
  set(LIST_OF_FACTORY_NAMES "")

  foreach (ImageFormat  Nifti Nrrd Gipl HDF5 JPEG GDCM BMP LSM PNG TIFF VTK Stimulate BioRad Meta MINC SCIFIO MGH MRC)
    string(TOUPPER ${ImageFormat} ImageFormat_UPPER) ## Need to check for uppercase name as well
    ADD_FACTORY_REGISTRATION("LIST_OF_FACTORIES_REGISTRATION" "LIST_OF_FACTORY_NAMES" ITKIO${ImageFormat} ${ImageFormat}ImageIO)
    if(NOT "${ImageFormat}" STREQUAL "${ImageFormat_UPPER}")
      ADD_FACTORY_REGISTRATION("LIST_OF_FACTORIES_REGISTRATION" "LIST_OF_FACTORY_NAMES" ITKIO${ImageFormat_UPPER} ${ImageFormat}ImageIO)
    endif()
  endforeach()

  get_filename_component(_selfdir "${CMAKE_CURRENT_LIST_FILE}" PATH)
  configure_file(${_selfdir}/itkImageIOFactoryRegisterManager.h.in
   "${CMAKE_CURRENT_BINARY_DIR}/ITKIOFactoryRegistration/itkImageIOFactoryRegisterManager.h" @ONLY)
  unset(LIST_OF_FACTORIES_REGISTRATION)
  unset(LIST_OF_FACTORY_NAMES)

  # for Transform IO Template
  set(LIST_OF_FACTORIES_REGISTRATION "")
  set(LIST_OF_FACTORY_NAMES "")

  foreach (TransformFormat  Matlab Txt HDF5)
    ADD_FACTORY_REGISTRATION("LIST_OF_FACTORIES_REGISTRATION" "LIST_OF_FACTORY_NAMES"
      ITKIOTransform${TransformFormat} ${TransformFormat}TransformIO)
  endforeach()
  ADD_FACTORY_REGISTRATION("LIST_OF_FACTORIES_REGISTRATION" "LIST_OF_FACTORY_NAMES"
    ITKIOMINC MINCTransformIO)
  ADD_FACTORY_REGISTRATION("LIST_OF_FACTORIES_REGISTRATION" "LIST_OF_FACTORY_NAMES"
    ITKIOTransformInsightLegacy TxtTransformIO)

  get_filename_component(_selfdir "${CMAKE_CURRENT_LIST_FILE}" PATH)
  configure_file(${_selfdir}/itkTransformIOFactoryRegisterManager.h.in
    "${CMAKE_CURRENT_BINARY_DIR}/ITKIOFactoryRegistration/itkTransformIOFactoryRegisterManager.h" @ONLY)
  unset(LIST_OF_FACTORIES_REGISTRATION)
  unset(LIST_OF_FACTORY_NAMES)

  #-------------------
  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS ITK_IO_FACTORY_REGISTER_MANAGER)
  include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/ITKIOFactoryRegistration)

endif()
