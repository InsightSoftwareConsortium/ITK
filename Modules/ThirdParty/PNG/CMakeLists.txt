project(ITK-PNG)
set(ITK-PNG_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_PNG "Use system-installed png" OFF)
mark_as_advanced(ITK_USE_SYSTEM_PNG)

if(ITK_USE_SYSTEM_PNG)
  find_package(PNG REQUIRED)
  set(ITK-PNG_INCLUDE_DIRS
    ${ITK-PNG_BINARY_DIR}/src
    )
  set(ITK-PNG_SYSTEM_INCLUDE_DIRS
    ${PNG_INCLUDE_DIRS}
    )
  set(ITK-PNG_LIBRARIES "${PNG_LIBRARIES}")
  set(ITK-PNG_NO_SRC 1)
else()
  set(ITK-PNG_INCLUDE_DIRS
    ${ITK-PNG_BINARY_DIR}/src
    ${ITK-PNG_SOURCE_DIR}/src
    )
  set(ITK-PNG_LIBRARIES itkpng)
endif()

configure_file(src/itk_png.h.in src/itk_png.h)

itk_module_impl()

install(FILES ${ITK-PNG_BINARY_DIR}/src/itk_png.h
  DESTINATION ${ITK-PNG_INSTALL_INCLUDE_DIR})
