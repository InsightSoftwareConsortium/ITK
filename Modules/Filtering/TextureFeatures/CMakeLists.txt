project(TextureFeatures)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK 4.10 REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})

  if(SKBUILD)
    set(
      PY_SITE_PACKAGES_PATH
      ${CMAKE_INSTALL_PREFIX}
      CACHE PATH
      "The install
    prefix for python package contents"
    )
    install(
      CODE
        "
      unset(CMAKE_INSTALL_COMPONENT)
      set(COMPONENT \"PythonWheelRuntimeLibraries\")
      set(CMAKE_INSTALL_DO_STRIP 1)
      include\(\"${PROJECT_BINARY_DIR}/cmake_install.cmake\")
      unset(CMAKE_INSTALL_COMPONENT)
      return()
    "
    )
  endif()

  include(ITKModuleExternal)
  include_directories(${ITK_INCLUDE_DIRS})
else()
  itk_module_impl()
endif()
