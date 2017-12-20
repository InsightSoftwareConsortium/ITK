project(ITKKWIML)
set(ITKKWIML_THIRD_PARTY 1)

if( POLICY CMP0063 )
  cmake_policy(SET CMP0063 NEW)
endif()

if(ITK_USE_SYSTEM_KWIML)
  set(ITKKWIML_INCLUDE_DIRS
    ${ITKKWIML_BINARY_DIR}/src
    )
  set(ITKKWIML_LIBRARIES kwiml::kwiml)
  # When this module is loaded by an app, load KWIML too.
  set(ITKKWIML_EXPORT_CODE_INSTALL "
set(KWIML_DIR \"${KWIML_DIR}\")
find_package(KWIML ${KWIML_VERSION} REQUIRED)
")
  set(ITKKWIML_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(KWIML_DIR \"${KWIML_DIR}\")
  find_package(KWIML ${KWIML_VERSION} REQUIRED)
endif()
")
else()
  set(ITKKWIML_INCLUDE_DIRS
    ${ITKKWIML_BINARY_DIR}/src
    ${ITKKWIML_SOURCE_DIR}/src
    )
  set(ITKKWIML_LIBRARIES "")
endif()

configure_file(src/itk_kwiml.h.in src/itk_kwiml.h)

itk_module_impl()

install(FILES ${ITKKWIML_BINARY_DIR}/src/itk_kwiml.h
  DESTINATION ${ITKKWIML_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )
