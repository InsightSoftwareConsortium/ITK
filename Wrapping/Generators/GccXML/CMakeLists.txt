###############################################################################
option(ITK_USE_SYSTEM_CASTXML "Use system castxml. If OFF, castxml is built as external project." OFF)
mark_as_advanced(ITK_USE_SYSTEM_CASTXML)
if(WIN32)
  set(exe .exe)
endif()
set(castxml_ep ${CMAKE_CURRENT_BINARY_DIR}/llvm/bin/castxml${exe})

if(ITK_USE_SYSTEM_CASTXML)
  # the path set for the EP build prevents find_program to do its job
  if("${CASTXML}" STREQUAL "${castxml_ep}")
    unset(CASTXML CACHE)
  endif()
  find_program(CASTXML castxml)
  if(NOT CASTXML)
    message(FATAL_ERROR "Could NOT find castxml. Turn ITK_USE_SYSTEM_CASTXML to OFF to build it with ITK.")
  endif()
else()
  include(ExternalProject)
  # might be set to on by default when llvm/clang 3.6 are released
  # option(ITK_USE_SYSTEM_LLVM "Use system llvm and clang. If OFF, llvm and clang are built as external projects." ON)
  set(ITK_USE_SYSTEM_LLVM OFF)
  if(ITK_USE_SYSTEM_LLVM)
    find_package(LLVM REQUIRED)
    set(castxml_deps)
  else()
    # If we are building ITK
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(LLVM)
      itk_download_attempt_check(Clang)
    endif()
    set(llvm_version 3.6.0)
    ExternalProject_Add(llvm
      URL http://midas3.kitware.com/midas/download/bitstream/454187/llvm-${llvm_version}.src.tar.gz
      URL_MD5 53dfa661317af324838b3c13c6e58770
      SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm-${llvm_version}
      CMAKE_ARGS -Wno-dev
      CMAKE_GENERATOR "${CMAKE_GENERATOR}"
      CMAKE_CACHE_ARGS
        -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
        "-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS} -w"
        -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
        "-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} -w"
        -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
      INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm
      )
    ExternalProject_Add(clang
      # This is the upstream source code repackages in a .tar.gz for
      # compatibility with older CMake. Also the tests/ and doc/ directories
      # are removed to remove symlink files and save space.
      URL http://midas3.kitware.com/midas/download/bitstream/454206/cfe-${llvm_version}.src.tar.gz
      URL_MD5 d6da7200e9ee9f956565a15242e5a5fe
      DEPENDS llvm
      SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/cfe-${llvm_version}
      CMAKE_ARGS -Wno-dev
      CMAKE_GENERATOR "${CMAKE_GENERATOR}"
      CMAKE_CACHE_ARGS
        -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
        "-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS} -w"
        -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
        "-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} -w"
        -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        -DCLANG_INCLUDE_DOCS:BOOL=OFF
        -DCLANG_INCLUDE_TESTS:BOOL=OFF
        -DLLVM_CONFIG:PATH=${CMAKE_CURRENT_BINARY_DIR}/llvm/bin/llvm-config${exe}
      INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm
      )
    set(LLVM_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm/share/llvm/cmake)
    set(castxml_deps llvm clang)
  endif()

  # If we are building ITK
  if(ITK_BINARY_DIR)
    itk_download_attempt_check(CastXML)
  endif()
  ExternalProject_Add(castxml
    GIT_REPOSITORY https://github.com/CastXML/CastXML.git
    GIT_TAG fe1bb977eef136daecec0d5717fbd7379f139e8a
    UPDATE_COMMAND ""
    DEPENDS ${castxml_deps}
    CMAKE_ARGS -Wno-dev
    CMAKE_GENERATOR "${CMAKE_GENERATOR}"
    CMAKE_CACHE_ARGS
      -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
      "-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS} -w"
      -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
      "-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} -w"
      -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
      -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
      -DLLVM_DIR:PATH=${LLVM_DIR}
    INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm
    )
  set(CASTXML ${CMAKE_CURRENT_BINARY_DIR}/llvm/bin/castxml${exe})
  set(CASTXML ${castxml_ep} CACHE FILEPATH "castxml executable." FORCE)
endif()
mark_as_advanced(CASTXML)

###############################################################################
# install the files requires for gccxml
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Generators/GccXML CMakeLists.txt)
  WRAP_ITK_INSTALL(/Configuration/Generators/GccXML gcc_xml.inc.in)
  WRAP_ITK_INSTALL(/Configuration/Generators/GccXML wrap_.cxx.in)
endif()


###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_GCCXML_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "gccxml source dir")
set(ITK_WRAP_GCCXML_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "gccxml binary dir")


set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${PROJECT_BINARY_DIR}/Typedefs")
set(WRAPPER_SWIG_LIBRARY_OUTPUT_DIR "${PROJECT_BINARY_DIR}/SWIG")



macro(itk_wrap_include_gccxml include_file)
  if("${include_file}" MATCHES "<.*>")
    set(GCC_XML_INCLUDES "${GCC_XML_INCLUDES}#include ${include_file}\n")
  else()
    set(GCC_XML_INCLUDES "${GCC_XML_INCLUDES}#include \"${include_file}\"\n")
  endif()
endmacro()


macro(itk_wrap_simple_type_gccxml wrap_class swig_name)
  set(GCC_XML_TYPEDEFS "${GCC_XML_TYPEDEFS}    typedef ${wrap_class} ${swig_name};\n")
  set(GCC_XML_FORCE_INSTANTIATE "${GCC_XML_FORCE_INSTANTIATE}    (void)sizeof(${swig_name});\n")
endmacro()


macro(itk_wrap_submodule_gccxml module)
  # clear the typedefs and the includes
  set(GCC_XML_TYPEDEFS )
  set(GCC_XML_INCLUDES )
  set(GCC_XML_FORCE_INSTANTIATE )
endmacro()

macro(itk_end_wrap_submodule_gccxml module)
  # write the wrap_*.cxx file
  #
  # Global vars used: WRAPPER_INCLUDE_FILES WRAPPER_MODULE_NAME and WRAPPER_TYPEDEFS
  # Global vars modified: none

  # Create the cxx file which will be given to gccxml.
  set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.cxx")
  configure_file("${ITK_WRAP_GCCXML_SOURCE_DIR}/wrap_.cxx.in" "${cxx_file}" @ONLY)

  # generate the xml file
  set(gccxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/gcc_xml.inc")
  set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.xml")

  set(_castxml_depends)
  if(NOT ITK_USE_SYSTEM_CASTXML)
    # ExternalProject target for GCC_XML.
    set(_castxml_depends castxml)
  endif()
  set(ccache_cmd)
  if(ITK_USE_CCACHE)
    set(_ccache_cmd ${CCACHE_EXECUTABLE})
  endif()
  add_custom_command(
    OUTPUT ${xml_file}
    COMMAND ${_ccache_cmd} ${CASTXML}
          -w
          -c # needed for ccache to think we are not calling for link
          -o ${xml_file}
          @${gccxml_inc_file}
          ${cxx_file}
          VERBATIM
    DEPENDS ${_castxml_depends} ${cxx_file} ${gccxml_inc_file}
  )

  list(APPEND GCC_XML_OUTPUT_FILES ${xml_file})

endmacro()


macro(itk_wrap_one_type_gccxml  wrap_method wrap_class swig_name template_params)
  # insert a blank line to separate the classes
  set(GCC_XML_TYPEDEFS "${GCC_XML_TYPEDEFS}\n")
  # add a piece of code for type instantiation
  set(GCC_XML_FORCE_INSTANTIATE "${GCC_XML_FORCE_INSTANTIATE}\n")
endmacro()

macro(itk_wrap_module_gccxml library_name)
  # create the files used to pass the file to include to gccxml
  set(gccxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/gcc_xml.inc")
  get_directory_property(include_dir_list INCLUDE_DIRECTORIES)
  list(REMOVE_DUPLICATES include_dir_list)

  set(c)
  foreach(dir ${include_dir_list})
    set(c "${c}-I${dir}\n")
  endforeach()
  if(MSVC)
    set(c "${c}--castxml-cc-msvc cl\n")
    if(MSVC90)
      # needed for VS2008 64 bit
      set(c "${c}-D\"_HAS_TR1=0\"\n")
    endif()
  else()
    set(c "${c}--castxml-cc-gnu ${CMAKE_CXX_COMPILER}\n")
  endif()
  set(c "${c}--castxml-gccxml\n")
  set(c "${c}--castxml-start _cable_\n")
  set(c "${c}-Qunused-arguments\n")
  set(c "${c}-DCABLE_CONFIGURATION\n")
  set(c "${c}-DITK_MANUAL_INSTANTIATION\n")

  set(CONFIG_GCCXML_INC_CONTENTS "${c}")
  configure_file("${ITK_WRAP_GCCXML_SOURCE_DIR}/gcc_xml.inc.in" "${gccxml_inc_file}" @ONLY)

  set(GCC_XML_OUTPUT_FILES )
endmacro()

macro(itk_end_wrap_module_gccxml)
  add_custom_target(${WRAPPER_LIBRARY_NAME}GccXML DEPENDS ${GCC_XML_OUTPUT_FILES})
  set(${WRAPPER_LIBRARY_NAME}XmlFiles ${GCC_XML_OUTPUT_FILES} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Xml file list.")
endmacro()
