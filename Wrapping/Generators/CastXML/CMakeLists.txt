###############################################################################
option(ITK_USE_SYSTEM_CASTXML "Use system castxml. If OFF, castxml is built as external project." OFF)
mark_as_advanced(ITK_USE_SYSTEM_CASTXML)
if(WIN32)
  set(exe .exe)
endif()
set(castxml_ep ${CMAKE_CURRENT_BINARY_DIR}/castxml/bin/castxml${exe})

if(ITK_USE_SYSTEM_CASTXML)
  # the path set for the EP build prevents find_program to do its job
  if("${CASTXML_EXECUTABLE}" STREQUAL "${castxml_ep}")
    unset(CASTXML_EXECUTABLE CACHE)
  endif()
  find_program(CASTXML_EXECUTABLE castxml)
  if(NOT CASTXML_EXECUTABLE)
    message(FATAL_ERROR "Could NOT find castxml. Turn ITK_USE_SYSTEM_CASTXML to OFF to build it with ITK.")
  endif()
else()
  include(ExternalProject)

  set(_download_castxml_binaries 0)
  set(_castxml_url )
  set(_castxml_hash )

  # If 64 bit Linux build host, use the CastXML binary
  if(CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64")

    set(_castxml_hash 84c2851a4df419b5834114bbfafa8d3e)
    set(_castxml_url "https://midas3.kitware.com/midas/api/rest?method=midas.bitstream.download&checksum=${_castxml_hash}&name=castxml-linux.tar.gz")
    set(_download_castxml_binaries 1)

  # If 64 bit Windows build host, use the CastXML binary
  elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Windows" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "AMD64")

    set(_castxml_hash b862fcc384b496c0d9a39dea0386454a)
    set(_castxml_url https://midas3.kitware.com/midas/download/bitstream/460537/castxml-windows.zip)
    set(_castxml_url "https://midas3.kitware.com/midas/api/rest?method=midas.bitstream.download&checksum=${_castxml_hash}&name=castxml-windows.zip")
    set(_download_castxml_binaries 1)

  # If 64 bit Mac OS X build host ( >= 10.9, Mavericks), use the CastXML binary
  elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64" AND (NOT CMAKE_HOST_SYSTEM_VERSION VERSION_LESS "13.0.0"))

    set(_castxml_hash e784aa985f59f57f4d71339bd60e27a8)
    set(_castxml_url "https://midas3.kitware.com/midas/api/rest?method=midas.bitstream.download&checksum=${_castxml_hash}&name=castxml-macosx.tar.gz")
    set(_download_castxml_binaries 1)

  endif()

  # Download binaries
  if(_download_castxml_binaries AND NOT TARGET castxml)
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(CastXML)
    endif()
    ExternalProject_Add(castxml
      URL ${_castxml_url}
      URL_MD5 ${_castxml_hash}
      CONFIGURE_COMMAND ""
      BUILD_COMMAND ""
      INSTALL_COMMAND "${CMAKE_COMMAND}" -E copy_directory "${CMAKE_CURRENT_BINARY_DIR}/castxml-prefix/src/castxml" "${CMAKE_CURRENT_BINARY_DIR}/castxml"
      )
    set(CASTXML_EXECUTABLE ${castxml_ep})
    set(CASTXML_EXECUTABLE ${castxml_ep} CACHE FILEPATH "castxml executable." FORCE)
  # Build from source
  elseif(NOT TARGET castxml)
    set(compiler_information)
    if(NOT CMAKE_CROSSCOMPILING)
      if(CMAKE_VERSION VERSION_LESS 3.4)
        set(CMAKE_CXX_COMPILER_LAUNCHER_FLAG )
        set(CMAKE_C_COMPILER_LAUNCHER_FLAG )
      else()
        set(CMAKE_CXX_COMPILER_LAUNCHER_FLAG -DCMAKE_CXX_COMPILER_LAUNCHER:FILEPATH=${CMAKE_CXX_COMPILER_LAUNCHER})
        set(CMAKE_C_COMPILER_LAUNCHER_FLAG -DCMAKE_C_COMPILER_LAUNCHER:FILEPATH=${CMAKE_C_COMPILER_LAUNCHER})
      endif()
      set(compiler_information
            -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
            ${CMAKE_CXX_COMPILER_LAUNCHER_FLAG}
            "-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS} -w"
            -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
            ${CMAKE_C_COMPILER_LAUNCHER_FLAG}
            "-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} -w"
          )
    endif()
    # might be set to on by default when llvm/clang 3.6 are released
    # option(ITK_USE_SYSTEM_LLVM "Use system llvm and clang. If OFF, llvm and clang are built as external projects." ON)
    set(ITK_USE_SYSTEM_LLVM OFF)
    if(ITK_USE_SYSTEM_LLVM)
      find_package(LLVM REQUIRED)
      set(castxml_deps)
    else()
      # check the version of clang compiler
      if(APPLE AND (((CMAKE_CXX_COMPILER_ID STREQUAL "Clang") AND CMAKE_CXX_COMPILER_VERSION  VERSION_LESS "3.2") OR
        ((CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang") AND CMAKE_CXX_COMPILER_VERSION  VERSION_LESS "5.0")))
        message(FATAL_ERROR "Mac OS X 10.9 (Mavericks) or newer is required to wrap ITK.")
      endif()
      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(LLVM)
        itk_download_attempt_check(Clang)
      endif()
      set(llvm_version 5.0.0)
      set(llvm_md5 e3782305ff4a60ad9bbc3a926425786f)
      ExternalProject_Add(llvm
        URL https://midas3.kitware.com/midas/api/rest?method=midas.bitstream.download&checksum=${llvm_md5}&name=llvm-${llvm_version}.src.tar.gz
        URL_MD5 ${llvm_md5}
        SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm-${llvm_version}
        CMAKE_ARGS -Wno-dev
        CMAKE_GENERATOR "${CMAKE_GENERATOR}"
        CMAKE_CACHE_ARGS
          ${compiler_information}
          -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
          -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
          -DLLVM_ENABLE_TERMINFO:BOOL=OFF
          -DLLVM_INCLUDE_TESTS:BOOL=OFF
          -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF
          -DLLVM_INCLUDE_DOCS:BOOL=OFF
        INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm
        )
      set(clang_md5 79eddb9b1f6e6ee8e9eeff1245d3f359)
      ExternalProject_Add(clang
        # This is the upstream source code repackages in a .tar.gz for
        # compatibility with older CMake. Also the tests/ and doc/ directories
        # are removed to remove symlink files and save space.
        URL https://midas3.kitware.com/midas/api/rest?method=midas.bitstream.download&checksum=${clang_md5}&name=cfe-${llvm_version}.src.tar.gz
        URL_MD5 ${clang_md5}
        DEPENDS llvm
        SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/cfe-${llvm_version}
        CMAKE_ARGS -Wno-dev
        CMAKE_GENERATOR "${CMAKE_GENERATOR}"
        CMAKE_CACHE_ARGS
          ${compiler_information}
          -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
          -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
          -DCLANG_INCLUDE_DOCS:BOOL=OFF
          -DCLANG_INCLUDE_TESTS:BOOL=OFF
          -DLLVM_CONFIG:PATH=${CMAKE_CURRENT_BINARY_DIR}/llvm/bin/llvm-config${exe}
        INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm
        )
      set(LLVM_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm/lib/cmake/llvm/)
      set(castxml_deps llvm clang)
    endif()

    # If we are building ITK
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(CastXML)
    endif()
    ExternalProject_Add(castxml
      GIT_REPOSITORY ${git_protocol}://github.com/CastXML/CastXML.git
      # CastXML master, 2017-11-06
      GIT_TAG 3a81f3cc3060be86064ba2e666d30b0c358cc847
      UPDATE_COMMAND ""
      DEPENDS ${castxml_deps}
      CMAKE_ARGS -Wno-dev
      CMAKE_GENERATOR "${CMAKE_GENERATOR}"
      CMAKE_CACHE_ARGS
        ${compiler_information}
        -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        -DLLVM_DIR:PATH=${LLVM_DIR}
      INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/castxml
      )
    set(CASTXML_EXECUTABLE ${castxml_ep})
    set(CASTXML_EXECUTABLE ${castxml_ep} CACHE FILEPATH "castxml executable." FORCE)
  endif()
endif()
mark_as_advanced(CASTXML_EXECUTABLE)

###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_CASTXML_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "castxml source dir")


macro(itk_wrap_include_castxml include_file)
  if("${include_file}" MATCHES "<.*>")
    set(CASTXML_INCLUDES "${CASTXML_INCLUDES}#include ${include_file}\n")
  else()
    set(CASTXML_INCLUDES "${CASTXML_INCLUDES}#include \"${include_file}\"\n")
  endif()
endmacro()


macro(itk_wrap_simple_type_castxml wrap_class swig_name)
  set(CASTXML_TYPEDEFS "${CASTXML_TYPEDEFS}    typedef ${wrap_class} ${swig_name};\n")
  set(CASTXML_FORCE_INSTANTIATE "${CASTXML_FORCE_INSTANTIATE}    (void)sizeof(${swig_name});\n")
endmacro()


macro(itk_wrap_submodule_castxml module)
  # clear the typedefs and the includes
  set(CASTXML_TYPEDEFS )
  set(CASTXML_INCLUDES )
  set(CASTXML_FORCE_INSTANTIATE )
endmacro()

macro(itk_end_wrap_submodule_castxml module)
  # write the wrap_*.cxx file
  #
  # Global vars used: WRAPPER_INCLUDE_FILES WRAPPER_MODULE_NAME and WRAPPER_TYPEDEFS
  # Global vars modified: none

  # Create the cxx file which will be given to castxml.
  set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.cxx")
  configure_file("${ITK_WRAP_CASTXML_SOURCE_DIR}/wrap_.cxx.in" "${cxx_file}" @ONLY)

  # generate the xml file
  set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.xml")

  set(_castxml_depends)
  if(NOT ITK_USE_SYSTEM_CASTXML)
    # ExternalProject target for CastXML.
    set(_castxml_depends castxml)
  endif()
  set(ccache_cmd)
  if(ITK_USE_CCACHE)
    set(_ccache_cmd ${CCACHE_EXECUTABLE})
  endif()
  set(_castxml_cc_flags ${CMAKE_CXX_FLAGS})
  separate_arguments(_castxml_cc_flags)
  if(MSVC)
    set(_castxml_cc --castxml-cc-msvc ( "${CMAKE_CXX_COMPILER}" ${_castxml_cc_flags} ) -fexceptions)
    if(MSVC90)
      # needed for VS2008 64 bit
      set(_castxml_cc ${_castxml_cc} "-D_HAS_TR1=0")
    endif()
  else()
    set(_castxml_cc --castxml-cc-gnu ( "${CMAKE_CXX_COMPILER}" ${_castxml_cc_flags} ))
  endif()
  set(_target)
  if(CMAKE_CROSSCOMPILING)
    if(NOT CMAKE_CXX_COMPILER_TARGET)
      message(FATAL_ERROR "Set the target triple in CMAKE_CXX_COMPILER_TARGET "
      " as described in http://clang.llvm.org/docs/CrossCompilation.html")
    endif()
    set(_target "--target=${CMAKE_CXX_COMPILER_TARGET}")
  endif()
  set(_build_env)
  if(APPLE)
    # If building on OS X, make sure that CastXML's calls to the compiler have the
    # settings that the output files will be compiled with.  This prevents headers
    # from one version of OS X from being used when building for another version.
    list(APPEND _build_env
      env
        "SDKROOT=${CMAKE_OSX_SYSROOT}"
        "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
    )
  endif()

  set(_include ${${WRAPPER_LIBRARY_NAME}_SOURCE_DIR}/include)
  set(_hdrs )
  set(glob_hdrs )
  if(EXISTS ${_include})
    file(GLOB_RECURSE glob_hdrs ${_include}/*.h)
  endif()
  foreach(header IN LISTS glob_hdrs)
    get_filename_component(header_name ${header} NAME)
    # This should be updated when cmake_minimum_required is 3.3
    # cmake_policy(SET CMP0057 NEW)
    # https://cmake.org/cmake/help/v3.3/policy/CMP0057.html
    # Next block should be replaced by:
    ###################
    # if(${header_name} IN_LIST WRAPPER_INCLUDE_FILES)
    list(FIND WRAPPER_INCLUDE_FILES ${header_name} pos)
    if( pos GREATER -1 )
    # end of block of code to be replaced
      list(APPEND _hdrs ${header})
    endif()
  endforeach()

  add_custom_command(
    OUTPUT ${xml_file}
    COMMAND ${_build_env} ${_ccache_cmd} ${CASTXML_EXECUTABLE}
          -o ${xml_file}
          --castxml-gccxml
          ${_target}
          --castxml-start _wrapping_
          ${_castxml_cc}
          -w
          -c # needed for ccache to think we are not calling for link
          @${castxml_inc_file}
          ${cxx_file}
          VERBATIM
    DEPENDS ${_castxml_depends} ${cxx_file} ${castxml_inc_file} ${_hdrs}
  )

  list(APPEND CastXML_OUTPUT_FILES ${xml_file})

endmacro()


macro(itk_wrap_one_type_castxml  wrap_method wrap_class swig_name template_params)
  # insert a blank line to separate the classes
  set(CASTXML_TYPEDEFS "${CASTXML_TYPEDEFS}\n")
  # add a piece of code for type instantiation
  set(CASTXML_FORCE_INSTANTIATE "${CASTXML_FORCE_INSTANTIATE}\n")
endmacro()

macro(itk_wrap_module_castxml library_name)
  # create the files used to pass the file to include to castxml
  set(castxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${library_name}.castxml.inc")
  get_directory_property(include_dir_list INCLUDE_DIRECTORIES)
  list(REMOVE_DUPLICATES include_dir_list)

  set(c)
  foreach(dir ${include_dir_list})
    set(c "${c}\"-I${dir}\"\n")
  endforeach()
  set(c "${c}-Qunused-arguments\n")
  set(c "${c}-DITK_WRAPPING_PARSER\n")
  set(c "${c}-DITK_MANUAL_INSTANTIATION\n")

  set(CONFIG_CASTXML_INC_CONTENTS "${c}")
  configure_file("${ITK_WRAP_CASTXML_SOURCE_DIR}/cast_xml.inc.in" "${castxml_inc_file}" @ONLY)

  set(CastXML_OUTPUT_FILES )
endmacro()

macro(itk_end_wrap_module_castxml)
  if(NOT TARGET ${WRAPPER_LIBRARY_NAME}CastXML)
    add_custom_target(${WRAPPER_LIBRARY_NAME}CastXML DEPENDS ${CastXML_OUTPUT_FILES})
    set(${WRAPPER_LIBRARY_NAME}XmlFiles ${CastXML_OUTPUT_FILES} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Xml file list.")
  endif()
endmacro()
