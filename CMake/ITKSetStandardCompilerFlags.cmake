# trict-null-sentinel Check the set of common warning flags supported by C and C++ compilers
# check_compiler_warning_flags(<c_flags_var> <cxx_flags_var>)
#  <c_flags_var> - variable to store valid C warning flags
#  <cxx_flags_var> - variable to store valid CXX warning flags
# This internally calls the check_c_compiler_flag and check_cxx_compiler_flag macros.

# To create a portable build system, it is best to not
# test for platforms, but to test for features.
#
# Instead of testing "if Windows then do this", test for
# "if the -Wno-invalid-offsetof flag works then use it".
#
# Typical use of this module is:
#
#  include(CheckCompilerWarningFlags)
#  check_compiler_warning_flags(C_WARNING_FLAGS CXX_WARNING_FLAGS)
#  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${C_WARNING_FLAGS}")
#  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CXX_WARNING_FLAGS}")

include(CheckCXXCompilerFlag)
include(CheckCCompilerFlag)
include(CheckPIESupported)
check_pie_supported()

function(check_c_compiler_flags c_flag_var)
  set(local_c_flags "")
  set(flag_list "${ARGN}")
  foreach(flag IN LISTS flag_list)
    string(
      REPLACE "="
              "_"
              flag_var
              ${flag})
    check_c_compiler_flag(${flag} C_HAS_WARNING${flag_var})
    if(${C_HAS_WARNING${flag_var}})
      set(local_c_flags "${local_c_flags} ${flag}")
    endif()
  endforeach()
  set(${c_flag_var}
      "${local_c_flags}"
      PARENT_SCOPE)
endfunction()

function(check_cxx_compiler_flags cxx_flag_var)
  set(local_cxx_flags "")
  set(flag_list "${ARGN}")
  foreach(flag IN LISTS flag_list)
    string(
      REPLACE "="
              "_"
              flag_var
              ${flag})
    check_cxx_compiler_flag(${flag} CXX_HAS_WARNING${flag_var})
    if(${CXX_HAS_WARNING${flag_var}})
      set(local_cxx_flags "${local_cxx_flags} ${flag}")
    endif()
  endforeach()
  set(${cxx_flag_var}
      "${local_cxx_flags}"
      PARENT_SCOPE)
endfunction()

function(check_compiler_warning_flags c_warning_flags_var cxx_warning_flags_var)
  set(${c_warning_flags_var}
      ""
      PARENT_SCOPE)
  set(${cxx_warning_flags_var}
      ""
      PARENT_SCOPE)

  # Check this list on C compiler only
  set(c_flags -Wno-uninitialized -Wno-unused-parameter)

  ## On windows, the most verbose compiler options
  ## is reporting 1000's of wanings in windows
  ## header files, for now, limit the number of
  ## warnings to level 3
  if(WIN32)
    set(VerboseWarningsFlag -W3)
    ## A better solution would be to use -Wall,
    ## and then disable warnings one by one
    ## set(VerboseWarningsFlag -Wall -wd4820 -wd4682)
  else()
    ## with Intel compiler, the -Wall compiler options
    ## is reporting 1000's of remarks of trivial items
    ## that will only slow day-to-day operations
    ## specify -w2 to restrict to only warnings and errors
    if(${CMAKE_C_COMPILER} MATCHES "icc.*$")
      set(USING_INTEL_ICC_COMPILER TRUE)
    endif()
    if(${CMAKE_CXX_COMPILER} MATCHES "icpc.*$")
      set(USING_INTEL_ICC_COMPILER TRUE)
    endif()
    if(USING_INTEL_ICC_COMPILER)
      # NOTE -w2 is close to gcc's -Wall warning level, -w5 is intels -Wall warning level, and it is too verbose.
      set(VerboseWarningsFlag
          -w2
          -wd1268
          -wd981
          -wd383
          -wd1418
          -wd1419
          -wd2259
          -wd1572
          -wd424)
      #-wd424  #Needed for Intel compilers with remarki  #424: extra ";" ignored
      #-wd383  #Needed for Intel compilers with remark   #383: value copied to temporary, reference to temporary used
      #-wd981  #Needed for Intel compilers with remark   #981: operands are evaluated in unspecified order
      #-wd1418 #Needed for Intel compilers with remark  #1418: external function definition with no prior declaration
      #-wd1419 #Needed for Intel compilers with remark  #1419: external declaration in primary source file
      #-wd1572 #Needed for Intel compilers with remark  #1572: floating-point equality and inequality comparisons are unreliable
      #-wd2259 #Needed for Intel compilers with remark  #2259: non-pointer conversion from "itk::SizeValueType={unsigned long}" to "double" may lose significant bits
      #-wd1268 #Needed for Intel compilers with warning #1268: support for exported templates is disabled
    else()
      set(VerboseWarningsFlag -Wall)
    endif()
  endif()

  # Check this list on both C and C++ compilers
  set(c_and_cxx_flags
      ${VerboseWarningsFlag}
      -Wno-long-double #Needed on APPLE
      -Wcast-align
      -Wdisabled-optimization
      -Wextra
      -Wformat=2
      -Winvalid-pch
      -Wno-format-nonliteral
      -Wpointer-arith
      -Wshadow
      -Wunused
      -Wwrite-strings
      -Wno-strict-overflow)

  # Check this list on C++ compiler only
  set(cxx_flags
      -Wno-deprecated
      -Wno-invalid-offsetof
      -Wno-undefined-var-template # suppress invalid warning when explicitly instantiated in another translation unit
      -Woverloaded-virtual
      -Wctad-maybe-unsupported
      -Wstrict-null-sentinel)
  ##-Wno-c++0x-static-nonintegral-init
  ## Clang compiler likes to warn about this feature that is technically only in
  ## c++0x, but works on many compilers, and if it fails, then alternate methods are used

  check_c_compiler_flags(CMAKE_C_WARNING_FLAGS ${c_flags} ${c_and_cxx_flags})
  check_cxx_compiler_flags(CMAKE_CXX_WARNING_FLAGS ${c_and_cxx_flags} ${cxx_flags})

  set(${c_warning_flags_var}
      "${CMAKE_C_WARNING_FLAGS}"
      PARENT_SCOPE)
  set(${cxx_warning_flags_var}
      "${CMAKE_CXX_WARNING_FLAGS}"
      PARENT_SCOPE)
endfunction()

# Check for the presence of AVX and figure out the flags to use for it.
# Adapted from https://gist.github.com/UnaNancyOwen/263c243ae1e05a2f9d0e
function(check_avx_flags avx_flags_var)
  set(avx_flags_var)

  include(CheckCXXSourceRuns)
  set(_safe_cmake_required_flags "${CMAKE_REQUIRED_FLAGS}")
  set(CMAKE_REQUIRED_FLAGS)

  # Check AVX
  if(MSVC)
    set(CMAKE_REQUIRED_FLAGS "/arch:AVX") # set flags to be used in check_cxx_source_runs below
  endif()
  check_cxx_source_runs(
    "
    #include <immintrin.h>
    int main()
    {
      __m256 a, b, c;
      const float src[8] = { 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f };
      float dst[8];
      a = _mm256_loadu_ps(src);
      b = _mm256_loadu_ps(src);
      c = _mm256_add_ps(a, b);
      _mm256_storeu_ps(dst, c);

      for(int i = 0; i < 8; i++){
        if(( src[i] + src[i]) != dst[i]){
          return -1;
        }
      }

      return 0;
    }"
    have_avx_extensions_var)

  # Check AVX2
  if(MSVC)
    set(CMAKE_REQUIRED_FLAGS "/arch:AVX2") # set flags to be used in check_cxx_source_runs below
  endif()
  check_cxx_source_runs(
    "
    #include <immintrin.h>
    int main()
    {
      __m256i a, b, c;
      const int src[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
      int dst[8];
      a =  _mm256_loadu_si256( (__m256i*)src);
      b =  _mm256_loadu_si256( (__m256i*)src);
      c = _mm256_add_epi32( a, b);
      _mm256_storeu_si256( (__m256i*)dst, c);

      for(int i = 0; i < 8; i++){
        if(( src[i] + src[i]) != dst[i]){
          return -1;
        }
      }

      return 0;
    }"
    have_avx2_extensions_var)

  set(CMAKE_REQUIRED_FLAGS "${_safe_cmake_required_flags}")

  # Set Flags
  if(have_avx2_extensions_var AND MSVC)
    set(avx_flags_var "${avx_flags_var} /arch:AVX2")
  elseif(have_avx_extensions_var AND MSVC)
    set(avx_flags_var "${avx_flags_var} /arch:AVX")
  endif()
endfunction()

# Check for the presence of SSE2.
# Adapted from the AVX check and https://github.com/InsightSoftwareConsortium/ITK/blob/master/Modules/ThirdParty/VNL/src/vxl/config/cmake/config/vxl_platform_tests.cxx#L164-L178
function(check_sse2_flags sse2_flags_var)

  # set flags to be used in check_cxx_source_runs below
  set(_safe_cmake_required_flags "${CMAKE_REQUIRED_FLAGS}")
  set(CMAKE_REQUIRED_FLAGS)
  if(MSVC)
    set(CMAKE_REQUIRED_FLAGS "/arch:SSE2")
  elseif(CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_REQUIRED_FLAGS "-msse2")
  endif()

  # perform test
  include(CheckCXXSourceRuns)
  check_cxx_source_runs(
    "
      #include <emmintrin.h>
      int main()
      {
        //try to do some sse2 calculations
        double d_a[]  = { 6.75, 3.42 };
        double d_b[]  = { 2.3, 9.2 };
        double res[2] = {0.0};

        __m128d z;
        z = _mm_mul_pd(_mm_loadu_pd(d_a),_mm_loadu_pd(d_b));

        _mm_storeu_pd(res,z);

        return 0;
      }"
    have_sse2_extensions_var)
  set(CMAKE_REQUIRED_FLAGS "${_safe_cmake_required_flags}")

  # Set SSE2 Flags
  set(${sse2_flags_var})
  if(have_sse2_extensions_var AND MSVC)
    set(${sse2_flags_var} "/arch:SSE2")
  elseif(have_sse2_extensions_var AND CMAKE_COMPILER_IS_GNUCXX)
    set(${sse2_flags_var} "-msse2")
  endif()
  set(${sse2_flags_var}
      "${${sse2_flags_var}}"
      PARENT_SCOPE)
endfunction()

function(check_compiler_optimization_flags c_optimization_flags_var cxx_optimization_flags_var)
  set(${c_optimization_flags_var}
      ""
      PARENT_SCOPE)
  set(${cxx_optimization_flags_var}
      ""
      PARENT_SCOPE)

  if("${CMAKE_SYSTEM_PROCESSOR}" MATCHES "(x86_64|AMD64)")
    if(MSVC)
      check_avx_flags(InstructionSetOptimizationFlags)
      if("${CMAKE_SIZEOF_VOID_P}" EQUAL "4")
        list(
          APPEND
          InstructionSetOptimizationFlags
          /arch:SSE
          /arch:SSE2)
      endif()
    elseif(NOT EMSCRIPTEN OR WASI)
      if(${CMAKE_C_COMPILER} MATCHES "icc.*$")
        set(USING_INTEL_ICC_COMPILER TRUE)
      endif()
      if(${CMAKE_CXX_COMPILER} MATCHES "icpc.*$")
        set(USING_INTEL_ICC_COMPILER TRUE)
      endif()
      if(USING_INTEL_ICC_COMPILER)
        set(InstructionSetOptimizationFlags "")
      else()
        set(InstructionSetOptimizationFlags "")
      endif()

      # Check this list on C compiler only
      set(c_flags "")

      # Check this list on C++ compiler only
      set(cxx_flags "")

      # Check this list on both C and C++ compilers
      set(InstructionSetOptimizationFlags
          # https://gcc.gnu.org/onlinedocs/gcc-4.8.0/gcc/i386-and-x86_002d64-Options.html
          # NOTE the corei7 release date was 2008
          #-mtune=native # Tune the code for the computer used compile ITK, but allow running on generic cpu archetectures
          -mtune=generic # for reproducible results https://github.com/InsightSoftwareConsortium/ITK/issues/1939
          -march=corei7 # Use ABI settings to support corei7 (circa 2008 ABI feature sets, core-avx circa 2013)
      )
    endif()
    set(c_and_cxx_flags ${InstructionSetOptimizationFlags})
  endif()

  check_c_compiler_flags(CMAKE_C_WARNING_FLAGS ${c_and_cxx_flags} ${c_flags})
  check_cxx_compiler_flags(CMAKE_CXX_WARNING_FLAGS ${c_and_cxx_flags} ${cxx_flags})

  set(${c_optimization_flags_var}
      "${CMAKE_C_WARNING_FLAGS}"
      PARENT_SCOPE)
  set(${cxx_optimization_flags_var}
      "${CMAKE_CXX_WARNING_FLAGS}"
      PARENT_SCOPE)
endfunction()

macro(check_compiler_platform_flags)
  # On Visual Studio 8 MS deprecated C. This removes all 1.276E1265 security
  # warnings
  if(WIN32)
    if(NOT MINGW)
      if(NOT ITK_ENABLE_VISUAL_STUDIO_DEPRECATED_C_WARNINGS)
        add_definitions(
          -D_CRT_FAR_MAPPINGS_NO_DEPRECATE
          -D_CRT_IS_WCTYPE_NO_DEPRECATE
          -D_CRT_MANAGED_FP_NO_DEPRECATE
          -D_CRT_NONSTDC_NO_DEPRECATE
          -D_CRT_SECURE_NO_DEPRECATE
          -D_CRT_SECURE_NO_DEPRECATE_GLOBALS
          -D_CRT_SETERRORMODE_BEEP_SLEEP_NO_DEPRECATE
          -D_CRT_TIME_FUNCTIONS_NO_DEPRECATE
          -D_CRT_VCCLRIT_NO_DEPRECATE
          -D_SCL_SECURE_NO_DEPRECATE)
      endif()
      # With MS compilers on Win64, we need the /bigobj switch, else generated
      # code results in objects with number of sections exceeding object file
      # format.
      # see https://msdn.microsoft.com/en-us/library/ms173499.aspx
      if(MSVC)
        set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} /bigobj")
      endif()
    endif()
  endif()

  if(WIN32)
    # Some libraries (e.g. vxl libs) have no dllexport markup, so we can
    # build full shared libraries only with the GNU toolchain. For non
    # gnu compilers on windows, only a few libraries are built shared.
    # This is controlled with ITK_LIBRARY_BUILD_TYPE used in the add_library
    # call. This allows for plugin type applications to use a dll for
    # ITKCommon which will contain the static for Modified time.
    if(CMAKE_COMPILER_IS_GNUCXX)
      # CMake adds --enable-all-exports on Cygwin (since Cygwin is
      # supposed to be UNIX-like), but we need to add it explicitly for
      # a native windows build with the MinGW tools.
      if(MINGW)
        set(CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS "-shared -Wl,--export-all-symbols -Wl,--enable-auto-import")
        set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS "-shared -Wl,--export-all-symbols -Wl,--enable-auto-import")
        set(CMAKE_EXE_LINKER_FLAGS "-Wl,--enable-auto-import")
      endif()
    else()
      # if CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS is on, then
      # BUILD_SHARED_LIBS works as it would on other systems
      if(NOT CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS)
        if(BUILD_SHARED_LIBS)
          set(ITK_LIBRARY_BUILD_TYPE "SHARED")
        else()
          set(ITK_LIBRARY_BUILD_TYPE "STATIC")
        endif()
        # turn off BUILD_SHARED_LIBS as ITK_LIBRARY_BUILD_TYPE
        # is used on the libraries that have markup.
        set(BUILD_SHARED_LIBS OFF)
      endif()
    endif()
  endif()
  #-----------------------------------------------------------------------------
  #ITK requires special compiler flags on some platforms.
  if(CMAKE_COMPILER_IS_GNUCXX)
    if(APPLE)
      option(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING "Turn on warnings on 64bits to 32bits truncations." OFF)
      mark_as_advanced(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING)

      execute_process(
        COMMAND "${CMAKE_C_COMPILER}" --version
        OUTPUT_VARIABLE _version
        ERROR_VARIABLE _version)

      # -fopenmp breaks compiling the HDF5 library in shared library mode
      # on the OS X platform -- at least with gcc 4.2 from Xcode.
      set(compile_flag_lists
          CMAKE_C_FLAGS
          CMAKE_CXX_FLAGS
          CMAKE_C_FLAGS_DEBUG
          CMAKE_C_FLAGS_MINSIZEREL
          CMAKE_C_FLAGS_RELEASE
          CMAKE_C_FLAGS_RELWITHDEBINFO
          CMAKE_CXX_FLAGS_DEBUG
          CMAKE_CXX_FLAGS_MINSIZEREL
          CMAKE_CXX_FLAGS_RELEASE
          CMAKE_CXX_FLAGS_RELWITHDEBINFO)
      foreach(listname ${compile_flag_lists})
        if("${${listname}}" MATCHES ".*-fopenmp.*")
          string(
            REPLACE "-fopenmp"
                    ""
                    tmpFlags
                    "${${listname}}")
          set(${listname} "${tmpFlags}")
          message("-fopenmp causes incorrect compilation of HDF, removing from ${listname}")
        endif()
      endforeach()
    endif()

    if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL "12")
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -fno-sized-deallocation")
    endif()

    check_sse2_flags(ITK_SSE2_CFLAGS_IF_AVAILABLE)
    set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} ${ITK_SSE2_CFLAGS_IF_AVAILABLE}")

  endif()

  #-----------------------------------------------------------------------------

  # for the gnu compiler a -D_PTHREADS is needed on sun
  # for the native compiler a -mt flag is needed on the sun
  if(CMAKE_SYSTEM MATCHES "SunOS.*")
    if(CMAKE_COMPILER_IS_GNUCXX)
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -D_PTHREADS")
      set(ITK_REQUIRED_LINK_FLAGS "${ITK_REQUIRED_LINK_FLAGS} -lrt")
    else()
      set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -mt")
      set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -mt")
    endif()
    # Add flags for the SUN compiler to provide all the methods for std::allocator.
    #
    check_cxx_source_compiles("-features=no%anachronisms" SUN_COMPILER)
    if(SUN_COMPILER)
      check_cxx_source_compiles("-library=stlport4" SUN_COMPILER_HAS_STL_PORT_4)
      if(SUN_COMPILER_HAS_STL_PORT_4)
        set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -library=stlport4")
      endif()
    endif()
  endif()

  # mingw thread support
  if(MINGW)
    set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -mthreads")
    set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -mthreads")
    set(ITK_REQUIRED_LINK_FLAGS "${ITK_REQUIRED_LINK_FLAGS} -mthreads")
  endif()

  #-----------------------------------------------------------------------------
  # Set the compiler-specific flag for disabling optimization.
  if(MSVC)
    set(ITK_CXX_DISABLE_OPTIMIZATION_FLAG "/Od")
  elseif("${CMAKE_CXX_COMPILER_ID}" MATCHES "^(GNU|Intel)$" AND NOT MINGW)
    set(ITK_CXX_DISABLE_OPTIMIZATION_FLAG "-O0")
  endif()
  if(DEFINED ITK_CXX_DISABLE_OPTIMIZATION_FLAG)
    check_cxx_source_compiles(${ITK_CXX_DISABLE_OPTIMIZATION_FLAG} CXX_HAS_DISABLE_OPTIMIZATION_FLAG)
  endif()
endmacro() #End the platform check function

if(NOT ITK_C_WARNING_FLAGS OR NOT ITK_CXX_WARNING_FLAGS) # Only check once if not explicitly set on command line
  #-----------------------------------------------------------------------------
  #Check the set of warning flags the compiler supports
  check_compiler_warning_flags(C_WARNING_FLAGS CXX_WARNING_FLAGS)
endif()

if(NOT ITK_C_WARNING_FLAGS) #Not set on cmake command line option -DITK_C_WARNING_FLAGS:STRING=""
  set(ITK_C_WARNING_FLAGS ${C_WARNING_FLAGS})
endif()
set(ITK_C_WARNING_FLAGS
    ${ITK_C_WARNING_FLAGS}
    CACHE STRING "ITK C compiler warning flags. Modify to suit your needs.")

if(NOT ITK_CXX_WARNING_FLAGS) #Not set on cmake command line option -DITK_CXX_WARNING_FLAGS:STRING=""
  set(ITK_CXX_WARNING_FLAGS ${CXX_WARNING_FLAGS})
endif()
set(ITK_CXX_WARNING_FLAGS
    ${ITK_CXX_WARNING_FLAGS}
    CACHE STRING "ITK CXX compiler warning flags. Modify to suit your needs.")

mark_as_advanced(ITK_CXX_WARNING_FLAGS)
mark_as_advanced(ITK_C_WARNING_FLAGS)
unset(C_WARNING_FLAGS)
unset(CXX_WARNING_FLAGS)

if(NOT ITK_C_OPTIMIZATION_FLAGS OR NOT ITK_CXX_OPTIMIZATION_FLAGS
)# Only check once if not explicitly set on command line
  #-----------------------------------------------------------------------------
  #Check the set of warning flags the compiler supports
  check_compiler_optimization_flags(C_OPTIMIZATION_FLAGS CXX_OPTIMIZATION_FLAGS)
endif()
if(NOT ITK_C_OPTIMIZATION_FLAGS) #Not set on cmake command line option -DITK_C_OPTIMIZATION_FLAGS:STRING=""
  set(ITK_C_OPTIMIZATION_FLAGS
      ${C_OPTIMIZATION_FLAGS}
      CACHE
        STRING
        "ITK C Compiler ABI/Optimization flags, Use '-march=native' to maximize performance, but break portabilitly.")
else()
  set(ITK_C_OPTIMIZATION_FLAGS
      ${ITK_C_OPTIMIZATION_FLAGS}
      CACHE
        STRING
        "ITK C Compiler ABI/Optimization flags, Use '-march=native' to maximize performance, but break portabilitly.")
endif()
if(NOT ITK_CXX_OPTIMIZATION_FLAGS) #Not set on cmake command line option -DITK_CXX_OPTIMIZATION_FLAGS:STRING=""
  set(ITK_CXX_OPTIMIZATION_FLAGS
      ${CXX_OPTIMIZATION_FLAGS}
      CACHE
        STRING
        "ITK CXX Compiler ABI/Optimization flags, Use '-march=native' to maximize performance, but break portabilitly.")
else()
  set(ITK_CXX_OPTIMIZATION_FLAGS
      ${ITK_CXX_OPTIMIZATION_FLAGS}
      CACHE
        STRING
        "ITK CXX Compiler ABI/Optimization flags, Use '-march=native' to maximize performance, but break portabilitly.")
endif()
mark_as_advanced(ITK_CXX_OPTIMIZATION_FLAGS)
mark_as_advanced(ITK_C_OPTIMIZATION_FLAGS)
unset(C_OPTIMIZATION_FLAGS)
unset(CXX_OPTIMIZATION_FLAGS)

#-----------------------------------------------------------------------------
#Check the set of platform flags the compiler supports
check_compiler_platform_flags()

# Append ITK warnings to the CMake flags.
# We do not set them in ITK_REQUIRED FLAGS because all project which
# use ITK don't require these flags .

string(
  REPLACE " "
          ";"
          CMAKE_C_FLAGS
          "${CMAKE_C_FLAGS} ${ITK_C_OPTIMIZATION_FLAGS} ${ITK_C_WARNING_FLAGS}")
list(REMOVE_DUPLICATES CMAKE_C_FLAGS)
string(
  REPLACE ";"
          " "
          CMAKE_C_FLAGS
          "${CMAKE_C_FLAGS}")

string(
  REPLACE " "
          ";"
          CMAKE_CXX_FLAGS
          "${CMAKE_CXX_FLAGS} ${ITK_CXX_OPTIMIZATION_FLAGS} ${ITK_CXX_WARNING_FLAGS}")
list(REMOVE_DUPLICATES CMAKE_CXX_FLAGS)
string(
  REPLACE ";"
          " "
          CMAKE_CXX_FLAGS
          "${CMAKE_CXX_FLAGS}")
