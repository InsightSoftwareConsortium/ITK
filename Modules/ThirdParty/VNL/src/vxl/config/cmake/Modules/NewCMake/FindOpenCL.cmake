#
# This open cl setup is based on the requirements for the AMD SDK.
# http://developer.amd.com/gpu/ATIStreamSDKBetaProgram/Pages/default.aspx
# Undoubtedly a different setup is required for the Nvidia SDK
#
# 3/14/2009 Support for both ATI and NVIDIA SDKs added by Octavian
# 4/06/2009 Force check of actual lib files in the path's provided using FIND_LIBRARY added by Octavian

set(OPENCL_FOUND "NO")
set(NVIDIA_FOUND "NO")
set(ATI_FOUND "NO")

if(WIN32)
  find_path( OPENCL_INCLUDE_PATH CL/cl_gl.h)

  if(OPENCL_INCLUDE_PATH)

    find_path( OPENCL_NVIDIA_LIBRARY_PATH OpenCL.lib )
    find_path( OPENCL_ATI_LIBRARY_PATH aticalcl.lib )

    if(OPENCL_NVIDIA_LIBRARY_PATH)
      find_library(OPENCL_NVIDIA_LIBRARY NAMES OpenCL.lib PATHS ${OPENCL_NVIDIA_LIBRARY_PATH} ${OPENCL_NVIDIA_LIBRARY_PATH}/x64 ${OPENCL_NVIDIA_LIBRARY_PATH}/Win32)
      if(OPENCL_NVIDIA_LIBRARY)
        set(OPENCL_FOUND "YES")
        set(NVIDIA_FOUND "YES")
        set( OPENCL_LIBRARIES ${OPENCL_NVIDIA_LIBRARY})
      endif()
    endif()
    if(OPENCL_ATI_LIBRARY_PATH)
    message(${CMAKE_CL_64})
          if( CMAKE_CL_64 )
                set( OPENCL_ATI_LIBRARY "${OPENCL_ATI_LIBRARY_PATH}/x86_64/OpenCL.lib")
                message(${OPENCL_ATI_LIBRARY})
          else()
                set(OPENCL_ATI_LIBRARY "${OPENCL_ATI_LIBRARY_PATH}/x86/OpenCL.lib")
          endif()

      #if(OPENCL_CALC_LIBRARY AND  OPENCL_ATICALRT_LIBRARY AND OPENCL_ATI_LIBRARY)
      set(OPENCL_LIBRARIES  ${OPENCL_ATI_LIBRARY})
      if(OPENCL_LIBRARIES)
        set(OPENCL_FOUND "YES")
        set(ATI_FOUND "YES")
      endif()
    endif()

    if(ATI_FOUND AND NVIDIA_FOUND)

      option(USE_NVIDIA_SDK "Default to NVIDIA SDK?" NO)

      if(USE_NVIDIA_SDK)
        set( OPENCL_LIBRARIES OPENCL_NVIDIA_LIBRARY)
      else()
        set(OPENCL_LIBRARIES    OPENCL_ATI_LIBRARY)
      endif()

    endif()
  endif()
endif()

if(APPLE)
  find_path(OPENCL_INCLUDE_PATH cl.h "Include for OpenCL on OSX")
  if(OPENCL_INCLUDE_PATH)
    find_library(OPENCL_LIBRARY OpenCL "OpenCL lib for OSX")
    if(OPENCL_LIBRARY)
      set(OPENCL_FOUND "YES")
      set(OPENCL_LIBRARIES ${OPENCL_LIBRARY})
    endif()
  endif()
endif()

if(UNIX)
  find_path( OPENCL_INCLUDE_PATH CL/cl_gl.h PATHS /usr/include /usr/include/nvidia-current /usr/local/cuda/include)
  find_path(OPENCL_LIBRARY_PATH libOpenCL.so PATHS /usr/lib /usr/lib/nvidia-current /usr/local/cuda/lib64 ${OPENCL_LIBRARY_PATH} )
  find_library(OPENCL_LIBRARIES NAMES libOpenCL.so PATHS /usr/lib /usr/lib/nvidia-current ${OPENCL_LIBRARY_PATH} )
  if(OPENCL_INCLUDE_PATH)
      if(OPENCL_LIBRARIES)
        set(OPENCL_FOUND "YES")
      endif()
  endif()
endif()
