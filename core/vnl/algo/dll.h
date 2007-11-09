#ifndef vnl_algo_dll_h_
#define vnl_algo_dll_h_

#include <vcl_compiler.h>

#define VNL_ALGO_DLL_DATA

#if defined(VCL_WIN32) && !defined(BUILDING_VNL_ALGO_DLL)
// if win32 and not buiding the DLL then you need a dllimport 
// Only if you are building a DLL linked application.
# ifdef BUILD_DLL
#  undef VNL_ALGO_DLL_DATA
#  define VNL_ALGO_DLL_DATA _declspec(dllimport)
# endif // BUILD_DLL
#endif // VCL_WIN32 and !Building_*_dll

#endif // vnl_algo_dll_h_
