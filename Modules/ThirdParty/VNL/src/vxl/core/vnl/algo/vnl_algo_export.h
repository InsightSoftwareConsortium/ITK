#ifndef VNL_ALGO_EXPORT_H
#define VNL_ALGO_EXPORT_H

#include <vxl_config.h> // get VXL_BUILD_SHARED_LIBS

#ifndef VXL_BUILD_SHARED_LIBS  // if not a shared build
# define VNL_ALGO_EXPORT
#else  // this is a shared build
# ifdef VNL_ALGO_EXPORTS  // if building this library
#  if defined(_WIN32) || defined(WIN32)
#   define VNL_ALGO_EXPORT __declspec(dllexport)
#  else
#   define VNL_ALGO_EXPORT
#  endif
# else // we are using this library and it is built shared
#  if defined(_WIN32) || defined(WIN32)
#   define VNL_ALGO_EXPORT __declspec(dllimport)
#  else
#   define VNL_ALGO_EXPORT
#  endif
# endif
#endif

#endif
