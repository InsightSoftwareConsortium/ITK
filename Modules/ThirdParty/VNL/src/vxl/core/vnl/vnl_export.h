#ifndef VNL_EXPORT_H
#define VNL_EXPORT_H

#include <vxl_config.h> // get VXL_BUILD_SHARED

#ifndef VXL_BUILD_SHARED  // if not a shared build
# define VNL_EXPORT
#else  // this is a shared build
# ifdef itkvnl_EXPORTS  // if building this library
#  if defined(_WIN32) || defined(WIN32)
#   define VNL_EXPORT __declspec(dllexport)
#  else
#   define VNL_EXPORT
#  endif
# else // we are using this library and it is built shared
#  if defined(_WIN32) || defined(WIN32)
#   define VNL_EXPORT __declspec(dllimport)
#  else
#   define VNL_EXPORT
#  endif
# endif
#endif

#endif
