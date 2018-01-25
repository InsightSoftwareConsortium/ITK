// This is core/vnl/vnl_matlab_header.h
#ifndef vnl_matlab_header_h_
#define vnl_matlab_header_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif

#undef swap32
#undef swap64

//:
//  \file
//  \brief MATLAB header structure
//  \author fsm
// \verbatim
//  Modifications
//   21 Apr 2009 Kent Williams - Taking care of the byte ordering of the MAT file
// \endverbatim

#include <vxl_config.h>
#include "vnl/vnl_export.h"

struct VNL_EXPORT vnl_matlab_header
{
  vxl_int_32 type; // sum of one byte order, one storage specifier and one precision specifier
  vxl_int_32 rows;
  vxl_int_32 cols;
  vxl_int_32 imag;
  vxl_int_32 namlen;

  enum type_t {
    // precision specifier
    vnl_DOUBLE_PRECISION = 0,
    vnl_SINGLE_PRECISION = 10,
    // storage specifier
    vnl_COLUMN_WISE = 0,
    vnl_ROW_WISE    = 100,
    // byte order
    vnl_LITTLE_ENDIAN = 0,
    vnl_BIG_ENDIAN    = 1000,
    //
    vnl_none = 0
  };
};

namespace byteswap // byteswap routines, stolen from ITK
{
 inline void
 swap32(void *ptr)
 {
  char one_byte;
  char *p = reinterpret_cast<char *>(ptr);
  one_byte = p[0]; p[0] = p[3]; p[3] = one_byte;
  one_byte = p[1]; p[1] = p[2]; p[2] = one_byte;
 }
 inline void
 swap64(void *ptr)
 {
  char one_byte;
  char *p = reinterpret_cast<char *>(ptr);
  one_byte = p[0]; p[0] = p[7]; p[7] = one_byte;
  one_byte = p[1]; p[1] = p[6]; p[6] = one_byte;
  one_byte = p[2]; p[2] = p[5]; p[5] = one_byte;
  one_byte = p[3]; p[3] = p[4]; p[4] = one_byte;
 }
}

#endif // vnl_matlab_header_h_
