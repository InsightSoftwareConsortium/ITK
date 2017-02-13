// This is core/vnl/vnl_identity_3x3.h
#ifndef vnl_identity_3x3_h_
#define vnl_identity_3x3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Contains class vnl_identity_3x3
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   30 Nov 96
//
// \verbatim
//  Modifications
//   LSB (Manchester) 23/3/01 tidied documentation
// \endverbatim
//-----------------------------------------------------------------------------

#include <vnl/vnl_double_3x3.h>
#include "vnl/vnl_export.h"

struct VNL_TEMPLATE_EXPORT vnl_identity_3x3 : public vnl_double_3x3
{
  vnl_identity_3x3() { set_identity(); }
};

#endif // vnl_identity_3x3_h_
