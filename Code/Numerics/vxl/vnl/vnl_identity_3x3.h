#ifndef vnl_identity_3x3_h_
#define vnl_identity_3x3_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_identity_3x3.h

//: \file
//  \brief  Undocumented class
//  \author Andrew W. Fitzgibbon, Oxford RRG, 30 Nov 96
//     

// Modifications:
// LSB (Manchester) 23/3/01 tidied documentation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_double_3x3.h>

struct vnl_identity_3x3 : public vnl_double_3x3 {
  vnl_identity_3x3() { set_identity(); }
};

#endif // vnl_identity_3x3_h_
