/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmMEC_MR3.h"

#include "gdcmext/mec_mr3_io.h"

namespace gdcm {
bool MEC_MR3::Print(const char *src, size_t srclen) {
  return mec_mr3_print(src, srclen);
}
const PrivateTag &MEC_MR3::GetPMTFInformationDataTag() {
  static const PrivateTag tseq(0x0029, 0x90, "PMTF INFORMATION DATA");
  return tseq;
}

const PrivateTag &MEC_MR3::GetCanonMECMR3Tag() {
  static const PrivateTag tseq(0x0029, 0x90, "CANON_MEC_MR3");
  return tseq;
}

const PrivateTag &MEC_MR3::GetToshibaMECMR3Tag() {
  static const PrivateTag tseq(0x0029, 0x90, "TOSHIBA_MEC_MR3");
  return tseq;
}

}  // end namespace gdcm
