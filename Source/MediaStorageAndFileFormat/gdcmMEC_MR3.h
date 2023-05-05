/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMMEC_MR3_H
#define GDCMMEC_MR3_H

#include "gdcmPrivateTag.h"

namespace gdcm {
/**
 * \brief Class for MEC_MR3
 *
 */
class GDCM_EXPORT MEC_MR3 {
 public:
  static bool Print(const char *src, size_t srclen);

  /// Return the private tag used by PMTF to store the MEC_MR3 data
  /// This is: PrivateTag(0x0029,0x90,"PMTF INFORMATION DATA");
  static const PrivateTag &GetPMTFInformationDataTag();

  /// Return the private tag used by CANON to store the MEC_MR3 data
  /// This is: PrivateTag(0x0029,0x90,"CANON_MEC_MR3");
  static const PrivateTag &GetCanonMECMR3Tag();

  /// Return the private tag used by TOSHIBA to store the MEC_MR3 data
  /// This is: PrivateTag(0x0029,0x90,"TOSHIBA_MEC_MR3");
  static const PrivateTag &GetToshibaMECMR3Tag();
};

}  // end namespace gdcm

#endif  // GDCMMEC_MR3_H
