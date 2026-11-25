/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMKAKADUCODEC_H
#define GDCMKAKADUCODEC_H

#include "gdcmImageCodec.h"

namespace gdcm
{

/**
 * \brief KAKADUCodec
 */
class KAKADUCodec : public ImageCodec
{
public:
  KAKADUCodec();
  ~KAKADUCodec() override;
  bool CanDecode(TransferSyntax const &ts) const override;
  bool CanCode(TransferSyntax const &ts) const override;

  bool Decode(DataElement const &is, DataElement &os) override;
  bool Code(DataElement const &in, DataElement &out) override;

  ImageCodec * Clone() const override;
private:
};

} // end namespace gdcm

#endif //GDCMKAKADUCODEC_H
