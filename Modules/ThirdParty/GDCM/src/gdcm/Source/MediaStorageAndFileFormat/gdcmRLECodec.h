/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMRLECODEC_H
#define GDCMRLECODEC_H

#include "gdcmImageCodec.h"

namespace gdcm
{

class RLEInternals;
/**
 * \brief Class to do RLE
 * \note
 * ANSI X3.9
 * A.4.2 RLE Compression
 * Annex G defines a RLE Compression Transfer Syntax. This transfer Syntax is identified by the UID value
 * "1.2.840.10008.1.2.5". If the object allows multi-frame images in the pixel data field, then each frame shall
 * be encoded separately. Each frame shall be encoded in one and only one Fragment (see PS 3.5.8.2).
 *
 */
class GDCM_EXPORT RLECodec : public ImageCodec
{
public:
  RLECodec();
  ~RLECodec();
  bool CanCode(TransferSyntax const &ts) const;
  bool CanDecode(TransferSyntax const &ts) const;
  bool Decode(DataElement const &is, DataElement &os);
  unsigned long GetBufferLength() const { return BufferLength; }
  void SetBufferLength(unsigned long l) { BufferLength = l; }

  bool Code(DataElement const &in, DataElement &out);
  bool GetHeaderInfo(std::istream &is, TransferSyntax &ts);

protected:
  bool Decode(std::istream &is, std::ostream &os);
public:

  void SetLength(unsigned long l)
    {
    Length = l;
    }
private:
  RLEInternals *Internals;
  unsigned long Length;
  unsigned long BufferLength;
};

} // end namespace gdcm

#endif //GDCMRLECODEC_H
