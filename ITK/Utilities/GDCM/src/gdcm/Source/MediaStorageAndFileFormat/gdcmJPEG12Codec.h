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
#ifndef GDCMJPEG12CODEC_H
#define GDCMJPEG12CODEC_H

#include "gdcmJPEGCodec.h"

namespace gdcm
{

class JPEGInternals;
class ByteValue;
/**
 * \brief Class to do JPEG 12bits (lossy & lossless)
 * \note internal class
 */
class JPEG12Codec : public JPEGCodec
{
public:
  JPEG12Codec();
  ~JPEG12Codec();

  bool Decode(std::istream &is, std::ostream &os);
  bool InternalCode(const char *input, unsigned long len, std::ostream &os);

  bool GetHeaderInfo(std::istream &is, TransferSyntax &ts);

private:
  JPEGInternals *Internals;
};

} // end namespace gdcm

#endif //GDCMJPEG12CODEC_H
