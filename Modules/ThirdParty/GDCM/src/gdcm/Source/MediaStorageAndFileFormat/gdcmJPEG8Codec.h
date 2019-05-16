/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMJPEG8CODEC_H
#define GDCMJPEG8CODEC_H

#include "gdcmJPEGCodec.h"

namespace gdcm
{

class JPEGInternals_8BIT;
class ByteValue;
/**
 * \brief Class to do JPEG 8bits (lossy & lossless)
 * \note internal class
 */
class JPEG8Codec : public JPEGCodec
{
public:
  JPEG8Codec();
  ~JPEG8Codec() override;

  bool DecodeByStreams(std::istream &is, std::ostream &os) override;
  bool InternalCode(const char *input, unsigned long len, std::ostream &os) override;

  bool GetHeaderInfo(std::istream &is, TransferSyntax &ts) override;

protected:
  bool IsStateSuspension() const override;
  bool EncodeBuffer(std::ostream &os, const char *data, size_t datalen) override;

private:
  JPEGInternals_8BIT *Internals;
};

} // end namespace gdcm

#endif //GDCMJPEG8CODEC_H
