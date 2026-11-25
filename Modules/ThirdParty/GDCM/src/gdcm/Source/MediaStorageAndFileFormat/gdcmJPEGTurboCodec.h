/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMJPEGTURBOCODEC_H
#define GDCMJPEGTURBOCODEC_H

#include "gdcmJPEGCodec.h"

namespace gdcm
{

class JPEGTurboInternals;
/**
 * \brief Class to do JPEG using libjpeg-turbo (8/12/16 bits, lossy & lossless)
 * \note internal class — single codec replaces JPEG8Codec/JPEG12Codec/JPEG16Codec
 */
class GDCM_EXPORT JPEGTurboCodec : public JPEGCodec
{
public:
  JPEGTurboCodec();
  ~JPEGTurboCodec() override;

  bool DecodeByStreams(std::istream &is, std::ostream &os) override;
  bool InternalCode(const char *input, unsigned long len, std::ostream &os) override;

  bool GetHeaderInfo(std::istream &is, TransferSyntax &ts) override;

protected:
  bool IsStateSuspension() const override;
  bool EncodeBuffer(std::ostream &os, const char *data, size_t datalen) override;

private:
  JPEGTurboInternals *Internals;
};

} // end namespace gdcm

#endif //GDCMJPEGTURBOCODEC_H
