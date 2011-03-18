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
#ifndef GDCMJPEGLSCODEC_H
#define GDCMJPEGLSCODEC_H

#include "gdcmImageCodec.h"

namespace gdcm
{

class JPEGLSInternals;
/**
 * \brief JPEG-LS
 * \note codec that implement the JPEG-LS compression
 * this is an implementation of ImageCodec for JPEG-LS
 *
 * It uses the CharLS JPEG-LS implementation http://charls.codeplex.com
 */
class GDCM_EXPORT JPEGLSCodec : public ImageCodec
{
public:
  JPEGLSCodec();
  ~JPEGLSCodec();
  bool CanDecode(TransferSyntax const &ts) const;
  bool CanCode(TransferSyntax const &ts) const;

  unsigned long GetBufferLength() const { return BufferLength; }
  void SetBufferLength(unsigned long l) { BufferLength = l; }

  bool Decode(DataElement const &is, DataElement &os);
  bool Code(DataElement const &in, DataElement &out);

  bool GetHeaderInfo(std::istream &is, TransferSyntax &ts);

  void SetLossless(bool l);
  bool GetLossless() const;

/*
 * test.acr can look pretty bad, even with a lossy error of 2. Explanation follows:
 * I agree that the test image looks ugly. In this particular case I can
 * explain though.
 *
 * The image is 8 bit, but it does not use the full 8 bit dynamic range. The
 * black pixels have value 234 and the white 255. If you set allowed lossy
 * error to 2, you allow an error of about 10% of the actual dynamic range.
 * That is of course very visible.
 */
  /// [0-3] generally
  void SetLossyError(int error);

private:
  unsigned long BufferLength;
  bool Lossless;
  int LossyError;
};

} // end namespace gdcm

#endif //GDCMJPEGLSCODEC_H
