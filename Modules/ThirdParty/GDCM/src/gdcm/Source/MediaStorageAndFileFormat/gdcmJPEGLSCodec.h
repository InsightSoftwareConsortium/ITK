/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
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
 * It uses the CharLS JPEG-LS implementation https://github.com/team-charls/charls
 */
class GDCM_EXPORT JPEGLSCodec : public ImageCodec
{
friend class ImageRegionReader;
public:
  JPEGLSCodec();
  ~JPEGLSCodec() override;
  bool CanDecode(TransferSyntax const &ts) const override;
  bool CanCode(TransferSyntax const &ts) const override;

  unsigned long GetBufferLength() const { return BufferLength; }
  void SetBufferLength(unsigned long l) { BufferLength = l; }

  bool Decode(DataElement const &is, DataElement &os) override;
  bool Decode(DataElement const &in, char* outBuffer, size_t inBufferLength,
              uint32_t inXMin, uint32_t inXMax, uint32_t inYMin,
              uint32_t inYMax, uint32_t inZMin, uint32_t inZMax);
  bool Code(DataElement const &in, DataElement &out) override;

  bool GetHeaderInfo(std::istream &is, TransferSyntax &ts) override;
  ImageCodec * Clone() const override;

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

protected:
  bool DecodeExtent(
    char *buffer,
    unsigned int xmin, unsigned int xmax,
    unsigned int ymin, unsigned int ymax,
    unsigned int zmin, unsigned int zmax,
    std::istream & is
  );

  bool StartEncode( std::ostream & ) override;
  bool IsRowEncoder() override;
  bool IsFrameEncoder() override;
  bool AppendRowEncode( std::ostream & out, const char * data, size_t datalen ) override;
  bool AppendFrameEncode( std::ostream & out, const char * data, size_t datalen ) override;
  bool StopEncode( std::ostream & ) override;

private:
  bool DecodeByStreamsCommon(const char *buffer, size_t totalLen, std::vector<unsigned char> &rgbyteOut);
  bool CodeFrameIntoBuffer(char * outdata, size_t outlen, size_t & complen, const char * indata, size_t inlen );

  unsigned long BufferLength;
  int LossyError;
};

} // end namespace gdcm

#endif //GDCMJPEGLSCODEC_H
