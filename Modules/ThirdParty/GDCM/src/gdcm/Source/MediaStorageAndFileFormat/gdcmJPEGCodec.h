/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMJPEGCODEC_H
#define GDCMJPEGCODEC_H

#include "gdcmImageCodec.h"

namespace gdcm
{

class PixelFormat;
class TransferSyntax;
/**
 * \brief JPEG codec
 * \details Class to do JPEG (8bits, 12bits, 16bits lossy & lossless).
 * It redispatch in between the different codec implementation: JPEG8Codec,
 * JPEG12Codec & JPEG16Codec
 * It also support inconsistency in between DICOM header and JPEG compressed stream
 * ImageCodec implementation for the JPEG case
 *
 * \note
 * Things you should know if you ever want to dive into DICOM/JPEG world (among other):
 *
 * - http://groups.google.com/group/comp.protocols.dicom/browse_thread/thread/625e46919f2080e1
 * - http://groups.google.com/group/comp.protocols.dicom/browse_thread/thread/75fdfccc65a6243
 * - http://groups.google.com/group/comp.protocols.dicom/browse_thread/thread/2d525ef6a2f093ed
 * - http://groups.google.com/group/comp.protocols.dicom/browse_thread/thread/6b93af410f8c921f
 */
class GDCM_EXPORT JPEGCodec : public ImageCodec
{
friend class ImageRegionReader;
public:
  JPEGCodec();
  ~JPEGCodec() override;
  bool CanDecode(TransferSyntax const &ts) const override;
  bool CanCode(TransferSyntax const &ts) const override;
  bool Decode(DataElement const &is, DataElement &os) override;
  void SetPixelFormat(PixelFormat const &pf) override;

  /// Compute the offset table:
  void ComputeOffsetTable(bool b);

  /// Compress into JPEG
  bool Code(DataElement const &in, DataElement &out) override;

  bool GetHeaderInfo(std::istream &is, TransferSyntax &ts) override;
  ImageCodec * Clone() const override;

  //void SetReversible(bool res);

  void SetQuality(double q);
  double GetQuality() const;

  void SetLossless(bool l);
  bool GetLossless() const;

  virtual bool EncodeBuffer( std::ostream & out,
    const char *inbuffer, size_t inlen);

protected:
  bool DecodeExtent(
    char *buffer,
    unsigned int xmin, unsigned int xmax,
    unsigned int ymin, unsigned int ymax,
    unsigned int zmin, unsigned int zmax,
    std::istream & is
  );

  bool DecodeByStreams(std::istream &is, std::ostream &os) override;
  bool IsValid(PhotometricInterpretation const &pi) override;

  bool StartEncode( std::ostream & ) override;
  bool IsRowEncoder() override;
  bool IsFrameEncoder() override;
  bool AppendRowEncode( std::ostream & out, const char * data, size_t datalen ) override;
  bool AppendFrameEncode( std::ostream & out, const char * data, size_t datalen ) override;
  bool StopEncode( std::ostream & ) override;

protected:
  // Internal method called by SetPixelFormat
  // Instantiate the right jpeg codec (8, 12 or 16)
  void SetBitSample(int bit);

  virtual bool IsStateSuspension() const;

protected:
  int BitSample;
  //bool Lossless;
  int Quality;

private:
  void SetupJPEGBitCodec(int bit);
  JPEGCodec *Internal;
};

} // end namespace gdcm

#endif //GDCMJPEGCODEC_H
