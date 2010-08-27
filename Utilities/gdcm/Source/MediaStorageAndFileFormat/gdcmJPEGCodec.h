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
#ifndef GDCMJPEGCODEC_H
#define GDCMJPEGCODEC_H

#include "gdcmImageCodec.h"

namespace gdcm
{

class PixelFormat;
class TransferSyntax;
/**
 * \brief JPEG codec
 * Class to do JPEG (8bits, 12bits, 16bits lossy & lossless).
 * It redispatch in between the different codec implementation: gdcm::JPEG8Codec,
 * gdcm::JPEG12Codec & gdcm::JPEG16Codec
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
public:
  JPEGCodec();
  ~JPEGCodec();
  bool CanDecode(TransferSyntax const &ts) const;
  bool CanCode(TransferSyntax const &ts) const;
  bool Decode(DataElement const &is, DataElement &os);
  void SetPixelFormat(PixelFormat const &pf);

  /// Compute the offset table:
  void ComputeOffsetTable(bool b);

  /// Compress into JPEG
  bool Code(DataElement const &in, DataElement &out);

  virtual bool GetHeaderInfo(std::istream &is, TransferSyntax &ts);

  //void SetReversible(bool res);

  void SetQuality(double q);
  double GetQuality() const;

  void SetLossless(bool l);
  bool GetLossless() const;

protected:
  bool Decode(std::istream &is, std::ostream &os);
  bool IsValid(PhotometricInterpretation const &pi);

protected:
  // Internal method called by SetPixelFormat
  // Instantiate the right jpeg codec (8, 12 or 16)
  void SetBitSample(int bit);

protected:
  int BitSample;
  bool Lossless;
  int Quality;

private:
  void SetupJPEGBitCodec(int bit);
  JPEGCodec *Internal;
};

} // end namespace gdcm

#endif //GDCMJPEGCODEC_H
