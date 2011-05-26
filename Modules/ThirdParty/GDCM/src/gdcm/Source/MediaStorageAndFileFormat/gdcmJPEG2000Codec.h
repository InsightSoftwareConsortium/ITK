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
#ifndef GDCMJPEG2000CODEC_H
#define GDCMJPEG2000CODEC_H

#include "gdcmImageCodec.h"

namespace gdcm
{

class JPEG2000Internals;
/**
 * \brief Class to do JPEG 2000
 * \note
 * the class will produce JPC (JPEG 2000 codestream), since some private implementor
 * are using full jp2 file the decoder tolerate jp2 input
 * this is an implementation of an ImageCodec
 */
class GDCM_EXPORT JPEG2000Codec : public ImageCodec
{
  friend class Bitmap;
public:
  JPEG2000Codec();
  ~JPEG2000Codec();

  bool CanDecode(TransferSyntax const &ts) const;
  bool CanCode(TransferSyntax const &ts) const;

  bool Decode(DataElement const &is, DataElement &os);
  bool Code(DataElement const &in, DataElement &out);

  virtual bool GetHeaderInfo(std::istream &is, TransferSyntax &ts);

  // JPEG-2000 / OpenJPEG specific way of encoding lossy-ness
  // ref: http://www.openjpeg.org/index.php?menu=doc#encoder
  void SetRate(unsigned int idx, double rate);
  double GetRate(unsigned int idx = 0) const;

  void SetQuality(unsigned int idx, double q);
  double GetQuality(unsigned int idx = 0) const;

  void SetTileSize(unsigned int tx, unsigned int ty);

  void SetNumberOfResolutions(unsigned int nres);

  void SetReversible(bool res);

protected:
  bool Decode(std::istream &is, std::ostream &os);
private:
  bool GetHeaderInfo(const char * dummy_buffer, size_t len, TransferSyntax &ts);
  JPEG2000Internals *Internals;
};

} // end namespace gdcm

#endif //GDCMJPEG2000CODEC_H
