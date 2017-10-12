/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMIMAGEEXTENTREADER_H
#define GDCMIMAGEEXTENTREADER_H

#include "gdcmImageReader.h"
#include "gdcmImage.h"
#include "gdcmRegion.h"

namespace gdcm
{

class ImageRegionReaderInternals;
/**
 * \brief ImageRegionReader
 * \details This class is able to read a region from a DICOM file containing an image. This implementation
 * requires that the information stored in the DICOM header are consistent with what is in the
 * encapsulated Pixel Data. This is technically not required by DICOM standard, which makes 
 * this implementation illegal with regards to the famous JPEG note:
 * http://dicom.nema.org/medical/dicom/current/output/chtml/part05/sect_8.2.html#para_4bcb841e-c6bf-4e26-82a5-3fad3c942da0
 * \see ImageReader
 */
class GDCM_EXPORT ImageRegionReader : public ImageReader
{
public:
  ImageRegionReader();
  ~ImageRegionReader();

  /// Set/Get Region to be read
  void SetRegion(Region const & region);
  Region const &GetRegion() const;

  /// Explicit call which will compute the minimal buffer length that can hold the whole
  /// uncompressed image as defined by Region `region`.
  /// \return 0 upon error
  size_t ComputeBufferLength() const;

  /// Read meta information (not Pixel Data) from the DICOM file.
  /// \return false upon error
  bool ReadInformation();

  /// Read into buffer:
  /// \return false upon error
  bool ReadIntoBuffer(char *inreadbuffer, size_t buflen);

protected:
  /// To prevent user from calling super class Read() function
  bool Read();

private:
  BoxRegion ComputeBoundingBox();
  bool ReadRAWIntoBuffer(char *buffer, size_t buflen);
  bool ReadRLEIntoBuffer(char *buffer, size_t buflen);
  bool ReadJPEG2000IntoBuffer(char *buffer, size_t buflen);
  bool ReadJPEGIntoBuffer(char *buffer, size_t buflen);
  bool ReadJPEGLSIntoBuffer(char *buffer, size_t buflen);
  ImageRegionReaderInternals *Internals;
};

} // end namespace gdcm

#endif //GDCMIMAGEEXTENTREADER_H
