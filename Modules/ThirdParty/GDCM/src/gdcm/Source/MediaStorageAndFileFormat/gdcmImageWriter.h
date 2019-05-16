/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMIMAGEWRITER_H
#define GDCMIMAGEWRITER_H

#include "gdcmPixmapWriter.h"
#include "gdcmImage.h"

namespace gdcm
{

class Image;
/**
 * \brief ImageWriter
 */
class GDCM_EXPORT ImageWriter : public PixmapWriter
{
public:
  ImageWriter();
  ~ImageWriter() override;

  /// Set/Get Image to be written
  /// It will overwrite anything Image infos found in DataSet
  /// (see parent class to see how to pass dataset)
  const Image& GetImage() const override { return dynamic_cast<const Image&>(*PixelData); }
  Image& GetImage() override { return dynamic_cast<Image&>(*PixelData); } // FIXME
  //void SetImage(Image const &img);

  /// Write
  bool Write() override; // Execute()

  /// internal function used to compute a target MediaStorage the most appropriate
  /// User may want to call this function ahead of time (before Write)
  MediaStorage ComputeTargetMediaStorage();
protected:

private:
};

} // end namespace gdcm

#endif //GDCMIMAGEWRITER_H
