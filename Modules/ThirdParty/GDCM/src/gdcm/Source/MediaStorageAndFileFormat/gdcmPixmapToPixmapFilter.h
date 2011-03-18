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
#ifndef GDCMPIXMAPTOPIXMAPFILTER_H
#define GDCMPIXMAPTOPIXMAPFILTER_H

#include "gdcmPixmap.h"

namespace gdcm
{

/**
 * \brief PixmapToPixmapFilter class
 * Super class for all filter taking an image and producing an output image
 */
class GDCM_EXPORT PixmapToPixmapFilter
{
public:
  PixmapToPixmapFilter();
  ~PixmapToPixmapFilter() {}

  /// Set input image
  void SetInput(const Pixmap& image);

  /// Get Output image
  const Pixmap &GetOutput() const { return *Output; }

protected:
  SmartPointer<Pixmap> Input;
  SmartPointer<Pixmap> Output;
};

} // end namespace gdcm

#endif //GDCMPIXMAPTOPIXMAPFILTER_H
