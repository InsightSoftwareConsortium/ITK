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
#include "gdcmPixmapToPixmapFilter.h"
#include <limits>
#include <stdlib.h> // abort
#include <string.h> // memcpy

namespace gdcm
{

PixmapToPixmapFilter::PixmapToPixmapFilter()
{
  if(!Input) Input = new Pixmap;
  if(!Output) Output = new Pixmap;
}

void PixmapToPixmapFilter::SetInput(const Pixmap& image)
{
  Input = image;
}


} // end namespace gdcm
