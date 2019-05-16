/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMIMAGEAPPLYLOOKUPTABLE_H
#define GDCMIMAGEAPPLYLOOKUPTABLE_H

#include "gdcmImageToImageFilter.h"

namespace gdcm
{

class DataElement;
/**
 * \brief ImageApplyLookupTable class
 * \details It applies the LUT the PixelData (only PALETTE_COLOR images)
 * Output will be a PhotometricInterpretation=RGB image
 */
class GDCM_EXPORT ImageApplyLookupTable : public ImageToImageFilter
{
public:
  ImageApplyLookupTable();
  ~ImageApplyLookupTable();

  /// Apply
  bool Apply();

  /// RGB8 ?
  void SetRGB8(bool b);

protected:

private:
  struct impl;
  // PIMPL idiom
  impl* pimpl;
};

} // end namespace gdcm

#endif //GDCMIMAGEAPPLYLOOKUPTABLE_H
