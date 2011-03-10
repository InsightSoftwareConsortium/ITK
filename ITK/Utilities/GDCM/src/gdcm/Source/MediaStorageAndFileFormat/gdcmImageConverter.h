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

#ifndef GDCMIMAGECONVERTER_H
#define GDCMIMAGECONVERTER_H

#include "gdcmTypes.h"

namespace gdcm
{

class Image;
/**
 * \brief Image Converter
 * \note
 * This is the class used to convert from on gdcm::Image to another
 * This is typically used to convert let say YBR JPEG compressed
 * gdcm::Image to a RAW RGB gdcm::Image. So that the buffer can be directly
 * pass to third party application.
 * This filter is application level and not integrated directly in GDCM
 */
class GDCM_EXPORT ImageConverter
{
public:
  ImageConverter();
  ~ImageConverter();

  void SetInput(Image const &input);
  const Image& GetOuput() const;

  void Convert();

private:
  Image *Input;
  Image *Output;
};

} // end namespace gdcm

#endif //GDCMIMAGECONVERTER_H
