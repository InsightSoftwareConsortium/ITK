/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMIMAGECHANGEPHOTOMETRICINTERPRETATION_H
#define GDCMIMAGECHANGEPHOTOMETRICINTERPRETATION_H

#include "gdcmImageToImageFilter.h"
#include "gdcmPhotometricInterpretation.h"

namespace gdcm
{

class DataElement;
/**
 * \brief ImageChangePhotometricInterpretation class
 * \details Class to change the Photometric Interpetation of an input DICOM
 */
class GDCM_EXPORT ImageChangePhotometricInterpretation : public ImageToImageFilter
{
public:
  ImageChangePhotometricInterpretation():PI() {}
  ~ImageChangePhotometricInterpretation() = default;

  /// Set/Get requested PhotometricInterpretation
  void SetPhotometricInterpretation(PhotometricInterpretation const &pi) { PI = pi; }
  const PhotometricInterpretation &GetPhotometricInterpretation() const { return PI; }

  /// Change
  bool Change();

  /// colorspace converstion (based on CCIR Recommendation 601-2)
  /// -> T.871
  template <typename T>
  static void RGB2YBR(T ybr[3], const T rgb[3]);
  template <typename T>
  static void YBR2RGB(T rgb[3], const T ybr[3]);

protected:
  bool ChangeMonochrome();
  bool ChangeYBR2RGB();
  bool ChangeRGB2YBR();

private:
  PhotometricInterpretation PI;
};

template <typename T>
static inline int Round(T x)
{
  return (int)(x+0.5);
}

template <typename T>
static inline int Clamp(T v)
{
  return v < 0 ? 0 : (v > 255 ? 255 : v);
}


template <typename T>
void ImageChangePhotometricInterpretation::RGB2YBR(T ybr[3], const T rgb[3])
{
  const double R = rgb[0];
  const double G = rgb[1];
  const double B = rgb[2];
  const int Y  = Round(  .299 * R + .587 * G + .114 * B             );
  const int CB = Round((-.299 * R - .587 * G + .886 * B)/1.772 + 128);
  const int CR = Round(( .701 * R - .587 * G - .114 * B)/1.402 + 128);
  ybr[0] = Clamp(Y );
  ybr[1] = Clamp(CB);
  ybr[2] = Clamp(CR);
}

template <typename T>
void ImageChangePhotometricInterpretation::YBR2RGB(T rgb[3], const T ybr[3])
{
  const double Y  = ybr[0];
  const double Cb = ybr[1];
  const double Cr = ybr[2];
  const int R = Round(Y                                     + 1.402 * (Cr-128)       );
  const int G = Round(Y -( 0.114 * 1.772 * (Cb-128) + 0.299 * 1.402 * (Cr-128))/0.587);
  const int B = Round(Y          + 1.772 * (Cb-128)                                  );
  rgb[0] = Clamp(R);
  rgb[1] = Clamp(G);
  rgb[2] = Clamp(B);
}

} // end namespace gdcm

#endif //GDCMIMAGECHANGEPHOTOMETRICINTERPRETATION_H
