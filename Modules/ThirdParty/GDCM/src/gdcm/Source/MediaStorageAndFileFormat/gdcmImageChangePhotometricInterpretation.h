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
#include <limits>

namespace gdcm
{

class DataElement;
/**
 * \brief ImageChangePhotometricInterpretation class
 * \details Class to change the Photometric Interpretation of an input DICOM
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

  /// colorspace conversion (based on CCIR Recommendation 601-2)
  /// -> T.871
  template <typename T>
  static void RGB2YBR(T ybr[3], const T rgb[3], unsigned short storedbits = 8);
  template <typename T>
  static void YBR2RGB(T rgb[3], const T ybr[3], unsigned short storedbits = 8);

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
static inline T Clamp(int v)
{
  assert( std::numeric_limits<T>::min() == 0 );
  return v < 0 ? 0 : (v > std::numeric_limits<T>::max() ? std::numeric_limits<T>::max() : v);
}


template <typename T>
void ImageChangePhotometricInterpretation::RGB2YBR(T ybr[3], const T rgb[3], unsigned short storedbits)
{
  // Implementation details, since the equations from:
  // http://dicom.nema.org/medical/dicom/current/output/chtml/part03/sect_C.7.6.3.html#sect_C.7.6.3.1.2
  // are rounded to the 4th decimal precision, prefer the exact equation from the original document at:
  // CCIR Recommendation 601-2, also found in T.871 (Section §7, page 4)
  const double R = rgb[0];
  const double G = rgb[1];
  const double B = rgb[2];
  assert( storedbits <= sizeof(T) * 8 );
  const int halffullscale = 1 << (storedbits - 1);
  const int Y  = Round(  0.299 * R + 0.587 * G + 0.114 * B                       );
  const int CB = Round((-0.299 * R - 0.587 * G + 0.886 * B)/1.772 + halffullscale);
  const int CR = Round(( 0.701 * R - 0.587 * G - 0.114 * B)/1.402 + halffullscale);
  ybr[0] = Clamp<T>(Y );
  ybr[1] = Clamp<T>(CB);
  ybr[2] = Clamp<T>(CR);
}

template <typename T>
void ImageChangePhotometricInterpretation::YBR2RGB(T rgb[3], const T ybr[3], unsigned short storedbits)
{
  const double Y  = ybr[0];
  const double Cb = ybr[1];
  const double Cr = ybr[2];
  assert( storedbits <= sizeof(T) * 8 );
  const int halffullscale = 1 << (storedbits - 1);
  const int R = Round(Y                                       + 1.402 * (Cr-halffullscale)               );
  const int G = Round(Y -( 0.114 * 1.772 * (Cb-halffullscale) + 0.299 * 1.402 * (Cr-halffullscale))/0.587);
  const int B = Round(Y          + 1.772 * (Cb-halffullscale)                                            );
  rgb[0] = Clamp<T>(R);
  rgb[1] = Clamp<T>(G);
  rgb[2] = Clamp<T>(B);
}

} // end namespace gdcm

#endif //GDCMIMAGECHANGEPHOTOMETRICINTERPRETATION_H
