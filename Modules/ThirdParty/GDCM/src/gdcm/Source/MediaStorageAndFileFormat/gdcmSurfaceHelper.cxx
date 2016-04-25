/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmSurfaceHelper.h"

#include <cmath>

namespace gdcm
{

std::vector< float > SurfaceHelper::RGBToXYZ(const std::vector<float> & RGB)
{
  std::vector< float > XYZ(3);
  float tmp[3];

  tmp[0] = RGB[0];
  tmp[1] = RGB[1];
  tmp[2] = RGB[2];

  const float A = 1.0f / 12.92f;
  const float B = 1.0f / 1.055f;

  if ( tmp[0] > 0.04045f ) tmp[0] = powf( ( tmp[0] + 0.055f ) * B, 2.4f);
  else                     tmp[0] *= A;
  if ( tmp[1] > 0.04045f ) tmp[1] = powf( ( tmp[1] + 0.055f ) * B, 2.4f);
  else                     tmp[1] *= A;
  if ( tmp[2] > 0.04045f ) tmp[2] = powf( ( tmp[2] + 0.055f ) * B, 2.4f);
  else                     tmp[2] *= A;

  tmp[0] *= 100;
  tmp[1] *= 100;
  tmp[2] *= 100;

  //Observer. = 2°, Illuminant = D65
  XYZ[0] = (tmp[0] * 0.4124f + tmp[1] * 0.3576f + tmp[2] * 0.1805f);
  XYZ[1] = (tmp[0] * 0.2126f + tmp[1] * 0.7152f + tmp[2] * 0.0722f);
  XYZ[2] = (tmp[0] * 0.0193f + tmp[1] * 0.1192f + tmp[2] * 0.9505f);

  return XYZ;
}

std::vector< float > SurfaceHelper::XYZToRGB(const std::vector<float> & XYZ)
{
  std::vector< float > RGB(3);
  float tmp[3];

  tmp[0] = XYZ[0];
  tmp[1] = XYZ[1];
  tmp[2] = XYZ[2];

  // Divide by 100
  tmp[0] *= 0.01f;        //X from 0 to  95.047      (Observer = 2°, Illuminant = D65)
  tmp[1] *= 0.01f;        //Y from 0 to 100.000
  tmp[2] *= 0.01f;        //Z from 0 to 108.883

  RGB[0] = (tmp[0] *  3.2406f + tmp[1] * -1.5372f + tmp[2] * -0.4986f);
  RGB[1] = (tmp[0] * -0.9689f + tmp[1] *  1.8758f + tmp[2] *  0.0415f);
  RGB[2] = (tmp[0] *  0.0557f + tmp[1] * -0.2040f + tmp[2] *  1.0570f);

  const float A = 1.0f / 2.4f;

  if ( RGB[0] > 0.0031308f )   RGB[0] = 1.055f * powf( RGB[0], A) - 0.055f;
  else                         RGB[0] *= 12.92f;
  if ( RGB[1] > 0.0031308f )   RGB[1] = 1.055f * powf( RGB[1], A) - 0.055f;
  else                         RGB[1] *= 12.92f;
  if ( RGB[2] > 0.0031308f )   RGB[2] = 1.055f * powf( RGB[2], A) - 0.055f;
  else                         RGB[2] *= 12.92f;

  return RGB;
}

std::vector< float > SurfaceHelper::XYZToCIELab(const std::vector<float> & XYZ)
{
  std::vector< float > CIELab(3);
  float tmp[3];

  tmp[0] = (XYZ[0] / 95.047f);   //ref_X =  95.047   Observer= 2°, Illuminant= D65
  tmp[1] = (XYZ[1] * 0.01f);     //ref_Y = 100.000
  tmp[2] = (XYZ[2] / 108.883f);  //ref_Z = 108.883

  const float A = 1.0f / 3.0f;
  const float B = 16.0f / 116.0f;

  if ( tmp[0] > 0.008856f )  tmp[0] = powf(tmp[0], A);
  else                       tmp[0] = (7.787f * tmp[0] + B);
  if ( tmp[1] > 0.008856f )  tmp[1] = powf(tmp[1], A);
  else                       tmp[1] = (7.787f * tmp[1] + B);
  if ( tmp[2] > 0.008856f )  tmp[2] = powf(tmp[2], A);
  else                       tmp[2] = (7.787f * tmp[2] + B);

  CIELab[0] = ( 116 * tmp[1] ) - 16;
  CIELab[1] = 500 * ( tmp[0] - tmp[1] );
  CIELab[2] = 200 * ( tmp[1] - tmp[2] );

  return CIELab;
}

std::vector< float > SurfaceHelper::CIELabToXYZ(const std::vector<float> & CIELab)
{
  std::vector< float > XYZ(3);
  float tmp[3];

  tmp[1] = (( CIELab[0] + 16 ) / 116.0f);
  tmp[0] = (CIELab[1] * 0.002f + tmp[1]);
  tmp[2] = (tmp[1] - CIELab[2] * 0.005f);

  // Compute t
  const float A = tmp[0]*tmp[0]*tmp[0];
  const float B = tmp[1]*tmp[1]*tmp[1];
  const float C = tmp[2]*tmp[2]*tmp[2];

  const float D = 16.0f / 116.0f;

  // Compute f(t)
  if ( B > 0.008856f) tmp[1] = B;
  else                tmp[1] = ( tmp[1] - D ) / 7.787f;
  if ( A > 0.008856f) tmp[0] = A;
  else                tmp[0] = ( tmp[0] - D ) / 7.787f;
  if ( C > 0.008856f) tmp[2] = C;
  else                tmp[2] = ( tmp[2] - D ) / 7.787f;

  XYZ[0] = (tmp[0] * 95.047f);   //ref_X =  95.047     Observer= 2°, Illuminant= D65
  XYZ[1] = (tmp[1] * 100.0f);    //ref_Y = 100.000
  XYZ[2] = (tmp[2] * 108.883f);  //ref_Z = 108.883

  return XYZ;
}

}
