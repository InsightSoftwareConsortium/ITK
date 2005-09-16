/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialOrientationAdapter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSpatialOrientationAdapter_txx
#define _itkSpatialOrientationAdapter_txx
#include "itkSpatialOrientationAdapter.h"
#include "itkImageBase.h"

namespace itk
{

namespace Function
{

inline unsigned Max3(float x, float y, float z)
{
  const double obliquityThresholdCosineValue = 0.001;
  
  double absX = vnl_math_abs(x);
  double absY = vnl_math_abs(y);
  double absZ = vnl_math_abs(z);

  if ( ( absX > obliquityThresholdCosineValue ) && ( absX > absY ) && ( absX > absZ ))
    {
    return 0;
    }
  else if (  ( absY > obliquityThresholdCosineValue ) && ( absY > absX ) && ( absY > absZ ) )
    {
    return 1;
    }
  else if ( ( absZ > obliquityThresholdCosineValue ) && ( absZ > absX ) && ( absZ > absY ) )
    {
    return 2;
    }
  // they must all be equal, so just say x
  return 0;
}

inline int Sign(float x)
{
  if(x < 0)
    return -1;
  return 1;
}

} // namespace Function

template <int Dimension>
typename SpatialOrientationAdapter<Dimension>::OrientationType 
SpatialOrientationAdapter<Dimension>
::FromDirectionCosines(const typename SpatialOrientationAdapter<Dimension>::DirectionType &Dir)
{
  //  const typename ImageBase<VImageDimension>::DirectionType &direction = 
  //    this->GetDirection();
 int axes[9] = {0,0,0,0,0,0,0,0,0};
  int dominant_axis;

  dominant_axis = Function::Max3(Dir[0][0],Dir[1][0],Dir[2][0]);
  axes[dominant_axis] = Function::Sign(Dir[dominant_axis][0]);
  dominant_axis = Function::Max3(Dir[0][1],Dir[1][1],Dir[2][1]);
  axes[dominant_axis+3] = Function::Sign(Dir[dominant_axis][1]);
  dominant_axis = Function::Max3(Dir[0][2],Dir[1][2],Dir[2][2]);
  axes[dominant_axis+6] = Function::Sign(Dir[dominant_axis][2]);
    
  SpatialOrientation::CoordinateTerms terms[3] = {SpatialOrientation::ITK_COORDINATE_UNKNOWN,SpatialOrientation::ITK_COORDINATE_UNKNOWN,SpatialOrientation::ITK_COORDINATE_UNKNOWN};

  for(unsigned i = 0; i < 3; i++)
    {
    if(int(axes[(i*3)]) == 1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Right;
      }
    else if(axes[(i*3)] == -1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Left;
      }
    else if(axes[(i*3)+1] == 1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Anterior;
      }
    else if(axes[(i*3)+1] == -1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Posterior;
      }
    else if(axes[(i*3)+2] == 1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Inferior;
      }
    else if(axes[(i*3)+2] == -1)
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Superior;
      }
    }
  //
  // all terms must be defined, otherwise just punt
  if(terms[0] == SpatialOrientation::ITK_COORDINATE_UNKNOWN || terms[1] == SpatialOrientation::ITK_COORDINATE_UNKNOWN || terms[2] == SpatialOrientation::ITK_COORDINATE_UNKNOWN)
    {
    return SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
    }
  return static_cast<SpatialOrientation::ValidCoordinateOrientationFlags>
    ((terms[0] << 
      SpatialOrientation::ITK_COORDINATE_PrimaryMinor) +
     (terms[1] << 
      SpatialOrientation::ITK_COORDINATE_SecondaryMinor) +
     (terms[2] << 
      SpatialOrientation::ITK_COORDINATE_TertiaryMinor));
}

template <int Dimension>
typename SpatialOrientationAdapter<Dimension>::DirectionType
SpatialOrientationAdapter<Dimension>
::ToDirectionCosines(const typename SpatialOrientationAdapter<Dimension>::OrientationType &Or)
{
  typedef SpatialOrientation::CoordinateTerms CoordinateTerms;

  CoordinateTerms terms[3];
  terms[0] = 
    static_cast<CoordinateTerms>
    ((Or >> SpatialOrientation::ITK_COORDINATE_PrimaryMinor) & 0xff);
  terms[1] = 
    static_cast<CoordinateTerms>
    ((Or >> SpatialOrientation::ITK_COORDINATE_SecondaryMinor) & 0xff);
  terms[2] = 
    static_cast<CoordinateTerms>
    ((Or >> SpatialOrientation::ITK_COORDINATE_TertiaryMinor) & 0xff);
  DirectionType direction;
  for(unsigned int i = 0; i < 3; i++)
    {
    direction[0][i] = 
      direction[1][i] = 
      direction[2][i] = 0.0;
    switch(terms[i])
      {
      case SpatialOrientation::ITK_COORDINATE_Right:
        direction[0][i] = 1;
        break;
      case SpatialOrientation::ITK_COORDINATE_Left:
        direction[0][i] = -1;
        break;
      case SpatialOrientation::ITK_COORDINATE_Anterior:
        direction[1][i] = 1;
        break;
      case SpatialOrientation::ITK_COORDINATE_Posterior:
        direction[1][i] = -1;
        break;
      case SpatialOrientation::ITK_COORDINATE_Inferior:
        direction[2][i] = 1;
        break;
      case SpatialOrientation::ITK_COORDINATE_Superior:
        direction[2][i] = -1;
        break;
      case SpatialOrientation::ITK_COORDINATE_UNKNOWN:
      default:
        //TODO:  Should there be a default?  Throw an exception?
        break;
      }
    }
  //TODO:  Should check that directions are orthoganal.
  return direction;
}

}

#endif // _itkSpatialOrientationAdapter_txx
