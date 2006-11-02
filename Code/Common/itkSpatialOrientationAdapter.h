/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialOrientationAdapter.h
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
#ifndef __itkSpatialOrientationAdapter_h
#define __itkSpatialOrientationAdapter_h
#include "itkOrientationAdapter.h"
#include "itkSpatialOrientation.h"
#include "itkConceptChecking.h"

namespace itk
{

//
// Helper functions, better than Macros
//
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
    {
    return -1;
    }
  return 1;
}

} // namespace Function



/** \class SpatialOrientationAdapter
 *  \brief converts SpatialOrientation flags to/from direction cosines
 */
class SpatialOrientationAdapter : 
  public OrientationAdapterBase<SpatialOrientation::ValidCoordinateOrientationFlags,3>
{
public:
  /** typedef for superclass */
  typedef SpatialOrientationAdapter Self;

  typedef OrientationAdapterBase<SpatialOrientation::ValidCoordinateOrientationFlags,3>
  SuperClass;

  typedef SpatialOrientation::ValidCoordinateOrientationFlags OrientationType;

  /** typedef for direction cosines */
  typedef SuperClass::DirectionType DirectionType;

  /** convert from direction cosines. */
  virtual OrientationType FromDirectionCosines(const DirectionType &Dir);

  /** convert to direction cosines. */
  virtual DirectionType ToDirectionCosines(const OrientationType &Or);

};

} // namespace itk

#endif // __itkSpatialOrientationAdapter_h
