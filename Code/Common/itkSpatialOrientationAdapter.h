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
/** \class SpatialOrientationAdapter
 *  \brief converts SpatialOrientation flags to/from direction cosines
 */
template <int Dimension>
class SpatialOrientationAdapter : 
    public OrientationAdapterBase<SpatialOrientation::ValidCoordinateOrientationFlags,Dimension>
{
public:
  /** typedef for superclass */
  typedef SpatialOrientationAdapter Self;

  typedef OrientationAdapterBase<SpatialOrientation::ValidCoordinateOrientationFlags,Dimension>
  SuperClass;

  typedef SpatialOrientation::ValidCoordinateOrientationFlags OrientationType;

  /** The dimension of the input image must be 3. */
  itkConceptMacro(DimensionShouldBe3,
    (Concept::SameDimension<Dimension,3>));

  /** typedef for direction cosines */
  typedef typename SuperClass::DirectionType DirectionType;

  /** convert from direction cosines. */
  virtual OrientationType FromDirectionCosines(const DirectionType &Dir);

  /** convert to direction cosines. */
  virtual DirectionType ToDirectionCosines(const OrientationType &Or);

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialOrientationAdapter.txx"
#endif

#endif // __itkSpatialOrientationAdapter_h
