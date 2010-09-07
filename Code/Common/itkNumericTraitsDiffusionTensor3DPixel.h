/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsDiffusionTensor3DPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsDiffusionTensor3DPixel_h
#define __itkNumericTraitsDiffusionTensor3DPixel_h

#include "itkNumericTraits.h"
#include "itkDiffusionTensor3D.h"

// This file is meant to define numeric traits for tensor pixels types in itk

namespace itk
{
/** \class NumericTraits<DiffusionTensor3D< T > >
 * \brief Define numeric traits for DiffusionTensor3D.
 *
 * We provide here a generic implementation based on creating types of
 * DiffusionTensor3D whose components are the types of the NumericTraits from
 * the original DiffusionTensor3D components. This implementation require
 * support for partial specializations, since it is based on the
 * concept that:
 *   NumericTraits<DiffusionTensor3D< T > >  is defined piecewise by
 *   DiffusionTensor3D< NumericTraits< T > >
 *
 * \sa NumericTraits
 * \ingroup DataRepresentation
 */
template< typename T >
class NumericTraits< DiffusionTensor3D< T > >
{
private:

  typedef typename NumericTraits< T >::AbsType        ElementAbsType;
  typedef typename NumericTraits< T >::AccumulateType ElementAccumulateType;
  typedef typename NumericTraits< T >::FloatType      ElementFloatType;
  typedef typename NumericTraits< T >::PrintType      ElementPrintType;
  typedef typename NumericTraits< T >::RealType       ElementRealType;
public:

  /** Return the type of the native component type. */
  typedef T ValueType;

  typedef DiffusionTensor3D< T > Self;

  /** Unsigned component type */
  typedef DiffusionTensor3D< ElementAbsType > AbsType;

  /** Accumulation of addition and multiplication. */
  typedef DiffusionTensor3D< ElementAccumulateType > AccumulateType;

  /** Typedef for operations that use floating point instead of real precision
    */
  typedef DiffusionTensor3D< ElementFloatType > FloatType;

  /** Return the type that can be printed. */
  typedef DiffusionTensor3D< ElementPrintType > PrintType;

  /** Type for real-valued scalar operations. */
  typedef DiffusionTensor3D< ElementRealType > RealType;

  /** Type for real-valued scalar operations. */
  typedef ElementRealType ScalarRealType;

  /** Component wise defined element
   *
   * \note minimum value for floating pointer types is defined as
   * minimum positive normalize value.
   */
  static const Self max(const Self &)
  {
    return Self( NumericTraits< T >::max() );
  }

  static const Self min(const Self &)
  {
    return Self( NumericTraits< T >::min() );
  }

  static const Self max()
  {
    return Self( NumericTraits< T >::max() );
  }

  static const Self min()
  {
    return Self( NumericTraits< T >::min() );
  }

  static const Self NonpositiveMin()
  {
    return Self( NumericTraits< T >::NonpositiveMin() );
  }

  static const Self ZeroValue()
  {
    return Self( NumericTraits< T >::ZeroValue() );
  }

  static const Self OneValue()
  {
    return Self( NumericTraits< T >::OneValue() );
  }

  /** \note: the functions are prefered over the member variables as
   * they are defined for all partial specialization
   */
  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;
};
} // end namespace itk

#endif // __itkNumericTraitsTensorPixel_h
