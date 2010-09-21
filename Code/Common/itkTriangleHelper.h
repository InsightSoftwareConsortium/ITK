/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleHelper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTriangleHelper_h
#define __itkTriangleHelper_h

#include "itkCrossHelper.h"

namespace itk
{
/** \class TriangleHelper
 * \brief Convenient class for various triangles elements computation in
 * 2D or 3D
 */
template< typename TPoint >
class TriangleHelper
{
public:
  typedef TriangleHelper                   Self;
  typedef TPoint                           PointType;
  typedef typename PointType::CoordRepType CoordRepType;
  typedef typename PointType::VectorType   VectorType;
  typedef CrossHelper< VectorType >        CrossVectorType;

  itkStaticConstMacro(PointDimension, unsigned int, PointType::PointDimension);

  /** \brief return true if (iA,iB,iC) forms an Obtuse angle (above 90
    degrees)*/
  static bool IsObtuse(const PointType & iA, const PointType & iB, const PointType & iC);

  /** \brief Compute Normal vector to the triangle formed by (iA,iB,iC)*/
  static VectorType ComputeNormal(const PointType & iA,
                                  const PointType & iB,
                                  const PointType & iC);

  /** \brief Compute cotangent(iA,iB,iC)*/
  static CoordRepType Cotangent(const PointType & iA,
                                const PointType & iB,
                                const PointType & iC);

  /** \brief Compute barycenter, with given weights*/
  static PointType ComputeBarycenter(
    const CoordRepType & iA1, const PointType & iP1,
    const CoordRepType & iA2, const PointType & iP2,
    const CoordRepType & iA3, const PointType & iP3);

  /** \brief Compute angles (iA,iB,iC)*/
  static CoordRepType ComputeAngle(const PointType & iP1, const PointType & iP2,
                                   const PointType & iP3);

  /** \brief Compute center of mass*/
  static PointType ComputeGravityCenter(
    const PointType & iP1,
    const PointType & iP2,
    const PointType & iP3);

  /** \brief Compute circum center*/
  static PointType ComputeCircumCenter(
    const PointType & iP1,
    const PointType & iP2,
    const PointType & iP3);

  /** \brief Compute circum center constrained to be inside the triangle.*/
  static PointType ComputeConstrainedCircumCenter(const PointType & iP1,
                                                  const PointType & iP2, const PointType & iP3);

  /** \brief Compute Area.*/
  static CoordRepType ComputeArea(const PointType & iP1, const PointType & iP2, const PointType & iP3);

protected:
  TriangleHelper();
  virtual ~TriangleHelper();

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  TriangleHelper(const Self &);  // purposely not implemented
  void operator=(const Self &);  // purposely not implemented
};
}

#include "itkTriangleHelper.txx"
#endif
