/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSurfaceSpatialObjectPoint_h
#define itkSurfaceSpatialObjectPoint_h

#include "itkSpatialObjectPoint.h"
#include "itkCovariantVector.h"

namespace itk
{
/** \class SurfaceSpatialObjectPoint
 * \brief Point used for a Surface definition
 *
 * This class contains all the functions necessary to define a point
 * that can be used to build surfaces.
 * A surface point has a position and only one normal
 *
 * \sa SpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TPointDimension = 3 >
class ITK_TEMPLATE_EXPORT SurfaceSpatialObjectPoint:
  public SpatialObjectPoint< TPointDimension >
{
public:

  typedef SurfaceSpatialObjectPoint                  Self;
  typedef SpatialObjectPoint< TPointDimension >      Superclass;
  typedef Point< double, TPointDimension >           PointType;
  typedef CovariantVector< double, TPointDimension > VectorType;

  /** Constructor */
  SurfaceSpatialObjectPoint();

  /** Destructor */
  virtual ~SurfaceSpatialObjectPoint() ITK_OVERRIDE;

  /** Get Normal */
  const VectorType & GetNormal() const;

  /** Set Normal */
  void SetNormal(const VectorType & normal);

  void SetNormal(const double normalx, const double normaly);

  void SetNormal(const double normalx, const double normaly,
                 const double normalz);

  /** Copy one SurfaceSpatialObjectPoint to another */
  Self & operator=(const SurfaceSpatialObjectPoint & rhs);

protected:

  VectorType m_Normal;

  /** Method to print the object. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSurfaceSpatialObjectPoint.hxx"
#endif

#endif // itkSurfaceSpatialObjectPoint_h
