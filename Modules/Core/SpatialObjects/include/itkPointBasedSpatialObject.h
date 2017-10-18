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
#ifndef itkPointBasedSpatialObject_h
#define itkPointBasedSpatialObject_h

#include "itkSpatialObject.h"
#include "itkSpatialObjectPoint.h"

namespace itk
{
/**
 * \class PointBasedSpatialObject
 * \brief This class serves as the base class for point-based spatial objects
 *
 * A PointBasedSpatialObject is an abstract class to support
 * PointBasedSpatialObject filters and algorithms.
 *
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT PointBasedSpatialObject:
  public SpatialObject< TDimension >
{
public:

  typedef PointBasedSpatialObject                  Self;
  typedef SpatialObject< TDimension >              Superclass;
  typedef SmartPointer< Self >                     Pointer;
  typedef SmartPointer< const Self >               ConstPointer;
  typedef double                                   ScalarType;
  typedef SpatialObjectPoint< TDimension >         SpatialObjectPointType;
  typedef typename Superclass::PointType           PointType;
  typedef typename Superclass::TransformType       TransformType;
  typedef typename Superclass::VectorType          VectorType;
  typedef typename Superclass::CovariantVectorType CovariantVectorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(PointBasedSpatialObject, SpatialObject);

  /** Return a SpatialObjectPoint given its position in the list */
  virtual const SpatialObjectPointType *
  GetPoint( IdentifierType itkNotUsed(id) ) const
  {
    itkWarningMacro(<< "PointBasedSpatialObject::GetPoint() is not implemented"
                    << " in the base class" << std::endl);
    return ITK_NULLPTR;
  }

  virtual SpatialObjectPointType *
  GetPoint( IdentifierType itkNotUsed(id) )
  {
    itkWarningMacro(<< "PointBasedSpatialObject::GetPoint() is not implemented"
                    << " in the base class" << std::endl);
    return ITK_NULLPTR;
  }

  /** Return the number of points in the list */
  virtual SizeValueType GetNumberOfPoints(void) const
  {
    itkWarningMacro(<< "PointBasedSpatialObject::GetNumberOfPoints() is not"
                    << " implemented in the base class" << std::endl);
    return 0;
  }

  /**  */
  bool ComputeLocalBoundingBox() const ITK_OVERRIDE
  {
    itkWarningMacro(<< "PointBasedSpatialObject::ComputeLocalBoundingBox() is"
                    << " not implemented in the base class" << std::endl);
    return false;
  }

protected:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointBasedSpatialObject);

  PointBasedSpatialObject();
  virtual ~PointBasedSpatialObject() ITK_OVERRIDE;

  /** Method to print the object.*/
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointBasedSpatialObject.hxx"
#endif

#endif // itkPointBasedSpatialObject_h
