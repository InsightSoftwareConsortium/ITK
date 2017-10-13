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
#ifndef itkDTITubeSpatialObject_h
#define itkDTITubeSpatialObject_h

#include <list>

#include "itkTubeSpatialObject.h"
#include "itkDTITubeSpatialObjectPoint.h"

namespace itk
{
/**
 * \class DTITubeSpatialObject
 * \brief Representation of a tube based on the spatial object classes.
 *
 * The tube is basically defined by a set of points. Each tube can
 * be connected to a tube network, by using the AddSpatialObject() methods
 * of a DTITubeSpatialObject Object.
 * A tube is also identified by an id number when connected to a network.
 *
 * \sa DTITubeSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TDimension = 3 >
class ITK_TEMPLATE_EXPORT DTITubeSpatialObject:
  public TubeSpatialObject< TDimension,
                            DTITubeSpatialObjectPoint< TDimension >  >
{
public:

  typedef DTITubeSpatialObject Self;
  typedef TubeSpatialObject< TDimension,
                             DTITubeSpatialObjectPoint< TDimension > >    Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;
  typedef DTITubeSpatialObjectPoint< TDimension >      TubePointType;
  typedef typename Superclass::PointListType           PointListType;
  typedef typename Superclass::PointType               PointType;
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::SpatialObjectPointType  SpatialObjectPointType;
  typedef VectorContainer< IdentifierType, PointType > PointContainerType;
  typedef SmartPointer< PointContainerType >           PointContainerPointer;
  typedef typename Superclass::VectorType              VectorType;
  typedef typename Superclass::CovariantVectorType     CovariantVectorType;
  typedef typename Superclass::BoundingBoxType         BoundingBoxType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Method for creation through the object factory. */
  itkTypeMacro(DTITubeSpatialObject, TubeSpatialObject);

protected:

  DTITubeSpatialObject();
  virtual ~DTITubeSpatialObject() ITK_OVERRIDE;

  /** Method to print the object.*/
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DTITubeSpatialObject);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDTITubeSpatialObject.hxx"
#endif

#endif // itkDTITubeSpatialObject_h
