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
#ifndef __itkSpatialObjectDuplicator_h
#define __itkSpatialObjectDuplicator_h

#include "itkObject.h"
#include "itkSpatialObject.h"

namespace itk
{
/** \class SpatialObjectDuplicator
 *  This helper class create an SpatialObject which is perfect
 *  copy of the input SpatialObject
 * \ingroup ITKSpatialObjects
 */
template< class TInputSpatialObject >
class ITK_EXPORT SpatialObjectDuplicator:public Object
{
public:
  /** Standard class typedefs. */
  typedef SpatialObjectDuplicator    Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectDuplicator, Object);

  /** Type definitions for the input SpatialObject. */
  typedef TInputSpatialObject                        SpatialObjectType;
  typedef typename TInputSpatialObject::Pointer      SpatialObjectPointer;
  typedef typename TInputSpatialObject::ConstPointer SpatialObjectConstPointer;

  itkStaticConstMacro(ObjectDimension, unsigned int,
                      SpatialObjectType::ObjectDimension);

  typedef SpatialObject< itkGetStaticConstMacro(ObjectDimension) >
  InternalSpatialObjectType;

  /** Get/Set the input SpatialObject. */
  itkSetConstObjectMacro(Input, SpatialObjectType);

  /** Get/Set the output SpatialObject. */
  itkGetModifiableObjectMacro(Output, SpatialObjectType);

  /** Compute of the input SpatialObject. */
  void Update(void);

protected:
  SpatialObjectDuplicator();
  virtual ~SpatialObjectDuplicator() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  void CopyObject(const InternalSpatialObjectType *source,
                  InternalSpatialObjectType *destination);

private:
  SpatialObjectDuplicator(const Self &); //purposely not implemented
  void operator=(const Self &);          //purposely not implemented

  SpatialObjectConstPointer m_Input;
  SpatialObjectPointer      m_Output;
  ModifiedTimeType          m_InternalSpatialObjectTime;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectDuplicator.hxx"
#endif

#endif /* __itkSpatialObjectDuplicator_h */
