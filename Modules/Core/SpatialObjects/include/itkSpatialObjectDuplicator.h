/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkSpatialObjectDuplicator_h
#define itkSpatialObjectDuplicator_h

#include "itkObject.h"
#include "itkSpatialObject.h"

namespace itk
{
/**
 *\class SpatialObjectDuplicator
 *  This helper class create an SpatialObject which is perfect
 *  copy of the input SpatialObject
 * \ingroup ITKSpatialObjects
 */
template <typename TInputSpatialObject>
class ITK_TEMPLATE_EXPORT SpatialObjectDuplicator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SpatialObjectDuplicator);

  /** Standard class type aliases. */
  using Self = SpatialObjectDuplicator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectDuplicator, Object);

  /** Type definitions for the input SpatialObject. */
  using SpatialObjectType = TInputSpatialObject;
  using SpatialObjectPointer = typename TInputSpatialObject::Pointer;
  using SpatialObjectConstPointer = typename TInputSpatialObject::ConstPointer;

  static constexpr unsigned int ObjectDimension = SpatialObjectType::ObjectDimension;

  using InternalSpatialObjectType = SpatialObject<Self::ObjectDimension>;

  /** Get/Set the input SpatialObject. */
  itkSetConstObjectMacro(Input, SpatialObjectType);

  /**
   * Provide an interface to match that
   * of other ProcessObjects
   * for this source generation object
   * by returning a non-const pointer
   * for the generated Object.
   */
  // NOTE:  The m_GeneratedImageSource is only
  //       exposed via the Source generation interface
  //       by the GetOutput() method that mimics
  //       a process object.
  virtual const SpatialObjectType *
  GetOutput() const
  {
    return this->m_DuplicateSpatialObject.GetPointer();
  }
  virtual SpatialObjectType *
  GetOutput()
  {
    return this->m_DuplicateSpatialObject.GetPointer();
  }

#if !defined(ITK_LEGACY_REMOVE)
  // This interface was exposed in ITKv4 when the itkGetModifiableObjectMacro was used
  virtual SpatialObjectType *
  GetModifiedOutput()
  {
    return this->m_DuplicateSpatialObject.GetPointer();
  }
#endif


  /** Compute of the input SpatialObject. */
  void
  Update();

protected:
  SpatialObjectDuplicator();
  ~SpatialObjectDuplicator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Recursive function to copy the objects */
  void
  CopyObject(const InternalSpatialObjectType * source, InternalSpatialObjectType * destination);

private:
  SpatialObjectConstPointer m_Input;
  SpatialObjectPointer      m_DuplicateSpatialObject;
  ModifiedTimeType          m_InternalSpatialObjectTime;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpatialObjectDuplicator.hxx"
#endif

#endif /* itkSpatialObjectDuplicator_h */
