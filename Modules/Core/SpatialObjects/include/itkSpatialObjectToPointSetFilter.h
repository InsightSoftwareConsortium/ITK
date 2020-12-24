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
#ifndef itkSpatialObjectToPointSetFilter_h
#define itkSpatialObjectToPointSetFilter_h

#include "itkPointSet.h"
#include "itkDataObject.h"
#include "itkMeshSource.h"
#include "itkPointBasedSpatialObject.h"

namespace itk
{
/**
 *\class SpatialObjectToPointSetFilter
 * \brief Base class for filters that take a SpatialObject
 *        as input and produce a PointSet as output.
 *  The pointset created is in physical space.
 * \ingroup ITKSpatialObjects
 */
template <typename TPointBasedSpatialObject, typename TOutputPointSet>
class ITK_TEMPLATE_EXPORT SpatialObjectToPointSetFilter : public MeshSource<TOutputPointSet>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SpatialObjectToPointSetFilter);

  /** Standard class type aliases. */
  using Self = SpatialObjectToPointSetFilter;
  using Superclass = MeshSource<TOutputPointSet>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputPointSetType = TOutputPointSet;
  using OutputPointSetPointer = typename OutputPointSetType::Pointer;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = DataObject::Pointer;

  using DataObjectIdentifierType = DataObject::DataObjectIdentifierType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectToPointSetFilter, ProcessObject);

  /** Some convenient type alias. */
  using PointBasedSpatialObjectType = TPointBasedSpatialObject;
  using PointBasedSpatialObjectPointer = typename PointBasedSpatialObjectType::Pointer;
  using PointBasedSpatialObjectConstPointer = typename PointBasedSpatialObjectType::ConstPointer;
  using ChildrenListType = typename TPointBasedSpatialObject::ChildrenListType;

  /** Dimension constants */
  static constexpr unsigned int ObjectDimension = PointBasedSpatialObjectType::ObjectDimension;

  using SpatialObjectPointType = itk::SpatialObjectPoint<Self::ObjectDimension>;

  using SpatialObjectType = itk::SpatialObject<Self::ObjectDimension>;

  /** Set/Get the PointSet input of this process object.  */
  using Superclass::SetInput;

  void
  SetInput(const SpatialObjectType * input);

  void
  SetInput(const DataObjectIdentifierType & key, const SpatialObjectType * object);

  /** Get the input Spatial Object. */
  const SpatialObjectType *
  GetInput();
  const SpatialObjectType *
  GetInput(unsigned int idx);

  /** The spatial object being transformed can be part of a hierarchy.
   * How deep in the hierarchy should we descend in generating the
   * PointSet?  A ChildrenDepth of 0 means to only include the object
   * itself. */
  itkSetMacro(ChildrenDepth, unsigned int);
  itkGetConstMacro(ChildrenDepth, unsigned int);

  /* Set the sampling factor of the object. The resulting pointset will have a
   * size inversely proportional to the sampling factor.*/
  itkSetMacro(SamplingFactor, unsigned int);
  itkGetConstMacro(SamplingFactor, unsigned int);

protected:
  SpatialObjectToPointSetFilter();
  ~SpatialObjectToPointSetFilter() override = default;

  void
  GenerateOutputInformation() override
  {} // do nothing
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  unsigned int m_ChildrenDepth{ 0 };
  unsigned int m_SamplingFactor{ 1 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpatialObjectToPointSetFilter.hxx"
#endif

#endif
