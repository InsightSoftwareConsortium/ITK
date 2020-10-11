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
#ifndef itkQuadEdgeMeshDecimationCriteria_h
#define itkQuadEdgeMeshDecimationCriteria_h

#include "itkIntTypes.h"
#include "itkPriorityQueueContainer.h"

namespace itk
{
/**
 * \class QuadEdgeMeshDecimationCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MinPriorityQueueElementWrapper<typename TMesh::QEType *, std::pair<bool, TMeasure>>>
class QuadEdgeMeshDecimationCriterion : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuadEdgeMeshDecimationCriterion);

  using Self = QuadEdgeMeshDecimationCriterion;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = Object;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuadEdgeMeshDecimationCriterion, Object);

  using MeshType = TMesh;
  using ElementType = TElement;
  using MeasureType = TMeasure;
  using PriorityQueueWrapperType = TPriorityQueueWrapper;
  using PriorityType = typename PriorityQueueWrapperType::ElementPriorityType;

  void
  SetNumberOfElements(const SizeValueType & numberOfElements)
  {
    this->m_SizeCriterion = true;
    this->m_NumberOfElements = numberOfElements;
  }

  void
  SetMeasureBound(const MeasureType & bound)
  {
    this->m_SizeCriterion = false;
    this->m_MeasureBound = bound;
  }

  itkBooleanMacro(TopologicalChange);
  itkGetConstMacro(TopologicalChange, bool);
  itkSetMacro(TopologicalChange, bool);

  virtual bool
  is_satisfied(MeshType * iMesh, const ElementType & iElement, const MeasureType & iValue) const = 0;

protected:
  QuadEdgeMeshDecimationCriterion()
  {
    this->m_TopologicalChange = true;
    this->m_SizeCriterion = true;
    this->m_NumberOfElements = 0;
    this->m_MeasureBound = itk::NumericTraits<MeasureType>::ZeroValue();
  }

  ~QuadEdgeMeshDecimationCriterion() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "TopologicalChange: " << (m_TopologicalChange ? "On" : "Off") << std::endl;
    os << indent << "SizeCriterion: " << (m_SizeCriterion ? "On" : "Off") << std::endl;
    os << indent << "NumberOfElements: " << m_NumberOfElements << std::endl;
    os << indent << "MeasureBound: " << m_MeasureBound << std::endl;
  }

  bool m_TopologicalChange;
  bool m_SizeCriterion;

  SizeValueType m_NumberOfElements;

  MeasureType m_MeasureBound;
};

/**
 * \class NumberOfPointsCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MinPriorityQueueElementWrapper<typename TMesh::QEType *, std::pair<bool, TMeasure>>>
class NumberOfPointsCriterion : public QuadEdgeMeshDecimationCriterion<TMesh, TElement, TMeasure, TPriorityQueueWrapper>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NumberOfPointsCriterion);

  using Self = NumberOfPointsCriterion;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = QuadEdgeMeshDecimationCriterion<TMesh, TElement, TMeasure, TPriorityQueueWrapper>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(NumberOfPointsCriterion, QuadEdgeMeshDecimationCriterion);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  using MeshType = typename Superclass::MeshType;
  using ElementType = typename Superclass::ElementType;
  using MeasureType = typename Superclass::MeasureType;
  using PriorityQueueWrapperType = typename Superclass::PriorityQueueWrapperType;
  using PriorityType = typename Superclass::PriorityType;

  inline bool
  is_satisfied(MeshType * iMesh, const ElementType & itkNotUsed(iElement), const MeasureType & itkNotUsed(iValue)) const
  {
    return (iMesh->GetNumberOfPoints() <= this->m_NumberOfElements);
  }

protected:
  NumberOfPointsCriterion() = default;
  ~NumberOfPointsCriterion() = default;
};

/**
 * \class NumberOfFacesCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MinPriorityQueueElementWrapper<typename TMesh::QEType *, std::pair<bool, TMeasure>>>
class NumberOfFacesCriterion : public QuadEdgeMeshDecimationCriterion<TMesh, TElement, TMeasure, TPriorityQueueWrapper>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NumberOfFacesCriterion);

  using Self = NumberOfFacesCriterion;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = QuadEdgeMeshDecimationCriterion<TMesh, TElement, TMeasure, TPriorityQueueWrapper>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(NumberOfFacesCriterion, QuadEdgeMeshDecimationCriterion);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  using MeshType = typename Superclass::MeshType;
  using CellsContainerConstIterator = typename MeshType::CellsContainerConstIterator;
  using ElementType = typename Superclass::ElementType;
  using MeasureType = typename Superclass::MeasureType;
  using PriorityQueueWrapperType = typename Superclass::PriorityQueueWrapperType;
  using PriorityType = typename Superclass::PriorityType;

  bool
  is_satisfied(MeshType *          iMesh,
               const ElementType & itkNotUsed(iElement),
               const MeasureType & itkNotUsed(iValue)) const override
  {
    return (iMesh->GetNumberOfFaces() <= this->m_NumberOfElements);
  }

protected:
  NumberOfFacesCriterion() = default;
  ~NumberOfFacesCriterion() override = default;
};

/**
 * \class MaxMeasureBoundCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MinPriorityQueueElementWrapper<typename TMesh::QEType *, std::pair<bool, TMeasure>>>
class MaxMeasureBoundCriterion
  : public QuadEdgeMeshDecimationCriterion<TMesh, TElement, TMeasure, TPriorityQueueWrapper>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MaxMeasureBoundCriterion);

  using Self = MaxMeasureBoundCriterion;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = QuadEdgeMeshDecimationCriterion<TMesh, TElement, TMeasure, TPriorityQueueWrapper>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(MaxMeasureBoundCriterion, QuadEdgeMeshDecimationCriterion);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  using MeshType = typename Superclass::MeshType;
  using CellsContainerConstIterator = typename MeshType::CellsContainerConstIterator;
  using ElementType = typename Superclass::ElementType;
  using MeasureType = typename Superclass::MeasureType;
  using PriorityQueueWrapperType = typename Superclass::PriorityQueueWrapperType;
  using PriorityType = typename Superclass::PriorityType;

  bool
  is_satisfied(MeshType *          itkNotUsed(iMesh),
               const ElementType & itkNotUsed(iElement),
               const MeasureType & iValue) const override
  {
    return (iValue <= this->m_MeasureBound);
  }

protected:
  MaxMeasureBoundCriterion()
    : Superclass()
  {}
  ~MaxMeasureBoundCriterion() override = default;
};

/**
 * \class MinMeasureBoundCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MaxPriorityQueueElementWrapper<typename TMesh::QEType *, std::pair<bool, TMeasure>>>
class MinMeasureBoundCriterion
  : public QuadEdgeMeshDecimationCriterion<TMesh, TElement, TMeasure, TPriorityQueueWrapper>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MinMeasureBoundCriterion);

  using Self = MinMeasureBoundCriterion;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = QuadEdgeMeshDecimationCriterion<TMesh, TElement, TMeasure, TPriorityQueueWrapper>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(MinMeasureBoundCriterion, QuadEdgeMeshDecimationCriterion);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  using MeshType = typename Superclass::MeshType;
  using CellsContainerConstIterator = typename MeshType::CellsContainerConstIterator;
  using ElementType = typename Superclass::ElementType;
  using MeasureType = typename Superclass::MeasureType;
  using PriorityQueueWrapperType = typename Superclass::PriorityQueueWrapperType;
  using PriorityType = typename Superclass::PriorityType;

  inline bool
  is_satisfied(MeshType *, const ElementType &, const MeasureType & iValue) const
  {
    return (iValue >= this->m_MeasureBound);
  }

protected:
  MinMeasureBoundCriterion() = default;
  ~MinMeasureBoundCriterion() = default;
};
} // namespace itk

#endif
