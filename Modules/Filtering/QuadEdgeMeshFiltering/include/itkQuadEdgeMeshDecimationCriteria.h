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
template< typename TMesh,
          typename TElement  = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MinPriorityQueueElementWrapper< typename TMesh::QEType *,
                                            std::pair< bool, TMeasure > > >
class QuadEdgeMeshDecimationCriterion:public Object
{
public:
  typedef QuadEdgeMeshDecimationCriterion Self;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;
  typedef Object                          Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuadEdgeMeshDecimationCriterion, Object);

  typedef TMesh                                                  MeshType;
  typedef TElement                                               ElementType;
  typedef TMeasure                                               MeasureType;
  typedef TPriorityQueueWrapper                                  PriorityQueueWrapperType;
  typedef typename PriorityQueueWrapperType::ElementPriorityType PriorityType;

  void SetNumberOfElements(const SizeValueType & numberOfElements)
  {
    this->m_SizeCriterion = true;
    this->m_NumberOfElements = numberOfElements;
  }

  void SetMeasureBound(const MeasureType & bound)
  {
    this->m_SizeCriterion = false;
    this->m_MeasureBound = bound;
  }

  itkGetConstMacro(TopologicalChange, bool);
  itkSetMacro(TopologicalChange, bool);

  virtual bool is_satisfied(MeshType *iMesh,
                            const ElementType & iElement,
                            const MeasureType & iValue) const = 0;

protected:
  QuadEdgeMeshDecimationCriterion()
  {
    this->m_TopologicalChange = true;
    this->m_SizeCriterion = true;
    this->m_NumberOfElements = 0;
    this->m_MeasureBound = itk::NumericTraits< MeasureType >::ZeroValue();
  }

  ~QuadEdgeMeshDecimationCriterion() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "TopologicalChange: "
              << (m_TopologicalChange ? "On" : "Off")
              << std::endl;
    os << indent << "SizeCriterion: "
              << (m_SizeCriterion ? "On" : "Off")
              << std::endl;
    os << indent << "NumberOfElements: "
              << m_NumberOfElements
              << std::endl;
    os << indent << "MeasureBound: "
              << m_MeasureBound
              << std::endl;
  }

  bool m_TopologicalChange;
  bool m_SizeCriterion;

  SizeValueType m_NumberOfElements;

  MeasureType m_MeasureBound;

private:
  QuadEdgeMeshDecimationCriterion(const Self &);
  void operator=(const Self &);
};

/**
 * \class NumberOfPointsCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MinPriorityQueueElementWrapper< typename TMesh::QEType *,
                                            std::pair< bool, TMeasure > > >
class NumberOfPointsCriterion:
  public QuadEdgeMeshDecimationCriterion< TMesh, TElement,
                                          TMeasure, TPriorityQueueWrapper >
{
public:
  typedef NumberOfPointsCriterion    Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDecimationCriterion<
    TMesh, TElement, TMeasure, TPriorityQueueWrapper >       Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(NumberOfPointsCriterion, QuadEdgeMeshDecimationCriterion);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef typename Superclass::MeshType                 MeshType;
  typedef typename Superclass::ElementType              ElementType;
  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::PriorityQueueWrapperType PriorityQueueWrapperType;
  typedef typename Superclass::PriorityType             PriorityType;

  inline bool is_satisfied( MeshType *iMesh,
                            const ElementType & itkNotUsed(iElement),
                            const MeasureType & itkNotUsed(iValue) ) const
  {
    return ( iMesh->GetNumberOfPoints() <= this->m_NumberOfElements );
  }

protected:
  NumberOfPointsCriterion() {}
  ~NumberOfPointsCriterion() {}

private:
  NumberOfPointsCriterion(const Self &);
  void operator=(const Self &);
};

/**
 * \class NumberOfFacesCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MinPriorityQueueElementWrapper< typename TMesh::QEType *,
                                            std::pair< bool, TMeasure > > >
class NumberOfFacesCriterion:
  public QuadEdgeMeshDecimationCriterion< TMesh, TElement,
                                          TMeasure, TPriorityQueueWrapper >
{
public:
  typedef NumberOfFacesCriterion     Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDecimationCriterion< TMesh, TElement,
                                           TMeasure, TPriorityQueueWrapper >                 Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(NumberOfFacesCriterion, QuadEdgeMeshDecimationCriterion);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef typename Superclass::MeshType                  MeshType;
  typedef typename MeshType::CellsContainerConstIterator CellsContainerConstIterator;
  typedef typename Superclass::ElementType               ElementType;
  typedef typename Superclass::MeasureType               MeasureType;
  typedef typename Superclass::PriorityQueueWrapperType  PriorityQueueWrapperType;
  typedef typename Superclass::PriorityType              PriorityType;

  bool is_satisfied( MeshType *iMesh,
                     const ElementType & itkNotUsed(iElement),
                     const MeasureType & itkNotUsed(iValue) ) const ITK_OVERRIDE
  {
    return ( iMesh->GetNumberOfFaces() <= this->m_NumberOfElements );
  }

protected:
  NumberOfFacesCriterion() {}
  ~NumberOfFacesCriterion() ITK_OVERRIDE {}

private:
  NumberOfFacesCriterion(const Self &);
  void operator=(const Self &);
};

/**
 * \class MaxMeasureBoundCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MinPriorityQueueElementWrapper< typename TMesh::QEType *,
                                            std::pair< bool, TMeasure > > >
class MaxMeasureBoundCriterion:
  public QuadEdgeMeshDecimationCriterion< TMesh, TElement,
                                          TMeasure, TPriorityQueueWrapper >
{
public:
  typedef MaxMeasureBoundCriterion   Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDecimationCriterion< TMesh, TElement,
                                           TMeasure, TPriorityQueueWrapper >                        Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(MaxMeasureBoundCriterion, QuadEdgeMeshDecimationCriterion);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef typename Superclass::MeshType                  MeshType;
  typedef typename MeshType::CellsContainerConstIterator CellsContainerConstIterator;
  typedef typename Superclass::ElementType               ElementType;
  typedef typename Superclass::MeasureType               MeasureType;
  typedef typename Superclass::PriorityQueueWrapperType  PriorityQueueWrapperType;
  typedef typename Superclass::PriorityType              PriorityType;

  bool is_satisfied(MeshType *itkNotUsed(iMesh),
                    const ElementType & itkNotUsed(iElement),
                    const MeasureType & iValue) const ITK_OVERRIDE
  {
    return ( iValue <= this->m_MeasureBound );
  }

protected:
  MaxMeasureBoundCriterion():Superclass() {}
  ~MaxMeasureBoundCriterion() ITK_OVERRIDE {}

private:
  MaxMeasureBoundCriterion(const Self &);
  void operator=(const Self &);
};

/**
 * \class MinMeasureBoundCriterion
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TMesh,
          typename TElement = IdentifierType,
          typename TMeasure = double,
          typename TPriorityQueueWrapper =
            MaxPriorityQueueElementWrapper< typename TMesh::QEType *,
                                            std::pair< bool, TMeasure > > >
class MinMeasureBoundCriterion:
  public QuadEdgeMeshDecimationCriterion< TMesh, TElement,
                                          TMeasure, TPriorityQueueWrapper >
{
public:
  typedef MinMeasureBoundCriterion   Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDecimationCriterion< TMesh, TElement,
                                           TMeasure, TPriorityQueueWrapper >                         Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(MinMeasureBoundCriterion, QuadEdgeMeshDecimationCriterion);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef typename Superclass::MeshType                  MeshType;
  typedef typename MeshType::CellsContainerConstIterator CellsContainerConstIterator;
  typedef typename Superclass::ElementType               ElementType;
  typedef typename Superclass::MeasureType               MeasureType;
  typedef typename Superclass::PriorityQueueWrapperType  PriorityQueueWrapperType;
  typedef typename Superclass::PriorityType              PriorityType;

  inline bool is_satisfied(MeshType *,
                           const ElementType & ,
                           const MeasureType & iValue) const
  {
    return ( iValue >= this->m_MeasureBound );
  }

protected:
  MinMeasureBoundCriterion() {}
  ~MinMeasureBoundCriterion() {}

private:
  MinMeasureBoundCriterion(const Self &);
  void operator=(const Self &);
};
}

#endif
