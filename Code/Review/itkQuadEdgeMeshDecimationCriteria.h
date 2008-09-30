#ifndef __itkQuadEdgeMeshDecimationCriteria_h
#define __itkQuadEdgeMeshDecimationCriteria_h

#include "itkPriorityQueueContainer.h"

namespace itk
{
/**
 * \class QuadEdgeMeshDecimationCriterion
 * \brief
*/
template< class TMesh,
  typename TElement  = unsigned long,
  typename TMeasure = double,
  class TPriorityQueueWrapper = 
    MinPriorityQueueElementWrapper< typename TMesh::QEType*, 
      std::pair< bool, TMeasure > > >
class QuadEdgeMeshDecimationCriterion : public Object
{
public:
  typedef QuadEdgeMeshDecimationCriterion Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef Object Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( QuadEdgeMeshDecimationCriterion, 
    Object );

  typedef TMesh MeshType;
  typedef TElement ElementType;
  typedef TMeasure MeasureType;
  typedef TPriorityQueueWrapper PriorityQueueWrapperType;
  typedef typename PriorityQueueWrapperType::ElementPriorityType
    PriorityType;

  void SetNumberOfElements( const unsigned long& iN )
  {
    m_SizeCriterion = true;
    m_NumberOfElements = iN;
  }
  void SetMeasureBound( const MeasureType& iBound )
  {
    m_SizeCriterion = false;
    m_MeasureBound = iBound;
  }

  itkGetMacro( TopologicalChange, bool );
  itkSetMacro( TopologicalChange, bool );
  
  virtual bool is_satisfied( MeshType* iMesh,
    const ElementType& iElement,
    const MeasureType& iValue ) const = 0;

protected:
  QuadEdgeMeshDecimationCriterion( ) :
    m_TopologicalChange( true ),
    m_SizeCriterion( true ),
    m_NumberOfElements( 0 ),
    m_MeasureBound( static_cast< MeasureType >( 0. ) ) {}

  ~QuadEdgeMeshDecimationCriterion()
  {}

  bool m_TopologicalChange;
  bool m_SizeCriterion;
  unsigned long m_NumberOfElements;
  MeasureType m_MeasureBound;

private:
  QuadEdgeMeshDecimationCriterion( const Self& );
  void operator = ( const Self& );
};

/**
 * \class NumberOfPointsCriterion
 * \brief
*/
template< class TMesh,
  typename TElement = unsigned long,
  typename TMeasure = double,
  class TPriorityQueueWrapper = 
    MinPriorityQueueElementWrapper< typename TMesh::QEType*, 
      std::pair< bool, TMeasure > > >
class NumberOfPointsCriterion :
public QuadEdgeMeshDecimationCriterion< TMesh, TElement,
  TMeasure, TPriorityQueueWrapper >
{
public:
  typedef NumberOfPointsCriterion Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDecimationCriterion< TMesh, TElement,
    TMeasure, TPriorityQueueWrapper > Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( NumberOfPointsCriterion, QuadEdgeMeshDecimationCriterion );
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );
  
  typedef typename Superclass::MeshType MeshType;
  typedef typename Superclass::ElementType ElementType;
  typedef typename Superclass::MeasureType MeasureType;
  typedef typename Superclass::PriorityQueueWrapperType 
    PriorityQueueWrapperType;
  typedef typename Superclass::PriorityType
    PriorityType;
    
  inline bool is_satisfied( MeshType* iMesh,
    const ElementType& iElement,
    const MeasureType& iValue ) const
  {
    return ( iMesh->GetNumberOfPoints() <= this->m_NumberOfElements );
  }

protected:
  NumberOfPointsCriterion( ) : Superclass( ) {}
  ~NumberOfPointsCriterion() {}

private:
  NumberOfPointsCriterion( const Self& );
  void operator = ( const Self& );
};

/**
 * \class NumberOfFacesCriterion
 * \brief
*/
template< class TMesh,
  typename TElement = unsigned long,
  typename TMeasure = double,
  class TPriorityQueueWrapper = 
    MinPriorityQueueElementWrapper< ITK_TYPENAME TMesh::QEType*, 
      std::pair< bool, TMeasure > > >
class NumberOfFacesCriterion :
public QuadEdgeMeshDecimationCriterion< TMesh, TElement,
  TMeasure, TPriorityQueueWrapper >
{
public:
  typedef NumberOfFacesCriterion Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDecimationCriterion< TMesh, TElement,
    TMeasure, TPriorityQueueWrapper > Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( NumberOfFacesCriterion, QuadEdgeMeshDecimationCriterion );
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );
  
  typedef typename Superclass::MeshType MeshType;
  typedef typename MeshType::CellsContainerConstIterator
    CellsContainerConstIterator;
  typedef typename Superclass::ElementType ElementType;
  typedef typename Superclass::MeasureType MeasureType;
  typedef typename Superclass::PriorityQueueWrapperType 
    PriorityQueueWrapperType;
  typedef typename Superclass::PriorityType
    PriorityType;

  inline bool is_satisfied( MeshType* iMesh,
    const ElementType& iElement,
    const MeasureType& iValue ) const
  {
    (void)iElement;
    (void)iValue;
    return ( iMesh->GetNumberOfFaces() <= this->m_NumberOfElements );
  }

protected:
  NumberOfFacesCriterion( ) : Superclass( ) {}
  ~NumberOfFacesCriterion() {}

private:
  NumberOfFacesCriterion( const Self& );
  void operator = ( const Self& );
};

/**
 * \class MaxMeasureBoundCriterion
 * \brief
*/
template< class TMesh,
  typename TElement = unsigned long,
  typename TMeasure = double,
  class TPriorityQueueWrapper = 
    MinPriorityQueueElementWrapper< typename TMesh::QEType*, 
      std::pair< bool, TMeasure > > >
class MaxMeasureBoundCriterion :
public QuadEdgeMeshDecimationCriterion< TMesh, TElement,
  TMeasure, TPriorityQueueWrapper >
{
public:
  typedef MaxMeasureBoundCriterion Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDecimationCriterion< TMesh, TElement,
    TMeasure, TPriorityQueueWrapper > Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( MaxMeasureBoundCriterion, QuadEdgeMeshDecimationCriterion );
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );
  
  typedef typename Superclass::MeshType MeshType;
  typedef typename MeshType::CellsContainerConstIterator
    CellsContainerConstIterator;
  typedef typename Superclass::ElementType ElementType;
  typedef typename Superclass::MeasureType MeasureType;
  typedef typename Superclass::PriorityQueueWrapperType 
    PriorityQueueWrapperType;
  typedef typename Superclass::PriorityType
    PriorityType;

  inline bool is_satisfied( MeshType* iMesh,
    const ElementType& iElement,
    const MeasureType& iValue ) const
  {
    (void) iMesh;
    (void) iElement;
    return ( iValue <= this->m_MeasureBound );
  }

protected:
  MaxMeasureBoundCriterion( ) : Superclass( ) {}
  ~MaxMeasureBoundCriterion() {}

private:
  MaxMeasureBoundCriterion( const Self& );
  void operator = ( const Self& );
};

/**
 * \class MinMeasureBoundCriterion
 * \brief
*/
template< class TMesh,
  typename TElement = unsigned long,
  typename TMeasure = double,
  class TPriorityQueueWrapper = 
    MaxPriorityQueueElementWrapper< typename TMesh::QEType*,
      std::pair< bool, TMeasure > > >
class MinMeasureBoundCriterion :
public QuadEdgeMeshDecimationCriterion< TMesh, TElement,
  TMeasure, TPriorityQueueWrapper >
{
public:
  typedef MinMeasureBoundCriterion Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDecimationCriterion< TMesh, TElement,
    TMeasure, TPriorityQueueWrapper > Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( MinMeasureBoundCriterion, QuadEdgeMeshDecimationCriterion );
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );
  
  typedef typename Superclass::MeshType MeshType;
  typedef typename MeshType::CellsContainerConstIterator
    CellsContainerConstIterator;
  typedef typename Superclass::ElementType ElementType;
  typedef typename Superclass::MeasureType MeasureType;
  typedef typename Superclass::PriorityQueueWrapperType 
    PriorityQueueWrapperType;
  typedef typename Superclass::PriorityType
    PriorityType;

  inline bool is_satisfied( MeshType* iMesh,
    const ElementType& iElement,
    const MeasureType& iValue ) const
  {
    return ( iValue >= this->m_MeasureBound );
  }

protected:
  MinMeasureBoundCriterion( ) : Superclass( ) {}
  ~MinMeasureBoundCriterion() {}

private:
  MinMeasureBoundCriterion( const Self& );
  void operator = ( const Self& );
};
}

#endif
