#ifndef __itkMeshToVTKPolyData_h
#define __itkMeshToVTKPolyData_h

#include "vtkPoints.h"
#include "vtkCellArray.h"
#include "vtkPolyData.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkPoint.h"
#include "itkObject.h"


namespace itk
{

/**
  \class MeshToVTKPolyData
  \brief
    \warning
  \sa
  */

template <class TMesh >
class MeshToVTKPolyData : public Object
{

 public:

  /** Standard class typedefs. */
  typedef MeshToVTKPolyData         Self;
  typedef Object                    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshToVTKPolyData, Object);

  typedef TMesh                                                      TriangleMeshType;
  typedef typename TriangleMeshType::MeshTraits                      TriangleMeshTraits;
  typedef typename TriangleMeshType::PointType                       PointType;
  typedef typename TriangleMeshType::PointsContainer                 InputPointsContainer;
  typedef typename InputPointsContainer::Pointer                     InputPointsContainerPointer;
  typedef typename InputPointsContainer::Iterator                    InputPointsContainerIterator;
  typedef typename TriangleMeshType::CellType                        CellType;

  typedef typename TriangleMeshType::CellsContainerPointer           CellsContainerPointer;
  typedef typename TriangleMeshType::CellsContainerIterator          CellsContainerIterator;

  /**
  The SetInput method provides pointer to the vtkPolyData
  */
  void SetInput(TriangleMeshType * mesh);
  TriangleMeshType * GetInput();

  vtkPolyData * GetOutput();

  void Update();

 private:
  MeshToVTKPolyData( void );
  virtual ~MeshToVTKPolyData( void );
  MeshToVTKPolyData(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typename TriangleMeshType::Pointer m_ItkTriangleMesh;

  vtkPoints  *   m_Points;
  vtkPolyData *  m_PolyData;
  vtkCellArray * m_Polys;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeshToVTKPolyData.txx"
#endif

#endif
