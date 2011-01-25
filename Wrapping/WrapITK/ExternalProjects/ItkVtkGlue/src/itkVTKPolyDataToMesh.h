#ifndef __itkVTKPolyDataToMesh_h
#define __itkVTKPolyDataToMesh_h

#include "vtkPoints.h"
#include "vtkCellArray.h"
#include "vtkPolyData.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkObject.h"


namespace itk
{

/**
  \class VTKPolyDataToMesh
  \brief
    \warning
  \sa
  */

template <class TMesh >
class VTKPolyDataToMesh : public Object
{

 public:

  /** Standard class typedefs. */
  typedef VTKPolyDataToMesh         Self;
  typedef Object                    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKPolyDataToMesh, Object);

  typedef TMesh                                 TriangleMeshType;
  typedef typename TriangleMeshType::MeshTraits TriangleMeshTraits;

  /**
  The SetInput method provides pointer to the vtkPolyData
  */
  void SetInput( vtkPolyData * polydata);
  vtkPolyData *  GetInput();

  TriangleMeshType * GetOutput();

  void Update();

 private:

  VTKPolyDataToMesh( void );
  virtual ~VTKPolyDataToMesh( void );
  VTKPolyDataToMesh(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typename TriangleMeshType::Pointer  m_ItkMesh;
  vtkPolyData                       * m_PolyData;


};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKPolyDataToMesh.txx"
#endif

#endif
