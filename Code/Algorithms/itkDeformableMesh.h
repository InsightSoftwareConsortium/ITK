#ifndef __itkdeformablemesh_h
#define __itkdeformablemesh_h

#include <vnl/vnl_matrix_fixed.h>
#include "itkMesh.h"
#include "itkVector.h"
#include "itkTriangleCell.h"

namespace itk
{

/** \class DeformableMesh
 * \brief 
 *
 * DeformableMesh is a class that define the geometry of the deformable
 * models used in segmentation work.
 *
 * The DeformableMesh is derive from the class mesh. Its basic shape is
 * a sphere with triangular cells on the surface. Each node can deform
 * under the effect of global force or the local forces and perform global
 * deformation or the local deformation.
 *
 * User can use SetResolution method to set the resolution in 2 direction.
 * The stiffness matrix of the is defined in itkBalloonForceFilter, we will
 * decide later where is the best to define the stiffness matrix.
 *
 * The filter is templated over the type of meshes.
 *
 * \sa itkBalloonForceFilter
 */
template <
  class TPixelType,
  class TMeshType = DefaultStaticMeshType< TPixelType >
>
class ITK_EXPORT DeformableMesh : public Mesh<TPixelType,TMeshType>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef DeformableMesh  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Mesh<TPixelType,TMeshType> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(DeformableMesh,Mesh);

/**
 * Define a few cell types which uses a TPixelType of "int".  Again,
 * use the defaults for the other parameters.  Note that a cell's template
 * parameters must match those of the mesh into which it is inserted.
 */

  typedef TMeshType   MeshType;
  typedef typename MeshType::CellType CellType;
  typedef itk::TriangleCell<TPixelType, CellType>	   TriCell;


  void SetResolution(int a, int b);
  void SetCenter(int a, int b, int c);
  void SetScale(float a, float b, float c);
  void SetDefault();
  void Allocate();

protected:
  DeformableMesh();
  ~DeformableMesh() {};


  int Center[3]; 
  int Resolution[2];
  float Scale[3]; 

};

} // end namespace itk
#endif
