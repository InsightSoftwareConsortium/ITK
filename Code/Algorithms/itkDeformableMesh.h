#ifndef __itkdeformablemesh_h
#define __itkdeformablemesh_h

#include "vnl/vnl_matrix_fixed.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkTriangleCell.h"

namespace itk
{

/** \class FilterMeshToMesh
 * \brief 
 *
 * FilterMeshToMesh is the base class for all process objects that output
 * mesh data, and require mesh data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 */
template <class PixelType>
class ITK_EXPORT DeformableMesh : public Mesh<PixelType>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef DeformableMesh  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Mesh<PixelType> Superclass;

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
 * Define a few cell types which uses a PixelType of "int".  Again,
 * use the defaults for the other parameters.  Note that a cell's template
 * parameters must match those of the mesh into which it is inserted.
 */

  typedef itk::TriangleCell<PixelType, CellType>	   TriCell;


/**
 * Typedef the generic cell type for the mesh.  It is an abstract class,
 * so we can only use information from it, like get its pointer type.
 */

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
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableMesh.txx"
#endif
#endif
