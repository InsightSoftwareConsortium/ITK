#ifndef __itkdeformabletest_h
#define __itkdeformabletest_h

#include <vnl_matrix_fixed.h>
#include "itkmesh.h"
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
class ITK_EXPORT DeformableMeshTest : public Mesh<PixelType>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef DeformableMeshTest  Self;

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
  itkTypeMacro(DeformableMeshTest,Mesh);

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
  DeformableMeshTest();
  ~DeformableMeshTest() {};


  int Center[3]; 
  int Resolution[2];
  float Scale[3]; 
//  unsigned long i, j, jn, e, p, numpts, numcells;
//  float x[3], ustep, vstep, ubeg, vbeg, u, v; 
//  int pts[3], signu, signv; 

};

} // end namespace itk
#endif