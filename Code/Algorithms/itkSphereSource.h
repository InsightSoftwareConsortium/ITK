#ifndef __itkSphereSource_h
#define __itkSphereSource_h

#include "vnl/vnl_matrix_fixed.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkDefaultStaticMeshTraits.h"

namespace itk
{

/** \class itkSphere Source
 * \brief 
 *
 * Input the center and resolutions in 2 direction(verizon and horizon)
 * to create a sphere-like deformable model. The cell on the surface is
 * in the shape of triangular. 
 * More parameters are added to make the sphere mesh has global and local
 * deform ability.
 */
template <typename TOutputMesh>
class ITK_EXPORT SphereSource : public MeshSource<TOutputMesh>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SphereSource         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshSource<TOutputMesh>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(SphereSource, MeshSource);

  typedef TOutputMesh OutputMeshType;

  /** 
   * Hold on to the type information specified by the template parameters.
   */
  typedef typename OutputMeshType::MeshTraits	OMeshTraits;
  typedef typename OMeshTraits::PixelType		PixelType;  

  /** 
   * Some typedefs.
   */
  typedef typename OutputMeshType::Pointer OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits CellTraits;
  
/**
 * Define the triangular cell types which forms the surface of the model
 * and will be used in FEM application.
 */

  typedef itk::TriangleCell<PixelType, CellTraits>	   TriCell;
  typedef typename TriCell::Pointer TriCellPointer;

//  bool GetCenter(PointType*) const;

/**
 * All these parameter setting function are public temporarily to make
 * the test easier
 */

  void SetResolution(int a, int b);
  void SetCenter(int a, int b, int c);
  void SetScale(float a, float b, float c);
  void GenerateData();

protected:
  SphereSource();
  ~SphereSource() {};

// model center
  int m_Center[3]; 

// model resolutions
  int m_Resolution[2];

// model scales
  float m_Scale[3];
  
// new parameters will be added when the class is stable

};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSphereSource.txx"
#endif
#endif
