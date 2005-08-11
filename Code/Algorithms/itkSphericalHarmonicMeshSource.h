/** \class SphericalHarmonicMeshSource
 *
 *  \brief This class provides convenient class for generating 3D meshes represented by Spherical Harmonics descriptor.
 *
 *  \author Christine Xu
 */
#ifndef __itkSphericalHarmonicMeshSource_h
#define __itkSphericalHarmonicMeshSource_h

#include "itkSphericalHarmonicPolynomial.h"
#include "itkSPHARMCoefSpatialObject.h"

#include "itkMeshSource.h"
#include "itkMesh.h"

namespace itk
{

class SphericalHarmonicMeshSourceException : public ExceptionObject 
{
public:
  /** Run-time information. */
  itkTypeMacro( SphericalHarmonicMeshSourceException, ExceptionObject );
  
  /** Constructor. */
  SphericalHarmonicMeshSourceException(const char *file, unsigned int line, 
                           const char* message = "Error in generating meshes from Spherical Harmonics") : 
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }

  /** Constructor. */
  SphericalHarmonicMeshSourceException(const std::string &file, unsigned int line, 
                           const char* message = "Error in generating meshes from Spherical Harmonics") : 
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }
};

/** Traits for Mesh, DefaultDynamicMeshTraits<TPixelType, VPointDimension, VMaxTopologicalDimension, TCoordRep> */
typedef SPHARMCoefSpatialObject::MeshType MeshType;
class SphericalHarmonicMeshSource : public MeshSource<MeshType> //default dimension for mesh is 3
{
public:
  typedef SphericalHarmonicMeshSource Self;  
  typedef MeshSource<MeshType>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(SphericalHarmonicMeshSource, MeshSource);
  
  typedef SPHARMCoefSpatialObject::ScalarType ScalarType;
  typedef SPHARMCoefSpatialObject::CoefType CoefType;
  typedef SPHARMCoefSpatialObject::CoefListType CoefListType;
  
  typedef double Point3[3];
  
  void SetCoefs(CoefListType& coeflist);
  
  typedef MeshType::PointsContainerPointer PointsContainerPointer;
  typedef MeshType::PointsContainer   PointsContainer;
  typedef MeshType::PointType PointType;
  typedef MeshType::CellsContainerPointer CellsContainerPointer;
  typedef MeshType::CellsContainer CellsContainer;
  typedef MeshType::CellType CellType;  
  
  itkGetConstReferenceMacro(Level, unsigned int);  
  itkSetMacro(Level, unsigned int);
  
  itkGetConstReferenceMacro(Degree, unsigned int);  
  void SetDegree(unsigned int d);
  
  itkGetConstReferenceMacro(FromL, unsigned int);  
  itkSetMacro(FromL, unsigned int);
  
  itkGetConstReferenceMacro(ToL, unsigned int);  
  itkSetMacro(ToL, unsigned int);
  
  itkGetConstReferenceMacro(Dimension, unsigned int);
  
protected:
  SphericalHarmonicMeshSource();
  ~SphericalHarmonicMeshSource();
  
  void GenerateData();
  
  void set_up_icosahedron_triangs(Point3* all_vert,
        Point3* all_triangs,
        int subdiv,
        int n_vert,
        int n_phi,
        int n_theta,
        double *icos,
        int n_triangs,
        int *triangs);        
  void interpol_vert(int n_phi,
        int n_theta,
        Point3 *mesh,
        int n_vertex,
        double *icos,
        Point3 *vertex);
  int mod(int x, int y);
  void interpol_2d(int n_phi,
      int n_theta,
      int xd,
      int xu,
      int yd,
      int yu,
      double ksi,
      double eta,
      Point3 *mesh,
      double *xi,
      double *yi,
      double *zi);

private:
  unsigned int m_Dimension;//dimension of the output mesh

  unsigned int m_Level; //subdivision level
    
  unsigned int m_Degree; //degree of the coefficients
  CoefListType m_Coefs;
  
  unsigned int m_FromL;
  unsigned int m_ToL;
};

} // end namespace itk

#endif
