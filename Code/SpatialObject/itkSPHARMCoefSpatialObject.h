/** \class SPHARMCoefSpatialObject
 *
 *  \brief This class provides 3D shape representation using spherical harmonic coefficients.
 *
 *  \author Christine Xu
 */
 
#ifndef __itkSPHARMCoefSpatialObject_h

#define __itkSPHARMCoefSpatialObject_h

#include "itkDefaultDynamicMeshTraits.h"
#include "itkMesh.h"
#include "itkSpatialObject.h"
#include "itkMeshSpatialObject.h"

#include <vector>

#define SPHARMCoefSpatialObjectDimension 3

namespace itk
{
   
/** \class SPHARMCoefSpatialObject
 * 
 * \brief This class describe a spherical harmonic coefficient object in 3D only.
 */
class SPHARMCoefSpatialObject : public SpatialObject<SPHARMCoefSpatialObjectDimension>
{
public:

 typedef SPHARMCoefSpatialObject  Self;
 typedef SpatialObject< SPHARMCoefSpatialObjectDimension >  Superclass;
 typedef SmartPointer < Self >  Pointer;
 typedef SmartPointer < const Self >  ConstPointer;
 typedef SmartPointer<Superclass>  SuperclassPointer;
 typedef double ScalarType;
 typedef Point<ScalarType, SPHARMCoefSpatialObjectDimension> CoefType;
 typedef std::vector<CoefType> CoefListType;
 typedef Superclass::PointType PointType;
 typedef Superclass::TransformType          TransformType;
 typedef Superclass::BoundingBoxType        BoundingBoxType;

 
 /** Traits for Mesh, DefaultDynamicMeshTraits<TPixelType, VPointDimension, VMaxTopologicalDimension, TCoordRep> */
 typedef DefaultDynamicMeshTraits< float , SPHARMCoefSpatialObjectDimension, SPHARMCoefSpatialObjectDimension, double > MeshTrait;
 typedef Mesh<float,SPHARMCoefSpatialObjectDimension,MeshTrait> MeshType;
 typedef MeshSpatialObject<MeshType> MeshSpatialObjectType;
 typedef MeshSpatialObjectType::Pointer MeshSpatialObjectPointer;
 
 //typedef typename SphericalHarmonicMeshSource MeshSourceType;

 itkStaticConstMacro(NumberOfDimension, unsigned int, SPHARMCoefSpatialObjectDimension);
  
  /** Method for creation through the object factory. */
 itkNewMacro( Self );

  /** Method for creation through the object factory. */
 itkTypeMacro( SPHARMCoefSpatialObject, SpatialObject );

  /** Get the count */
 itkGetConstReferenceMacro(Count,int);

 /** Get the harmonic */ 
 itkGetConstReferenceMacro(Harmonic,int);
  
 /** Get the subdiv */
 itkGetConstReferenceMacro(Subdiv, int);
 
 /** Get the theta */
 //itkGetConstReferenceMacro(Theta, int);
 
 /** Get the phi */
 //itkGetConstReferenceMacro(Phi, int);
 
 /** Get the vertex */
 //itkGetConstReferenceMacro(Vertex, int);
 
 /** Get the triangs */
 //itkGetConstReferenceMacro(Triangs, int);
 
 /** Get the coefficients */
 void GetCoefs(CoefListType& coeflist) const; //Deep copy of the m_Coefs
 CoefListType & GetCoefs(){return m_Coefs;}
 void SetCoefs(CoefListType& coeflist);
 
 /** Derived spatial geometric functions from the superclass SpatialObject,
     resulting directly from calling corresponding functions of p_coefsMeshSpatialObject. */
 virtual bool  ValueAt (const PointType &point, double &value, unsigned int depth=0, char *name=NULL) const;

 virtual bool  IsEvaluableAt (const PointType &point, unsigned int depth=0, char *name=NULL) const;

 virtual bool  IsInside (const PointType &point, unsigned int depth, char *name=NULL) const;
 virtual bool  IsInside (const PointType &point) const;

 //virtual bool  ComputeBoundingBox () const;
 virtual bool ComputeLocalBoundingBox() const;
  
 virtual void PrintSelf(std::ostream& os, Indent indent) const; 
/** The four vitual geometric functions calls this function whenever the b_Dirty is true. */
 void ComputeHiddenMeshSpatialObject();
protected:

 SPHARMCoefSpatialObject();  
 virtual ~SPHARMCoefSpatialObject(); 
 
 /** Set functions */
 itkSetMacro(Count,int);
 itkSetMacro(Harmonic,int);
 itkSetMacro(Subdiv,int);
 //itkSetMacro(Theta, int);
 //itkSetMacro(Phi, int);
 //itkSetMacro(Vertex, int);
 //itkSetMacro(Triangs, int); 
 
 
 
 /** List of coefficients. */
 CoefListType m_Coefs;
 /** Number of the x,y,z coefficients in total. */

 int m_Count;
 /** Maximal degree of the spherical harmonics. */

 int m_Harmonic;
 
 /** Level of icosahedral subdivision of the background MeshSpatialObject(p_coefsMeshSpatialObject). */
 int m_Subdiv;
 /** Number of theta */
 //int m_Theta;
 /** Number of phi */
 //int m_Phi;
 /** Number of vertices */
 //int m_Vertex;
 /** Number of triangles */
 //int m_Triangs;
 MeshType::Pointer m_CoefsMesh;
 /** This MeshSpatialObject is hidden in the background 
     and is used for providing results for the above geometric functions. */
 MeshSpatialObjectPointer m_CoefsMeshSpatialObject;


};

} // end namespace itk

#endif //__itkSPHARMCoefSpatialObject_h
