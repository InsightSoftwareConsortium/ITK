/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkDeformableSimplexMesh3DFilter.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformableSimplexMesh3DFilter_H
#define __itkDeformableSimplexMesh3DFilter_H

#include "itkMeshToMeshFilter.h"
#include "itkSimplexMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkImage.h"
#include "itkCovariantVector.h"

#include <set>

namespace itk
  {

  /** \class DeformableSimplexMesh3DFilter
  * \brief Three-dimensional deformable model for image segmentation
  *
  * DeformableSimplexMesh3DFilter is a discrete three-dimensional deformable model, which 
  * can be used to deform a 3-D SimplexMesh. 
  *
  * The mesh deformation is constrained by internal forces. The interal force can be scaled 
  * via SetAlpha (typical values are 0.01 < alpha < 0.3). The external force is derived from 
  * the image one wants to delineate. Therefore an image of type GradientImageType needs to 
  * be set by calling SetGradientImage(...). The external forces are scaled 
  * via SetBeta (typical values are 0.01 < beta < 1). One still needs to play around with 
  * these values.
  * 
  * To control the smoothness of the mesh a rigidity parameter can be adjusted. Low values (1 or 0) 
  * allow areas with high curvature. Higher values (around 7 or 8) will make the mesh smoother.
  *
  * By setting the gamma parameter the regularity of the mesh is controlled. Low values (< 0.03) 
  * produce more regular mesh. Higher values ( 0.3 < gamma < 0.2) will allow to move the vertices to 
  * regions of higher curvature.
  *
  * This approach for segmentation follows that of Delingette et al. (1997).
  *
  * This filter currently assumes that the spacing of the input image is 1. 
  *
  * The user has to set the number of iterations for mesh evolution.
  * 
  * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
  * 
  * \ingroup MeshFilters
  * \ingroup MeshSegmentation 
  */
  template <class TInputMesh, class TOutputMesh>
class DeformableSimplexMesh3DFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
  {
  public:
    /** Standard "Self" typedef. */
    typedef DeformableSimplexMesh3DFilter  Self;

    /** Standard "Superclass" typedef. */
    typedef MeshToMeshFilter<TInputMesh, TOutputMesh> Superclass;

    /** Smart pointer typedef support */
    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;

    /** Method of creation through the object factory. */
    itkNewMacro(Self);

    /** Run-time type information (and related methods). */
    itkTypeMacro(DeformableSimplexMesh3DFilter,MeshToMeshFilter);

    /** Some typedefs. */
    typedef TInputMesh InputMeshType;
    typedef TOutputMesh OutputMeshType;

    typedef typename InputMeshType::PointsContainerPointer  InputPointsContainerPointer;
    typedef typename InputMeshType::PointsContainer  InputPointsContainer;
    typedef typename InputMeshType::PointsContainer::Iterator  InputPointsContainerIterator;

    /** Other definitions. */
    typedef typename InputMeshType::PointType             PointType;
    typedef typename PointType::VectorType                VectorType;
    typedef CovariantVector< 
                    typename VectorType::ValueType, 3 >   CovariantVectorType;
    typedef typename InputMeshType::PixelType             PixelType;

    /** Image and Image iterator definition. */
    typedef CovariantVector<PixelType, 3>                   GradientType;
    typedef Image<GradientType, 3>                          GradientImageType;
    typedef typename GradientImageType::Pointer             GradientImagePointer;
    typedef typename GradientImageType::IndexType           GradientIndexType;
    typedef typename GradientImageType::PixelType           GradientPixelType;
    typedef typename GradientIndexType::IndexValueType      GradientIndexValueType;
    typedef typename GradientImageType::SizeType            GradientImageSizeType;


    //typedef Image<PixelType, 3>                                   GradientIntensityImageType;
    //typedef typename GradientIntensityImageType::Pointer          GradientIntensityImagePointer;

    //typedef typename itk::MapContainer<unsigned long, PointType> ForceOriginMapType;
    //typedef typename itk::MapContainer<unsigned long, VectorType> ForceMapType;

    /* Mesh pointer definition. */
    typedef typename InputMeshType::Pointer     InputMeshPointer;
    typedef typename OutputMeshType::Pointer    OutputMeshPointer;


    typedef typename InputMeshType::PointType                     MeshPointType;
    typedef typename InputMeshType::CellsContainerPointer         CellsContainerPointer;
    typedef typename InputMeshType::CellsContainer::Iterator      CellsContainerIterator;
    typedef typename InputMeshType::NeighborListType              InputNeighbors;
    typedef typename InputMeshType::NeighborListType::iterator    InputNeighborsIterator;

    typedef itk::MapContainer<unsigned long, std::set<unsigned long> > VertexNeighborListType;
    typedef std::set<unsigned long>      NeighborSetType;
    typedef std::set<unsigned long>      IndexSetType;
    typedef typename NeighborSetType::iterator    NeighborSetIterator;
    typedef typename IndexSetType::iterator       IndexSetIterator;


    typedef typename InputMeshType::GeometryMapType   GeometryMapType;
    typedef typename GeometryMapType::Pointer         GeometryMapPointer;
    typedef typename GeometryMapType::Iterator        GeometryMapIterator;


    /** Routines. */

    /** Set/Get routines. */

    /** 
    *  Setter for gradient image
    */
    itkSetMacro(Gradient, GradientImagePointer);

    /** 
    *  Getter for gradient image
    */
    itkGetMacro(Gradient, GradientImagePointer);

    /** 
    *Set number of iterations for deformation process  
    */
    itkSetMacro(Iterations, int);
    itkGetMacro(Iterations, int);

    /** Set scalar factor for internal force */
    itkSetMacro(Alpha, double);

    /** Get internal force scaling factor */
    itkGetMacro(Alpha, double);

    /** Set external force scaling factor */
    itkSetMacro(Beta, double);

    /** Get external force scaling factor */
    itkGetMacro(Beta, double);

    /** Set reference metrics update scaling factor */
    itkSetMacro(Gamma, double);

    /** Get reference metrics update scaling factor */
    itkGetMacro(Gamma, double);

    /** control smoothness of the mesh */
    itkSetMacro(Rigidity, unsigned int);

    /** control smoothness of the mesh */
    itkGetMacro(Rigidity, unsigned int);

    itkSetObjectMacro(Data, GeometryMapType );
    itkGetObjectMacro(Data, GeometryMapType );

    /** current iteration number */
    itkGetMacro(Step, int);

  protected:
    DeformableSimplexMesh3DFilter();
    ~DeformableSimplexMesh3DFilter();
    DeformableSimplexMesh3DFilter(const Self&); //purposely not implemented
    void operator=(const Self&); //purposely not implemented

    void PrintSelf(std::ostream& os, Indent indent) const;


    /** */
    virtual void GenerateData();


    /**
    * Initializes the datastructures necessary for mesh 
    * deformation with the values from the passed input 
    * mesh.
    */
    virtual void Initialize();

    /**
    * Compute geometric properties like curvature and 
    * normals, which are necessary for the computation 
    * of the internal force components for each point of the mesh.
    */
    virtual void ComputeGeometry();

    /**
    * Computes the displacement of each point. Therefore 
    * internal and external forces are computed and multiplied 
    * by the constants (alpha and beta) set by the user.
    */
    virtual void ComputeDisplacement();

    /**
    * Compute the internal force component
    */
    virtual void ComputeInternalForce(SimplexMeshGeometry* data);

    /**
    * Compute the external force component
    */
    virtual void ComputeExternalForce(SimplexMeshGeometry* data);  

    /**
    * At the and of the deformation the output mesh is created
    * by creating a new mesh
    */
    virtual void ComputeOutput();

    /**
    * Method updates the reference metrics for each mesh point
    */ 
    virtual void UpdateReferenceMetrics();

    /**
    *  L function implemented follwoing the paper of Delingette
    */
    double L_Func(double r,double d, double phi);

    /**
    *  Method computes the barycentric coordinates of the passed point 
    */
    PointType ComputeBarycentricCoordinates(PointType p, SimplexMeshGeometry* data);


    /** Parameters definitions. */

    /**
    * Scalar defining the influence of the internal forces
    * Values should lie between 0.001 and 0.3. Higher values
    * increase the stiffness of the mesh
    */
    double    m_Alpha;

    /**
    * Scalar defining the influence of the external force components
    * The choice for this parameter strongly depends on the underlying 
    * data. Typical value range from 0.00001 to 0.3
    * 
    */
    double    m_Beta;

    /**
    * Gamma influneces the distribution of the mesh points. It should 
    * lie between 0.01 and 0.2. Smaller values force the mesh to be 
    * more regular. When increasing gamma, mesh points will have higher
    * density in places of high curvature.
    */
    double    m_Gamma;      

    /**
    * This scalar determines the smoothness of the surface model. Values
    * should range from 0 to 10. It determines the radius of the neighborhood
    * during internal force computation using the curvature shape contraint. 
    * The higher the rigidity the higher the smoothness.
    */
    unsigned int m_Rigidity;

    // definition of internal parameters
    /** Number of iterations */
    int       m_Step;          
    /** Image size */
    int       m_ImageWidth;      
    /** Image size */
    int       m_ImageHeight;
    /** Image size */
    int       m_ImageDepth;

    /** This threshold decides when to stop the model. */
    int       m_Iterations;


    /**
    * map stores a Geometry object for every mesh point 
    */
    GeometryMapPointer m_Data;

    /* gradient image pointer */
    GradientImagePointer m_Gradient;


  }; // end of class


  } // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableSimplexMesh3DFilter.txx"
#endif

#endif //__itkDeformableSimplexMesh3DFilter_H
