/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMRegistrationFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkFEMRegistrationFilter_h_
#define _itkFEMRegistrationFilter_h_

#include "itkFEMLinearSystemWrapperItpack.h"
#include "itkFEMLinearSystemWrapperDenseVNL.h"

#include "itkImage.h"
#include "itkCastImageFilter.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkVector.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkMeanImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkVectorCastImageFilter.h"
#include "itkWarpImageFilter.h"
#include "itkImageFileReader.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"

#include "itkTranslationTransform.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_vector_fixed.h"

#include "itkVectorIndexSelectionCastImageFilter.h"

#include <iostream>
#include <string>
#include <fstream>

#include "itkFEMGenerateMesh.h"


namespace itk {
namespace fem {

/** \class FEMRegistrationFilter 
    \brief FEM Image registration example class.

     The image registration problem is modeled here with the finite element method.
     Image registration is, in general, an ill-posed problem.  Thus, we use an optimization
     scheme where the optimization criterion is given by a regularized variational energy.
     The variational energy arises from modeling the image as a physical body on which 
     external forces act.  The body is allowed to deform so as to minimize the 
     applied force.  The resistance of the physical body to deformation, determined by 
     the physics associated with the body, serves to regularize the solution.
     The forces applied to the body are, generally, highly non-linear and so the 
     body is allowed to deform slowly and incrementally.  The direction it deforms
     follows the gradient of the potential energy (the force) we define.  The potential
     energies we may choose from are given by the itk image-to-image metrics. 
     The choices and the associated direction of descent are : 
        Mean Squares (minimize), 
        Normalized Cross-Correlation (maximize)
        Pattern Intensity  (maximize)
        Mutual Information (maximize).
     Note that we have to set the direction (SetDescentDirection) when we choose a metric. 
     The forces driving the problem may also be given by user-supplied landmarks.  
     The corners of the image, in this example, are always pinned.  This example is 
     designed for 2D or 3D images.  A rectilinear mesh is generated automatically 
     given the correct element type (Quadrilateral or Hexahedral).

     Our specific Solver for this example uses trapezoidal time stepping.  This is 
     a method for solving a second-order PDE in time.  The solution is penalized 
     by the zeroth (mass matrix) and first derivatives (stiffness matrix) of the 
     shape functions.  There is an option to perform a line search on the energy 
     after each iteration.  Optimal parameter settings require experimentation.
     The following approach tends to work well :  
        Choose the relative size of density  to elasticity (e.g. Rho / E ~= 1.)
          such that the image deforms locally and slowly.
          This also affects the stability of the solution.
        Choose the time step to control the size of the deformation at each step.
        Choose enough iterations to allow the solution to converge (this may be automated).

     Reading images is up to the user.  Either set the images using 
     SetReference/TargetImage or see the ReadImages function.  Outputs are raw images
     of the same type as the reference image.

     \note This code works for only 2 or 3 dimensions.

  \note  Not yet a REAL itk filter!
*/

template<class TReference,class TTarget> 
class FEMRegistrationFilter //: public ImageToImageFilter<TReference, TTarget>
{
public:
  typedef FEMRegistrationFilter                              Self;
//  typedef ImageToImageFilter<TReference, TTarget> Superclass;
  
  /** Run-time type information (and related methods) */
//  itkTypeMacro(FEMRegistrationFilter, ImageToImageFilter );
  
  typedef LinearSystemWrapperItpack                 LinearSystemSolverType;
  typedef SolverCrankNicolson                       SolverType;
  enum Sign { positive = 1, negative = -1 };
  typedef double                                    Float;
  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  typedef TReference                                ImageType;
  typedef TTarget                                   TargetImageType;
  enum { ImageDimension = ImageType::ImageDimension };
  typedef typename ImageType::PixelType             ImageDataType;
  typedef Image< float, ImageDimension >            FloatImageType;
  typedef typename ImageType::SizeType              ImageSizeType;
  typedef ImageToImageMetric<TTarget,TReference >   MetricBaseType;
  typedef typename MetricBaseType::Pointer          MetricBaseTypePointer;
  typedef itk::Vector<Float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::WarpImageFilter<ImageType,ImageType, FieldType> WarperType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType>         ImageIterator; 
  typedef itk::ImageRegionIteratorWithIndex<FieldType>         FieldIterator; 
  typedef itk::VectorIndexSelectionCastImageFilter<FieldType,FloatImageType> IndexSelectCasterType;

/** Instantiate the load class with the correct image type. */
  typedef  ImageMetricLoad<ImageType,ImageType>     ImageMetricLoadType;
  
  /**
   * Easy access to the FEMObjectFactory. We create a new class
   * whose name is shorter and it's not templated...
   */
  class FEMOF : public FEMObjectFactory<FEMLightObject>{};
  
  /* Main functions */
 
  /** Read the configuration file to set up the example parameters */
  bool      ReadConfigFile(const char*);

  /** Call this to register two images. */
  void      RunRegistration(); 
  
 /** Call this to write out images - a counter is attached to the 
  *  file name so we can output a numbered sequence tracking the deformation.
  */
  void      WriteWarpedImage(const char* fn);


  /** Helper functions */

  /** This function generates a regular mesh of ElementsPerSide^D size */
  void      CreateMesh(ImageSizeType MeshOrigin, ImageSizeType MeshSize, 
                              double ElementsPerSide, Solver& S);

  /** The loads are entered into the solver. */
  void      ApplyLoads(SolverType& S,ImageSizeType Isz); 

  /**  Builds the itpack linear system wrapper with appropriate parameters. 
       Currently undefined */
  void      CreateLinearSystemSolver();

  /** The solution loop */
  void      IterativeSolve(SolverType& S);  

  /** The solution loop for a simple multi-resolution strategy. */
  void      MultiResSolve();

  /** Evaluates the image similarity energy by calling the image metric */
  Float     EvaluateEnergy();
 
  /** Interpolates the vector field over the domain.  
    * Our convention is to always keep the vector field
    * at the scale of the original images.
    */
  void      GetVectorField(SolverType& S); 
  
  /** This is used for changing between mesh resolutions. */
  void      SampleVectorFieldAtNodes(SolverType& S);

  /** Applies the warp to the reference image. */
  void      WarpImage();      

  /** Writes the displacement field to a file. */
  int       WriteDisplacementField(unsigned int index);

  /** Set the following parameters to run the example */
  /** One can set the referencen file names to read images from files */
  void      SetReferenceFile(const char* r) {m_ReferenceFileName=r;}
  const char* GetReferenceFile() {return m_ReferenceFileName;}
  void      SetTargetFile(const char* t) {m_TargetFileName=t;}
  const char* GetTargetFile() {return m_TargetFileName;}
  /** One can set the images directly to input images in an application */ 
  /** Define the reference (moving) image. */
  void SetReferenceImage(ImageType* R);
  /** Define the target (fixed) image. */
  void SetTargetImage(TargetImageType* T);
  ImageType* GetReferenceImage(){return m_RefImg;}
  TargetImageType* GetTargetImage(){return m_TarImg;}

  void      SetLandmarkFile(const char* l) {m_LandmarkFileName=l; }
  void      SetResultsFile(const char* r) {m_ResultsFileName=r;}
  void      SetDisplacementsFile(const char* r) {m_DisplacementsFileName=r;}
  void      SetMeshResolution(unsigned int i){ m_MeshResolution=i;}
  void      SetNumberOfIntegrationPoints(unsigned int i){ m_NumberOfIntegrationPoints=i;}
  void      SetWidthOfMetricRegion(unsigned int i) { m_MetricWidth=i;}
  void      SetMaximumIterations(unsigned int i) { m_Maxiters=i;}
  void      SetTimeStep(Float i) { m_dT=i;}
  void      SetElasticity(Float i) { m_E=i;} /** Stiffness Matrix weight */
  Float     GetElasticity() { return m_E;} /** Stiffness Matrix weight */
  void      SetRho(Float r) { m_Rho=r;} /** Mass matrix weight */  
  void      SetDescentDirectionMinimize() { m_DescentDirection=positive;} /** Tries to minimize energy */
  void      SetDescentDirectionMaximize() { m_DescentDirection=negative;} /** Tries to maximize energy */
  void      DoLineSearch(bool b) { m_DoLineSearchOnImageEnergy=b; } /** Finds the minimum energy between the current and next solution by linear search.*/
  void      DoMultiRes(bool b) { m_DoMultiRes=b; } 
  void      DoSearchForMinAtEachResolution(bool b) { m_SearchForMinAtEachLevel=b; } 
  void      UseLandmarks(bool b) {m_UseLandmarks=b;}
  void      SetWriteDisplacements(bool b) {m_WriteDisplacementField=b;}
  bool      GetWriteDisplacements() {return m_WriteDisplacementField;}
  SolverType* GetSolver(){return &m_Solver;}
  void      SetConfigFileName (const char* f){m_ConfigFileName=f;}
  const char* GetConfigFileName () {return m_ConfigFileName; }
  void      SetResultsFileName (const char* f){m_ResultsFileName=f;}
  const char* GetResultsFileName () {return m_ResultsFileName;} 
  ImageSizeType GetImageSize(){ return m_ImageSize; }

  /** Set/Get the Metric.  */
  void      SetMetric(MetricBaseTypePointer MP) { m_Metric=MP; }
  
  /** This function allows one to set the element and its material externally. */
  void      SetElement(Element::Pointer e) {m_Element=e;}


  /** constructor */
  FEMRegistrationFilter( ); 
  ~FEMRegistrationFilter(); 


private :

  FEMRegistrationFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
    
  SolverType m_Solver; // Defines the solver to use

  const char*      m_ConfigFileName;
  const char*      m_ResultsFileName;
  const char*      m_ReferenceFileName;  
  const char*      m_TargetFileName;
  const char*      m_LandmarkFileName;
  const char*      m_DisplacementsFileName;

  unsigned int     m_MeshResolution; // determines maximum resolution of regular mesh
  unsigned int     m_NumberOfIntegrationPoints;// resolution of integration
  unsigned int     m_MetricWidth;
  unsigned int     m_Maxiters; // max iterations
  unsigned int     m_TotalIterations;
  unsigned int     m_NumLevels; // Number of Resolution Levels
  unsigned int     m_MaxLevel;  // Maximum Level (NumLevels is original resolution).
  unsigned int     m_MeshLevels;// Number of Mesh Resolutions ( should be >= 1)
  unsigned int     m_MeshStep;  // Ratio Between Mesh Resolutions ( currently set to 2, should be >= 1)
  unsigned int     m_FileCount; // keeps track of number of files written
 
  /** Stores the number of elements per dimension of the mesh for each
      resolution of the multi-resolution pyramid */
  vnl_vector<unsigned int> m_MeshElementsPerDimensionAtEachResolution;

  Float     m_dT; // time step
  Float     m_E;  // elasticity 
  Float     m_Energy; // current value of energy
  Float     m_MinE;  // minimum recorded energy
  Float     m_Rho;   // mass matrix weight
  Float     m_Alpha; // difference parameter
  Float     m_LineSearchStep;

  bool  m_DoLineSearchOnImageEnergy;  
  bool  m_WriteDisplacementField;
  bool  m_DoMultiRes;
  bool  m_SearchForMinAtEachLevel;
  bool  m_UseLandmarks;
  Sign  m_DescentDirection;

  ImageSizeType     m_ImageSize; // image size
  ImageSizeType     m_ImageOrigin; // image size
  /** Gives the ratio of original image size to current image size - for dealing with multi-res.*/
  ImageSizeType     m_ImageScaling; 
  typename ImageType::RegionType   m_FieldRegion;
  typename ImageType::SizeType     m_FieldSize;
  typename FieldType::Pointer      m_Field;

  ImageMetricLoad<ImageType,ImageType>* m_Load; // Defines the load to use
   
  // define the warper
  typename WarperType::Pointer m_Warper; 

 // declare a new image to hold the warped  reference
  typename ImageType::Pointer      m_WarpedImage;
  typename ImageType::RegionType   m_Wregion; 
  typename ImageType::IndexType    m_Windex;
 
 // declare images for target and reference
  typename ImageType::Pointer      m_RefImg;
  typename ImageType::Pointer      m_TarImg;
  typename ImageType::RegionType   m_Rregion;
  typename ImageType::RegionType   m_Tregion;
  typename ImageType::IndexType    m_Rindex;
  typename ImageType::IndexType    m_Tindex;

  // element and metric pointers
  typename Element::Pointer        m_Element;
  MetricBaseTypePointer            m_Metric;
 

};

}} // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMRegistrationFilter.txx"
#endif

#endif
