/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMRegistrationFilter_h
#define itkFEMRegistrationFilter_h

#include "itkFEMLinearSystemWrapperItpack.h"
#include "itkFEMLinearSystemWrapperDenseVNL.h"
#include "itkFEMObject.h"
#include "itkFEMSolverCrankNicolson.h"
#include "itkFEMMaterialLinearElasticity.h"
#include "itkFEMImageMetricLoad.h"
#include "itkFEMFiniteDifferenceFunctionLoad.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageToRectilinearFEMObjectFilter.h"
#include "itkVectorCastImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkWarpImageFilter.h"
#include "itkImageToImageMetric.h"
#include "itkTranslationTransform.h"
#include "itkVectorExpandImageFilter.h"
#include "itkFixedArray.h"

#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkFEMLoadLandmark.h"

#include "vnl/vnl_vector.h"
#include "itkMath.h"
#include "vnl/vnl_vector_fixed.h"

#include <iostream>
#include <string>

namespace itk
{
namespace fem
{

/** \class FEMRegistrationFilter
 *  \brief FEM Image registration filter.
 * The image registration problem is modelled here with the finite
 * element method. Image registration is, in general, an ill-posed
 * problem. Thus, we use an optimization scheme where the
 * optimization criterion is given by a regularized variational
 * energy. The variational energy arises from modeling the image as a
 * physical body on which external forces act. The body is allowed to
 * deform so as to minimize the applied force. The resistance of the
 * physical body to deformation, determined by the physics associated
 * with the body, serves to regularize the solution. The forces
 * applied to the body are, generally, highly non-linear and so the
 * body is allowed to deform slowly and incrementally. The direction
 * it deforms follows the gradient of the potential energy (the force)
 * we define. The potential energies we may choose from are given by
 * the itk image-to-image metrics. The choices and the associated
 * direction of descent are :
 *       Mean Squares (minimize),
 *       Normalized Cross-Correlation (maximize), and
 *       Mutual Information (maximize).
 * Note that we have to set the direction (SetDescentDirection)
 * when we choose a metric.
 *
 * The forces driving the problem may also be given by user-supplied
 * landmarks. The corners of the image, in this example, are always
 * pinned. This example is designed for 2D or 3D images.  A
 * rectilinear mesh is generated automatically given the correct
 * element type (Quadrilateral or Hexahedral). Our specific Solver for
 * this example uses trapezoidal time stepping. This is a method for
 * solving a second-order PDE in time. The solution is penalized by
 * the zeroth (mass matrix) and first derivatives (stiffness matrix)
 * of the shape functions. There is an option to perform a line
 * search on the energy after each iteration. Optimal parameter
 * settings require experimentation.
 *   The following approach tends to work well :
 *       Choose the relative size of density  to elasticity (e.g. Rho
 *       / E ~= 1.) such that the image deforms locally and
 *       slowly. This also affects the stability of the
 *       solution. Choose the time step to control the size of the
 *       deformation at each step. Choose enough iterations to allow
 *       the solution to converge (this may be automated).
 *
 *
 * To use this filter the user will at a minimum set the Fixed and
 * Moving images. If the user does not specify a mesh using
 * the SetInputFEMObject() then a mesh will be created automatically
 * of the approriate type (2d=quads and 3d=hex). The user has
 * significant control over the registration process including
 * setting number of resolution levels, material properties, and
 * the metric used to define correspondence between images.
 *
 *   \note This code works for only 2 or 3 dimensions b/c we do not
 *   have > 3D elements.
 *
 *   \note TODO :  Keep the full field around (if using
 *   re-gridding). Introduce compensation for kinematic non-linearity
 *   in time (if using Eulerian frame).
 * \ingroup ITKFEMRegistration
 */

template <typename TMovingImage, typename TFixedImage, typename TFemObjectType>
class ITK_TEMPLATE_EXPORT FEMRegistrationFilter : public ImageToImageFilter<TMovingImage, TFixedImage>
{
public:
  typedef FEMRegistrationFilter                         Self;
  typedef ImageToImageFilter<TMovingImage, TFixedImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(FEMRegistrationFilter, ImageToImageFilter );

  typedef TMovingImage                       MovingImageType;
  typedef TFixedImage                        FixedImageType;
  typedef TFemObjectType                     FEMObjectType;
  typedef typename FixedImageType::PixelType PixelType;
  typedef typename FixedImageType::SizeType  ImageSizeType;
  typedef typename FixedImageType::PointType PointType;

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      FixedImageType::ImageDimension);

  typedef Image<float, itkGetStaticConstMacro(ImageDimension)> FloatImageType;
  typedef LinearSystemWrapperItpack                            LinearSystemSolverType;
  typedef SolverCrankNicolson<ImageDimension>                  SolverType;

  enum Sign { positive = 1, negative = -1 };
  typedef double          Float;
  typedef Load::ArrayType LoadArray;

  typedef std::vector<typename LoadLandmark::Pointer>                      LandmarkArrayType;
  typedef itk::Vector<float, itkGetStaticConstMacro(ImageDimension)>       VectorType;
  typedef itk::Image<VectorType, itkGetStaticConstMacro(ImageDimension)>   FieldType;
  typedef itk::WarpImageFilter<MovingImageType, FixedImageType, FieldType> WarperType;

  typedef MaterialLinearElasticity                          MaterialType;
  typedef itk::ImageRegionIteratorWithIndex<FixedImageType> ImageIterator;
  typedef itk::ImageRegionIteratorWithIndex<FloatImageType> FloatImageIterator;
  typedef itk::ImageRegionIteratorWithIndex<FieldType>      FieldIterator;

  typedef itk::VectorIndexSelectionCastImageFilter<FieldType, FloatImageType> IndexSelectCasterType;

  /** Typedef support for the interpolation function */
  typedef double                                                  CoordRepType;
  typedef VectorInterpolateImageFunction<FieldType, CoordRepType> InterpolatorType;
  typedef typename InterpolatorType::Pointer                      InterpolatorPointer;

  typedef VectorLinearInterpolateImageFunction<FieldType, CoordRepType> DefaultInterpolatorType;

  typedef typename itk::Image<Element::ConstPointer, ImageDimension> InterpolationGridType;
  typedef typename InterpolationGridType::SizeType                   InterpolationGridSizeType;
  typedef typename InterpolationGridType::PointType                  InterpolationGridPointType;

  typedef itk::fem::ImageToRectilinearFEMObjectFilter<TMovingImage> ImageToMeshType;

  typedef itk::VectorExpandImageFilter<FieldType, FieldType> ExpanderType;
  typedef typename ExpanderType::ExpandFactorsType           ExpandFactorsType;

  typedef  typename FieldType::Pointer FieldPointer;

  /** Instantiate the load class with the correct image type. */
  typedef FiniteDifferenceFunctionLoad<MovingImageType, FixedImageType>
  ImageMetricLoadType;
  typedef PDEDeformableRegistrationFunction<FixedImageType, MovingImageType, FieldType>
  MetricBaseType;

  typedef typename MetricBaseType::Pointer     MetricBaseTypePointer;
  typedef FixedArray< double, ImageDimension > StandardDeviationsType;

  /** Set the Moving image. */
  void SetMovingImage(MovingImageType* R);

  /**
   * Get the Moving image. This image is dependent on the current resolution
   *in a multi-resolution registration.
   */
  MovingImageType * GetMovingImage()
  {
    return m_MovingImage;
  }

  /** Get the original full resolution moving image. */
  MovingImageType * GetOriginalMovingImage()
  {
    return m_OriginalMovingImage;
  }

  /** Get/Set the target (fixed) image. */
  void SetFixedImage(FixedImageType* T);

  FixedImageType * GetFixedImage()
  {
    return m_FixedImage;
  }

  /**
   * Get/Set the finite element mesh for the registration. A separate
   * mesh will be returned for each level of the registration. If the
   * user provides a mesh, one should be provided for each level.
   */
  void SetInputFEMObject(FEMObjectType* F, unsigned int level = 0);

  FEMObjectType * GetInputFEMObject(unsigned int level = 0);

  /** Call this to register two images. */
  void RunRegistration();

  /** The solution loop. */
  void IterativeSolve(SolverType *S);

  /** The solution loop for a simple multi-resolution strategy. */
  void MultiResSolve();

  /** Applies the warp to the input image. */
  void WarpImage(const MovingImageType * R);

  /** Get the reference image warped to the target image.
      Must first apply the warp using WarpImage() */
  FixedImageType * GetWarpedImage()
  {
    return m_WarpedImage;
  }

  /** Compute the jacobian of the current deformation field. */
  void ComputeJacobian();

  /** Get the image that gives the jacobian of the deformation field. */
  FloatImageType * GetJacobianImage()
  {
    return m_FloatImage;
  }

  /** Outputs the FE deformation field interpolated over the entire image domain. */
  FieldType * GetDisplacementField()
  {
    return m_Field;
  }

  /** Sets the FE deformation field. */
  void SetDisplacementField(FieldType* F)
  {
    m_FieldSize = F->GetLargestPossibleRegion().GetSize();
    m_Field = F;
  }

  /** Add a way to include landmarks. */
  void AddLandmark(PointType source, PointType target);

  void InsertLandmark(unsigned int i, PointType source, PointType target);

  void DeleteLandmark(unsigned int i);

  void ClearLandmarks();

  void GetLandmark(unsigned int i, PointType& source, PointType& target);

  /** We check the jacobian of the current deformation field.
    *  If it is < threshold, we begin diffeomorphism enforcement:
    *    1)  Warp the moving image.
    *    2)  Set the vector field to zero.
    *    3)  Set the warped moving image as the new moving image,
    *        resizing if necessary.
    */
  void EnforceDiffeomorphism(float thresh, SolverType *S,  bool onlywriteimages);

  /** The FEM filter can generate its own mesh for 2 or 3 dimensions, if none is provided.
      The mesh is generated for quadrilaterals in 2D and hexahedra in 3D.  This function
      sets the number of elements generated along each dimension at the resolution
      designated by "which".
      E.g. to generate 10 pixels per element in each dimension in the 1st resolution, use SetMeshResolution(10,0);.
    */
  void SetMeshPixelsPerElementAtEachResolution(unsigned int i, unsigned int which = 0)
  {
    m_MeshPixelsPerElementAtEachResolution[which] = i;
  }

  /** This determines the number of integration points to use at each resolution.
      These integration points are used to generate the force.  The actual number
      used will be i^d, where d is the number of parameters in the elements local domain. */
  void SetNumberOfIntegrationPoints(unsigned int i, unsigned int which = 0)
  {
    m_NumberOfIntegrationPoints[which] = i;
  }

  /** The metric region allows one to compute the derivative (force) of the similarity metric
    * using a region of size [i,i] in 2D and [i,i,i] in 3D.
    * \param i number of elements
    * \param which determines the region at a given resolution of the solution process.
    */
  void SetWidthOfMetricRegion(unsigned int i, unsigned int which = 0)
  {
    m_MetricWidth[which] = i;
  }

  unsigned int GetWidthOfMetricRegion(unsigned int which = 0)
  {
    return m_MetricWidth[which];
  }

  /** Setting the maximum iterations stops the solution after i iterations regardless of energy.
    * \param i number of elements
    * \param which determines the resolution of the solution process the call is applied to.
    */
  void SetMaximumIterations(unsigned int i, unsigned int which)
  {
    m_Maxiters[which] = i;
  }

  /**
   * Get/Set the time step. This it typically set to 1.0, which
   * is the default value. It may be preferable to use Rho to
   * control step sizes.
   */
  itkSetMacro(TimeStep, Float);
  itkGetMacro(TimeStep, Float);

  /**
   * Get/Set Difference parameter for the trapezoidal rule. This is usually set
   *  to 1.0, which is the default value.
   */
  itkSetMacro(Alpha, Float);
  itkGetMacro(Alpha, Float);

  /**
  * Get/Set if landmarks are being used.
  */
  itkSetMacro(UseLandmarks, bool);
  itkGetMacro(UseLandmarks, bool);
  itkBooleanMacro(UseLandmarks);
#if !defined(ITK_LEGACY_REMOVE)
  /** \deprecated Replaced by UseLandmarksOff() as of ITK 4.12. */
  itkLegacyMacro(void SetUseLandmarksOff())
  {
    SetUseLandmarks(false);
  }

  /** \deprecated Replaced by UseLandmarksOn() as of ITK 4.12. */
  itkLegacyMacro(void SetUseLandmarksOn())
  {
    SetUseLandmarks(true);
  }
#endif
  /**
   * Get/Set Use of the mass matrix in FEM solution. This should be true.
   */
  itkSetMacro(UseMassMatrix, bool);
  itkGetMacro(UseMassMatrix, bool);
  itkBooleanMacro(UseMassMatrix);

  /**
   * Get/Set the energy below which we decide the solution has converged.
   */
  itkSetMacro(EnergyReductionFactor, Float);
  itkGetMacro(EnergyReductionFactor, Float);

  /** Sets the stiffness Matrix weight. */
  void SetElasticity(Float i, unsigned int which = 0)
  {
    m_E[which] = i;
  }

  /** Gets the stiffness Matrix weight. */
  Float GetElasticity(unsigned int which = 0)
  {
    return m_E[which];
  }

  /** Set mass matrix weight. */
  void SetRho(Float r, unsigned int which = 0)
  {
    m_Rho[which] = r;
  }

  /** Set image similarity energy weight. */
  void SetGamma(Float r, unsigned int which = 0)
  {
    m_Gamma[which] = r;
  }

  /** Image Metric minimizes energy. */
  void  SetDescentDirectionMinimize()
  {
    m_DescentDirection = positive;
  }

  /** Image Metric maximizes energy. */
  void SetDescentDirectionMaximize()
  {
    m_DescentDirection = negative;
  }

  /**
   * Get/Set the minimum energy between the current and next solution
   * by linear search.
   */
  itkSetMacro(DoLineSearchOnImageEnergy, unsigned int);
  itkGetMacro(DoLineSearchOnImageEnergy, unsigned int);


  /**
   * Get/Set the use of normalized gradient values in the image
   * metric during registration.
   */
  itkSetMacro(UseNormalizedGradient, bool);
  itkGetMacro(UseNormalizedGradient, bool);
  itkBooleanMacro(UseNormalizedGradient);
#if !defined(ITK_LEGACY_REMOVE)
  /** \deprecated Replaced by UseNormalizedGradientOff() as of ITK 4.12. */
  itkLegacyMacro(void SetUseNormalizedGradientOff())
  {
    SetUseNormalizedGradient(false);
  }

  /** \deprecated Replaced by UseNormalizedGradientOn() as of ITK 4.12. */
  itkLegacyMacro(void SetUseNormalizedGradientOn())
  {
    SetUseNormalizedGradient(true);
  }
#endif
  /**
   * Get/Set the number of iterations before regridding is employed.
   */
  itkSetMacro(EmployRegridding, unsigned int);
  itkGetMacro(EmployRegridding, unsigned int);

  /**
   * Get/Set the line search maximum number of iterations. The
   * default value is 100.
   */
  itkSetMacro(LineSearchMaximumIterations, unsigned int);
  itkGetMacro(LineSearchMaximumIterations, unsigned int);

  /**
   * Return the size of the full size image.
   */
  ImageSizeType GetImageSize()
  {
    return m_FullImageSize;
  }

  /**
   * Get/Set the Metric used to define correspondence between
   * images.
   */
  itkGetModifiableObjectMacro(Metric, MetricBaseType);
  itkSetObjectMacro(Metric, MetricBaseType);

  /**
   * Select the metric used for image correspondence.
   * The options are:
   *  0 = Mean squares
   *  1 = Cross correlation
   *  2 = Mutual information
   *  3 = Demons
   */
  void ChooseMetric( unsigned int whichmetric );

  /**
   * Return the type of image metric used for the registration.
   */
  unsigned int GetMetricType()
  {
    return m_WhichMetric;
  }

  /** This function allows one to set the element and its material externally. */
  void SetElement(Element::Pointer e)
  {
    m_Element = e;
  }

  /** This sets the pointer to the material. */
  void SetMaterial(MaterialType::Pointer m)
  {
    m_Material = m;
  }

  /** Print vector field for debugging. */
  void PrintVectorField(unsigned int modnum = 1000);

  /**
   * Get/Set the maximum number of levels for multi-resolution.
   */
  void SetMaxLevel(unsigned int level);
  itkGetMacro(MaxLevel, unsigned int);

  /**
   * Get/Set if the FEM Mesh should be created from the fixed image or is
   * provided by the user.
   */
  itkSetMacro(CreateMeshFromImage, bool);
  itkGetMacro(CreateMeshFromImage, bool);
  itkBooleanMacro(CreateMeshFromImage);
#if !defined(ITK_LEGACY_REMOVE)
  /** \deprecated Replaced by CreateMeshFromImageOn() as of ITK 4.12. */
  itkLegacyMacro(void SetCreateMeshFromImageOn())
  {
    SetCreateMeshFromImage(true);
  }

  /** \deprecated Replaced by CreateMeshFromImageOff() as of ITK 4.12. */
  itkLegacyMacro(void SetCreateMeshFromImageOff())
  {
    SetCreateMeshFromImage(false);
  }
#endif
  /** Set the interpolator function. */
  itkSetObjectMacro( Interpolator, InterpolatorType );

  /** Get a pointer to the interpolator function. */
  itkGetModifiableObjectMacro( Interpolator, InterpolatorType );

  /** Set the Gaussian smoothing standard deviations for the
   * displacement field. The values are set with respect to pixel
   * coordinates. */
  itkSetMacro(StandardDeviations, StandardDeviationsType);
  virtual void SetStandardDeviations(double value);

  /** Get the Gaussian smoothing standard deviations use for smoothing
   * the displacement field. */
  itkGetConstReferenceMacro(StandardDeviations, StandardDeviationsType);

  /** Set/Get the desired limits of the Gaussian kernel width.
   * \sa GaussianOperator. */
  itkSetMacro(MaximumKernelWidth, unsigned int);
  itkGetConstMacro(MaximumKernelWidth, unsigned int);

  /** Set/Get the desired maximum error of the Gaussian kernel approximate.
   * \sa GaussianOperator. */
  itkSetMacro(MaximumError, double);
  itkGetConstMacro(MaximumError, double);

protected:
  FEMRegistrationFilter();
  ~FEMRegistrationFilter() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This function generates a regular mesh of ElementsPerSide^D size. */
  void CreateMesh(unsigned int ElementsPerSide, SolverType *solver);

  /** The non-image loads are entered into the solver. */
  void ApplyLoads(ImageSizeType Isz, double* spacing = ITK_NULLPTR);

  /** The image loads are entered into the solver. */
  void ApplyImageLoads(MovingImageType* i1, FixedImageType* i2);

  // FIXME - Not implemented
  /**  Builds the itpack linear system wrapper with appropriate parameters.
       Currently undefined. */
  void  CreateLinearSystemSolver();

  // FIXME - Not implemented
  /** Evaluates the image similarity energy by calling the image metric. */
  Float EvaluateEnergy();

  /** Interpolates the vector field over the domain.
    * Our convention is to always keep the vector field
    * at the scale of the original images.
    */
  void InterpolateVectorField(SolverType *S);

  // FIXME - Not implemented
  /** Calculates the metric over the domain given the vector field.
    */
  FloatImageType * GetMetricImage(FieldType* F);

  /** Re-size the vector field (smaller to larger). */
  FieldPointer ExpandVectorField(ExpandFactorsType* expandFactors, FieldType* f);

  /** This is used for changing between mesh resolutions. */
  void SampleVectorFieldAtNodes(SolverType *S);

  /** This is used to calculate residual error. */
  Float EvaluateResidual(SolverType *mySolver, Float t);

  // FIXME - Replace with BSD Code
  /* Finds a triplet that brackets the energy minimum. From Numerical Recipes.*/
  // void FindBracketingTriplet(SolverType& mySolver,Float* a, Float* b, Float* c);
  void FindBracketingTriplet(SolverType *mySolver, Float* a, Float* b, Float* c);

  /**
   * Finds the optimum value between the last two solutions
   * and sets the current solution to that value. Uses Evaluate Residual
   */
  Float GoldenSection(SolverType *mySolver, Float tol = 0.01, unsigned int MaxIters = 25);

  /** Get/Set the solver's current load. */
  itkSetObjectMacro( Load, ImageMetricLoadType );
  itkGetModifiableObjectMacro(Load, ImageMetricLoadType );

  /** Smooth the current displacement field using a separable Gaussian kernel. */
  void SmoothDisplacementField();

private:

  void InitializeField();

  ITK_DISALLOW_COPY_AND_ASSIGN(FEMRegistrationFilter);

  unsigned int m_DoLineSearchOnImageEnergy;
  unsigned int m_LineSearchMaximumIterations;

  /** Parameters used to define Multi-resolution registration. */
  vnl_vector<unsigned int> m_NumberOfIntegrationPoints; // resolution of integration
  vnl_vector<unsigned int> m_MetricWidth;               // number of iterations at each level
  vnl_vector<unsigned int> m_Maxiters;
  unsigned int             m_TotalIterations;           // total number of iterations that were run
  unsigned int             m_MaxLevel;
  unsigned int             m_FileCount;                 // keeps track of number of files written
  unsigned int             m_CurrentLevel;              // current resolution level

  typename FixedImageType::SizeType     m_CurrentLevelImageSize;

  unsigned int m_WhichMetric;

  /** Stores the number of  pixels per element  of the mesh for each
      resolution of the multi-resolution pyramid */
  vnl_vector<unsigned int> m_MeshPixelsPerElementAtEachResolution;

  Float             m_TimeStep;
  vnl_vector<Float> m_E;
  vnl_vector<Float> m_Rho;
  vnl_vector<Float> m_Gamma;
  Float             m_Energy;          // current value of energy
  Float             m_MinE;            // minimum recorded energy
  Float             m_MinJacobian;     // minimum recorded energy
  Float             m_Alpha;

  bool          m_UseLandmarks;
  bool          m_UseMassMatrix;
  bool          m_UseNormalizedGradient;
  bool          m_CreateMeshFromImage;
  unsigned int  m_EmployRegridding;
  Sign          m_DescentDirection;
  Float         m_EnergyReductionFactor;
  ImageSizeType m_FullImageSize;
  ImageSizeType m_ImageOrigin;

  /** Gives the ratio of original image size to current image size - for
   * dealing with multi-resolution. */
  ImageSizeType                  m_ImageScaling;
  ImageSizeType                  m_CurrentImageScaling;
  typename FieldType::RegionType m_FieldRegion;
  typename FieldType::SizeType   m_FieldSize;
  typename FieldType::Pointer    m_Field;

  // Only use TotalField if re-gridding is employed.
  typename FieldType::Pointer               m_TotalField;
  typename ImageMetricLoadType::Pointer     m_Load; // Defines the load to use

  // Define the warper
  typename WarperType::Pointer          m_Warper;

  // Declare a new image to hold the warped reference
  typename FixedImageType::Pointer      m_WarpedImage;
  typename FloatImageType::Pointer      m_FloatImage;
  typename FixedImageType::RegionType   m_Wregion;
  typename FixedImageType::IndexType    m_Windex;

  // Declare images for target and reference
  typename MovingImageType::Pointer     m_MovingImage;
  typename MovingImageType::Pointer     m_OriginalMovingImage;
  typename FixedImageType::Pointer      m_FixedImage;

  // Element and metric pointers
  typename Element::Pointer             m_Element;
  typename MaterialType::Pointer        m_Material;
  MetricBaseTypePointer                 m_Metric;
  typename FEMObjectType::Pointer       m_FEMObject;

  LandmarkArrayType   m_LandmarkArray;
  InterpolatorPointer m_Interpolator;

  double m_MaximumError;

  unsigned int m_MaximumKernelWidth;

  StandardDeviationsType m_StandardDeviations;

};

}
}  // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMRegistrationFilter.hxx"
#endif

#endif
