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
#ifndef itkRegionBasedLevelSetFunction_h
#define itkRegionBasedLevelSetFunction_h

#include "itkFiniteDifferenceFunction.h"
#include "itkRegularizedHeavisideStepFunction.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
/** \class RegionBasedLevelSetFunction
 *
 * \brief LevelSet function that computes a speed image based on regional integrals
 *
 * This class implements a level set function that computes the speed image by
 * integrating values on the image domain.
 *
 * Based on the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese.
 *         In Scale-Space Theories in Computer Vision, pages 141-151, 1999.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 * NOTE: The convention followed is
 * inside of the level-set function is negative and outside is positive.
 * \ingroup ITKReview
 */
template< typename TInput,   // LevelSetImageType
          typename TFeature, // FeatureImageType
          typename TSharedData >
class ITK_TEMPLATE_EXPORT RegionBasedLevelSetFunction:public
  FiniteDifferenceFunction< TInput >
{
public:
  /** Standard class typedefs. */
  typedef RegionBasedLevelSetFunction        Self;
  typedef FiniteDifferenceFunction< TInput > Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  // itkNewMacro() is not provided since this is an abstract class.

  /** Run-time type information (and related methods) */
  itkTypeMacro(RegionBasedLevelSetFunction, FiniteDifferenceFunction);

  /** Extract some parameters from the superclass. */
  typedef double                                      TimeStepType;
  typedef typename Superclass::ImageType              ImageType;
  typedef typename Superclass::PixelType              PixelType;
  typedef PixelType                                   ScalarValueType;
  typedef typename Superclass::RadiusType             RadiusType;
  typedef typename Superclass::NeighborhoodType       NeighborhoodType;
  typedef typename Superclass::NeighborhoodScalesType NeighborhoodScalesType;
  typedef typename Superclass::FloatOffsetType        FloatOffsetType;
  typedef FixedArray< ScalarValueType, itkGetStaticConstMacro(ImageDimension) >
  VectorType;

  /* This structure is derived from LevelSetFunction and stores intermediate
  values for computing time step sizes */
  struct GlobalDataStruct {
    GlobalDataStruct()
    {
      ScalarValueType null_value = NumericTraits< ScalarValueType >::ZeroValue();

      m_MaxCurvatureChange   = null_value;
      m_MaxAdvectionChange   = null_value;
      m_MaxGlobalChange      = null_value;
    }

    ~GlobalDataStruct() {}

    vnl_matrix_fixed< ScalarValueType,
                      itkGetStaticConstMacro(ImageDimension),
                      itkGetStaticConstMacro(ImageDimension) > m_dxy;

    ScalarValueType m_dx[itkGetStaticConstMacro(ImageDimension)];

    ScalarValueType m_dx_forward[itkGetStaticConstMacro(ImageDimension)];
    ScalarValueType m_dx_backward[itkGetStaticConstMacro(ImageDimension)];

    ScalarValueType m_GradMagSqr;
    ScalarValueType m_GradMag;

    ScalarValueType m_MaxCurvatureChange;
    ScalarValueType m_MaxAdvectionChange;
    ScalarValueType m_MaxGlobalChange;
  };

  typedef TInput                                  InputImageType;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::PixelType      InputPixelType;
  typedef typename InputImageType::IndexType      InputIndexType;
  typedef typename InputImageType::IndexValueType InputIndexValueType;
  typedef typename InputImageType::SizeType       InputSizeType;
  typedef typename InputImageType::SizeValueType  InputSizeValueType;
  typedef typename InputImageType::RegionType     InputRegionType;
  typedef typename InputImageType::PointType      InputPointType;

  typedef TFeature                                FeatureImageType;
  typedef typename FeatureImageType::ConstPointer FeatureImageConstPointer;
  typedef typename FeatureImageType::PixelType    FeaturePixelType;
  typedef typename FeatureImageType::IndexType    FeatureIndexType;
  typedef typename FeatureImageType::SpacingType  FeatureSpacingType;
  typedef typename FeatureImageType::OffsetType   FeatureOffsetType;

  typedef TSharedData                      SharedDataType;
  typedef typename SharedDataType::Pointer SharedDataPointer;

  typedef HeavisideStepFunctionBase< InputPixelType, InputPixelType > HeavisideFunctionType;
  typedef typename HeavisideFunctionType::ConstPointer                HeavisideFunctionConstPointer;

  void SetDomainFunction(const HeavisideFunctionType *f)
  {
    this->m_DomainFunction = f;
  }

  virtual void Initialize(const RadiusType & r)
  {
    this->SetRadius(r);

    // Dummy neighborhood.
    NeighborhoodType it;
    it.SetRadius(r);

    // Find the center index of the neighborhood.
    m_Center =  it.Size() / 2;

    // Get the stride length for each axis.
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      m_xStride[i] = it.GetStride(i);
      }
  }

#if !defined( ITK_WRAPPING_PARSER )
  void SetSharedData(SharedDataPointer sharedDataIn)
  {
    this->m_SharedData = sharedDataIn;
  }
#endif

  void UpdateSharedData(bool forceUpdate);

  void * GetGlobalDataPointer() const ITK_OVERRIDE
  {
    return new GlobalDataStruct;
  }

  TimeStepType ComputeGlobalTimeStep(void *GlobalData) const ITK_OVERRIDE;

  /** Compute the equation value. */
  virtual PixelType ComputeUpdate( const NeighborhoodType & neighborhood,
                                   void *globalData, const FloatOffsetType & = FloatOffsetType(0.0) ) ITK_OVERRIDE;

  void SetInitialImage(InputImageType *f)
  {
    m_InitialImage = f;
  }

  virtual const FeatureImageType * GetFeatureImage() const
  { return m_FeatureImage.GetPointer(); }
  virtual void SetFeatureImage(const FeatureImageType *f)
  {
    m_FeatureImage = f;

    FeatureSpacingType spacing = m_FeatureImage->GetSpacing();
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      this->m_InvSpacing[i] = 1 / spacing[i];
      }
  }

  /** Advection field.  Default implementation returns a vector of zeros. */
  virtual VectorType AdvectionField(const NeighborhoodType &,
                                    const FloatOffsetType &, GlobalDataStruct * = 0)  const
  { return this->m_ZeroVectorConstant; }

  /** Nu. Area regularization values */
  void SetAreaWeight(const ScalarValueType & nu)
  { this->m_AreaWeight = nu; }
  ScalarValueType GetAreaWeight() const
  { return this->m_AreaWeight; }

  /** Lambda1. Internal intensity difference weight */
  void SetLambda1(const ScalarValueType & lambda1)
  { this->m_Lambda1 = lambda1; }
  ScalarValueType GetLambda1() const
  { return this->m_Lambda1; }

  /** Lambda2. External intensity difference weight */
  void SetLambda2(const ScalarValueType & lambda2)
  { this->m_Lambda2 = lambda2; }
  ScalarValueType GetLambda2() const
  { return this->m_Lambda2; }

  /** Gamma. Overlap penalty */
  void SetOverlapPenaltyWeight(const ScalarValueType & gamma)
  { this->m_OverlapPenaltyWeight = gamma; }
  ScalarValueType GetOverlapPenaltyWeight() const
  { return this->m_OverlapPenaltyWeight; }

  /** Gamma. Scales all curvature weight values */
  virtual void SetCurvatureWeight(const ScalarValueType c)
  { m_CurvatureWeight = c; }
  ScalarValueType GetCurvatureWeight() const
  { return m_CurvatureWeight; }

  void SetAdvectionWeight(const ScalarValueType & iA)
  { this->m_AdvectionWeight = iA; }
  ScalarValueType GetAdvectionWeight() const
  { return this->m_AdvectionWeight; }

  /** Weight of the laplacian smoothing term */
  void SetReinitializationSmoothingWeight(const ScalarValueType c)
  { m_ReinitializationSmoothingWeight = c; }
  ScalarValueType GetReinitializationSmoothingWeight() const
  { return m_ReinitializationSmoothingWeight; }

  /** Volume matching weight.  */
  void SetVolumeMatchingWeight(const ScalarValueType & tau)
  { this->m_VolumeMatchingWeight = tau; }
  ScalarValueType GetVolumeMatchingWeight() const
  { return this->m_VolumeMatchingWeight; }

  /** Pixel Volume = Number of pixels inside the level-set  */
  void SetVolume(const ScalarValueType & volume)
  { this->m_Volume = volume; }
  ScalarValueType GetVolume() const
  { return this->m_Volume; }

  /** Set function id.  */
  void SetFunctionId(const unsigned int & iFid)
  { this->m_FunctionId = iFid; }

  virtual void ReleaseGlobalDataPointer(void *GlobalData) const ITK_OVERRIDE
  { delete (GlobalDataStruct *)GlobalData; }

  virtual ScalarValueType ComputeCurvature(const NeighborhoodType &,
                                           const FloatOffsetType &, GlobalDataStruct *gd);

  /** \brief Laplacian smoothing speed can be used to spatially modify the
    effects of laplacian smoothing of the level set function */
  virtual ScalarValueType LaplacianSmoothingSpeed(
    const NeighborhoodType &,
    const FloatOffsetType &, GlobalDataStruct * = 0) const
  { return NumericTraits< ScalarValueType >::OneValue(); }

  /** \brief Curvature speed can be used to spatially modify the effects of
    curvature . The default implementation returns one. */
  virtual ScalarValueType CurvatureSpeed(const NeighborhoodType &,
                                         const FloatOffsetType &, GlobalDataStruct * = 0
                                         ) const
  { return NumericTraits< ScalarValueType >::OneValue(); }

  /** This method must be defined in a subclass to implement a working function
   * object.  This method is called before the solver begins its work to
   * produce the speed image used as the level set function's Advection field
   * term.  See LevelSetFunction for more information. */
  virtual void CalculateAdvectionImage() {}

protected:

  RegionBasedLevelSetFunction();
  virtual ~RegionBasedLevelSetFunction() {}

  /** The initial level set image */
  InputImageConstPointer m_InitialImage;

  /** The feature image */
  FeatureImageConstPointer m_FeatureImage;

  SharedDataPointer m_SharedData;

  HeavisideFunctionConstPointer m_DomainFunction;

  /** Area regularization weight */
  ScalarValueType m_AreaWeight;

  /** Internal functional of the level set weight */
  ScalarValueType m_Lambda1;

  /** External functional of the level set weight */
  ScalarValueType m_Lambda2;

  /** Overlap Penalty Weight */
  ScalarValueType m_OverlapPenaltyWeight;

  /** Volume Regularization Weight */
  ScalarValueType m_VolumeMatchingWeight;

  /** Volume Constraint in pixels */
  ScalarValueType m_Volume;

  /** Curvature Regularization Weight */
  ScalarValueType m_CurvatureWeight;

  ScalarValueType m_AdvectionWeight;

  /** Laplacian Regularization Weight */
  ScalarValueType m_ReinitializationSmoothingWeight;

  unsigned int m_FunctionId;

  std::slice x_slice[itkGetStaticConstMacro(ImageDimension)];
  OffsetValueType m_Center;
  OffsetValueType m_xStride[itkGetStaticConstMacro(ImageDimension)];
  double m_InvSpacing[itkGetStaticConstMacro(ImageDimension)];

  static double m_WaveDT;
  static double m_DT;

  void ComputeHImage();

  /** \brief Compute the global term as a combination of the internal, external,
    overlapping and volume regularization terms.  */
  ScalarValueType ComputeGlobalTerm(
    const ScalarValueType & imagePixel,
    const InputIndexType & inputIndex);

  /** \brief Compute the internal term
  \param[in] iValue Feature Image Value
  \param[in] iIdx Feature Image Index
  */
  virtual ScalarValueType ComputeInternalTerm(const FeaturePixelType & iValue,
                                              const FeatureIndexType & iIdx) = 0;

  /** \brief Compute the external term
  \param[in] iValue Feature Image Value
  \param[in] iIdx Feature Image Index */
  virtual ScalarValueType ComputeExternalTerm(const FeaturePixelType & iValue,
                                              const FeatureIndexType & iIdx) = 0;

  /** \brief Compute the overlap term
  \param[in] featIndex
  \param[out] pr = \f$ \prod_{i \neq j} H(\phi_i)\f$
  \return OverlapTerm = \f$ \sum_{i \neq j} H(\phi_i)\f$ */
  virtual ScalarValueType ComputeOverlapParameters(const FeatureIndexType & featIndex,
                                                   ScalarValueType & pr) = 0;

  /** \brief Compute the overlap term
      \return \f$ \int_{p \in \Omega} H(\phi_i) dp - this->Volume \f$
      \note the volume regularization does not depend on the spacing.
        So the volume must be set in number of pixels (not in real world unit). */
  ScalarValueType ComputeVolumeRegularizationTerm();

  /** \brief Compute the laplacian term
      \return \f$ \Delta \phi - \div(\frac{\nabla \phi}{|\nabla \phi|}) \f$
      For details see

      \par REFERENCE
      Li, C.M. and Xu, C.Y. and Gui, C. and Fox, M.D.
      "Level Set Evolution without Re-Initialization: A New Variational Formulation",
      CVPR05. 2005. pp. 430-436.
  */

  /** \brief Compute the laplacian
  \return \f$ \Delta \phi \f$ */
  ScalarValueType ComputeLaplacian(GlobalDataStruct *gd);

  /** \brief Compute Hessian Matrix */
  void ComputeHessian(const NeighborhoodType & it,
                      GlobalDataStruct *globalData);

  /** \brief Compute Parameters for the inner and outer parts. */
  virtual void ComputeParameters() = 0;

  /** \brief Update and save the inner and outer parameters in the shared data
    structure. */
  virtual void UpdateSharedDataParameters() = 0;

  bool m_UpdateC;

  /** This method's only purpose is to initialize the zero vector
   * constant. */
  static VectorType InitializeZeroVectorConstant();

  /** Zero vector constant. */
  static VectorType m_ZeroVectorConstant;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegionBasedLevelSetFunction);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionBasedLevelSetFunction.hxx"
#endif

#endif
