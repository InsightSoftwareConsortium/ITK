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
#ifndef itkMultiScaleHessianEnhancementImageFilter_h
#define itkMultiScaleHessianEnhancementImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkHessianGaussianImageFilter.h"
#include "itkSymmetricEigenAnalysisImageFilter.h"
#include "itkMaximumAbsoluteValueImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"
#include "itkSpatialObject.h"
#include "itkEigenToMeasureImageFilter.h"
#include "itkEigenToMeasureParameterEstimationFilter.h"

namespace itk
{
/** \class MultiScaleHessianEnhancementImageFilter
 * \brief Compute an image enhancement using eigenvalues of the local hessian matrix over many scales.
 *
 * This class enhances an image using many of the bone image enhancement filters. Other filters based
 * on a functional of the eigenvalues can be written using this class by extending EigenToMeasureImageFilter.
 * This class works by computing the second derivative and cross derivatives usign HessianRecursiveGaussianImageFilter.
 * The hessian matrix is decomposed into the eigenvalues using SymmetricEigenAnalysisImageFilter. By setting a filter
 * using SetEigenToMeasureImageFilter( ), a filter is used to convert eigenvalues back into a scalar values. This is
 * repeated at multiple scales and the maximum response (in an absolute sense) is taken over all scales.
 * 
 * To enhance the bone image, call SetEigenToMeasureImageFilter( ) with an appropriate class derived from the
 * EigenToMeasureImageFilter. This filter should be constructed outside of this class. You will also need
 * to set the number of scales by which the image is enhanced using SetSigmaArray( ). Convenient static methods
 * GenerateSigmaArray( ), GenerateEquispacedSigmaArray( ), and GenerateLogarithmicSigmaArray( ) can be used to
 * generate naturally spaced sigma values. Note that you still need to pass the array to SetSigmaArray( ). 
 * Otherwise, an explicit SigmaArrayType can be passed to SetSigmaArray( ).
 * 
 * The maximum response from SetEigenToMeasureImageFilter( ) is taken over all sigma values using
 * MaximumAbsoluteValueImageFilter. This is valid for filters which enhance both the positive and negative
 * second derivatives.
 * 
 * This class is heavily derived from \see MultiScaleHessianBasedMeasureImageFilter
 * 
 * \sa MaximumAbsoluteValueImageFilter
 * \sa EigenToMeasureImageFilter
 * \sa SymmetricEigenAnalysisImageFilter
 * \sa HessianRecursiveGaussianImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT MultiScaleHessianEnhancementImageFilter
  : public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiScaleHessianEnhancementImageFilter);

  /** Standard Self type alias */
  using Self          = MultiScaleHessianEnhancementImageFilter;
  using Superclass    = ImageToImageFilter< TInputImage, TOutputImage >;
  using Pointer       = SmartPointer< Self >;
  using ConstPointer  = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MultiScaleHessianEnhancementImageFilter, ImageToImageFilter);

  /** Input Image typedefs. */
  using InputImageType          = TInputImage;
  using InputImagePointer       = typename InputImageType::Pointer;
  using InputImageConstPointer  = typename InputImageType::ConstPointer;
  using InputImageRegionType    = typename InputImageType::RegionType;
  using InputImagePixelType     = typename InputImageType::PixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,  TInputImage::ImageDimension);

  /** Output image typedefs. */
  using OutputImageType         = TOutputImage;
  using OutputImagePointer      = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType   = typename OutputImageType::RegionType;
  using OutputImagePixelType    = typename OutputImageType::PixelType;

  /** Mask related typedefs. */
  using MaskSpatialObjectType             = SpatialObject< ImageDimension >;
  using MaskSpatialObjectTypeConstPointer = typename MaskSpatialObjectType::ConstPointer;

  /** Methods to set/get the mask image */
  itkSetInputMacro(ImageMask, MaskSpatialObjectType);
  itkGetInputMacro(ImageMask, MaskSpatialObjectType);

  /** Hessian related typedefs. */
  // using HessianFilterType = HessianRecursiveGaussianImageFilter< TInputImage >;
  using HessianFilterType = HessianGaussianImageFilter< TInputImage >;
  using HessianImageType  = typename HessianFilterType::OutputImageType;
  using HessianPixelType  = typename HessianImageType::PixelType;
  using InternalRealType  = typename HessianFilterType::InternalRealType;

  /** Eigenvalue analysis related type alias. The ITK python wrapping usually wraps floating types
   * and not double types. For this reason, the eigenvalues are of type float.
   */
  using RealType                = typename NumericTraits< InputImagePixelType >::RealType;
  using FloatType               = typename NumericTraits< InputImagePixelType >::FloatType;
  using EigenValueArrayType     = Vector< FloatType, HessianPixelType::Dimension >;
  using EigenValueImageType     = Image< EigenValueArrayType, TInputImage::ImageDimension >;
  using EigenAnalysisFilterType = SymmetricEigenAnalysisImageFilter< HessianImageType, EigenValueImageType >;

  /** Maximum over scale related type alias. */
  using MaximumAbsoluteValueFilterType = MaximumAbsoluteValueImageFilter< TOutputImage >;

  /** Eigenvalue image to measure image related typedefs */
  using EigenToMeasureImageFilterType               = EigenToMeasureImageFilter< EigenValueImageType, TOutputImage >;
  using EigenToMeasureParameterEstimationFilterType = EigenToMeasureParameterEstimationFilter< EigenValueImageType >;
  
  /** Need some types to determine how to order the eigenvalues */
  using InternalEigenValueOrderType = typename EigenAnalysisFilterType::FunctorType::EigenValueOrderType;
  using ExternalEigenValueOrderType = typename EigenToMeasureImageFilterType::EigenValueOrderType;

  /** Set/Get the EigenToMeasureImageFilter. */
  itkSetObjectMacro(EigenToMeasureImageFilter, EigenToMeasureImageFilterType);
  itkGetModifiableObjectMacro(EigenToMeasureImageFilter, EigenToMeasureImageFilterType);

  /** Set/Get the EigenToMeasureParameterEstimationFilter. */
  itkSetObjectMacro(EigenToMeasureParameterEstimationFilter, EigenToMeasureParameterEstimationFilterType);
  itkGetModifiableObjectMacro(EigenToMeasureParameterEstimationFilter, EigenToMeasureParameterEstimationFilterType);

  /** Sigma values. */
  using SigmaType       = RealType;
  using SigmaArrayType  = Array< SigmaType >;
  using SigmaStepsType  = unsigned int;
  typedef enum {
    EquispacedSigmaSteps = 0,
    LogarithmicSigmaSteps = 1
  } SigmaStepMethodEnum;

  /** Set/Get macros for SigmaArray */
  itkSetMacro(SigmaArray, SigmaArrayType);
  itkGetConstMacro(SigmaArray, SigmaArrayType);

  /**
   * Static methods for generating an array of sigma values. Note that these still need to be passed
   * into the class using SetSigmaArray. Implementation taken from itkMultiScaleHessianBasedMeasureImageFilter.
   * Note that these methods cannot throw exceptions according to the standard itkExceptionMacro since they are static
   * methods. Instead, they will return an empty sigma array on error. TODO: Not ture any more.
   */
  static SigmaArrayType GenerateSigmaArray(SigmaType SigmaMinimum, SigmaType SigmaMaximum, SigmaStepsType NumberOfSigmaSteps, SigmaStepMethodEnum SigmaStepMethod);
  static SigmaArrayType GenerateEquispacedSigmaArray(SigmaType SigmaMinimum, SigmaType SigmaMaximum, SigmaStepsType NumberOfSigmaSteps);
  static SigmaArrayType GenerateLogarithmicSigmaArray(SigmaType SigmaMinimum, SigmaType SigmaMaximum, SigmaStepsType NumberOfSigmaSteps);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputOutputHaveSamePixelDimensionCheck,
                   ( Concept::SameDimension< TInputImage::ImageDimension, TOutputImage::ImageDimension >) );
  // End concept checking
#endif
protected:
  MultiScaleHessianEnhancementImageFilter();
  virtual ~MultiScaleHessianEnhancementImageFilter() {}

  /** Single threaded since we are connecting data */
  void GenerateData() override;

  /** Internal function to generate the response at a scale */
  inline typename TOutputImage::Pointer generateResponseAtScale(SigmaStepsType scaleLevel);

  /** Internal function to convert types for EigenValueOrder */
  InternalEigenValueOrderType ConvertType(ExternalEigenValueOrderType order);

  /** Override since the filter needs all the data for the algorithm */
  void GenerateInputRequestedRegion() override;

  OutputImageRegionType GetOutputRegion();

  /** Override since the filter produces all of its output */
  void EnlargeOutputRequestedRegion(DataObject *data) override;

  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Internal filters. */
  typename HessianFilterType::Pointer                           m_HessianFilter;
  typename EigenAnalysisFilterType::Pointer                     m_EigenAnalysisFilter;
  typename MaximumAbsoluteValueFilterType::Pointer              m_MaximumAbsoluteValueFilter;
  typename EigenToMeasureImageFilterType::Pointer               m_EigenToMeasureImageFilter;
  typename EigenToMeasureParameterEstimationFilterType::Pointer m_EigenToMeasureParameterEstimationFilter;

  /** Sigma member variables. */
  SigmaArrayType  m_SigmaArray;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiScaleHessianEnhancementImageFilter.hxx"
#endif

#endif // itkMultiScaleHessianEnhancementImageFilter_h
