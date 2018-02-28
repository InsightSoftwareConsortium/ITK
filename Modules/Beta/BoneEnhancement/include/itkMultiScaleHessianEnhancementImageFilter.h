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
#include "itkImageMaskSpatialObject.h"
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
class ITK_TEMPLATE_EXPORT MultiScaleHessianEnhancementImageFilter:
public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef MultiScaleHessianEnhancementImageFilter         Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MultiScaleHessianEnhancementImageFilter, ImageToImageFilter);

  /** Input Image typedefs. */
  typedef TInputImage                             InputImageType;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType;
  typedef typename InputImageType::PixelType      InputImagePixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,  TInputImage::ImageDimension);

  /** Output image typedefs. */
  typedef TOutputImage                            OutputImageType;
  typedef typename OutputImageType::Pointer       OutputImagePointer;
  typedef typename OutputImageType::ConstPointer  OutputImageConstPointer;
  typedef typename OutputImageType::RegionType    OutputImageRegionType;
  typedef typename OutputImageType::PixelType     OutputImagePixelType;

  /** Mask related typedefs. */
  typedef ImageMaskSpatialObject< ImageDimension >  SpatialObjectType;
  typedef typename SpatialObjectType::ConstPointer  SpatialObjectConstPointer;

  /** Methods to set/get the mask image */
  itkSetInputMacro(ImageMask, SpatialObjectType);
  itkGetInputMacro(ImageMask, SpatialObjectType);

  /** Hessian related typedefs. */
  // typedef HessianRecursiveGaussianImageFilter< TInputImage >  HessianFilterType;
  typedef HessianGaussianImageFilter< TInputImage >           HessianFilterType;
  typedef typename HessianFilterType::OutputImageType         HessianImageType;
  typedef typename HessianImageType::PixelType                HessianPixelType;
  typedef typename HessianFilterType::InternalRealType        InternalRealType;

  /** Eigenvalue analysis related typedefs. The ITK python wrapping usually wraps floating types
   * and not double types. For this reason, the eigenvalues are of type float.
   */
  typedef typename NumericTraits< InputImagePixelType >::RealType                     RealType;
  typedef typename NumericTraits< InputImagePixelType >::FloatType                    FloatType;
  typedef Vector< FloatType, HessianPixelType::Dimension >                            EigenValueArrayType;
  typedef Image< EigenValueArrayType, TInputImage::ImageDimension >                   EigenValueImageType;
  typedef SymmetricEigenAnalysisImageFilter< HessianImageType, EigenValueImageType >  EigenAnalysisFilterType;

  /** Maximum over scale related typedefs. */
  typedef MaximumAbsoluteValueImageFilter< TOutputImage > MaximumAbsoluteValueFilterType;

  /** Eigenvalue image to measure image related typedefs */
  typedef EigenToMeasureImageFilter< EigenValueImageType, TOutputImage, SpatialObjectType > EigenToMeasureImageFilterType;
  typedef EigenToMeasureParameterEstimationFilter< EigenValueImageType, SpatialObjectType > EigenToMeasureParameterEstimationFilterType;
  
  /** Need some types to determine how to order the eigenvalues */
  typedef typename EigenAnalysisFilterType::FunctorType::EigenValueOrderType  InternalEigenValueOrderType;
  typedef typename EigenToMeasureImageFilterType::EigenValueOrderType         ExternalEigenValueOrderType;

  /** Set/Get the EigenToMeasureImageFilter. */
  itkSetObjectMacro(EigenToMeasureImageFilter, EigenToMeasureImageFilterType);
  itkGetModifiableObjectMacro(EigenToMeasureImageFilter, EigenToMeasureImageFilterType);

  /** Set/Get the EigenToMeasureParameterEstimationFilter. */
  itkSetObjectMacro(EigenToMeasureParameterEstimationFilter, EigenToMeasureParameterEstimationFilterType);
  itkGetModifiableObjectMacro(EigenToMeasureParameterEstimationFilter, EigenToMeasureParameterEstimationFilterType);

  /** Sigma values. */
  typedef RealType            SigmaType;
  typedef Array< SigmaType >  SigmaArrayType;
  typedef unsigned int        SigmaStepsType;
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
  static Self::SigmaArrayType GenerateSigmaArray(SigmaType SigmaMinimum, SigmaType SigmaMaximum, SigmaStepsType NumberOfSigmaSteps, SigmaStepMethodEnum SigmaStepMethod);
  static Self::SigmaArrayType GenerateEquispacedSigmaArray(SigmaType SigmaMinimum, SigmaType SigmaMaximum, SigmaStepsType NumberOfSigmaSteps);
  static Self::SigmaArrayType GenerateLogarithmicSigmaArray(SigmaType SigmaMinimum, SigmaType SigmaMaximum, SigmaStepsType NumberOfSigmaSteps);

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
  void GenerateData() ITK_OVERRIDE;

  /** Internal function to generate the response at a scale */
  inline typename TOutputImage::Pointer generateResponseAtScale(SigmaStepsType scaleLevel);

  /** Internal function to convert types for EigenValueOrder */
  InternalEigenValueOrderType ConvertType(ExternalEigenValueOrderType order);

  /** Override since the filter needs all the data for the algorithm */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  OutputImageRegionType GetOutputRegion();

  /** Override since the filter produces all of its output */
  void EnlargeOutputRequestedRegion(DataObject *data) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  /** Internal filters. */
  typename HessianFilterType::Pointer                           m_HessianFilter;
  typename EigenAnalysisFilterType::Pointer                     m_EigenAnalysisFilter;
  typename MaximumAbsoluteValueFilterType::Pointer              m_MaximumAbsoluteValueFilter;
  typename EigenToMeasureImageFilterType::Pointer               m_EigenToMeasureImageFilter;
  typename EigenToMeasureParameterEstimationFilterType::Pointer m_EigenToMeasureParameterEstimationFilter;

  /** Sigma member variables. */
  SigmaArrayType  m_SigmaArray;

  ITK_DISALLOW_COPY_AND_ASSIGN(MultiScaleHessianEnhancementImageFilter);
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiScaleHessianEnhancementImageFilter.hxx"
#endif

#endif // itkMultiScaleHessianEnhancementImageFilter_h
