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
#include "itkHessianRecursiveGaussianImageFilter.h"
#include "itkSymmetricEigenAnalysisImageFilter.h"
#include "itkMaximumAbsoluteValueImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"
#include "itkEigenToScalarImageFilter.h"

namespace itk
{
/** \class MultiScaleHessianEnhancementImageFilter
 * \brief Compute an image enhancement using eigenvalues of the local hessian matrix over many scales.
 *
 * This class enhances an image using many of the bone image enhancement filters. Other filters based
 * on a functional of the eigenvalues can be written using this class by extending EigenToScalarImageFilter.
 * This class works by computing the second derivative and cross derivatives usign HessianRecursiveGaussianImageFilter.
 * The hessian matrix is decomposed into the eigenvalues using SymmetricEigenAnalysisImageFilter. By setting a filter
 * using SetEigenToScalarImageFilter( ), a filter is used to convert eigenvalues back into a scalar values. This is
 * repeated at multiple scales and the maximum response (in an absolute sense) is taken over all scales.
 * 
 * To enhance the bone image, call SetEigenToScalarImageFilter( ) with an appropriate class derived from the
 * EigenToScalarImageFilter. This filter should be constructed outside of this class. You will also need
 * to set the number of scales by which the image is enhanced using SetSigmaArray( ). Convenient static methods
 * GenerateSigmaArray( ), GenerateEquispacedSigmaArray( ), and GenerateLogarithmicSigmaArray( ) can be used to
 * generate naturally spaced sigma values. Note that you still need to pass the array to SetSigmaArray( ). 
 * Otherwise, an explicit SigmaArrayType can be passed to SetSigmaArray( ).
 * 
 * The maximum response from SetEigenToScalarImageFilter( ) is taken over all sigma values using
 * MaximumAbsoluteValueImageFilter. This is valid for filters which enhance both the positive and negative
 * second derivatives.
 * 
 * This class is heavily derived from \see MultiScaleHessianBasedMeasureImageFilter
 * 
 * \sa MaximumAbsoluteValueImageFilter
 * \sa EigenToScalarImageFilter
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
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiScaleHessianEnhancementImageFilter);

  /** Standard Self typedef */
  typedef MultiScaleHessianEnhancementImageFilter         Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MultiScaleHessianEnhancementImageFilter, ImageToImageFilter);

  /** Image related typedefs. */
  typedef typename TInputImage::PixelType  PixelType;
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Hessian related typedefs. */
  typedef HessianRecursiveGaussianImageFilter< TInputImage >  HessianFilterType;
  typedef typename HessianFilterType::OutputImageType         HessianImageType;
  typedef typename HessianImageType::PixelType                HessianPixelType;

  /** Eigenvalue analysis related typedefs. The ITK python wrapping usually wraps floating types
   * and not double types. For this reason, the eigenvalues are of type float.
   */
  typedef typename NumericTraits< PixelType >::RealType                               RealType;
  typedef typename NumericTraits< PixelType >::FloatType                              FloatType;
  typedef Vector< FloatType, HessianPixelType::Dimension >                            EigenValueArrayType;
  typedef Image< EigenValueArrayType, TInputImage::ImageDimension >                   EigenValueImageType;
  typedef SymmetricEigenAnalysisImageFilter< HessianImageType, EigenValueImageType >  EigenAnalysisFilterType;

  /** Maximum over scale related typedefs. */
  typedef MaximumAbsoluteValueImageFilter< TOutputImage > MaximumAbsoluteValueFilterType;

  /** Eigenvalue image to scalar image related typedefs */
  typedef EigenToScalarImageFilter< EigenValueImageType, TOutputImage > EigenToScalarImageFilterType;

  /** Need some types to determine how to order the eigenvalues */
  // typedef typename Functor::SymmetricEigenAnalysisFunction::EigenValueOrderType InternalEigenValueOrderType;
  typedef typename EigenAnalysisFilterType::FunctorType::EigenValueOrderType InternalEigenValueOrderType;
  typedef typename EigenToScalarImageFilterType::EigenValueOrderType         ExternalEigenValueOrderType;

  /** Set/Get the EigenToScalarImageFilter. */
  itkSetObjectMacro(EigenToScalarImageFilter, EigenToScalarImageFilterType);
  itkGetModifiableObjectMacro(EigenToScalarImageFilter, EigenToScalarImageFilterType);

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
   * into the class using SetSigmaArray . Implementation taken from itkMultiScaleHessianBasedMeasureImageFilter.
   * Note that these methods cannot throw exceptions according to the standard itkExceptionMacro since they are static
   * methods. Instead, they will return an empty sigma array on error.
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

  /** Override since the filter produces all of its output */
  void EnlargeOutputRequestedRegion(DataObject *data) override;

  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Internal filters. */
  typename HessianFilterType::Pointer               m_HessianFilter;
  typename EigenAnalysisFilterType::Pointer         m_EigenAnalysisFilter;
  typename MaximumAbsoluteValueFilterType::Pointer  m_MaximumAbsoluteValueFilter;
  typename EigenToScalarImageFilterType::Pointer    m_EigenToScalarImageFilter;

  /** Sigma member variables. */
  SigmaArrayType  m_SigmaArray;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiScaleHessianEnhancementImageFilter.hxx"
#endif

#endif // itkMultiScaleHessianEnhancementImageFilter_h
