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

#ifndef itkKrcahEigenToScalarImageFilter_h
#define itkKrcahEigenToScalarImageFilter_h

#include "itkEigenToScalarImageFilter.h"
#include "itkKrcahEigenToScalarParameterEstimationImageFilter.h"
#include "itkKrcahEigenToScalarFunctorImageFilter.h"

namespace itk {
/** \class KrcahEigenToScalarImageFilter
 * \brief Compute the Krcah sheetness measure from the eigenvalues
 *
 * This is a convenience class implementing the EigenToScalarImageFilter
 * abstract class. This class computes the Krcah sheetness measure for
 * cortical bone. Internally, KrcahEigenToScalarParameterEstimationImageFilter
 * and KrcahEigenToScalarFunctorImageFilter are used for automatic parameter
 * estimation and implementation of the functor.
 * 
 * Before passing an input to MultiScaleHessianEnhancementImageFilter,
 * the KrcahEigenToScalarPreprocessingImageToImageFilter should be used.
 * 
 * \sa MultiScaleHessianEnhancementImageFilter
 * \sa EigenToScalarImageFilter
 * \sa KrcahEigenToScalarParameterEstimationImageFilter
 * \sa KrcahEigenToScalarFunctorImageFilter
 * \sa KrcahEigenToScalarPreprocessingImageToImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage, typename TMaskImage = Image< unsigned char, TInputImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT KrcahEigenToScalarImageFilter:
public EigenToScalarImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef KrcahEigenToScalarImageFilter                         Self;
  typedef EigenToScalarImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KrcahEigenToScalarImageFilter, EigenToScalarImageFilter);

  /** Mask related typedefs. */
  typedef typename TMaskImage::PixelType    MaskPixelType;

  /** Procesing filters */
  typedef KrcahEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >   ParameterEstimationFilterType;
  typedef typename ParameterEstimationFilterType::KrcahImplementationType               KrcahImplementationType;
  typedef KrcahEigenToScalarFunctorImageFilter< TInputImage, TOutputImage >             UnaryFunctorFilterType;

  /** Explicitely state the eigenvalues are ordered by magnitude for this filter */
  typename Superclass::EigenValueOrderType GetEigenValueOrder() const ITK_OVERRIDE
  {
    return Superclass::OrderByMagnitude;
  }

  /** Methods to set/get the mask image */
  void SetMaskImage(const TMaskImage * mask)
  {
    this->m_ParameterEstimationFilter->SetMaskImage(mask);
    this->Modified();
  }
  virtual const TMaskImage * GetMaskImage() const
  {
    return this->m_ParameterEstimationFilter->GetMaskImage();
  }

  /** Methods to set/get the background value */
  virtual void SetBackgroundValue(const MaskPixelType back)
  {
    this->m_ParameterEstimationFilter->SetBackgroundValue(back);
    this->Modified();
  }
  virtual MaskPixelType GetBackgroundValue() const
  {
    return this->m_ParameterEstimationFilter->GetBackgroundValue();
  }

  /** Methods to set/get the parameter set type */
  virtual void SetParameterSet(const KrcahImplementationType back)
  {
    this->m_ParameterEstimationFilter->SetParameterSet(back);
    this->Modified();
  }
  virtual KrcahImplementationType GetParameterSet() const
  {
    return this->m_ParameterEstimationFilter->GetParameterSet();
  }
  virtual void SetParameterSetToImplementation(){
    this->m_ParameterEstimationFilter->SetParameterSetToImplementation();
    this->Modified();
  }
  virtual void SetParameterSetToJournalArticle(){
    this->m_ParameterEstimationFilter->SetParameterSetToJournalArticle();
    this->Modified();
  }

  /** Methods to get the computed parameters */
  typename UnaryFunctorFilterType::RealType GetAlpha() const {
    return m_UnaryFunctorFilter->GetAlpha();
  }
  typename UnaryFunctorFilterType::RealType GetBeta() const {
    return m_UnaryFunctorFilter->GetBeta();
  }
  typename UnaryFunctorFilterType::RealType GetGamma() const {
    return m_UnaryFunctorFilter->GetGamma();
  }

  /** Methods to set/get the enhancment method */
  void SetEnhanceBrightObjects()
  {
    this->m_UnaryFunctorFilter->SetEnhanceBrightObjects();
    this->Modified();
  }
  void SetEnhanceDarkObjects()
  {
    this->m_UnaryFunctorFilter->SetEnhanceDarkObjects();
    this->Modified();
  }
  typename UnaryFunctorFilterType::RealType GetEnhanceType() const
  {
    return this->m_UnaryFunctorFilter->GetEnhanceType();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHaveDimension3Check,
                   ( Concept::SameDimension< TInputImage::ImageDimension, 3u >) );
  itkConceptMacro( OutputHaveDimension3Check,
                   ( Concept::SameDimension< TOutputImage::ImageDimension, 3u >) );
  itkConceptMacro( InputFixedArrayHasDimension3Check,
                  ( Concept::SameDimension< TInputImage::PixelType::Dimension, 3u >) );
  // End concept checking
#endif
protected:
  KrcahEigenToScalarImageFilter();
  virtual ~KrcahEigenToScalarImageFilter() {}

  /** Override since the filter needs all the data for the algorithm */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Override since the filter produces all of its output */
  void EnlargeOutputRequestedRegion(DataObject *data) ITK_OVERRIDE;

  /** Single threaded since we are connecting data */
  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(KrcahEigenToScalarImageFilter);

  /* Filter pipeline */
  typename ParameterEstimationFilterType::Pointer m_ParameterEstimationFilter;
  typename UnaryFunctorFilterType::Pointer        m_UnaryFunctorFilter;
}; //end class
} // end namespace 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKrcahEigenToScalarImageFilter.hxx"
#endif

#endif // itkKrcahEigenToScalarImageFilter_h
