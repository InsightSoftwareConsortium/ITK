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

#ifndef itkDescoteauxEigenToScalarImageFilter_h
#define itkDescoteauxEigenToScalarImageFilter_h

#include "itkEigenToScalarImageFilter.h"
#include "itkDescoteauxEigenToScalarParameterEstimationImageFilter.h"
#include "itkDescoteauxEigenToScalarFunctorImageFilter.h"

namespace itk
{
/** \class DescoteauxEigenToScalarImageFilter
 * \brief Compute the Descoteaux sheetness measure from the eigenvalues
 *
 * This is a convenience class implementing the EigenToScalarImageFilter
 * abstract class. This class computes the Descoteaux sheetness measure for
 * cortical bone. Internally, DescoteauxEigenToScalarParameterEstimationImageFilter
 * and DescoteauxEigenToScalarFunctorImageFilter are used for automatic parameter
 * estimation and implementation of the functor.
 * 
 * Before passing an input to MultiScaleHessianEnhancementImageFilter,
 * the DescoteauxEigenToScalarPreprocessingImageToImageFilter should be used.
 * 
 * \sa MultiScaleHessianEnhancementImageFilter
 * \sa EigenToScalarImageFilter
 * \sa DescoteauxEigenToScalarParameterEstimationImageFilter
 * \sa DescoteauxEigenToScalarFunctorImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage, typename TMaskImage = Image< unsigned char, TInputImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT DescoteauxEigenToScalarImageFilter:
public EigenToScalarImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef DescoteauxEigenToScalarImageFilter                    Self;
  typedef EigenToScalarImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DescoteauxEigenToScalarImageFilter, EigenToScalarImageFilter);

  /** Useful template typedefs. */
  typedef typename TInputImage::Pointer       InputImagePointer;
  typedef typename TInputImage::ConstPointer  InputImageConstPointer;
  typedef typename TMaskImage::Pointer        MaskImagePointer;
  typedef typename TMaskImage::ConstPointer   MaskImageConstPointer;
  typedef typename TMaskImage::PixelType      MaskPixelType;

  /** Procesing filters */
  typedef DescoteauxEigenToScalarParameterEstimationImageFilter< TInputImage, TMaskImage >
            ParameterEstimationFilterType;
  typedef DescoteauxEigenToScalarFunctorImageFilter< TInputImage, TOutputImage >
            UnaryFunctorFilterType;

  /** Explicitely state the eigenvalues are ordered by magnitude for this filter */
  typename Superclass::EigenValueOrderType GetEigenValueOrder() const override
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

  /** Methods to set/get the FrobeniusNormWeight */
  virtual void SetFrobeniusNormWeight(const typename ParameterEstimationFilterType::RealType weight)
  {
    this->m_ParameterEstimationFilter->SetFrobeniusNormWeight(weight);
    this->Modified();
  }
  typename ParameterEstimationFilterType::RealType GetFrobeniusNormWeight() const
  {
    return this->m_ParameterEstimationFilter->GetFrobeniusNormWeight();
  }

  /** Methods to get the computed parameters */
  typename UnaryFunctorFilterType::RealType GetAlpha() const {
    return m_UnaryFunctorFilter->GetAlpha();
  }
  typename UnaryFunctorFilterType::RealType GetBeta() const {
    return m_UnaryFunctorFilter->GetBeta();
  }
  typename UnaryFunctorFilterType::RealType GetC() const {
    return m_UnaryFunctorFilter->GetC();
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
  DescoteauxEigenToScalarImageFilter();
  virtual ~DescoteauxEigenToScalarImageFilter() {}

  /** Override since the filter needs all the data for the algorithm */
  void GenerateInputRequestedRegion() override;

  /** Override since the filter produces all of its output */
  void EnlargeOutputRequestedRegion(DataObject *data) override;

  /** Single threaded since we are connecting data */
  void GenerateData() override;

  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DescoteauxEigenToScalarImageFilter);

  /* Filter pipeline */
  typename ParameterEstimationFilterType::Pointer m_ParameterEstimationFilter;
  typename UnaryFunctorFilterType::Pointer        m_UnaryFunctorFilter;
}; //end class
} // end namespace 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDescoteauxEigenToScalarImageFilter.hxx"
#endif

#endif // itkDescoteauxEigenToScalarImageFilter_h
