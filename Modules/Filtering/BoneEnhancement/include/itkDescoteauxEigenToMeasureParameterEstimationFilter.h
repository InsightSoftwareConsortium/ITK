/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkDescoteauxEigenToMeasureParameterEstimationFilter_h
#define itkDescoteauxEigenToMeasureParameterEstimationFilter_h

#include "itkMath.h"
#include "itkEigenToMeasureParameterEstimationFilter.h"
#include <mutex>

namespace itk
{
/** \class DescoteauxEigenToMeasureParameterEstimationFilter
 * \brief Automatic parameter estimation as defined by Descoteaux et al.
 *
 * The default parameters are:
 *   \f{eqnarray*}{
 *      \alpha &=& 0.5 \\
 *      \beta &=& 0.5 \\
 *      \gamma &=& 0.5 max\( Frobenius norm \)
 *   \f}
 *
 * Where the Frobenius norm for a real, symmetric matrix is given by
 * the square root of the sum of squares of the eigenvalues.
 *
 * The parameters are estimated over the whole volume unless a mask is given.
 * If a mask is given, parameters are evaluated only where IsInside returns
 * true.
 *
 * \sa DescoteauxEigenToMeasureImageFilter
 * \sa EigenToMeasureParameterEstimationFilter
 * \sa MultiScaleHessianEnhancementImageFilter
 *
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class DescoteauxEigenToMeasureParameterEstimationFilter
  : public EigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DescoteauxEigenToMeasureParameterEstimationFilter);

  /** Standard Self typedef */
  using Self = DescoteauxEigenToMeasureParameterEstimationFilter;
  using Superclass = EigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Input typedefs */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using InputImageRegionType = typename Superclass::InputImageRegionType;
  using InputImagePixelType = typename Superclass::InputImagePixelType;
  using PixelValueType = typename Superclass::PixelValueType;

  /** Output typedefs */
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;

  /** Input Mask typedefs. */
  using MaskSpatialObjectType = typename Superclass::MaskSpatialObjectType;
  using MaskSpatialObjectTypeConstPointer = typename Superclass::MaskSpatialObjectTypeConstPointer;

  /** Parameter typedefs */
  using RealType = typename Superclass::RealType;
  using ParameterArrayType = typename Superclass::ParameterArrayType;
  using ParameterDecoratedType = typename Superclass::ParameterDecoratedType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DescoteauxEigenToMeasureParameterEstimationFilter, EigenToMeasureParameterEstimationFilter);

  /** Setter/Getter methods for setting FrobeniusNormWeight */
  itkSetMacro(FrobeniusNormWeight, RealType);
  itkGetConstMacro(FrobeniusNormWeight, RealType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHaveDimension3Check, (Concept::SameDimension<TInputImage::ImageDimension, 3u>));
  itkConceptMacro(InputFixedArrayHasDimension3Check, (Concept::SameDimension<TInputImage::PixelType::Dimension, 3u>));
  // End concept checking
#endif
protected:
  DescoteauxEigenToMeasureParameterEstimationFilter();
  ~DescoteauxEigenToMeasureParameterEstimationFilter() override = default;

  /** Initialize some accumulators before the threads run. */
  void
  BeforeThreadedGenerateData() override;

  /** Do final mean and variance computation from data accumulated in threads. */
  void
  AfterThreadedGenerateData() override;

  /** Multi-thread version GenerateData. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  inline RealType
  CalculateFrobeniusNorm(const InputImagePixelType & pixel) const;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /* Member variables */
  RealType m_FrobeniusNormWeight;
  RealType m_MaxFrobeniusNorm;

  std::mutex m_Mutex;
}; // end class
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDescoteauxEigenToMeasureParameterEstimationFilter.hxx"
#endif

#endif /* itkDescoteauxEigenToMeasureParameterEstimationFilter_h */
