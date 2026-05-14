/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkRieszFrequencyFilterBankGenerator_h
#define itkRieszFrequencyFilterBankGenerator_h

#include <itkImageRegionIterator.h>
#include <itkGenerateImageSource.h>
#include <complex>
#include "itkRieszFrequencyFunction.h"
#include <itkFrequencyFFTLayoutImageRegionIteratorWithIndex.h>

namespace itk
{
/** \class RieszFrequencyFilterBankGenerator
 * Generate a filter bank of M components using the RieszFrequencyFunction.
 * \f$ M = p(N,d) \f$ where N = Order of the RieszTransform, and d = ImageDimension.
 *
 * \f$ M := p(N,d) = \frac{(N+d-1)!}{(d-1)! N!} \f$
 *
 * \sa RieszFrequencyFunction
 *
 * \ingroup IsotropicWavelets
 */
template <typename TOutputImage,
          typename TRieszFunction = itk::RieszFrequencyFunction<std::complex<double>, TOutputImage::ImageDimension>,
          typename TFrequencyRegionIterator = FrequencyFFTLayoutImageRegionIteratorWithIndex<TOutputImage>>
class RieszFrequencyFilterBankGenerator : public itk::GenerateImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RieszFrequencyFilterBankGenerator);

  /** Standard type alias */
  using Self = RieszFrequencyFilterBankGenerator;
  using Superclass = itk::GenerateImageSource<TOutputImage>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Type macro */
  itkNewMacro(Self);

  /** Creation through object factory macro */
  itkOverrideGetNameOfClassMacro(RieszFrequencyFilterBankGenerator);

  /** Inherit types from Superclass. */
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  /** Basic type alias */
  using OutputRegionIterator = TFrequencyRegionIterator;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  /** RieszFunction types */
  using RieszFunctionType = TRieszFunction;
  using RieszFunctionPointer = typename RieszFunctionType::Pointer;
  using FunctionValueType = typename RieszFunctionType::FunctionValueType;

  using OutputsType = typename std::vector<OutputImagePointer>;
  // using OutputsType = typename itk::VectorContainer<int, OutputImagePointer>;

  /** Dimension */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Get Outputs *****/
  /** Return vector of images from all directions */
  OutputsType
  GetOutputs();

  // #ifdef ITK_USE_CONCEPT_CHECKING
  //   itkConceptMacro( OutputPixelTypeIsFloatCheck,
  //                    ( Concept::IsFloatingPoint< typename OutputImageType::PixelType > ) );
  // #endif

  /** Order of the generalized riesz transform. */
  virtual void
  SetOrder(const unsigned int inputOrder)
  {
    // Precondition
    if (inputOrder < 1)
    {
      itkExceptionMacro(<< "Error: inputOrder = " << inputOrder << ". It has to be greater than 0.");
    }

    if (this->m_Order != inputOrder)
    {
      this->m_Order = inputOrder;
      this->m_Evaluator->SetOrder(inputOrder);

      this->SetNumberOfRequiredOutputs(this->m_Evaluator->ComputeNumberOfComponents(inputOrder));
      for (unsigned int comp = 0; comp < this->GetNumberOfRequiredOutputs(); ++comp)
      {
        this->SetNthOutput(comp, this->MakeOutput(comp));
      }
      this->Modified();
    }
  }
  itkGetConstReferenceMacro(Order, unsigned int);

  /** Modifiable pointer to the Generalized RieszFunction */
  itkGetModifiableObjectMacro(Evaluator, RieszFunctionType);

protected:
  RieszFrequencyFilterBankGenerator();
  ~RieszFrequencyFilterBankGenerator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate data */
  void
  BeforeThreadedGenerateData() override;
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & threadRegion) override;

private:
  unsigned int         m_Order{ 0 };
  RieszFunctionPointer m_Evaluator;
}; // end of class
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRieszFrequencyFilterBankGenerator.hxx"
#endif

#endif
