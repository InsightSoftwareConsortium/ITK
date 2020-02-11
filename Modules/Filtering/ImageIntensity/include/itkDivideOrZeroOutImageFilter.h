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
#ifndef itkDivideOrZeroOutImageFilter_h
#define itkDivideOrZeroOutImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include "itkArithmeticOpsFunctors.h"

namespace itk
{


/** \class DivideOrZeroOutImageFilter
 * \brief
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la
 * Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 * \ingroup ITKImageIntensity
 *
 */
template <typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class DivideOrZeroOutImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DivideOrZeroOutImageFilter);

  /** Standard class type aliases. */
  using Self = DivideOrZeroOutImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using FunctorType = Functor::DivideOrZeroOut<typename TInputImage1::PixelType,
                                               typename TInputImage2::PixelType,
                                               typename TOutputImage::PixelType>;

  using NumeratorPixelType = typename TInputImage1::PixelType;
  using DenominatorPixelType = typename TInputImage2::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DivideOrZeroOutImageFilter, BinaryGeneratorImageFilter);

  /** Print internal ivars */
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Threshold: " << GetThreshold() << std::endl;
  }

  /** Set/get the threshold below which pixels in the denominator will
   * be considered zero. */
  void
  SetThreshold(DenominatorPixelType threshold)
  {
    if (Math::NotExactlyEquals(threshold, this->GetFunctor().m_Threshold))
    {
      this->GetFunctor().m_Threshold = threshold;
      this->Modified();
    }
  }
  DenominatorPixelType
  GetThreshold() const
  {
    return this->GetFunctor().m_Threshold;
  }

  /** Set/get the constant value returned when the denominator input
   * value is considered zero. */
  void
  SetConstant(OutputPixelType constant)
  {
    if (Math::NotExactlyEquals(constant, this->GetFunctor().m_Constant))
    {
      this->GetFunctor().m_Constant = constant;
      this->Modified();
    }
  }
  OutputPixelType
  GetConstant() const
  {
    return this->GetFunctor().m_Constant;
  }

protected:
  DivideOrZeroOutImageFilter() = default;
  ~DivideOrZeroOutImageFilter() override = default;

  void
  BeforeThreadedGenerateData() override
  {
    this->SetFunctor(this->GetFunctor());
  }

private:
  itkGetConstReferenceMacro(Functor, FunctorType);
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }

  FunctorType m_Functor;
};

} // end namespace itk
#endif
