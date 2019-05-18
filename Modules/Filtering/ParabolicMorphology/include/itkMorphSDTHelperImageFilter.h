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
#ifndef itkMorphSDTHelperImageFilter_h
#define itkMorphSDTHelperImageFilter_h

#include "itkTernaryFunctorImageFilter.h"
#include "itkMath.h"

namespace itk
{
/** \class MorphSDTHelperImageFilter
 * \brief Implements a pixel-wise operator to form a signed distance transform.
 *
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * \ingroup ParabolicMorphology
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Function
{
template <typename TInput1, typename TInput2 = TInput1, typename TInput3 = TInput1, typename TOutput = TInput1>
class MorphSDTHelper
{
public:
  MorphSDTHelper() {}
  ~MorphSDTHelper() {}
  void
  SetVal(double i)
  {
    m_Val = i;
  }
  bool
  operator!=(const MorphSDTHelper &) const
  {
    return false;
  }

  bool
  operator==(const MorphSDTHelper & other) const
  {
    return !(*this != other);
  }

  inline TOutput
  operator()(const TInput1 & A, const TInput2 & B, const TInput3 & C)
  {
    // A should be the output of the erosion, B the dilation, C the mask
    if (C > 0)
    {
      // inside the mask
      return static_cast<TOutput>(std::sqrt((double)A + m_Val));
    }
    else
    {
      // outside the mask
      return static_cast<TOutput>(-std::sqrt(m_Val - (double)B));
    }
  }

private:
  double m_Val;
};
} // namespace Function

template <typename TInputImage1,
          typename TInputImage2 = TInputImage1,
          typename TInputImage3 = TInputImage1,
          typename TOutputImage = TInputImage1>
class ITK_EXPORT MorphSDTHelperImageFilter
  : public TernaryFunctorImageFilter<TInputImage1,
                                     TInputImage2,
                                     TInputImage3,
                                     TOutputImage,
                                     Function::MorphSDTHelper<typename TInputImage1::PixelType,
                                                              typename TInputImage2::PixelType,
                                                              typename TInputImage3::PixelType,
                                                              typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MorphSDTHelperImageFilter);

  /** Standard class type alias. */
  using Self = MorphSDTHelperImageFilter;
  using Superclass = TernaryFunctorImageFilter<TInputImage1,
                                               TInputImage2,
                                               TInputImage3,
                                               TOutputImage,
                                               Function::MorphSDTHelper<typename TInputImage1::PixelType,
                                                                        typename TInputImage2::PixelType,
                                                                        typename TInputImage3::PixelType,
                                                                        typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MorphSDTHelperImageFilter, TernaryFunctorImageFilter);

  void
  SetVal(double val)
  {
    this->GetFunctor().SetVal(val);
    this->Modified();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(Input1ConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage1::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(Input2ConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage2::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(GreaterThanComparable,
                  (Concept::GreaterThanComparable<typename TInputImage3::PixelType, typename TInputImage3::PixelType>));
  /** End concept checking */
#endif
protected:
  MorphSDTHelperImageFilter() {}
  virtual ~MorphSDTHelperImageFilter() {}
};
} // end namespace itk

#endif
