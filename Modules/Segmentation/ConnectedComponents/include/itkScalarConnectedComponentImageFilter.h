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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkScalarConnectedComponentImageFilter_h
#define itkScalarConnectedComponentImageFilter_h

#include "itkMath.h"
#include "itkNumericTraits.h"
#include "itkConnectedComponentFunctorImageFilter.h"

namespace itk
{
/**
 *\class ScalarConnectedComponentImageFilter
 *
 *  \brief A connected components filter that labels the
 *         objects in an arbitrary image.  Two pixels are
 *         similar if they are within threshold of each other.
 *         Uses ConnectedComponentFunctorImageFilter.
 * \ingroup ITKConnectedComponents
 *
 * \sphinx
 * \sphinxexample{Segmentation/ConnectedComponents/LabelConnectComponentsInGrayscaleImage,Label Connect Components In
 * Grayscale Image} \endsphinx
 */

namespace Functor
{
template <typename TInput>
class SimilarPixelsFunctor
{
public:
  SimilarPixelsFunctor() { m_Threshold = NumericTraits<TInput>::ZeroValue(); }

  ~SimilarPixelsFunctor() = default;

  bool
  operator!=(const SimilarPixelsFunctor & other) const
  {
    if (m_Threshold != other.m_Threshold)
    {
      return true;
    }
    return false;
  }

  bool
  operator==(const SimilarPixelsFunctor & other) const
  {
    return !(*this != other);
  }

  void
  SetDistanceThreshold(const TInput & thresh)
  {
    m_Threshold = thresh;
  }

  TInput
  GetDistanceThreshold()
  {
    return (m_Threshold);
  }

  bool
  operator()(const TInput & a, const TInput & b) const
  {
    using InputRealType = typename NumericTraits<TInput>::RealType;
    auto absDifference =
      static_cast<TInput>(itk::Math::abs(static_cast<InputRealType>(a) - static_cast<InputRealType>(b)));
    if (absDifference <= m_Threshold)
    {
      return true;
    }
    else
    {
      return false;
    }
  }

protected:
  TInput m_Threshold;
};
} // end namespace Functor

template <typename TInputImage, typename TOutputImage, typename TMaskImage = TInputImage>
class ScalarConnectedComponentImageFilter
  : public ConnectedComponentFunctorImageFilter<TInputImage,
                                                TOutputImage,
                                                Functor::SimilarPixelsFunctor<typename TInputImage::ValueType>,
                                                TMaskImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarConnectedComponentImageFilter);

  /** Standard class type aliases. */
  using Self = ScalarConnectedComponentImageFilter;
  using Superclass =
    ConnectedComponentFunctorImageFilter<TInputImage,
                                         TOutputImage,
                                         Functor::SimilarPixelsFunctor<typename TInputImage::ValueType>,
                                         TMaskImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarConnectedComponentImageFilter, ConnectedComponentFunctorImageFilter);

  using InputPixelType = typename TInputImage::PixelType;

  virtual void
  SetDistanceThreshold(const InputPixelType & thresh)
  {
    this->GetFunctor().SetDistanceThreshold(thresh);
  }

  virtual InputPixelType
  GetDistanceThreshold()
  {
    return (this->GetFunctor().GetDistanceThreshold());
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputEqualityComparableCheck, (Concept::EqualityComparable<InputPixelType>));
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<typename TOutputImage::PixelType>));
  itkConceptMacro(MaskEqualityComparableCheck, (Concept::EqualityComparable<typename TMaskImage::PixelType>));
  itkConceptMacro(OutputIncrementDecrementOperatorsCheck,
                  (Concept::IncrementDecrementOperators<typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  ScalarConnectedComponentImageFilter() = default;
  ~ScalarConnectedComponentImageFilter() override = default;
};
} // end namespace itk

#endif
