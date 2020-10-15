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
#ifndef itkChangeLabelImageFilter_h
#define itkChangeLabelImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkConceptChecking.h"
#include "itkSimpleDataObjectDecorator.h"

#include <map>

namespace itk
{
/**
 *\class ChangeLabelImageFilter
 *
 * \brief Change Sets of Labels
 *
 * This filter produces an output image whose pixels
 * are either copied from the input if they are not being changed
 * or are rewritten based on the change parameters
 *
 * This filter is templated over the input image type
 * and the output image type.
 *
 * The filter expect both images to have the same number of dimensions.
 *
 * \author Tim Kelliher. GE Research, Niskayuna, NY.
 * \note This work was supported by a grant from DARPA, executed by the
 *  U.S. Army Medical Research and Materiel Command/TATRC Assistance
 *  Agreement, Contract#W81XWH-05-2-0059.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageLabel
 */
namespace Functor
{
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT ChangeLabel
{
public:
  ChangeLabel() = default;
  ~ChangeLabel() = default;

  using ChangeMapType = std::map<TInput, TOutput>;

  bool
  operator!=(const ChangeLabel & other) const
  {
    if (m_ChangeMap != other.m_ChangeMap)
    {
      return true;
    }
    return false;
  }

  bool
  operator==(const ChangeLabel & other) const
  {
    return !(*this != other);
  }

  TOutput
  GetChange(const TInput & original)
  {
    return m_ChangeMap[original];
  }

  void
  SetChange(const TInput & original, const TOutput & result)
  {
    m_ChangeMap[original] = result;
  }

  void
  SetChangeMap(const ChangeMapType & changeMap)
  {
    m_ChangeMap = changeMap;
  }

  void
  ClearChangeMap()
  {
    m_ChangeMap.clear();
  }

  inline TOutput
  operator()(const TInput & A) const
  {
    const typename ChangeMapType::const_iterator it = m_ChangeMap.find(A);
    if (it != m_ChangeMap.end())
    {
      return it->second;
    }
    return A;
  }

private:
  ChangeMapType m_ChangeMap;
};
} // namespace Functor

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ChangeLabelImageFilter
  : public UnaryFunctorImageFilter<
      TInputImage,
      TOutputImage,
      Functor::ChangeLabel<typename TInputImage::PixelType, typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ChangeLabelImageFilter);

  /** Standard class type aliases. */
  using Self = ChangeLabelImageFilter;
  using Superclass =
    UnaryFunctorImageFilter<TInputImage,
                            TOutputImage,
                            Functor::ChangeLabel<typename TInputImage::PixelType, typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChangeLabelImageFilter, UnaryFunctorImageFilter);

  /** Pixel types. */
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Type of the change map to use for change requests */
  using ChangeMapType = std::map<InputPixelType, OutputPixelType>;

  /** Set up a change of a single label */
  void
  SetChange(const InputPixelType & original, const OutputPixelType & result);

  /** Set the entire change map */
  void
  SetChangeMap(const ChangeMapType & changeMap);

  /** Clears the entire change map */
  void
  ClearChangeMap();

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, OutputPixelType>));
  itkConceptMacro(PixelTypeComparable, (Concept::Comparable<InputPixelType>));
  // End concept checking
#endif

protected:
  ChangeLabelImageFilter() = default;
  ~ChangeLabelImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkChangeLabelImageFilter.hxx"
#endif

#endif
