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
#ifndef itkMyFilter_h
#define itkMyFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class MyFilter
 *
 * \brief Filters a image by iterating over its pixels.
 *
 * Filters a image by iterating over its pixels in a multi-threaded way
 * and {to be completed by the developer}.
 *
 * \ingroup Fpfh
 *
 */
template <typename TInputPointSet, typename TOutputPointSet>
class MyFilter : public MeshToMeshFilter<TInputPointSet, TOutputPointSet>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MyFilter);

  static constexpr unsigned int InputDimension = TInputPointSet::Dimension;
  static constexpr unsigned int OutputDimension = TOutputPointSet::Dimension;

  using InputPointSetType = TInputPointSet;
  using OutputPointSetType = TOutputPointSet;
  using InputPixelType = typename InputPointSetType::PixelType;
  using OutputPixelType = typename OutputPointSetType::PixelType;

  /** Standard class aliases. */
  using Self = MyFilter<InputPointSetType, OutputPointSetType>;
  using Superclass = MeshToMeshFilter<InputPointSetType, OutputPointSetType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information. */
  itkTypeMacro(MyFilter, MeshToMeshFilter);

  /** Standard New macro. */
  itkNewMacro(Self);

protected:
  MyFilter();
  ~MyFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
#ifdef ITK_USE_CONCEPT_CHECKING
  // Add concept checking such as
  // itkConceptMacro( FloatingPointPixel, ( itk::Concept::IsFloatingPoint< typename InputImageType::PixelType > ) );
#endif
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMyFilter.hxx"
#endif

#endif // itkMyFilter
