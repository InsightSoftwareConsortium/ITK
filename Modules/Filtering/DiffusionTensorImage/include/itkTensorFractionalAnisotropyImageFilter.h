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
#ifndef itkTensorFractionalAnisotropyImageFilter_h
#define itkTensorFractionalAnisotropyImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkImage.h"

namespace itk
{
// This functor class invokes the computation of fractional anisotropy from
// every pixel.
namespace Functor
{
template <typename TInput>
class TensorFractionalAnisotropyFunction
{
public:
  using RealValueType = typename TInput::RealValueType;
  TensorFractionalAnisotropyFunction() = default;
  ~TensorFractionalAnisotropyFunction() = default;
  bool
  operator!=(const TensorFractionalAnisotropyFunction &) const
  {
    return false;
  }

  bool
  operator==(const TensorFractionalAnisotropyFunction & other) const
  {
    return !(*this != other);
  }

  inline RealValueType
  operator()(const TInput & x) const
  {
    return x.GetFractionalAnisotropy();
  }
};
} // end namespace Functor

/** \class TensorFractionalAnisotropyImageFilter
 * \brief Computes the Fractional Anisotropy for every pixel of a input tensor image.
 *
 * TensorFractionalAnisotropyImageFilter applies pixel-wise the invocation for
 * computing the fractional anisotropy of every pixel. The pixel type of the
 * input image is expected to implement a method GetFractionalAnisotropy(), and
 * to specify its return type as  RealValueType.
 *
 * \sa TensorRelativeAnisotropyImageFilter
 * \sa DiffusionTensor3D
 *
 * \ingroup IntensityImageFilters  MultiThreaded  TensorObjects
 *
 * \ingroup ITKDiffusionTensorImage
 */
template <typename TInputImage,
          typename TOutputImage =
            Image<typename NumericTraits<typename TInputImage::PixelType::ValueType>::RealType, TInputImage::Dimension>>
class TensorFractionalAnisotropyImageFilter
  : public UnaryFunctorImageFilter<TInputImage,
                                   TOutputImage,
                                   Functor::TensorFractionalAnisotropyFunction<typename TInputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TensorFractionalAnisotropyImageFilter);

  /** Standard class type aliases. */
  using Self = TensorFractionalAnisotropyImageFilter;
  using Superclass =
    UnaryFunctorImageFilter<TInputImage,
                            TOutputImage,
                            Functor::TensorFractionalAnisotropyFunction<typename TInputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputImageType = typename Superclass::OutputImageType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputValueType = typename InputPixelType::ValueType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(TensorFractionalAnisotropyImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Print internal ivars */
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    this->Superclass::PrintSelf(os, indent);
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputValueType>));
  // End concept checking
#endif

protected:
  TensorFractionalAnisotropyImageFilter() = default;
  ~TensorFractionalAnisotropyImageFilter() override = default;
};
} // end namespace itk

#endif
