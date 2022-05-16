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
#ifndef itkImageKernelOperator_h
#define itkImageKernelOperator_h

#include "itkNeighborhoodOperator.h"

#include "itkImage.h"

namespace itk
{
/**
 * \class ImageKernelOperator
 * \brief A NeighborhoodOperator whose coefficients are from an image.
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Image Kernel Convolution"
 * by Tustison N., Gee J.
 * https://www.insight-journal.org/browse/publication/208
 *
 *
 * \note ImageKernelOperator does not have any user-declared "special member function",
 * following the C++ Rule of Zero: the compiler will generate them if necessary.
 *
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 */
template <typename TPixel, unsigned int VDimension = 2, typename TAllocator = NeighborhoodAllocator<TPixel>>
class ITK_TEMPLATE_EXPORT ImageKernelOperator : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{
public:
  /** Standard class type aliases. */
  using Self = ImageKernelOperator;
  using Superclass = NeighborhoodOperator<TPixel, VDimension, TAllocator>;

  using ImageType = Image<TPixel, VDimension>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageKernelOperator, NeighborhoodOperator);

  /** Set the image kernel. Only images with odd size in all
   * dimensions are allowed. If an image with an even size is passed
   * as an argument, an exception will be thrown. */
  void
  SetImageKernel(const ImageType * kernel);

  /** Get the image kernel. */
  const ImageType *
  GetImageKernel() const;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }

protected:
  /** Type alias support for coefficient vector type.*/
  using typename Superclass::CoefficientVector;

  /** Calculates operator coefficients. */
  CoefficientVector
  GenerateCoefficients() override;

  /** Arranges coefficients spatially in the memory buffer. */
  void
  Fill(const CoefficientVector & coeff) override;

private:
  typename ImageType::ConstPointer m_ImageKernel;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageKernelOperator.hxx"
#endif

#endif
