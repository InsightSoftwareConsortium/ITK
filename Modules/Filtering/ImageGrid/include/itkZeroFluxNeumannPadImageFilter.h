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
#ifndef itkZeroFluxNeumannPadImageFilter_h
#define itkZeroFluxNeumannPadImageFilter_h

#include "itkPadImageFilter.h"

#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{

/**
 *\class ZeroFluxNeumannPadImageFilter
 * \brief Increase the image size by padding according to the
 * zero-flux Neumann boundary condition.
 *
 * A filter which extends the image size and fill the missing pixels
 * according to a Neumann boundary condition where first,
 * upwind derivatives on the boundary are zero.  This is a useful condition
 * in solving some classes of differential equations.
 *
 * For example, invoking this filter on an image with a corner like:
   \code
                 * * * * * * *
                 * * * * * * *
                 * * 1 2 3 4 5  (where * denotes pixels that lie
                 * * 3 3 5 5 6          outside of the image boundary)
                 * * 4 4 6 7 8
   \endcode
 * returns the following padded image:
   \code
                 1 1 1 2 3 4 5
                 1 1 1 2 3 4 5
                 1 1 1 2 3 4 5
                 3 3 3 3 5 5 6   (note the corner values)
                 4 4 4 4 6 7 8
   \endcode
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 * \sa WrapPadImageFilter, MirrorPadImageFilter, ConstantPadImageFilter, ZeroFluxNeumannBoundaryCondition
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ZeroFluxNeumannPadImageFilter : public PadImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ZeroFluxNeumannPadImageFilter);

  /** Standard class type aliases. */
  using Self = ZeroFluxNeumannPadImageFilter;
  using Superclass = PadImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ZeroFluxNeumannPadImageFilter, PadImageFilter);

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using InputImageRegionType = typename Superclass::InputImageRegionType;
  using RegionType = typename Superclass::InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;
  using InputImagePixelType = typename Superclass::InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  using OutputImageIndexType = typename Superclass::OutputImageIndexType;
  using InputImageIndexType = typename Superclass::InputImageIndexType;
  using OutputImageSizeType = typename Superclass::OutputImageSizeType;
  using InputImageSizeType = typename Superclass::InputImageSizeType;
  using IndexType = typename Superclass::InputImageIndexType;
  using SizeType = typename Superclass::InputImageSizeType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputImagePixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputImagePixelType>));
  // End concept checking
#endif

protected:
  ZeroFluxNeumannPadImageFilter();
  ~ZeroFluxNeumannPadImageFilter() override = default;

private:
  ZeroFluxNeumannBoundaryCondition<TInputImage, TOutputImage> m_InternalBoundaryCondition;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkZeroFluxNeumannPadImageFilter.hxx"
#endif

#endif
