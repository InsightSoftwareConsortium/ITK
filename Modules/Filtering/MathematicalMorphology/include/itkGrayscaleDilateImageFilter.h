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
#ifndef itkGrayscaleDilateImageFilter_h
#define itkGrayscaleDilateImageFilter_h

#include "itkKernelImageFilter.h"
#include "itkMathematicalMorphologyEnums.h"
#include "itkMovingHistogramDilateImageFilter.h"
#include "itkBasicDilateImageFilter.h"
#include "itkAnchorDilateImageFilter.h"
#include "itkVanHerkGilWermanDilateImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "itkNeighborhood.h"

namespace itk
{
/**
 * \class GrayscaleDilateImageFilter
 * \brief Grayscale dilation of an image.
 *
 * Dilate an image using grayscale morphology. Dilation takes the
 * maximum of all the pixels identified by the structuring element.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \sa MorphologyImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 *
 * \sphinx
 * \sphinxexample{Filtering/MathematicalMorphology/DilateAGrayscaleImage,Dilate A Grayscale Image}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT GrayscaleDilateImageFilter : public KernelImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GrayscaleDilateImageFilter);

  /** Standard class type aliases. */
  using Self = GrayscaleDilateImageFilter;
  using Superclass = KernelImageFilter<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GrayscaleDilateImageFilter, KernelImageFilter);

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;
  using OffsetType = typename TInputImage::OffsetType;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  using HistogramFilterType = MovingHistogramDilateImageFilter<TInputImage, TOutputImage, TKernel>;
  using BasicFilterType = BasicDilateImageFilter<TInputImage, TOutputImage, TKernel>;

  using FlatKernelType = FlatStructuringElement<Self::ImageDimension>;

  using AnchorFilterType = AnchorDilateImageFilter<TInputImage, FlatKernelType>;
  using VHGWFilterType = VanHerkGilWermanDilateImageFilter<TInputImage, FlatKernelType>;
  using CastFilterType = CastImageFilter<TInputImage, TOutputImage>;

  /** Typedef for boundary conditions. */
  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<InputImageType> *;
  using ImageBoundaryConditionConstPointerType = const ImageBoundaryCondition<InputImageType> *;
  using DefaultBoundaryConditionType = ConstantBoundaryCondition<InputImageType>;

  /** Kernel type alias. */
  using KernelType = TKernel;
  //   using KernelSuperclass = typename KernelType::Superclass;
  //   using KernelSuperclass = Neighborhood< typename KernelType::PixelType, ImageDimension >;

  using AlgorithmEnum = MathematicalMorphologyEnums::Algorithm;

#if !defined(ITK_LEGACY_REMOVE)
  /** Backwards compatibility for enum values */
  using AlgorithmType = AlgorithmEnum;
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr AlgorithmType BASIC = AlgorithmEnum::BASIC;
  static constexpr AlgorithmType HISTO = AlgorithmEnum::HISTO;
  static constexpr AlgorithmType ANCHOR = AlgorithmType::ANCHOR;
  static constexpr AlgorithmType VHGW = AlgorithmEnum::VHGW;
#endif

  /** Set kernel (structuring element). */
  void
  SetKernel(const KernelType & kernel) override;

  /** Set/Get the boundary value. */
  void
  SetBoundary(const PixelType value);

  itkGetConstMacro(Boundary, PixelType);

  /** Set/Get the backend filter class. */
  void
  SetAlgorithm(AlgorithmEnum algo);
  itkGetConstMacro(Algorithm, AlgorithmEnum);

  /** GrayscaleDilateImageFilter need to set its internal filters as modified */
  void
  Modified() const override;

  void
  SetNumberOfWorkUnits(ThreadIdType nb) override;

protected:
  GrayscaleDilateImageFilter();
  ~GrayscaleDilateImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  PixelType m_Boundary;

  // the filters used internally
  typename HistogramFilterType::Pointer m_HistogramFilter;

  typename BasicFilterType::Pointer m_BasicFilter;

  typename AnchorFilterType::Pointer m_AnchorFilter;

  typename VHGWFilterType::Pointer m_VHGWFilter;

  // and the name of the filter
  AlgorithmEnum m_Algorithm;

  // the boundary condition need to be stored here
  DefaultBoundaryConditionType m_BoundaryCondition;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGrayscaleDilateImageFilter.hxx"
#endif

#endif
