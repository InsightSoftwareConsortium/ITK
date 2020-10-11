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
#ifndef itkMorphologyImageFilter_h
#define itkMorphologyImageFilter_h

#include "itkKernelImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkNeighborhood.h"
#include "itkConstSliceIterator.h"
#include "itkConstantBoundaryCondition.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/** \class MorphologyImageFilter
 * \brief Base class for the morphological operations such as erosion and dilation
 *
 * This class provides the infrastructure to support most
 * morphological operations. Subclasses of MorphologyImageFilter
 * implement specific "binary" and "grayscale" operations.  The
 * "binary" subclasses can operate on gray level data, where a
 * specified a pixel value is consider the "foreground" and every
 * other pixel value is considered the background.  This is useful for
 * operating on segment masks where all pixels assigned to segment #1
 * have value 1, all pixels assigned to segment #2 have value 2, etc.
 * Here, a given segment can be dilated (expanded) while treating all
 * other segment identifiers are background.
 *
 * The "kernel" specified represents a morphology structuring element.
 * The structuring element is a small Neighborhood with values
 * indicating an element is "on" (value > 0) or "off" (value <=0).
 * Morphological operations are defined by placing the structuring
 * element over a pixel, and calculating a nonlinear function (min,
 * max) over the pixels of the image that are under pixels in the
 * structuring element that are "on". The result of this calculation
 * is the value of the pixel in the output image.  Under most
 * circumstances, the "center pixel" of the structuring element -- or
 * structuring element pixel over the input pixel under consideration
 * -- is prescribed to be "on". This is not a strict requirement but
 * the subclasses of this filter are not guaranteed to produce the
 * correct result if the "center pixel" is not part of the structuring
 * element.
 *
 * Subclasses of this class can define their own operations by simply
 * providing their own Evaluate() protected member function.
 *
 * \sa BinaryErodeImageFilter
 * \sa BinaryDilateImageFilter
 * \sa GrayscaleErodeImageFilter
 * \sa GrayscaleDilateImageFilter
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT MorphologyImageFilter : public KernelImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MorphologyImageFilter);

  /** Standard Self type alias */
  using Self = MorphologyImageFilter;
  using Superclass = KernelImageFilter<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Runtime information support. */
  itkTypeMacro(MorphologyImageFilter, KernelImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Typedef for boundary conditions. */
  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<InputImageType> *;
  using ImageBoundaryConditionConstPointerType = const ImageBoundaryCondition<InputImageType> *;
  using DefaultBoundaryConditionType = ConstantBoundaryCondition<InputImageType>;

  /** Neighborhood iterator type. */
  using NeighborhoodIteratorType = ConstNeighborhoodIterator<TInputImage>;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** n-dimensional Kernel radius. */
  using RadiusType = typename KernelType::SizeType;

  /** Allows a user to override the internal boundary condition. Care should be
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition. */
  void
  OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
  {
    m_BoundaryCondition = i;
  }

  /** Rest the boundary condition to the default */
  void
  ResetBoundaryCondition()
  {
    m_BoundaryCondition = &m_DefaultBoundaryCondition;
  }

  /** Get the current boundary condition. */
  itkGetConstMacro(BoundaryCondition, ImageBoundaryConditionPointerType);

protected:
  MorphologyImageFilter();
  ~MorphologyImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Multi-thread version of GenerateData. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Evaluate image neighborhood with kernel to find the new value
   * for the center pixel value. */
  virtual PixelType
  Evaluate(const NeighborhoodIteratorType & nit,
           const KernelIteratorType         kernelBegin,
           const KernelIteratorType         kernelEnd) = 0;

private:
  /** Pointer to a persistent boundary condition object used
   * for the image iterator. */
  ImageBoundaryConditionPointerType m_BoundaryCondition;

  /** Default boundary condition */
  DefaultBoundaryConditionType m_DefaultBoundaryCondition;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMorphologyImageFilter.hxx"
#endif

#endif
