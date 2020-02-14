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
#ifndef itkObjectMorphologyImageFilter_h
#define itkObjectMorphologyImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstSliceIterator.h"
#include "itkConstantBoundaryCondition.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/**
 *\class ObjectMorphologyImageFilter
 * \brief Base class for the morphological operations
 * being applied to isolated objects in an image.
 *
 * This class provides the infrastructure to support of
 * morphological operations being applied to images in which
 * the foreground and background intensities are fixed. This filter
 * operates significantly faster than itkBinaryMorhologicalImageFilters;
 * however itkBinaryMorhologicalImageFilters preserve
 * background pixels based on values of neighboring background
 * pixels - potentially important during erosion.
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
 * providing their own Evaluate() protected member functions - one
 * that operates using a smart neighborhood operator for edge faces and
 * one that operates using a standard neighborhood operator..
 *
 * \sa ErodeObjectMorphologyImageFilter
 * \sa DilateObjectMorphologyImageFilter
 * \sa BinaryErodeImageFilter
 * \sa BinaryDilateImageFilter
 * \sa GrayscaleErodeImageFilter
 * \sa GrayscaleDilateImageFilter
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKBinaryMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT ObjectMorphologyImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ObjectMorphologyImageFilter);

  /** Standard Self type alias */
  using Self = ObjectMorphologyImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Runtime information support. */
  itkTypeMacro(ObjectMorphologyImageFilter, ImageToImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;

  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<InputImageType> *;
  using ImageBoundaryConditionConstPointerType = const ImageBoundaryCondition<InputImageType> *;
  using DefaultBoundaryConditionType = ConstantBoundaryCondition<InputImageType>;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int KernelDimension = TKernel::NeighborhoodDimension;

  /** Neighborhood iterator type. */
  using InputNeighborhoodIteratorType = ConstNeighborhoodIterator<TInputImage>;
  using OutputNeighborhoodIteratorType = NeighborhoodIterator<TOutputImage>;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** n-dimensional Kernel radius. */
  using RadiusType = typename KernelType::SizeType;

  /** Set kernel (structuring element). */
  itkSetMacro(Kernel, KernelType);

  /** Get the kernel (structuring element). */
  itkGetConstReferenceMacro(Kernel, KernelType);

  /** Get the pixel value being used to identify the object of interest */
  itkGetConstMacro(ObjectValue, PixelType);

  /** Set the pixel value being used to identify the object of interest */
  itkSetMacro(ObjectValue, PixelType);

  /** ObjectMorphologyImageFilters need to make sure they request enough of an
   * input image to account for the structuring element size.  The input
   * requested region is expanded by the radius of the structuring element.
   * If the request extends past the LargestPossibleRegion for the input,
   * the request is cropped by the LargestPossibleRegion. */
  void
  GenerateInputRequestedRegion() override;

  /** Allows a user to override the internal boundary condition. Care should be
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition.
   * NOTE: Don't forget to set UseBoundaryCondition to true! */
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

  /** Enable/disable the use of boundary condition.  Defaults to false.
   * if false, a neighborhood operator extends outside an image, it does
   * not consider that outside extent when determining if a pixel is on
   * an object's boundary. */
  itkSetMacro(UseBoundaryCondition, bool);

  /** Enable/disable the use of boundary condition.  Defaults to false.
   * if false, a neighborhood operator extends outside an image, it does
   * not consider that outside extent when determining if a pixel is on
   * an object's boundary. */
  itkGetConstMacro(UseBoundaryCondition, bool);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck1, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(SameDimensionCheck2, (Concept::SameDimension<ImageDimension, KernelDimension>));
  itkConceptMacro(OutputInputEqualityComparableCheck,
                  (Concept::EqualityComparable<typename TOutputImage::PixelType, PixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(IntConvertibleToOutputCheck, (Concept::Convertible<int, typename TOutputImage::PixelType>));
  itkConceptMacro(InputEqualityComparable, (Concept::EqualityComparable<PixelType>));
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<PixelType>));
  // End concept checking
#endif

protected:
  ObjectMorphologyImageFilter();
  ~ObjectMorphologyImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Multi-thread version GenerateData. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType &) override;

  /** Evaluate image neighborhood with kernel to find the new value
   * for the center pixel value. */
  virtual void
  Evaluate(OutputNeighborhoodIteratorType & nit, const KernelType & kernel) = 0;

  /** Evaluate a pixel (assumed to have a value of ObjectValue) to
   * determine if one of its neighboring pixels (8-neigh in 2d, etc) is a
   * non-ObjectValue pixel. */
  bool
  IsObjectPixelOnBoundary(const InputNeighborhoodIteratorType & nit);

  /** Pointer to a persistent boundary condition object used
   * for the image iterator. */
  ImageBoundaryConditionPointerType m_BoundaryCondition;

  /** Default boundary condition */
  DefaultBoundaryConditionType m_DefaultBoundaryCondition;

  /** Defaults to false */
  bool m_UseBoundaryCondition;

  /** kernel or structuring element to use. */
  KernelType m_Kernel;

  /** Pixel value that indicates the object be operated upon */
  PixelType m_ObjectValue;

  void
  BeforeThreadedGenerateData() override;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkObjectMorphologyImageFilter.hxx"
#endif

#endif
