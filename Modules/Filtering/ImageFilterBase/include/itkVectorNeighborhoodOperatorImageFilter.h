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
#ifndef itkVectorNeighborhoodOperatorImageFilter_h
#define itkVectorNeighborhoodOperatorImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class VectorNeighborhoodOperatorImageFilter
 * \brief Applies a single scalar NeighborhoodOperator to an
 * itk::Vector image region.
 *
 * This filter calculates successive inner products between a single
 * NeighborhoodOperator and a NeighborhoodIterator, which is swept
 * across every pixel in an image region.  For operators that are
 * symmetric across their axes, the result is a fast convolution
 * with the image region.  Apply the mirror()'d operator for
 * non-symmetric NeighborhoodOperators.
 *
 * This filter assumes that the input and output images have
 * pixels which are itk::Vectors of the same vector dimension.
 * The input NeighbourhoodOperator must have a scalar type
 * that matches the ValueType of vector pixels.
 *
 * To apply a scalar NeighborhoodOperator to a scalar image
 * use NeighborhoodOperatorImageFilter instead.
 *
 * \ingroup ImageFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa NeighborhoodOperatorImageFilter
 * \ingroup ITKImageFilterBase
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT VectorNeighborhoodOperatorImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VectorNeighborhoodOperatorImageFilter);

  /** Standard class type aliases. */
  using Self = VectorNeighborhoodOperatorImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VectorNeighborhoodOperatorImageFilter, ImageToImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using InputImagePointer = typename TInputImage::Pointer;
  using OutputImagePointer = typename TOutputImage::Pointer;
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  using ScalarValueType = typename OutputPixelType::ValueType;

  /** Determine image dimension. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Image type alias support */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Typedef for generic boundary condition pointer */
  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<OutputImageType> *;

  /** Superclass type alias. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** Sets the operator that is used to filter the image. Note
   * that the operator is stored as an internal COPY (it
   * is not part of the pipeline). */
  void
  SetOperator(const Neighborhood<ScalarValueType, Self::ImageDimension> & p)
  {
    m_Operator = p;
    this->Modified();
  }

  /** Allows a user to override the internal boundary condition. Care should be
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition. */
  void
  OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
  {
    m_BoundsCondition = i;
  }

  /** VectorNeighborhoodOperatorImageFilter needs a larger input requested
   * region than the output requested region.  As such,
   * VectorNeighborhoodOperatorImageFilter needs to provide an implementation for
   * GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::PixelType::ValueType>));
  itkConceptMacro(OutputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<typename TOutputImage::PixelType::ValueType>));
  // End concept checking
#endif

protected:
  VectorNeighborhoodOperatorImageFilter()
    : m_BoundsCondition(nullptr)
  {
    this->DynamicMultiThreadingOn();
    this->ThreaderUpdateProgressOff();
  }

  ~VectorNeighborhoodOperatorImageFilter() override = default;

  /** VectorNeighborhoodOperatorImageFilter can be implemented as a
   * multithreaded filter.  Therefore, this implementation provides a
   * DynamicThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }

private:
  /** Pointer to the internal operator used to filter the image. */
  Neighborhood<ScalarValueType, Self::ImageDimension> m_Operator;

  /** Pointer to a persistent boundary condition object used
   * for the image iterator. */
  ImageBoundaryConditionPointerType m_BoundsCondition;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorNeighborhoodOperatorImageFilter.hxx"
#endif

#endif
