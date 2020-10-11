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
#ifndef itkNaryFunctorImageFilter_h
#define itkNaryFunctorImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkImageIterator.h"
#include "itkArray.h"

namespace itk
{
/** \class NaryFunctorImageFilter
 * \brief Perform a generic pixel-wise operation on N images.
 *
 * This class is templated over the types of the input images
 * and the type of the output image.  It is also templated by the
 * operation to be applied.  A Functor style is used to represent the
 * function.
 *
 * All the input images must be of the same type.
 *
 * \ingroup IntensityImageFilters MultiThreaded
 * \ingroup ITKImageIntensity
 */

template <typename TInputImage, typename TOutputImage, typename TFunction>
class ITK_TEMPLATE_EXPORT NaryFunctorImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NaryFunctorImageFilter);

  /** Standard class type aliases. */
  using Self = NaryFunctorImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NaryFunctorImageFilter, InPlaceImageFilter);

  /** Some type alias. */
  using FunctorType = TFunction;
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using NaryArrayType = std::vector<InputImagePixelType>;

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer). */
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }

  /** Set the functor object.  This replaces the current Functor with a
   * copy of the specified Functor. This allows the user to specify a
   * functor that has ivars set differently than the default functor.
   * This method requires an operator!=() be defined on the functor
   * (or the compiler's default implementation of operator!=() being
   * appropriate). */
  void
  SetFunctor(FunctorType & functor)
  {
    if (m_Functor != functor)
    {
      m_Functor = functor;
      this->Modified();
    }
  }

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(OutputHasZeroCheck, (Concept::HasZero<OutputImagePixelType>));
  // End concept checking
#endif

protected:
  NaryFunctorImageFilter();
  ~NaryFunctorImageFilter() override = default;

  /** NaryFunctorImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


private:
  FunctorType m_Functor;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNaryFunctorImageFilter.hxx"
#endif

#endif
