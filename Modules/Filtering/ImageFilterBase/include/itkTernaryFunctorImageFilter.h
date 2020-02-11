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
#ifndef itkTernaryFunctorImageFilter_h
#define itkTernaryFunctorImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class TernaryFunctorImageFilter
 * \brief Implements pixel-wise generic operation of three images.
 *
 * This class is parameterized over the types of the three input images
 * and the type of the output image.  It is also parameterized by the
 * operation to be applied, using a Functor style.
 *
 * \sa BinaryFunctorImageFilter UnaryFunctorImageFilter
 *
 * \ingroup IntensityImageFilters MultiThreaded
 * \ingroup ITKImageFilterBase
 */
template <typename TInputImage1,
          typename TInputImage2,
          typename TInputImage3,
          typename TOutputImage,
          typename TFunction>
class ITK_TEMPLATE_EXPORT TernaryFunctorImageFilter : public InPlaceImageFilter<TInputImage1, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TernaryFunctorImageFilter);

  /** Standard class type aliases. */
  using Self = TernaryFunctorImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage1, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TernaryFunctorImageFilter, InPlaceImageFilter);

  /** Some type alias. */
  using FunctorType = TFunction;
  using Input1ImageType = TInputImage1;
  using Input1ImagePointer = typename Input1ImageType::ConstPointer;
  using Input1ImageRegionType = typename Input1ImageType::RegionType;
  using Input1ImagePixelType = typename Input1ImageType::PixelType;
  using Input2ImageType = TInputImage2;
  using Input2ImagePointer = typename Input2ImageType::ConstPointer;
  using Input2ImageRegionType = typename Input2ImageType::RegionType;
  using Input2ImagePixelType = typename Input2ImageType::PixelType;
  using Input3ImageType = TInputImage3;
  using Input3ImagePointer = typename Input3ImageType::ConstPointer;
  using Input3ImageRegionType = typename Input3ImageType::RegionType;
  using Input3ImagePixelType = typename Input3ImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** Connect one of the operands for pixel-wise addition. */
  void
  SetInput1(const TInputImage1 * image1);

  /** Connect one of the operands for pixel-wise addition. */
  void
  SetInput2(const TInputImage2 * image2);

  /** Connect one of the operands for pixel-wise addition. */
  void
  SetInput3(const TInputImage3 * image3);

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer). */
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer.) */
  const FunctorType &
  GetFunctor() const
  {
    return m_Functor;
  }

  /** Set the functor object.  This replaces the current Functor with a
   * copy of the specified Functor. This allows the user to specify a
   * functor that has ivars set differently than the default functor.
   * This method requires an operator==() be defined on the functor
   * (or the compiler's default implementation of operator==() being
   * appropriate). */
  void
  SetFunctor(const FunctorType & functor)
  {
    if (!(functor == m_Functor))
    {
      m_Functor = functor;
      this->Modified();
    }
  }

  /** Image dimensions */
  static constexpr unsigned int Input1ImageDimension = TInputImage1::ImageDimension;
  static constexpr unsigned int Input2ImageDimension = TInputImage2::ImageDimension;
  static constexpr unsigned int Input3ImageDimension = TInputImage3::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck1, (Concept::SameDimension<Input1ImageDimension, Input2ImageDimension>));
  itkConceptMacro(SameDimensionCheck2, (Concept::SameDimension<Input1ImageDimension, Input3ImageDimension>));
  itkConceptMacro(SameDimensionCheck3, (Concept::SameDimension<Input1ImageDimension, OutputImageDimension>));
  // End concept checking
#endif

protected:
  TernaryFunctorImageFilter();
  ~TernaryFunctorImageFilter() override = default;

  /** Validate the presence of all three inputs. If one or more inputs
   * are missing, throw an exception. */
  void
  BeforeThreadedGenerateData() override;

  /** TernaryFunctorImageFilter can be implemented as a multithreaded filter.
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
#  include "itkTernaryFunctorImageFilter.hxx"
#endif

#endif
