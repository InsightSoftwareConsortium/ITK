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
#ifndef itkUnaryFunctorImageFilter_h
#define itkUnaryFunctorImageFilter_h

#include "itkMath.h"
#include "itkInPlaceImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class UnaryFunctorImageFilter
 * \brief Implements pixel-wise generic operation on one image.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.  It is also parameterized by the
 * operation to be applied, using a Functor style.
 *
 * UnaryFunctorImageFilter allows the output dimension of the filter
 * to be larger than the input dimension. Thus subclasses of the
 * UnaryFunctorImageFilter (like the CastImageFilter) can be used
 * to promote a 2D image to a 3D image, etc.
 *
 * \sa UnaryGeneratorImageFilter
 * \sa BinaryFunctorImageFilter TernaryFunctorImageFilter
 *
 * \ingroup   IntensityImageFilters     MultiThreaded
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{ImageProcessing/UnaryFunctorImageFilter,Apply Custom Operation To Each Pixel In Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage, typename TFunction>
class ITK_TEMPLATE_EXPORT UnaryFunctorImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(UnaryFunctorImageFilter);

  /** Standard class type aliases. */
  using Self = UnaryFunctorImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(UnaryFunctorImageFilter, InPlaceImageFilter);

  /** Some type alias. */
  using FunctorType = TFunction;

  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer.) */
  FunctorType &
  GetFunctor()
  {
    return m_Functor;
  }
  const FunctorType &
  GetFunctor() const
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
  SetFunctor(const FunctorType & functor)
  {
    if (m_Functor != functor)
    {
      m_Functor = functor;
      this->Modified();
    }
  }

protected:
  UnaryFunctorImageFilter();
  ~UnaryFunctorImageFilter() override = default;

  /** UnaryFunctorImageFilter can produce an image which is a different
   * resolution than its input image.  As such, UnaryFunctorImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
   *
   * \sa ProcessObject::GenerateOutputInformaton()  */
  void
  GenerateOutputInformation() override;

  /** UnaryFunctorImageFilter can be implemented as a multithreaded filter.
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
#  include "itkUnaryFunctorImageFilter.hxx"
#endif

#endif
