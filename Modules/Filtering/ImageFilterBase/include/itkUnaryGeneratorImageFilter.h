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
#ifndef itkUnaryGeneratorImageFilter_h
#define itkUnaryGeneratorImageFilter_h

#include "itkMath.h"
#include "itkInPlaceImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

#include <functional>

namespace itk
{

/** \class UnaryGeneratorImageFilter
 * \brief Implements pixel-wise generic "operation" on one image.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 * This filter allows per-pixel operations to be specified in several
 * ways:
 * - traditional ITK "Functor" class with operator()
 * - C++11 lambda functions, with closures
 * - C++ std::function
 * - C-style function pointers
 *
 * UnaryGeneratorImageFilter allows the output dimension of the filter
 * to be larger than the input dimension. Thus subclasses of the
 * UnaryGeneratorImageFilter can be used to promote a 2D image to a 3D
 * image, etc.
 *
 * \sa UnaryFunctorImageFilter
 * \sa BinaryGeneratorImageFilter TernaryGeneratormageFilter
 *
 * \ingroup ITKImageFilterBase MultiThreaded
 *
 */
template <typename TInputImage, typename TOutputImage>
class UnaryGeneratorImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(UnaryGeneratorImageFilter);

  /** Standard class typedefs. */
  using Self = UnaryGeneratorImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(UnaryGeneratorImageFilter, InPlaceImageFilter);

  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using ConstRefFunctionType = OutputImagePixelType(const InputImagePixelType &);
  using ValueFunctionType = OutputImagePixelType(InputImagePixelType);


#if !defined(ITK_WRAPPING_PARSER)
  /** Set the pixel functor by an std::function wrapper
   *
   * The functor defines an operation done per pixel.
   */
  void
  SetFunctor(const std::function<ConstRefFunctionType> & f)
  {
    m_DynamicThreadedGenerateDataFunction = [this, f](const OutputImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(f, outputRegionForThread);
    };

    this->Modified();
  }
  void
  SetFunctor(const std::function<ValueFunctionType> & f)
  {
    m_DynamicThreadedGenerateDataFunction = [this, f](const OutputImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(f, outputRegionForThread);
    };

    this->Modified();
  }


  /** Set the pixel functor by a C function pointer
   *
   * The functor defines an operation done per pixel.
   */
  void
  SetFunctor(ConstRefFunctionType * funcPointer)
  {
    m_DynamicThreadedGenerateDataFunction = [this, funcPointer](const OutputImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(funcPointer, outputRegionForThread);
    };

    this->Modified();
  }
  void
  SetFunctor(ValueFunctionType * funcPointer)
  {
    m_DynamicThreadedGenerateDataFunction = [this, funcPointer](const OutputImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(funcPointer, outputRegionForThread);
    };

    this->Modified();
  }


  /** Set the pixel functor by a "Functor Object"
   *
   * The functor defines an operation done per pixel. A single copy of
   * the argument is created an used for all threads, so the functor
   * must be concurrent thread-safe. The functor must a have an
   * operator() method which accept arguments of InputImagePixelType.
   */
  template <typename TFunctor>
  void
  SetFunctor(const TFunctor & functor)
  {
    m_DynamicThreadedGenerateDataFunction = [this, functor](const OutputImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(functor, outputRegionForThread);
    };

    this->Modified();
  }
#endif // !defined( ITK_WRAPPING_PARSER )

protected:
  UnaryGeneratorImageFilter();
  ~UnaryGeneratorImageFilter() override = default;

  /** UnaryGeneratorImageFilter can produce an image which is a different
   * resolution than its input image.  As such, UnaryGeneratorImageFilter
   * needs to provide an implementation for
   * GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below.
   *
   * \sa ProcessObject::GenerateOutputInformation()  */
  void
  GenerateOutputInformation() override;


  /** UnaryGeneratorImageFilter is implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread.
   *
   * The template method is instantiated by the SetFunctor method, and
   * the generated code is run during the update.
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  template <typename TFunctor>
  void
  DynamicThreadedGenerateDataWithFunctor(const TFunctor &, const OutputImageRegionType & outputRegionForThread);
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  std::function<void(const OutputImageRegionType &)> m_DynamicThreadedGenerateDataFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkUnaryGeneratorImageFilter.hxx"
#endif

#endif
