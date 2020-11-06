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
#ifndef itkTernaryGeneratorImageFilter_h
#define itkTernaryGeneratorImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkSimpleDataObjectDecorator.h"

#include <functional>

namespace itk
{
/** \class TernaryGeneratorImageFilter
 * \brief Implements pixel-wise generic operation of three images or images with constants.
 *
 * This class is parameterized over the types of the three input images
 * and the type of the output image.
 *
 * This filter allows per-pixel operations to be specified in several
 * ways:
 * - traditional ITK "Functor", with operator()
 * - C++11 lambda functions, with closures
 * - C++ std::function
 * - C-style function pointers
 *
 * A constant must be of the same type as the pixel type of the corresponding
 * input image. It is wrapped in a SimpleDataObjectDecorator so it can be updated through
 * the pipeline. The SetConstantN() and GetConstantN() methods are provided as shortcuts
 * to set or get the constant value without manipulating the decorator.
 *
 * \sa TernaryFunctorImageFilter
 * \sa BinaryGeneratorImageFilter UnaryGeneratorImageFilter
 *
 * \ingroup IntensityImageFilters MultiThreaded
 * \ingroup ITKImageFilterBase
 */
template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
class ITK_TEMPLATE_EXPORT TernaryGeneratorImageFilter : public InPlaceImageFilter<TInputImage1, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TernaryGeneratorImageFilter);

  /** Standard class type aliases. */
  using Self = TernaryGeneratorImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage1, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TernaryGeneratorImageFilter, InPlaceImageFilter);

  /** Some type alias. */
  using Input1ImageType = TInputImage1;
  using Input1ImagePointer = typename Input1ImageType::ConstPointer;
  using Input1ImageRegionType = typename Input1ImageType::RegionType;
  using Input1ImagePixelType = typename Input1ImageType::PixelType;
  using DecoratedInput1ImagePixelType = SimpleDataObjectDecorator<Input1ImagePixelType>;

  using Input2ImageType = TInputImage2;
  using Input2ImagePointer = typename Input2ImageType::ConstPointer;
  using Input2ImageRegionType = typename Input2ImageType::RegionType;
  using Input2ImagePixelType = typename Input2ImageType::PixelType;
  using DecoratedInput2ImagePixelType = SimpleDataObjectDecorator<Input2ImagePixelType>;

  using Input3ImageType = TInputImage3;
  using Input3ImagePointer = typename Input3ImageType::ConstPointer;
  using Input3ImageRegionType = typename Input3ImageType::RegionType;
  using Input3ImagePixelType = typename Input3ImageType::PixelType;
  using DecoratedInput3ImagePixelType = SimpleDataObjectDecorator<Input3ImagePixelType>;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;


  using FunctionType = OutputImagePixelType (*)(const Input1ImagePixelType &,
                                                const Input2ImagePixelType &,
                                                const Input3ImagePixelType &);

  using ConstRefFunctionType = OutputImagePixelType(const Input1ImagePixelType &,
                                                    const Input2ImagePixelType &,
                                                    const Input3ImagePixelType &);
  using ValueFunctionType = OutputImagePixelType(Input1ImagePixelType, Input2ImagePixelType, Input3ImagePixelType);

  /** Connect one of the operands for pixel-wise operation. */
  void
  SetInput1(const TInputImage1 * image1);
  virtual void
  SetInput1(const DecoratedInput1ImagePixelType * input1);
  virtual void
  SetInput1(const Input1ImagePixelType & input1);

  /** Set the first operand as a constant. */
  virtual void
  SetConstant1(const Input1ImagePixelType & input1);

  /** Get the constant value of the first operand. An exception is thrown if
   * the first operand is not a constant. */
  virtual const Input1ImagePixelType &
  GetConstant1() const;

  /** Connect one of the operands for pixel-wise operation. */
  void
  SetInput2(const TInputImage2 * image2);
  virtual void
  SetInput2(const DecoratedInput2ImagePixelType * input2);
  virtual void
  SetInput2(const Input2ImagePixelType & input2);

  /** Set the second operand as a constant. */
  virtual void
  SetConstant2(const Input2ImagePixelType & input2);

  /** Get the constant value of the second operand. An exception is thrown if
   * the second operand is not a constant. */
  virtual const Input2ImagePixelType &
  GetConstant2() const;

  /** Connect one of the operands for pixel-wise operation. */
  void
  SetInput3(const TInputImage3 * image3);
  virtual void
  SetInput3(const DecoratedInput3ImagePixelType * input3);
  virtual void
  SetInput3(const Input3ImagePixelType & input3);

  /** Set the second operand as a constant. */
  virtual void
  SetConstant3(const Input3ImagePixelType & input3);

  /** Get the constant value of the second operand. An exception is throw if
   * the second operand is not a constant. */
  virtual const Input3ImagePixelType &
  GetConstant3() const;


#if !defined(ITK_WRAPPING_PARSER)
  /** Set the pixel functor
   *
   * The functor defines an operation done per pixel.
   */
  void
  SetFunctor(const std::function<ConstRefFunctionType> & f)
  {
    // the closure creates a copy of f
    m_DynamicThreadedGenerateDataFunction = [this, f](const OutputImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(f, outputRegionForThread);
    };

    this->Modified();
  }
  void
  SetFunctor(const std::function<ValueFunctionType> & f)
  {
    // the capture creates a copy of f
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
   * the argument is created and used for all threads, so the functor
   * must be concurrent thread-safe. The functor must have an
   * operator() method which accepts arguments of Input1ImagePixelType,
   * Input2ImagePixelType, and Input3ImagePixelType.
   */
  template <typename TFunctor>
  void
  SetFunctor(const TFunctor & functor)
  {
    // the capture creates a copy of the functor
    m_DynamicThreadedGenerateDataFunction = [this, functor](const OutputImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(functor, outputRegionForThread);
    };

    this->Modified();
  }
#endif // !defined( ITK_WRAPPING_PARSER )

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
  TernaryGeneratorImageFilter();
  ~TernaryGeneratorImageFilter() override = default;

  void
  GenerateOutputInformation() override;

  /** TernaryGeneratorImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
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
#  include "itkTernaryGeneratorImageFilter.hxx"
#endif

#endif
