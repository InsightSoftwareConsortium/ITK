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
#ifndef itkBinaryGeneratorImageFilter_h
#define itkBinaryGeneratorImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"


#include <functional>

namespace itk
{
/** \class BinaryGeneratorImageFilter
 * \brief Implements pixel-wise generic operation of two images,
 * or of an image and a constant.
 *
 * This class is parameterized over the types of the two input images
 * and the type of the output image.
 *
 * This filter allows per-pixel operations to be specified in several
 * ways:
 * - traditional ITK "Functor", with operator()
 * - C++11 lambda functions, with closures
 * - C++ std::function
 * - C-style function pointers
 *
 * The constant must be of the same type as the pixel type of the corresponding
 * image. It is wrapped in a SimpleDataObjectDecorator so it can be updated through
 * the pipeline. The SetConstant() and GetConstant() methods are provided as shortcuts
 * to set or get the constant value without manipulating the decorator.
 *
 * \sa UnaryGeneratorImageFilter
 * \sa BinaryFunctorImageFilter
 *
 * \ingroup IntensityImageFilters   MultiThreaded
 * \ingroup ITKImageFilterBase
 *
 */
template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
class ITK_TEMPLATE_EXPORT BinaryGeneratorImageFilter : public InPlaceImageFilter<TInputImage1, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryGeneratorImageFilter);

  /** Standard class type aliases. */
  using Self = BinaryGeneratorImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage1, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryGeneratorImageFilter, InPlaceImageFilter);

  /** Some convenient type alias. */
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

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using FunctionType = OutputImagePixelType (*)(const Input1ImagePixelType &, const Input2ImagePixelType &);

  using ConstRefFunctionType = OutputImagePixelType(const Input1ImagePixelType &, const Input2ImagePixelType &);
  using ValueFunctionType = OutputImagePixelType(Input1ImagePixelType, Input2ImagePixelType);

  /** Connect the first operand for pixel-wise operation. */
  virtual void
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

  /** Connect the second operand for pixel-wise operation. */
  virtual void
  SetInput2(const TInputImage2 * image2);
  virtual void
  SetInput2(const DecoratedInput2ImagePixelType * input2);
  virtual void
  SetInput2(const Input2ImagePixelType & input2);


  /** Set the second operand as a constant. */
  virtual void
  SetConstant2(const Input2ImagePixelType & input2);
  void
  SetConstant(Input2ImagePixelType ct)
  {
    this->SetConstant2(ct);
  }
  const Input2ImagePixelType &
  GetConstant() const
  {
    return this->GetConstant2();
  }

  /** Get the constant value of the second operand. An exception is thrown if
   * the second operand is not a constant. */
  virtual const Input2ImagePixelType &
  GetConstant2() const;

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
   * the argument is created an used for all threads, so the functor
   * must be concurrent thread-safe. The functor must a have an
   * operator() method which accept arguments of Input1ImagePixelType,
   * Input2ImagePixelType.
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


  /** ImageDimension constants */
  itkStaticConstMacro(InputImage1Dimension, unsigned int, TInputImage1::ImageDimension);
  itkStaticConstMacro(InputImage2Dimension, unsigned int, TInputImage2::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck1,
                  (Concept::SameDimension<itkGetStaticConstMacro(InputImage1Dimension),
                                          itkGetStaticConstMacro(InputImage2Dimension)>));
  itkConceptMacro(SameDimensionCheck2,
                  (Concept::SameDimension<itkGetStaticConstMacro(InputImage1Dimension),
                                          itkGetStaticConstMacro(OutputImageDimension)>));
  // End concept checking
#endif

protected:
  BinaryGeneratorImageFilter();
  ~BinaryGeneratorImageFilter() override = default;

  /** BinaryGeneratorImageFilter can be implemented as a multithreaded filter.
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

  // Needed to take the image information from the 2nd input, if the first one is
  // a simple decorated object.
  void
  GenerateOutputInformation() override;

private:
  std::function<void(const OutputImageRegionType &)> m_DynamicThreadedGenerateDataFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryGeneratorImageFilter.hxx"
#endif

#endif
