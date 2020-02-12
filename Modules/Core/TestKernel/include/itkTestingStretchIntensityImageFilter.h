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
#ifndef itkTestingStretchIntensityImageFilter_h
#define itkTestingStretchIntensityImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
namespace Testing
{

/**
 *\class StretchIntensityImageFilter
 *
 * \brief Applies a linear transformation to the intensity levels of the
 * input Image.
 *
 * StretchIntensityImageFilter applies pixel-wise a linear transformation
 * to the intensity values of input image pixels. The linear transformation
 * is defined by the user in terms of the minimum and maximum values that
 * the output image should have.
 *
 * \ingroup ITKTestKernel
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT StretchIntensityImageFilter : public ImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(StretchIntensityImageFilter);

  /** Standard class type aliases. */
  using Self = StretchIntensityImageFilter;
  using Superclass = ImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<InputPixelType>::RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StretchIntensityImageFilter, ImageSource);

  itkSetMacro(OutputMinimum, OutputPixelType);
  itkSetMacro(OutputMaximum, OutputPixelType);
  itkGetConstReferenceMacro(OutputMinimum, OutputPixelType);
  itkGetConstReferenceMacro(OutputMaximum, OutputPixelType);

  /** Get the Scale and Shift used for the linear transformation
      of gray level values.
   \warning These Values are only valid after the filter has been updated */
  itkGetConstReferenceMacro(Scale, RealType);
  itkGetConstReferenceMacro(Shift, RealType);

  /** Get the Minimum and Maximum values of the input image.
   \warning These Values are only valid after the filter has been updated */
  itkGetConstReferenceMacro(InputMinimum, InputPixelType);
  itkGetConstReferenceMacro(InputMaximum, InputPixelType);

  /** Set/Get the image input of this process object.  */
  using Superclass::SetInput;
  virtual void
  SetInput(const TInputImage * image);
  const TInputImage *
  GetInput() const;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputPixelType>));
  itkConceptMacro(RealTypeMultiplyOperatorCheck, (Concept::MultiplyOperator<RealType>));
  itkConceptMacro(RealTypeAdditiveOperatorsCheck, (Concept::AdditiveOperators<RealType>));
  // End concept checking
#endif

protected:
  StretchIntensityImageFilter();
  ~StretchIntensityImageFilter() override = default;

  /** Process to execute before entering the multithreaded section */
  void
  BeforeThreadedGenerateData() override;

  /** Print internal ivars */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using InputImageRegionType = typename TInputImage::RegionType;

  /** UnaryFunctorImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  RealType m_Scale;
  RealType m_Shift;

  InputPixelType m_InputMinimum;
  InputPixelType m_InputMaximum;

  OutputPixelType m_OutputMinimum;
  OutputPixelType m_OutputMaximum;
};
} // end namespace Testing
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTestingStretchIntensityImageFilter.hxx"
#endif

#endif
