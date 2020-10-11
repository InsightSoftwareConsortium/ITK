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
#ifndef itkImageClassifierFilter_h
#define itkImageClassifierFilter_h

#include <vector>

#include "itkMembershipFunctionBase.h"
#include "itkDecisionRule.h"
#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/**
 *\class ImageClassifierFilter
 *
 *  \brief Image classification class
 *
 *  This filter takes input image, membership functions,
 *  decision rule and produces as output image with each pixel labeled
 *  according to the classification result.
 *
 *  This class is templated over the type of input and output image and
 *  sample type.
 *
 * \sa SampleClassifierFilter
 * \ingroup ITKStatistics
 */

template <typename TSample, typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ImageClassifierFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageClassifierFilter);

  /** Standard class type alias */
  using Self = ImageClassifierFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(ImageClassifierFilter, ImageToImagefilter);
  itkNewMacro(Self);

  /** Image pixel value type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;
  using OutputImagePointer = typename TOutputImage::Pointer;

  using InputSizeType = typename TInputImage::SizeType;
  using InputIndexType = typename TInputImage::IndexType;
  using InputImageRegionType = typename TInputImage::RegionType;
  using OutputSizeType = typename TOutputImage::SizeType;
  using OutputIndexType = typename TOutputImage::IndexType;
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Image related type alias. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Type of the input Sample */
  using SampleType = TSample;

  /** type alias from SampleType object */
  using MeasurementType = typename SampleType::MeasurementType;
  using MeasurementVectorType = typename SampleType::MeasurementVectorType;

  /** type alias for the MembershipFunction */
  using MembershipFunctionType = MembershipFunctionBase<MeasurementVectorType>;
  using MembershipFunctionPointer = typename MembershipFunctionType::ConstPointer;
  using MembershipFunctionVectorType = std::vector<MembershipFunctionPointer>;
  using MembershipFunctionVectorObjectType = SimpleDataObjectDecorator<MembershipFunctionVectorType>;
  using MembershipFunctionVectorObjectPointer = typename MembershipFunctionVectorObjectType::Pointer;

  /** type alias for membership functions weight proprtion */
  using MembershipFunctionsWeightsArrayType = Array<double>;

  using MembershipFunctionsWeightsArrayObjectType = SimpleDataObjectDecorator<MembershipFunctionsWeightsArrayType>;
  using MembershipFunctionsWeightsArrayPointer = typename MembershipFunctionsWeightsArrayObjectType::Pointer;

  /** type alias for class label type */
  using ClassLabelType = IdentifierType;
  using ClassLabelVectorType = std::vector<ClassLabelType>;
  using ClassLabelVectorObjectType = SimpleDataObjectDecorator<ClassLabelVectorType>;
  using ClassLabelVectorObjectPointer = ClassLabelVectorObjectType::Pointer;

  /** type of the decision rule */
  using DecisionRuleType = DecisionRule;
  using DecisionRulePointer = DecisionRuleType::ConstPointer;

  /** Sets the input image */
  void
  SetImage(const InputImageType * image);

  const InputImageType *
  GetImage() const;

  /** Number of classes. This must match the number of labels and membership
   * functions provided by the user, otherwise an exception will be thrown at
   */
  itkSetMacro(NumberOfClasses, unsigned int);
  itkGetConstMacro(NumberOfClasses, unsigned int);

  /** Set/Get the decision rule. */
  itkSetConstObjectMacro(DecisionRule, DecisionRuleType);
  itkGetConstObjectMacro(DecisionRule, DecisionRuleType);

  /** Sets input vector of class labels. The length of this vector must match
   * the number of classes, otherwise an exception will be thrown at run time.
   * */
  void
  SetClassLabels(const ClassLabelVectorObjectType * classLabels);

  /** Sets input vector of membership functions. The length of this vector must match
   * the number of classes, otherwise an exception will be thrown at run time.
   * */
  void
  SetMembershipFunctions(const MembershipFunctionVectorObjectType * membershipFunctions);

  /** Sets array of weights for the membership functions */
  void
  SetMembershipFunctionsWeightsArray(const MembershipFunctionsWeightsArrayObjectType * weightsArray);

protected:
  ImageClassifierFilter();
  ~ImageClassifierFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Starts the classification process */
  void
  GenerateData() override;

private:
  unsigned int m_NumberOfClasses;

  /** Decision Rule */
  DecisionRulePointer m_DecisionRule;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageClassifierFilter.hxx"
#endif

#endif
