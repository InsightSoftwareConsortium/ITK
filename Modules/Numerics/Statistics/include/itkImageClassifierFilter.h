/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
/** \class ImageClassifierFilter
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

template< typename TSample, typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ImageClassifierFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedef */
  typedef ImageClassifierFilter                           Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Standard macros */
  itkTypeMacro(ImageClassifierFilter, ImageToImagefilter);
  itkNewMacro(Self);

  /** Image pixel value typedef. */
  typedef           TInputImage            InputImageType;
  typedef           TOutputImage           OutputImageType;
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer  InputImagePointer;
  typedef typename TOutputImage::Pointer OutputImagePointer;

  typedef typename TInputImage::SizeType    InputSizeType;
  typedef typename TInputImage::IndexType   InputIndexType;
  typedef typename TInputImage::RegionType  InputImageRegionType;
  typedef typename TOutputImage::SizeType   OutputSizeType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Image related typedefs. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Type of the input Sample */
  typedef TSample SampleType;

  /** typedefs from SampleType object */
  typedef typename SampleType::MeasurementType       MeasurementType;
  typedef typename SampleType::MeasurementVectorType MeasurementVectorType;

  /** typedef for the MembershipFunction */
  typedef MembershipFunctionBase< MeasurementVectorType > MembershipFunctionType;
  typedef typename MembershipFunctionType::ConstPointer   MembershipFunctionPointer;
  typedef std::vector< MembershipFunctionPointer >        MembershipFunctionVectorType;
  typedef SimpleDataObjectDecorator<
    MembershipFunctionVectorType >                        MembershipFunctionVectorObjectType;
  typedef typename
  MembershipFunctionVectorObjectType::Pointer MembershipFunctionVectorObjectPointer;

  /** typedef for membership functions weight proprtion */
  typedef Array< double > MembershipFunctionsWeightsArrayType;

  typedef SimpleDataObjectDecorator<
    MembershipFunctionsWeightsArrayType >                 MembershipFunctionsWeightsArrayObjectType;
  typedef typename
  MembershipFunctionsWeightsArrayObjectType::Pointer MembershipFunctionsWeightsArrayPointer;

  /** typedef for class label type */
  typedef IdentifierType                ClassLabelType;
  typedef std::vector< ClassLabelType > ClassLabelVectorType;
  typedef SimpleDataObjectDecorator<
    ClassLabelVectorType >                            ClassLabelVectorObjectType;
  typedef ClassLabelVectorObjectType::Pointer ClassLabelVectorObjectPointer;

  /** type of the decision rule */
  typedef DecisionRule                   DecisionRuleType;
  typedef DecisionRuleType::ConstPointer DecisionRulePointer;

  /** Sets the input image */
  void SetImage(const InputImageType *image);

  const InputImageType *  GetImage() const;

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
  void SetClassLabels(const ClassLabelVectorObjectType *classLabels);

  /** Sets input vector of membership functions. The length of this vector must match
   * the number of classes, otherwise an exception will be thrown at run time.
   * */
  void SetMembershipFunctions(const MembershipFunctionVectorObjectType *membershipFunctions);

  /** Sets array of weights for the membership functions */
  void SetMembershipFunctionsWeightsArray(const MembershipFunctionsWeightsArrayObjectType *weightsArray);

protected:
  ImageClassifierFilter();
  virtual ~ImageClassifierFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  ITK_DISALLOW_COPY_AND_ASSIGN(ImageClassifierFilter);

  /** Starts the classification process */
  void GenerateData() ITK_OVERRIDE;

private:

  unsigned int m_NumberOfClasses;

  /** Decision Rule */
  DecisionRulePointer m_DecisionRule;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageClassifierFilter.hxx"
#endif

#endif
