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
#ifndef itkScalarChanAndVeseDenseLevelSetImageFilter_h
#define itkScalarChanAndVeseDenseLevelSetImageFilter_h

#include "itkMultiphaseDenseFiniteDifferenceImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunction.h"

namespace itk
{
/** \class ScalarChanAndVeseDenseLevelSetImageFilter
 * \brief Dense implementation of the Chan and Vese multiphase level set image filter.
 *
 * This code was adapted from the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese.
 *         In Scale-Space Theories in Computer Vision, pages 141-151, 1999.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 * \ingroup ITKReview
 */
template <typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFunction = ScalarChanAndVeseLevelSetFunction<TInputImage, TFeatureImage>,
          class TSharedData = typename TFunction::SharedDataType>
class ITK_TEMPLATE_EXPORT ScalarChanAndVeseDenseLevelSetImageFilter
  : public MultiphaseDenseFiniteDifferenceImageFilter<TInputImage, TFeatureImage, TOutputImage, TFunction>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarChanAndVeseDenseLevelSetImageFilter);

  using Self = ScalarChanAndVeseDenseLevelSetImageFilter;
  using Superclass = MultiphaseDenseFiniteDifferenceImageFilter<TInputImage, TFeatureImage, TOutputImage, TFunction>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarChanAndVeseDenseLevelSetImageFilter, MultiphaseDenseFiniteDifferenceImageFilter);

  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Inherited type alias from the superclass. */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputPointType = typename Superclass::InputPointType;
  using ValueType = typename Superclass::ValueType;
  using InputSpacingType = typename InputImageType::SpacingType;

  using FeatureImageType = TFeatureImage;
  using FeatureImagePointer = typename FeatureImageType::Pointer;
  using FeaturePixelType = typename FeatureImageType::PixelType;
  using FeatureIndexType = typename FeatureImageType::IndexType;
  using FeatureIndexValueType = typename FeatureIndexType::IndexValueType;
  using FeatureRegionType = typename FeatureImageType::RegionType;

  /** Output image type type alias */
  using OutputImageType = TOutputImage;
  using IndexType = typename OutputImageType::IndexType;
  using OutputPixelType = typename OutputImageType::PixelType;

  using TimeStepType = typename Superclass::TimeStepType;
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;

  using FunctionType = TFunction;
  using FunctionPointer = typename FunctionType::Pointer;

  using SharedDataType = TSharedData;
  using SharedDataPointer = typename SharedDataType::Pointer;

  using ROIFilterType = RegionOfInterestImageFilter<FeatureImageType, FeatureImageType>;
  using ROIFilterPointer = typename ROIFilterType::Pointer;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputPixelType>));
  // End concept checking
#endif

  /** Set/Get the feature image to be used for speed function of the level set
   *  equation.  Equivalent to calling Set/GetInput(1, ..) */
  virtual void
  SetFeatureImage(const FeatureImagePointer f)
  {
    this->SetInput(f);
  }

protected:
  ScalarChanAndVeseDenseLevelSetImageFilter() { this->m_SharedData = SharedDataType::New(); }

  ~ScalarChanAndVeseDenseLevelSetImageFilter() override = default;

  SharedDataPointer m_SharedData;

  void
  Initialize() override;

  void
  InitializeIteration() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarChanAndVeseDenseLevelSetImageFilter.hxx"
#endif

#endif
