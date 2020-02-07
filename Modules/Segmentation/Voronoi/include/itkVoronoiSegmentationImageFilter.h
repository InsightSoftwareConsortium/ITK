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
#ifndef itkVoronoiSegmentationImageFilter_h
#define itkVoronoiSegmentationImageFilter_h

#include "itkVoronoiSegmentationImageFilterBase.h"

namespace itk
{
/** \class VoronoiSegmentationImageFilter
 *
 * Perform the segmentation of 2D images (single channel) by Voronoi Diagram.
 * Used as a node of the segmentation toolkits.
 * The homogeneity operator here is the testing of mean and standard deviation value.
 * By setting the tolerance level, the "internal" region was defined as those
 * that is closed to the gold-standard value in the sense that the difference
 * is within the tolerance value.
 *
 * See VoronoiSegmentationImageFilterBase for detail description of voronoi
 * segmentation principles.
 *
 * The parameters here are:
 * 1. the estimation of the statistics of the object. (mean and std.)
 * 2. the tolerance for the classification. (around the mean ans std. estimated value).
 *
 * The parameters can also be automatically set by given a prior, as a binary
 * image.
 *
 * Detail information about this algorithm can be found in:
 *  " Semi-automated color segmentation of anatomical tissue,"
 *   C. Imelinska, M. Downes, and W. Yuan
 *  Computerized Medical Imaging and Graphics, Vor.24, pp 173-180, 2000.
 *
 * \ingroup HybridSegmentation
 * \ingroup ITKVoronoi
 */
template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage = Image<unsigned char, 2>>
class ITK_TEMPLATE_EXPORT VoronoiSegmentationImageFilter
  : public VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VoronoiSegmentationImageFilter);

  /** Standard class type aliases. */
  using Self = VoronoiSegmentationImageFilter;
  using Superclass = VoronoiSegmentationImageFilterBase<TInputImage, TOutputImage, TBinaryPriorImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoronoiSegmentationImageFilter, VoronoiSegmentationImageFilterBase);

  /** Convenient type alias. */
  using BinaryObjectImage = typename Superclass::BinaryObjectImage;
  using IndexList = typename Superclass::IndexList;
  using IndexType = typename Superclass::IndexType;
  using RegionType = typename Superclass::RegionType;
  using InputImageType = typename Superclass::InputImageType;

  /** Set/Get the Estimation of the mean pixel value for the object. */
  itkSetMacro(Mean, double);
  itkGetConstMacro(Mean, double);

  /** Set/Get the estimation of the STD of the pixel value for the
   *  object. */
  itkSetMacro(STD, double);
  itkGetConstMacro(STD, double);

  /** Set/Get the Tolearance of Mean for classifying the regions. */
  itkSetMacro(MeanTolerance, double);
  itkGetConstMacro(MeanTolerance, double);

  /** Set the Tolearance of STD for classifying the regions. */
  itkSetMacro(STDTolerance, double);

  /** Get the Tolearance of Variance for classifying the regions. */
  itkGetConstMacro(STDTolerance, double);

  /** Set/Get the mean percent error. */
  void
  SetMeanPercentError(double x);

  itkGetConstMacro(MeanPercentError, double);

  /** Set/Get the STD percent error. */
  itkGetConstMacro(STDPercentError, double);
  void
  SetSTDPercentError(double x);

  /** Take a prior from other segmentation node, should be an
   * binary object. */
  void
  TakeAPrior(const BinaryObjectImage * aprior) override;

  /** ImageDimension enumeration   */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(IntConvertibleToOutputCheck, (Concept::Convertible<int, typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  VoronoiSegmentationImageFilter() = default;
  ~VoronoiSegmentationImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double m_Mean{ 0.0 };
  double m_STD{ 0.0 };
  double m_MeanTolerance{ 0.0 };
  double m_STDTolerance{ 0.0 };
  double m_MeanPercentError{ 0.10 };
  double m_STDPercentError{ 1.5 };

  bool
  TestHomogeneity(IndexList & Plist) override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVoronoiSegmentationImageFilter.hxx"
#endif

#endif
