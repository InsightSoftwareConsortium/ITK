/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLabelOverlapMeasuresImageFilter_h
#define itkLabelOverlapMeasuresImageFilter_h

#include "itkImageSink.h"
#include "itkNumericTraits.h"
#include <mutex>
#include <unordered_map>

namespace itk
{

/** \class LabelOverlapMeasuresImageFilter
 * \brief Computes overlap measures between the set same set of labels of
 * pixels of two images. Background is assumed to be 0.
 *
 * This code was contributed in the Insight Journal paper:
 * "Introducing Dice, Jaccard, and Other Label Overlap Measures To ITK"
 * by Nicholas J. Tustison, James C. Gee
 * https://www.insight-journal.org/browse/publication/707
 *
 * \author Nicholas J. Tustison
 * \sa LabelOverlapMeasuresImageFilter
 *
 * \ingroup ITKImageStatistics
 * \ingroup MultiThreaded
 */
template <typename TLabelImage>
class ITK_TEMPLATE_EXPORT LabelOverlapMeasuresImageFilter : public ImageSink<TLabelImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelOverlapMeasuresImageFilter);

  /** Standard Self type alias */
  using Self = LabelOverlapMeasuresImageFilter;
  using Superclass = ImageSink<TLabelImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelOverlapMeasuresImageFilter, ImageSink);

  /** Image related type alias. */
  using LabelImageType = TLabelImage;
  using LabelImagePointer = typename TLabelImage::Pointer;
  using LabelImageConstPointer = typename TLabelImage::ConstPointer;

  using RegionType = typename TLabelImage::RegionType;
  using SizeType = typename TLabelImage::SizeType;
  using IndexType = typename TLabelImage::IndexType;

  using LabelType = typename TLabelImage::PixelType;

  /** Type to use for computations. */
  using RealType = typename NumericTraits<LabelType>::RealType;

  /** \class LabelSetMeasures
   * \brief Metrics stored per label
   * \ingroup ITKImageStatistics
   */
  class LabelSetMeasures
  {
  public:
    // default constructor/copy/move etc...

    SizeValueType m_Source{ 0 };
    SizeValueType m_Target{ 0 };
    SizeValueType m_Union{ 0 };
    SizeValueType m_Intersection{ 0 };
    SizeValueType m_SourceComplement{ 0 };
    SizeValueType m_TargetComplement{ 0 };
  };

  /** Type of the map used to store data per label */
  using MapType = std::unordered_map<LabelType, LabelSetMeasures>;
  using MapIterator = typename MapType::iterator;
  using MapConstIterator = typename MapType::const_iterator;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TLabelImage::ImageDimension;

  /** Set the label image */
  itkSetInputMacro(TargetImage, LabelImageType);
  itkGetInputMacro(TargetImage, LabelImageType);
  itkSetInputMacro(SourceImage, LabelImageType);
  itkGetInputMacro(SourceImage, LabelImageType);


  /** Get the label set measures. */
  MapType
  GetLabelSetMeasures()
  {
    return this->m_LabelSetMeasures;
  }

  // Overlap agreement metrics

  /** Get the total overlap over all labels. */
  RealType
  GetTotalOverlap() const;

  /** Get the target overlap for the specified individual label. */
  RealType GetTargetOverlap(LabelType) const;

  /** Get the union overlap (Jaccard coefficient) over all labels. */
  RealType
  GetUnionOverlap() const;
  RealType
  GetJaccardCoefficient() const
  {
    return this->GetUnionOverlap();
  }

  /** Get the union overlap (Jaccard coefficient) for the specified individual
   * label. */
  RealType GetUnionOverlap(LabelType) const;
  RealType
  GetJaccardCoefficient(LabelType label) const
  {
    return this->GetUnionOverlap(label);
  }

  /** Get the mean overlap (Dice coefficient) over all labels. */
  RealType
  GetMeanOverlap() const;
  RealType
  GetDiceCoefficient() const
  {
    return this->GetMeanOverlap();
  }

  /** Get the mean overlap (Dice coefficient) for the specified individual
   * label. */
  RealType GetMeanOverlap(LabelType) const;
  RealType
  GetDiceCoefficient(LabelType label) const
  {
    return this->GetMeanOverlap(label);
  }

  /** Get the volume similarity over all labels. */
  RealType
  GetVolumeSimilarity() const;

  /** Get the volume similarity for the specified individual label. */
  RealType GetVolumeSimilarity(LabelType) const;

  // Overlap error metrics

  /** Get the false negative error over all labels. */
  RealType
  GetFalseNegativeError() const;

  /** Get the false negative error for the specified individual label. */
  RealType GetFalseNegativeError(LabelType) const;

  /** Get the false positive error over all labels. */
  RealType
  GetFalsePositiveError() const;

  /** Get the false positive error for the specified individual label. */
  RealType GetFalsePositiveError(LabelType) const;

  /** Get the false discovery rate over all labels. */
  RealType
  GetFalseDiscoveryRate() const;

  /** Get the false discovery rate for the specified individual label. */
  RealType GetFalseDiscoveryRate(LabelType) const;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1HasNumericTraitsCheck, (Concept::HasNumericTraits<LabelType>));
  // End concept checking
#endif

protected:
  LabelOverlapMeasuresImageFilter();
  ~LabelOverlapMeasuresImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  BeforeStreamedGenerateData() override;


  void
  ThreadedStreamedGenerateData(const RegionType &) override;

  void
  MergeMap(MapType & m1, MapType & m2) const;

private:
  MapType m_LabelSetMeasures;

  std::mutex m_Mutex;
}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelOverlapMeasuresImageFilter.hxx"
#endif

#endif
