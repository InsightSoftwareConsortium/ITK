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
#ifndef itkLabelOverlapMeasuresImageFilter_h
#define itkLabelOverlapMeasuresImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"

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
 * https://hdl.handle.net/10380/3141
 * http://www.insight-journal.org/browse/publication/707
 *
 * \author Nicholas J. Tustison
 * \sa LabelOverlapMeasuresImageFilter
 *
 * \ingroup ITKImageStatistics
 * \ingroup MultiThreaded
 */
template <typename TLabelImage>
class ITK_TEMPLATE_EXPORT LabelOverlapMeasuresImageFilter : public ImageToImageFilter<TLabelImage, TLabelImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelOverlapMeasuresImageFilter);

  /** Standard Self type alias */
  using Self = LabelOverlapMeasuresImageFilter;
  using Superclass = ImageToImageFilter<TLabelImage, TLabelImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelOverlapMeasuresImageFilter, ImageToImageFilter);

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
    // default constructor
    LabelSetMeasures()
    {
      m_Source = 0;
      m_Target = 0;
      m_Union = 0;
      m_Intersection = 0;
      m_SourceComplement = 0;
      m_TargetComplement = 0;
    }

    // added for completeness
    LabelSetMeasures &
    operator=(const LabelSetMeasures & l)
    {
      if (this != &l)
      {
        m_Source = l.m_Source;
        m_Target = l.m_Target;
        m_Union = l.m_Union;
        m_Intersection = l.m_Intersection;
        m_SourceComplement = l.m_SourceComplement;
        m_TargetComplement = l.m_TargetComplement;
      }
      return *this;
    }

    unsigned long m_Source;
    unsigned long m_Target;
    unsigned long m_Union;
    unsigned long m_Intersection;
    unsigned long m_SourceComplement;
    unsigned long m_TargetComplement;
  };

  /** Type of the map used to store data per label */
  using MapType = std::unordered_map<LabelType, LabelSetMeasures>;
  using MapIterator = typename MapType::iterator;
  using MapConstIterator = typename MapType::const_iterator;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TLabelImage::ImageDimension;

  /** Set the source image. */
  void
  SetSourceImage(const LabelImageType * image)
  {
    this->SetNthInput(0, const_cast<LabelImageType *>(image));
  }

  /** Set the target image. */
  void
  SetTargetImage(const LabelImageType * image)
  {
    this->SetNthInput(1, const_cast<LabelImageType *>(image));
  }

  /** Get the source image. */
  const LabelImageType *
  GetSourceImage()
  {
    return this->GetInput(0);
  }

  /** Get the target image. */
  const LabelImageType *
  GetTargetImage()
  {
    return this->GetInput(1);
  }

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

  /**
   * Pass the input through unmodified. Do this by setting the output to the
   * source this by setting the output to the source image in the
   * AllocateOutputs() method.
   */
  void
  AllocateOutputs() override;

  void
  BeforeThreadedGenerateData() override;

  void
  AfterThreadedGenerateData() override;

  /** Multi-thread version GenerateData. */
  void
  ThreadedGenerateData(const RegionType &, ThreadIdType) override;

  void
  DynamicThreadedGenerateData(const RegionType &) override
  {
    itkExceptionMacro("This class requires threadId so it must use classic multi-threading model");
  }

  // Override since the filter produces all of its output
  void
  EnlargeOutputRequestedRegion(DataObject * data) override;

private:
  std::vector<MapType> m_LabelSetMeasuresPerThread;
  MapType              m_LabelSetMeasures;
}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelOverlapMeasuresImageFilter.hxx"
#endif

#endif
