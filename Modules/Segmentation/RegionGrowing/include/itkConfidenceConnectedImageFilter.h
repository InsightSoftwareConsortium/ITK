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
#ifndef itkConfidenceConnectedImageFilter_h
#define itkConfidenceConnectedImageFilter_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class ConfidenceConnectedImageFilter
 * \brief Segment pixels with similar statistics using connectivity
 *
 * This filter extracts a connected set of pixels whose pixel
 * intensities are consistent with the pixel statistics of a seed
 * point. The mean and variance across a neighborhood (8-connected,
 * 26-connected, etc.) are calculated for a seed point.  Then
 * pixels connected to this seed point whose values are within
 * the confidence interval for the seed point are grouped. The
 * width of the confidence interval is controlled by the "Multiplier"
 * variable (the confidence interval is the mean plus or minus
 * the "Multiplier" times the standard deviation). If the intensity
 * variations across a segment were gaussian, a "Multiplier" setting
 * of 2.5 would define a confidence interval wide enough to capture
 * 99% of samples in the segment.
 *
 * After this initial segmentation is calculated, the mean and
 * variance are re-calculated. All the pixels in the previous
 * segmentation are used to calculate the mean the standard deviation
 * (as opposed to using the pixels in the neighborhood of the seed
 * point).  The segmentation is then recalculated using these refined
 * estimates for the mean and variance of the pixel values.  This
 * process is repeated for the specified number of iterations.
 * Setting the "NumberOfIterations" to zero stops the algorithm
 * after the initial segmentation from the seed point.
 *
 * NOTE: the lower and upper threshold are restricted to lie within the
 * valid numeric limits of the input data pixel type. Also, the limits
 * may be adjusted to contain the seed point's intensity.
 * \ingroup RegionGrowingSegmentation
 * \ingroup ITKRegionGrowing
 *
 * \sphinx
 * \sphinxexample{Segmentation/RegionGrowing/SegmentPixelsWithSimilarStats,SegmentPixelsWithSimilarStats}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ConfidenceConnectedImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConfidenceConnectedImageFilter);

  /** Standard class type aliases. */
  using Self = ConfidenceConnectedImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).  */
  itkTypeMacro(ConfidenceConnectedImageFilter, ImageToImageFilter);

  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using IndexType = typename InputImageType::IndexType;
  using SizeType = typename InputImageType::SizeType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using SeedsContainerType = std::vector<IndexType>;

  using InputRealType = typename NumericTraits<InputImagePixelType>::RealType;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Set seed point. This method is deprecated, please use AddSeed() */
  void
  SetSeed(const IndexType & seed);

  /** Clear all the seeds. */
  void
  ClearSeeds();

  /** Add seed point. */
  void
  AddSeed(const IndexType & seed);

  /** Set/Get the multiplier to define the confidence interval.  Multiplier
   * can be anything greater than zero. A typical value is 2.5 */
  itkSetMacro(Multiplier, double);
  itkGetConstMacro(Multiplier, double);

  /** Set/Get the number of iterations */
  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstMacro(NumberOfIterations, unsigned int);

  /** Set/Get value to replace thresholded pixels */
  itkSetMacro(ReplaceValue, OutputImagePixelType);
  itkGetConstMacro(ReplaceValue, OutputImagePixelType);

  /** Get/Set the radius of the neighborhood over which the
      statistics are evaluated */
  itkSetMacro(InitialNeighborhoodRadius, unsigned int);
  itkGetConstReferenceMacro(InitialNeighborhoodRadius, unsigned int);

  /** Method to get access to the mean of the pixels accepted in the output
   * region.  This method should only be invoked after the filter has been
   * executed using the Update() method. */
  itkGetConstReferenceMacro(Mean, InputRealType);

  /** Method to get access to the variance of the pixels accepted in the output
   * region.  This method should only be invoked after the filter has been
   * executed using the Update() method. */
  itkGetConstReferenceMacro(Variance, InputRealType);

  /** Method to access seed container */
  virtual const SeedsContainerType &
  GetSeeds() const;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputImagePixelType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputImagePixelType>));
  // End concept checking
#endif

protected:
  ConfidenceConnectedImageFilter();
  ~ConfidenceConnectedImageFilter() override = default;

  // Override since the filter needs all the data for the algorithm
  void
  GenerateInputRequestedRegion() override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  void
  GenerateData() override;

private:
  SeedsContainerType   m_Seeds;
  double               m_Multiplier;
  unsigned int         m_NumberOfIterations;
  OutputImagePixelType m_ReplaceValue;
  unsigned int         m_InitialNeighborhoodRadius;
  InputRealType        m_Mean;
  InputRealType        m_Variance;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConfidenceConnectedImageFilter.hxx"
#endif

#endif
