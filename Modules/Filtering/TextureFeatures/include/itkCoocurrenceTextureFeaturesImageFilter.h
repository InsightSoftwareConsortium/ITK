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
#ifndef itkCoocurrenceTextureFeaturesImageFilter_h
#define itkCoocurrenceTextureFeaturesImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkScalarImageToRunLengthMatrixFilter.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
namespace Statistics
{
/** \class CoocurrenceTextureFeaturesImageFilter
 *  \brief This class computes texture descriptions for each voxel of
 *  a given image and a mask image if provided. The output image can then be
 *  displayed by using colormaps.
 *
 * This filter computes a N-D image where each voxel will contain
 * a vector of up to 8 scalars representing the texture features
 * (of the specified neighborhood) from a N-D scalar image.
 * The texture features are computed for each spatial
 * direction and averaged afterward.
 *
 * This filter use grey-level co-occurence matrix in order to compute  description a la Haralick. (See
 * Haralick, R.M., K. Shanmugam and I. Dinstein. 1973. Textural Features for
 * Image Classification. IEEE Transactions on Systems, Man and Cybernetics.
 * SMC-3(6):610-620. See also Haralick, R.M. 1979. Statistical and Structural
 * Approaches to Texture. Proceedings of the IEEE, 67:786-804.)
 *
 * Template Parameters:
 * -# The input image type: a N dimensional image where the pixel type MUST be integer.
 * -# The output image type: a N dimensional image where the pixel type MUST be a vector of floating points or a
 *VectorImage.
 *
 * Inputs and parameters:
 * -# An image
 * -# A mask defining the region over which texture features will be
 *    calculated. (Optional)
 * -# The pixel value that defines the "inside" of the mask. (Optional, defaults
 *    to 1 if a mask is set.)
 * -# The number of intensity bins. (Optional, defaults to 256.)
 * -# The set of directions (offsets) to average across. (Optional, defaults to
 *    {(-1, 0), (-1, -1), (0, -1), (1, -1)} for 2D images and scales analogously
 *    for ND images.)
 * -# The pixel intensity range over which the features will be calculated.
 *    (Optional, defaults to the full dynamic range of the pixel type.)
 * -# The size of the neighborhood radius. (Optional, defaults to 2.)
 *
 * Recommendations:
 * -# Input image: To improve the computation time, the useful data should take as much
 *    space as possible in the input image. If the useful data is concentrated in one part of
 *    the image a crop step should be considered prior to the usage of this filter.
 * -# Mask: Even if optional, the usage of a mask will greatly improve the computation time.
 * -# Number of intensity bins: The number of bins should be adapted to the type of results expected
 *    and the intensity range. In addition a high number of bins will increase the
 *    computation time.
 * -# Pixel intensity range: For better results the Pixel intensity should be adapted to the input image
 *    intensity range. For example they could be the minimum and maximum intensity of the image, or 0 and
 *    the maximum intensity (if the negative values are considered as noise).
 *
 * WARNING: This probably won't work for pixels of double or long-double type
 * unless you set the histogram min and max manually. This is because the largest
 * histogram bin by default has max value of the largest possible pixel value
 * plus 1. For double and long-double types, whose "RealType" as defined by the
 * NumericTraits class is the same, and thus cannot hold any larger values,
 * this would cause a float overflow.
 *
 * \sa HistogramToTextureFeaturesFilter
 * \sa ScalarImageToCooccurrenceMatrixFilte
 * \sa ScalarImageToTextureFeaturesFilter
 *
 * \author: Jean-Baptiste Vimort
 * \ingroup TextureFeatures
 **/

template <typename TInputImage,
          typename TOutputImage,
          typename TMaskImage = Image<unsigned char, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT CoocurrenceTextureFeaturesImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard type alias */
  using Self = CoocurrenceTextureFeaturesImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CoocurrenceTextureFeaturesImageFilter, ImageToImageFilter);

  /** standard New() method support */
  itkNewMacro(Self);

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using MaskImageType = TMaskImage;

  using PixelType = typename InputImageType::PixelType;
  using MaskPixelType = typename MaskImageType::PixelType;
  using IndexType = typename InputImageType::IndexType;
  using PointType = typename InputImageType::PointType;

  using OffsetType = typename InputImageType::OffsetType;
  using OffsetVector = VectorContainer<unsigned char, OffsetType>;
  using OffsetVectorPointer = typename OffsetVector::Pointer;
  using OffsetVectorConstPointer = typename OffsetVector::ConstPointer;

  using InputRegionType = typename InputImageType::RegionType;
  using OutputRegionType = typename OutputImageType::RegionType;

  using NeighborhoodRadiusType = typename itk::ConstNeighborhoodIterator<InputImageType>::RadiusType;

  using MeasurementType = typename NumericTraits<PixelType>::RealType;
  using RealType = typename NumericTraits<PixelType>::RealType;

  /** Method to set/get the Neighborhood radius */
  itkSetMacro(NeighborhoodRadius, NeighborhoodRadiusType);
  itkGetConstMacro(NeighborhoodRadius, NeighborhoodRadiusType);

  /** Method to set the mask image */
  itkSetInputMacro(MaskImage, MaskImageType);

  /** Method to get the mask image */
  itkGetInputMacro(MaskImage, MaskImageType);


  /** Specify the default number of bins per axis */
  static constexpr unsigned int DefaultBinsPerAxis = 256;

  /**
   * Set the offsets over which the intensities pairs will be computed.
   * Invoking this function clears the previous offsets.
   * Note: for each individual offset in the OffsetVector, the rightmost non-zero
   * offset element must be positive. For example, in the offset list of a 2D image,
   * (1, 0) means the offset  along x-axis. (1, 0) has to be set instead
   * of (-1, 0). This is required from the iterating order of pixel iterator.
   *
   */
  itkSetObjectMacro(Offsets, OffsetVector);

  /**
   * Set offset over which the intensities pairs will be computed.
   * Invoking this function clears the previous offset(s).
   * Note: for each individual offset, the rightmost non-zero
   * offset element must be positive. For example, in the offset list of a 2D image,
   * (1, 0) means the offset  along x-axis. (1, 0) has to be set instead
   * of (-1, 0). This is required from the iterating order of pixel iterator.
   *
   */
  void
  SetOffset(const OffsetType offset);

  /**
   * Get the current offset(s).
   */
  itkGetModifiableObjectMacro(Offsets, OffsetVector);

  /** Set number of histogram bins along each axis */
  itkSetMacro(NumberOfBinsPerAxis, unsigned int);

  /** Get number of histogram bins along each axis */
  itkGetConstMacro(NumberOfBinsPerAxis, unsigned int);

  /** Get the max pixel value defining one dimension of the joint histogram. */
  itkGetConstMacro(HistogramMaximum, PixelType);
  itkSetMacro(HistogramMaximum, PixelType);

  /** Get the min pixel value defining one dimension of the joint histogram. */
  itkGetConstMacro(HistogramMinimum, PixelType);
  itkSetMacro(HistogramMinimum, PixelType);

  /**
   * Set the pixel value of the mask that should be considered "inside" the
   * object. Defaults to 1.
   */
  itkSetMacro(InsidePixelValue, MaskPixelType);
  itkGetConstMacro(InsidePixelValue, MaskPixelType);

  /** Set the calculator to normalize the histogram (divide all bins by the
    total frequency). Normalization is off by default. */
  itkSetMacro(Normalize, bool);
  itkGetConstMacro(Normalize, bool);
  itkBooleanMacro(Normalize);

  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputRealType = typename NumericTraits<OutputPixelType>::ScalarRealType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputPixelTypeCheck, (Concept::IsFloatingPoint<OutputRealType>));
  // End concept checking
#endif

protected:
  using HistogramIndexType = int;
  using DigitizedImageType = itk::Image<HistogramIndexType, TInputImage::ImageDimension>;
  using NeighborhoodIteratorType = typename itk::ConstNeighborhoodIterator<DigitizedImageType>;
  using NeighborIndexType = typename NeighborhoodIteratorType::NeighborIndexType;

  CoocurrenceTextureFeaturesImageFilter();
  ~CoocurrenceTextureFeaturesImageFilter() override {}

  bool
  IsInsideNeighborhood(const OffsetType & iteratedOffset);
  void
  ComputeFeatures(const vnl_matrix<unsigned int> &   hist,
                  const unsigned int                 totalNumberOfFreq,
                  typename TOutputImage::PixelType & outputPixel);
  void
  ComputeMeansAndVariances(const vnl_matrix<unsigned int> & hist,
                           const unsigned int               totalNumberOfFreq,
                           double &                         pixelMean,
                           double &                         marginalMean,
                           double &                         marginalDevSquared,
                           double &                         pixelVariance);
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method causes the filter to generate its output. */
  void
  BeforeThreadedGenerateData() override;
  void
  AfterThreadedGenerateData() override;
  void
  DynamicThreadedGenerateData(const OutputRegionType & outputRegionForThread) override;
  void
  GenerateOutputInformation() override;

private:
  typename DigitizedImageType::Pointer m_DigitizedInputImage;

  NeighborhoodRadiusType m_NeighborhoodRadius;
  OffsetVectorPointer    m_Offsets;
  unsigned int           m_NumberOfBinsPerAxis;
  PixelType              m_HistogramMinimum;
  PixelType              m_HistogramMaximum;
  MaskPixelType          m_InsidePixelValue;
  bool                   m_Normalize;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCoocurrenceTextureFeaturesImageFilter.hxx"
#endif

#endif
