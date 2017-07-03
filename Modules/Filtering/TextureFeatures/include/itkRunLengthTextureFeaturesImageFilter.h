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
#ifndef itkRunLengthTextureFeaturesImageFilter_h
#define itkRunLengthTextureFeaturesImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkScalarImageToRunLengthMatrixFilter.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
namespace Statistics
{
/** \class RunLengthTextureFeaturesImageFilter
 *  \brief This class computes run length features for each voxel of
 *  a given image and a mask image if provided. The output image can then be
 *  displayed by using colormaps.
 *
 * This filter computes a N-D image where each voxel will contain
 * a vector of up to 10 scalars representing the run length features
 * (of the specified neighborhood) from a N-D scalar image.
 * The run length features are computed for each spatial
 * direction and averaged afterward.
 * The result obtained is a possible texture description. See the following references.
 * M. M. Galloway. Texture analysis using gray level run lengths. Computer
 * Graphics and Image Processing, 4:172-179, 1975.
 *
 * A. Chu, C. M. Sehgal, and J. F. Greenleaf. Use of gray value distribution of
 * run lengths for texture analysis.  Pattern Recognition Letters, 11:415-420,
 * 1990.
 *
 * B. R. Dasarathy and E. B. Holder. Image characterizations based on joint
 * gray-level run-length distributions. Pattern Recognition Letters, 12:490-502,
 * 1991.
 *
 * Template Parameters:
 * -# The input image type: a N dimensional image where the pixel type MUST be integer.
 * -# The output image type: a N dimensional image where the pixel type MUST be a vector of floating points or an
 * ImageVector.
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
 * -# The distance range over which the features will be calculated.
 *    (Optional, defaults to the full dynamic range of double type.)
 * -# The size of the neighborhood radius. (Optional, defaults to 2.)
 *
 * Recommendations:
 * -# Input image: To improve the computation time, the useful data should take as much
 *    space as possible in the input image. If the useful data is concentrated in one part of
 *    the image a crop step should be considered prior to the usage of this filter.
 * -# Mask: Even if optional, the usage of a mask will greatly improve the computation time.
 * -# Number of intensity bins: The number of bins should be adapted to the type of results expected
 *    and the intensity and distances ranges. In addition a high number of bins will increase the
 *    computation time.
 * -# Pixel intensity range: For better results the Pixel intensity should be adapted to the input image
 *    intensity range. For example they could be the minimum and maximum intensity of the image, or 0 and
 *    the maximum intensity (if the negative values are considered as noise).
 * -# Distance range: For better results the distance range should be adapted to the spacing of the input image
 *    and the size of the neighborhood.
 *
 * \sa ScalarImageToRunLengthFeaturesFilter
 * \sa ScalarImageToRunLengthMatrixFilter
 * \sa HistogramToRunLengthFeaturesFilter
 *
 * \author: Jean-Baptiste Vimort
 * \ingroup TextureFeatures
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT RunLengthTextureFeaturesImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard typedefs */
  typedef RunLengthTextureFeaturesImageFilter           Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RunLengthTextureFeaturesImageFilter, ImageToImageFilter);

  /** standard New() method support */
  itkNewMacro(Self);

  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  typedef typename InputImageType::PixelType PixelType;
  typedef typename InputImageType::IndexType IndexType;
  typedef typename InputImageType::PointType PointType;

  typedef typename InputImageType::OffsetType        OffsetType;
  typedef VectorContainer<unsigned char, OffsetType> OffsetVector;
  typedef typename OffsetVector::Pointer             OffsetVectorPointer;
  typedef typename OffsetVector::ConstPointer        OffsetVectorConstPointer;

  typedef typename InputImageType::RegionType  InputRegionType;
  typedef typename OutputImageType::RegionType OutputRegionType;

  typedef typename itk::ConstNeighborhoodIterator<InputImageType> NeighborhoodIteratorType;
  typedef typename NeighborhoodIteratorType::RadiusType           NeighborhoodRadiusType;
  typedef typename NeighborhoodIteratorType::NeighborIndexType    NeighborIndexType;

  typedef typename NumericTraits<PixelType>::RealType MeasurementType;
  typedef typename NumericTraits<PixelType>::RealType RealType;

  /** Method to set/get the Neighborhood radius */
  itkSetMacro(NeighborhoodRadius, NeighborhoodRadiusType);
  itkGetConstMacro(NeighborhoodRadius, NeighborhoodRadiusType);

  /** Method to set the mask image */
  void
  SetMaskImage(const InputImageType * image);

  /** Method to get the mask image */
  const InputImageType *
  GetMaskImage() const;

  /** Specify the default number of bins per axis */
  itkStaticConstMacro(DefaultBinsPerAxis, unsigned int, 256);

  /**
   * Set the offsets over which the intensity/distance pairs will be computed.
   * Invoking this function clears the previous offsets.
   * Note: for each individual offset in the OffsetVector, the rightmost non-zero
   * offset element must be positive. For example, in the offset list of a 2D image,
   * (1, 0) means the offset  along x-axis. (1, 0) has to be set instead
   * of (-1, 0). This is required from the iterating order of pixel iterator.
   *
   */
  itkSetObjectMacro(Offsets, OffsetVector);

  /**
   * Set offset over which the intensity/distance pairs will be computed.
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

  /**
   * Set the min and max (inclusive) pixel value that will be used in
   * generating the histogram.
   */
  void
  SetPixelValueMinMax(PixelType min, PixelType max);

  /** Get the min pixel value defining one dimension of the joint histogram. */
  itkGetConstMacro(Min, PixelType);

  /** Get the max pixel value defining one dimension of the joint histogram. */
  itkGetConstMacro(Max, PixelType);

  /**
   * Set the min and max (inclusive) pixel value that will be used in
   * generating the histogram.
   */
  void
  SetDistanceValueMinMax(RealType min, RealType max);

  /**
   * Get the min distance value defining one dimension of the joint histogram.
   */
  itkGetConstMacro(MinDistance, RealType);

  /**
   * Get the max distance value defining one dimension of the joint histogram.
   */
  itkGetConstMacro(MaxDistance, RealType);

  /**
   * Set the pixel value of the mask that should be considered "inside" the
   * object. Defaults to 1.
   */
  itkSetMacro(InsidePixelValue, PixelType);
  itkGetConstMacro(InsidePixelValue, PixelType);

  typedef typename OutputImageType::PixelType                     OutputPixelType;
  typedef typename NumericTraits<OutputPixelType>::ScalarRealType OutputRealType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputPixelTypeCheck, (Concept::IsInteger<typename InputImageType::PixelType>));
  itkConceptMacro(OutputPixelTypeCheck, (Concept::IsFloatingPoint<OutputRealType>));
  // End concept checking
#endif

protected:
  RunLengthTextureFeaturesImageFilter();
  virtual ~RunLengthTextureFeaturesImageFilter() {}

  void
  NormalizeOffsetDirection(OffsetType & offset);
  bool
  IsInsideNeighborhood(const OffsetType & iteratedOffset);
  void
  IncreaseHistograme(unsigned int **      hist,
                     unsigned int &       totalNumberOfRuns,
                     const PixelType &    curentInNeighborhoodPixelIntensity,
                     const OffsetType &   offset,
                     const unsigned int & pixelDistance);
  void
  ComputeFeatures(unsigned int **                    hist,
                  const unsigned int &               totalNumberOfRuns,
                  typename TOutputImage::PixelType & outputPixel);
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This method causes the filter to generate its output. */
  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;
  virtual void
  ThreadedGenerateData(const OutputRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;
  virtual void
  UpdateOutputInformation() ITK_OVERRIDE;

private:
  typename InputImageType::Pointer  m_DigitalisedInputImageg;
  NeighborhoodRadiusType            m_NeighborhoodRadius;
  OffsetVectorPointer               m_Offsets;
  unsigned int                      m_NumberOfBinsPerAxis;
  PixelType                         m_Min;
  PixelType                         m_Max;
  RealType                          m_MinDistance;
  RealType                          m_MaxDistance;
  PixelType                         m_InsidePixelValue;
  typename TInputImage::SpacingType m_Spacing;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRunLengthTextureFeaturesImageFilter.hxx"
#endif

#endif
