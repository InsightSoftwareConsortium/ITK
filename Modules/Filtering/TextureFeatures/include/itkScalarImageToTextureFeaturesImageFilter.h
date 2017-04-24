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
#ifndef itkScalarImageToTextureFeaturesImageFilter_h
#define itkScalarImageToTextureFeaturesImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkScalarImageToRunLengthMatrixFilter.h"

namespace itk
{
namespace Statistics
{
/** \class ScalarImageToTextureFeaturesImageFilter
 *  \brief
 *
 * \author: Jean-Baptiste Vimort
 * \ingroup TextureFeatures
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ScalarImageToTextureFeaturesImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard typedefs */
  typedef ScalarImageToTextureFeaturesImageFilter       Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarImageToTextureFeaturesImageFilter, ImageToImageFilter);

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
   * Set the pixel value of the mask that should be considered "inside" the
   * object. Defaults to 1.
   */
  itkSetMacro(InsidePixelValue, PixelType);
  itkGetConstMacro(InsidePixelValue, PixelType);

  /** Set the calculator to normalize the histogram (divide all bins by the
    total frequency). Normalization is off by default. */
  itkSetMacro(Normalize, bool);
  itkGetConstMacro(Normalize, bool);
  itkBooleanMacro(Normalize);

  typedef typename OutputImageType::PixelType                     OutputPixelType;
  typedef typename NumericTraits<OutputPixelType>::ScalarRealType OutputRealType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputPixelTypeCheck, (Concept::IsInteger<typename InputImageType::PixelType>));
  itkConceptMacro(OutputPixelTypeCheck, (Concept::IsFloatingPoint<OutputRealType>));
  // End concept checking
#endif

protected:
  ScalarImageToTextureFeaturesImageFilter();
  virtual ~ScalarImageToTextureFeaturesImageFilter() {}

  bool
  IsInsideNeighborhood(const OffsetType & iteratedOffset);
  void
  ComputeFeatures(unsigned int **                    hist,
                  const unsigned int &               totalNumberOfFreq,
                  typename TOutputImage::PixelType & outputPixel);
  void
  ComputeMeansAndVariances(unsigned int **      hist,
                           const unsigned int & totalNumberOfFreq,
                           double &             pixelMean,
                           double &             marginalMean,
                           double &             marginalDevSquared,
                           double &             pixelVariance);
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
  PixelType                         m_InsidePixelValue;
  typename TInputImage::SpacingType m_Spacing;
  bool                              m_Normalize;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarImageToTextureFeaturesImageFilter.hxx"
#endif

#endif
