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
#ifndef itkScalarImageToRunLengthFeaturesImageFilter_h
#define itkScalarImageToRunLengthFeaturesImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegion.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkHistogramToRunLengthFeaturesFilter.h"
#include "itkScalarImageToRunLengthMatrixFilter.h"

namespace itk
{
namespace Statistics
{
/** \class ScalarImageToRunLengthFeaturesImageFilter
 *  \brief This class computes colormaps thanks to run length descriptors from an image.
 *
 *
 * \sa ScalarImageToRunLengthFeaturesFilter
 * \sa ScalarImageToRunLengthMatrixFilter
 * \sa HistogramToRunLengthFeaturesFilter
 *
 * \author: Vimort Jean-Baptiste
 * \ingroup ITKStatistics
 */

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer = DenseFrequencyContainer2>
class ITK_TEMPLATE_EXPORT ScalarImageToRunLengthFeaturesImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard typedefs */
  typedef ScalarImageToRunLengthFeaturesImageFilter     Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarImageToRunLengthFeaturesImageFilter, ImageToImageFilter);

  /** standard New() method support */
  itkNewMacro(Self);

  typedef TInputImage                       InputImageType;
  typedef typename InputImageType::Pointer  InputImagePointer;
  typedef TOutputImage                      OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef THistogramFrequencyContainer      FrequencyContainerType;

  typedef typename InputImageType::PixelType         PixelType;
  typedef typename InputImageType::OffsetType        OffsetType;
  typedef VectorContainer<unsigned char, OffsetType> OffsetVector;
  typedef typename OffsetVector::Pointer             OffsetVectorPointer;
  typedef typename OffsetVector::ConstPointer        OffsetVectorConstPointer;

  typedef typename InputImageType::RegionType InputRegionType;
  typedef typename InputRegionType::IndexType InputRegionIndexType;
  typedef typename InputRegionType::SizeType  InputRegionSizeType;

  typedef typename OutputImageType::RegionType OutputRegionType;
  typedef typename OutputRegionType::IndexType OutputRegionIndexType;
  typedef typename OutputRegionType::SizeType  OutputRegionSizeType;

  typedef typename itk::ConstNeighborhoodIterator<InputImageType> NeighborhoodIteratorType;
  typedef typename NeighborhoodIteratorType::RadiusType           NeighborhoodRadiusType;


  typedef ScalarImageToRunLengthMatrixFilter<InputImageType, FrequencyContainerType> RunLengthMatrixFilterType;
  typedef typename RunLengthMatrixFilterType::HistogramType                          HistogramType;
  typedef HistogramToRunLengthFeaturesFilter<HistogramType>                          RunLengthFeaturesFilterType;

  typedef short                                                RunLengthFeatureName;
  typedef VectorContainer<unsigned char, RunLengthFeatureName> FeatureNameVector;
  typedef typename FeatureNameVector::Pointer                  FeatureNameVectorPointer;
  typedef typename FeatureNameVector::ConstPointer             FeatureNameVectorConstPointer;
  typedef VectorContainer<unsigned char, double>               FeatureValueVector;
  typedef typename FeatureValueVector::Pointer                 FeatureValueVectorPointer;

  /** Connects the input image for which the features are going to be computed
   */
  using Superclass::SetInput;
  void
  SetInput(const InputImageType *);

  const InputImageType *
  GetInput() const;

  /** Return the feature means and deviations.  */
  itkGetConstReferenceObjectMacro(FeatureMeans, FeatureValueVector);
  itkGetConstReferenceObjectMacro(FeatureStandardDeviations, FeatureValueVector);

  /** Set the desired feature set. Optional, for default value see above. */
  itkSetConstObjectMacro(RequestedFeatures, FeatureNameVector);
  itkGetConstObjectMacro(RequestedFeatures, FeatureNameVector);

  /** Set the  offsets over which the co-occurrence pairs will be computed.
      Optional; for default value see above. */
  itkSetConstObjectMacro(Offsets, OffsetVector);
  itkGetConstObjectMacro(Offsets, OffsetVector);

  itkSetMacro(NeighborhoodRadius, NeighborhoodRadiusType);
  itkGetConstMacro(NeighborhoodRadius, NeighborhoodRadiusType);

  /** Set number of histogram bins along each axis.
      Optional; for default value see above. */
  void
  SetNumberOfBinsPerAxis(unsigned int);

  /** Set the min and max (inclusive) pixel value that will be used for
      feature calculations. Optional; for default value see above. */
  void
  SetPixelValueMinMax(PixelType min, PixelType max);

  /** Set the min and max (inclusive) pixel value that will be used for
      feature calculations. Optional; for default value see above. */
  void
  SetDistanceValueMinMax(double min, double max);

  /** Connects the mask image for which the histogram is going to be computed.
      Optional; for default value see above. */
  void
  SetMaskImage(const InputImageType *);

  const InputImageType *
  GetMaskImage() const;

  /** Set the pixel value of the mask that should be considered "inside" the
      object. Optional; for default value see above. */
  void
  SetInsidePixelValue(PixelType InsidePixelValue);

  itkGetConstMacro(FastCalculations, bool);
  itkSetMacro(FastCalculations, bool);
  itkBooleanMacro(FastCalculations);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputPixelTypeCheck, (Concept::IsInteger<typename InputImageType::PixelType>));
  itkConceptMacro(OutputPixelTypeCheck, (Concept::IsFloatingPoint<typename OutputImageType::PixelType>));
  // End concept checking
#endif

protected:
  ScalarImageToRunLengthFeaturesImageFilter();
  virtual ~ScalarImageToRunLengthFeaturesImageFilter() {}
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This method causes the filter to generate its output. */
  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;
  virtual void
  ThreadedGenerateData(const OutputRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;
  virtual void
  GenerateOutputInformation() ITK_OVERRIDE;

private:
  typename RunLengthMatrixFilterType::Pointer m_RunLengthMatrixGenerator;

  NeighborhoodRadiusType        m_NeighborhoodRadius;
  FeatureValueVectorPointer     m_FeatureMeans;
  FeatureValueVectorPointer     m_FeatureStandardDeviations;
  FeatureNameVectorConstPointer m_RequestedFeatures;
  OffsetVectorConstPointer      m_Offsets;
  bool                          m_FastCalculations;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarImageToRunLengthFeaturesImageFilter.hxx"
#endif

#endif
