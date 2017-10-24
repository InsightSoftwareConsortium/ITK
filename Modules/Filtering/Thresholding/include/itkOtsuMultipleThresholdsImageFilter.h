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
#ifndef itkOtsuMultipleThresholdsImageFilter_h
#define itkOtsuMultipleThresholdsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"
#include "itkOtsuMultipleThresholdsCalculator.h"
#include "itkScalarImageToHistogramGenerator.h"

namespace itk
{
/** \class OtsuMultipleThresholdsImageFilter
 * \brief Threshold an image using multiple Otsu Thresholds.
 *
 * This filter creates a labeled image that separates the input
 * image into various classes. The filter
 * computes the thresholds using the OtsuMultipleThresholdsCalculator and
 * applies those thresholds to the input image using the
 * ThresholdLabelerImageFilter. The NumberOfHistogramBins and
 * NumberOfThresholds can be set
 * for the Calculator. The LabelOffset can be set
 * for the ThresholdLabelerImageFilter.
 *
 * This filter also includes an option to use the valley emphasis algorithm from
 * H.F. Ng, "Automatic thresholding for defect detection", Pattern Recognition Letters, (27): 1644-1649, 2006.
 * The valley emphasis algorithm is particularly effective when the object to be thresholded is small.
 * See the following tests for examples:
 * itkOtsuMultipleThresholdsImageFilterTest3 and itkOtsuMultipleThresholdsImageFilterTest4
 * To use this algorithm, simple call the setter: SetValleyEmphasis(true)
 * It is turned off by default.
 *
 * \sa ScalarImageToHistogramGenerator
 * \sa OtsuMultipleThresholdsCalculator
 * \sa ThresholdLabelerImageFilter
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKThresholding
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT OtsuMultipleThresholdsImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef OtsuMultipleThresholdsImageFilter               Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(OtsuMultipleThresholdsImageFilter, ImageToImageFilter);

  /** Image pixel value typedef. */
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

  /** Threshold vector types. */
  typedef itk::Statistics::ScalarImageToHistogramGenerator< TInputImage > HistogramGeneratorType;
  typedef typename HistogramGeneratorType::HistogramType                  HistogramType;
  typedef OtsuMultipleThresholdsCalculator< HistogramType >               OtsuCalculatorType;
  typedef typename OtsuCalculatorType::OutputType                         ThresholdVectorType;

  /** Image related typedefs. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Set/Get the number of histogram bins. Default is 128. */
  itkSetClampMacro( NumberOfHistogramBins, SizeValueType, 1, NumericTraits< SizeValueType >::max() );
  itkGetConstMacro(NumberOfHistogramBins, SizeValueType);

  /** Set/Get the number of thresholds. Default is 1. */
  itkSetClampMacro( NumberOfThresholds, SizeValueType, 1, NumericTraits< SizeValueType >::max() );
  itkGetConstMacro(NumberOfThresholds, SizeValueType);

  /** Set/Get the offset which labels have to start from. Default is 0. */
  itkSetClampMacro( LabelOffset, OutputPixelType, NumericTraits< OutputPixelType >::ZeroValue(),
                    NumericTraits< OutputPixelType >::max() );
  itkGetConstMacro(LabelOffset, OutputPixelType);

  /** Set/Get the use of valley emphasis. Default is false. */
  itkSetMacro(ValleyEmphasis, bool);
  itkGetConstReferenceMacro(ValleyEmphasis, bool);
  itkBooleanMacro(ValleyEmphasis);

  /** Get the computed threshold. */
  const ThresholdVectorType & GetThresholds() const
  {
    return m_Thresholds;
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputComparableCheck,
                   ( Concept::Comparable< OutputPixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< OutputPixelType > ) );
  // End concept checking
#endif

protected:
  OtsuMultipleThresholdsImageFilter();
  ~OtsuMultipleThresholdsImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(OtsuMultipleThresholdsImageFilter);

  SizeValueType       m_NumberOfHistogramBins;
  SizeValueType       m_NumberOfThresholds;
  OutputPixelType     m_LabelOffset;
  ThresholdVectorType m_Thresholds;
  bool                m_ValleyEmphasis;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOtsuMultipleThresholdsImageFilter.hxx"
#endif

#endif
