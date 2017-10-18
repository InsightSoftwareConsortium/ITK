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

#ifndef itkHistogramThresholdImageFilter_h
#define itkHistogramThresholdImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkHistogram.h"
#include "itkHistogramThresholdCalculator.h"

namespace itk
{

/** \class HistogramThresholdImageFilter
 * \brief Threshold an image using a HistogramThresholdCalculator
 *
 * This filter creates a binary thresholded image that separates an
 * image into foreground and background components. The filter
 * computes the threshold using a user provided HistogramThresholdCalculator and
 * applies that threshold to the input image using the
 * BinaryThresholdImageFilter.
 *
 * The filter also has the option of providing a mask, in which case
 * the histogram and therefore the threshold is computed from the
 * parts of the mask with values indicated by MaskValue. The output
 * image is, by default, masked by the same image. This output
 * masking can be disabled using SetMaskOutput(false). Note that there
 * is an inconsistency here. The MaskImageFilter (used internally)
 * masks by non zero values, where as the MaskedImageToHistogramFilter
 * uses explicit values. If this does not match your usage then the
 * output masking will need to be managed by the user.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/10380/3279  or
 * http://www.insight-journal.org/browse/publication/811
 *
 * \ingroup Multithreaded
 * \ingroup ITKThresholding
 */

template<typename TInputImage, typename TOutputImage, typename TMaskImage=TOutputImage>
class ITK_TEMPLATE_EXPORT HistogramThresholdImageFilter :
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard Self typedef */
  typedef HistogramThresholdImageFilter                    Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>     Superclass;
  typedef SmartPointer<Self>                               Pointer;
  typedef SmartPointer<const Self>                         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(HistogramThresholdImageFilter, ImageToImageFilter);

  typedef TInputImage                       InputImageType;
  typedef TOutputImage                      OutputImageType;
  typedef TMaskImage                        MaskImageType;

  /** Image pixel value typedef. */
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename MaskImageType::PixelType    MaskPixelType;

  /** Image related typedefs. */
  typedef typename InputImageType::Pointer  InputImagePointer;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename MaskImageType::Pointer   MaskImagePointer;

  typedef typename InputImageType::SizeType    InputSizeType;
  typedef typename InputImageType::IndexType   InputIndexType;
  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef typename OutputImageType::SizeType   OutputSizeType;
  typedef typename OutputImageType::IndexType  OutputIndexType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename MaskImageType::SizeType     MaskSizeType;
  typedef typename MaskImageType::IndexType    MaskIndexType;
  typedef typename MaskImageType::RegionType   MaskImageRegionType;

   typedef typename NumericTraits< InputPixelType >::ValueType ValueType;
   typedef typename NumericTraits< ValueType >::RealType       ValueRealType;
   typedef Statistics::Histogram< ValueRealType >              HistogramType;
   typedef typename HistogramType::Pointer                     HistogramPointer;
   typedef typename HistogramType::ConstPointer                HistogramConstPointer;
   typedef typename HistogramType::SizeType                    HistogramSizeType;
   typedef typename HistogramType::MeasurementType             HistogramMeasurementType;
   typedef typename HistogramType::MeasurementVectorType       HistogramMeasurementVectorType;
   typedef HistogramThresholdCalculator<HistogramType, InputPixelType>
                                                               CalculatorType;
   typedef typename CalculatorType::Pointer                    CalculatorPointer;

  /** Image related typedefs. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension );
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      OutputImageType::ImageDimension );
  itkStaticConstMacro(MaskImageDimension, unsigned int,
                      MaskImageType::ImageDimension );

  /** Set and Get the mask image */
  itkSetInputMacro(MaskImage, TMaskImage);
  itkGetInputMacro(MaskImage, TMaskImage);

  /** Set the input image */
  void SetInput1(const TInputImage *input)
  {
    this->SetInput(input);
  }

  /** Set the marker image */
  void SetInput2(const TMaskImage *input)
  {
    this->SetMaskImage(input);
  }

  /** Set the "outside" pixel value. The default value
   * NumericTraits<OutputPixelType>::ZeroValue(). */
  itkSetMacro(OutsideValue, OutputPixelType);

  /** Get the "outside" pixel value. */
  itkGetConstMacro(OutsideValue, OutputPixelType);

  /** Set the "inside" pixel value. The default value
   * NumericTraits<OutputPixelType>::max() */
  itkSetMacro(InsideValue, OutputPixelType);

  /** Get the "inside" pixel value. */
  itkGetConstMacro(InsideValue, OutputPixelType);

  /** Set the number of histogram bins */
  itkSetMacro(NumberOfHistogramBins, unsigned int);
  itkGetConstMacro(NumberOfHistogramBins, unsigned int);

  /** Does histogram generator compute min and max from data?
    * Default is true for all but char types */
  itkSetMacro(AutoMinimumMaximum, bool);
  itkGetConstMacro(AutoMinimumMaximum, bool);
  itkBooleanMacro(AutoMinimumMaximum);

  /** Do you want the output to be masked by the mask used in
    * histogram construction. Only relevant if masking is in
    * use. Default is true. */
  itkSetMacro(MaskOutput, bool);
  itkGetConstMacro(MaskOutput, bool);
  itkBooleanMacro(MaskOutput);

  /** The value in the mask image, if used, indicating voxels that
  should be included. Default is the max of pixel type, as in the
  MaskedImageToHistogramFilter */
  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);

  /** Get the computed threshold. */
  itkGetConstMacro(Threshold, InputPixelType);

  /** Set/Get the calculator to use to compute the threshold */
  itkSetObjectMacro(Calculator, CalculatorType);
  itkGetModifiableObjectMacro(Calculator, CalculatorType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck,
    (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<InputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck,
    (Concept::OStreamWritable<OutputPixelType>));
  // End concept checking
#endif

protected:
  HistogramThresholdImageFilter();
  ~HistogramThresholdImageFilter() ITK_OVERRIDE {};
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  void GenerateInputRequestedRegion() ITK_OVERRIDE;
  void GenerateData () ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramThresholdImageFilter);

  OutputPixelType     m_InsideValue;
  OutputPixelType     m_OutsideValue;
  InputPixelType      m_Threshold;
  MaskPixelType       m_MaskValue;
  CalculatorPointer   m_Calculator;
  unsigned            m_NumberOfHistogramBins;
  bool                m_AutoMinimumMaximum;
  bool                m_MaskOutput;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogramThresholdImageFilter.hxx"
#endif

#endif
