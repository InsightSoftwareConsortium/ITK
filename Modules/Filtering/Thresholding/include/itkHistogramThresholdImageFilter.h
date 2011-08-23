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

#ifndef __itkHistogramThresholdImageFilter_h
#define __itkHistogramThresholdImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkHistogram.h"
#include "itkHistogramThresholdCalculator.h"

namespace itk {

/** \class HistogramThresholdImageFilter
 * \brief Threshold an image using a HistogramThresholdCalculator
 *
 * This filter creates a binary thresholded image that separates an
 * image into foreground and background components. The filter
 * computes the threshold using a user provided HistogramThresholdCalculator and
 * applies that theshold to the input image using the
 * BinaryThresholdImageFilter.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/10380/3279  or
 * http://www.insight-journal.org/browse/publication/811
 *
 * \ingroup Multithreaded
 * \ingroup ITKThresholding
 */

template<class TInputImage, class TOutputImage>
class ITK_EXPORT HistogramThresholdImageFilter :
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

  /** Image pixel value typedef. */
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename OutputImageType::PixelType  OutputPixelType;

  /** Image related typedefs. */
  typedef typename InputImageType::Pointer  InputImagePointer;
  typedef typename OutputImageType::Pointer OutputImagePointer;

  typedef typename InputImageType::SizeType    InputSizeType;
  typedef typename InputImageType::IndexType   InputIndexType;
  typedef typename InputImageType::RegionType  InputImageRegionType;
  typedef typename OutputImageType::SizeType   OutputSizeType;
  typedef typename OutputImageType::IndexType  OutputIndexType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

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

  /** Set the "outside" pixel value. The default value
   * NumericTraits<OutputPixelType>::Zero. */
  itkSetMacro(OutsideValue,OutputPixelType);

  /** Get the "outside" pixel value. */
  itkGetConstMacro(OutsideValue,OutputPixelType);

  /** Set the "inside" pixel value. The default value
   * NumericTraits<OutputPixelType>::max() */
  itkSetMacro(InsideValue,OutputPixelType);

  /** Get the "inside" pixel value. */
  itkGetConstMacro(InsideValue,OutputPixelType);

  /** Set the number of histogram bins */
  itkSetMacro(NumberOfHistogramBins, unsigned int);
  itkGetConstMacro(NumberOfHistogramBins, unsigned int);

  /** Does histogram generator compute min and max from data?
    * Default is false for all but char types */
  itkSetMacro(AutoMinimumMaximum, bool);
  itkGetConstMacro(AutoMinimumMaximum, bool);
  itkBooleanMacro(AutoMinimumMaximum);

  /** Get the computed threshold. */
  itkGetConstMacro(Threshold,InputPixelType);

  /** Set/Get the calculator to use to compute the threshold */
  itkSetObjectMacro(Calculator, CalculatorType);
  itkGetObjectMacro(Calculator, CalculatorType);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(OutputEqualityComparableCheck,
    (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<InputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck,
    (Concept::OStreamWritable<OutputPixelType>));
  /** End concept checking */
#endif
protected:
  HistogramThresholdImageFilter();
  ~HistogramThresholdImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateInputRequestedRegion();
  void GenerateData ();

private:
  HistogramThresholdImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  OutputPixelType     m_InsideValue;
  OutputPixelType     m_OutsideValue;
  InputPixelType      m_Threshold;
  CalculatorPointer   m_Calculator;
  unsigned            m_NumberOfHistogramBins;
  bool                m_AutoMinimumMaximum;
}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogramThresholdImageFilter.hxx"
#endif

#endif
