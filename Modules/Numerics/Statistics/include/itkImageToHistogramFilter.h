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
#ifndef itkImageToHistogramFilter_h
#define itkImageToHistogramFilter_h

#include "itkHistogram.h"
#include "itkImageTransformer.h"
#include "itkBarrier.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkProgressReporter.h"

namespace itk
{
namespace Statistics
{
/** \class ImageToHistogramFilter
 *  \brief This class generates an histogram from an image.
 *
 *  The concept of Histogram in ITK is quite generic. It has been designed to
 *  manage multiple components data. This class facilitates the computation of
 *  an histogram from an image. Internally it creates a List that is feed into
 *  the SampleToHistogramFilter.
 *
 * \ingroup ITKStatistics
 */

template< typename TImage >
class ITK_TEMPLATE_EXPORT ImageToHistogramFilter:public ImageTransformer<TImage>
{
public:
  /** Standard typedefs */
  typedef ImageToHistogramFilter     Self;
  typedef ImageTransformer<TImage>   Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToHistogramFilter, ImageTransformer);

  /** standard New() method support */
  itkNewMacro(Self);

  typedef TImage                                         ImageType;
  typedef typename ImageType::PixelType                  PixelType;
  typedef typename ImageType::RegionType                 RegionType;
  typedef typename NumericTraits< PixelType >::ValueType ValueType;
  typedef typename NumericTraits< ValueType >::RealType  ValueRealType;

  typedef Histogram< ValueRealType >                    HistogramType;
  typedef typename HistogramType::Pointer               HistogramPointer;
  typedef typename HistogramType::ConstPointer          HistogramConstPointer;
  typedef typename HistogramType::SizeType              HistogramSizeType;
  typedef typename HistogramType::MeasurementType       HistogramMeasurementType;
  typedef typename HistogramType::MeasurementVectorType HistogramMeasurementVectorType;

public:

  /** Return the output histogram. */
  const HistogramType * GetOutput() const;
  HistogramType * GetOutput();

  /** Type of DataObjects to use for Size inputs */
  typedef SimpleDataObjectDecorator<
    HistogramSizeType > InputHistogramSizeObjectType;

  /** Type of DataObjects to use for Marginal Scale inputs */
  typedef SimpleDataObjectDecorator<
    HistogramMeasurementType > InputHistogramMeasurementObjectType;

  /** Type of DataObjects to use for Minimum and Maximums values of the
   * histogram bins. */
  typedef SimpleDataObjectDecorator<
    HistogramMeasurementVectorType > InputHistogramMeasurementVectorObjectType;

  /** Type of DataObjects to use for AutoMinimumMaximum input */
  typedef SimpleDataObjectDecorator< bool > InputBooleanObjectType;

  /** Methods for setting and getting the histogram size.  The histogram size
   * is encapsulated inside a decorator class. For this reason, it is possible
   * to set and get the decorator class, but it is only possible to set the
   * histogram size by value. This macro declares the methods
   * SetHistogramSize(), SetHistogramSizeInput(), GetHistogramSizeInput().
   */
  itkSetGetDecoratedInputMacro(HistogramSize, HistogramSizeType);

  /** Methods for setting and getting the Marginal scale value.  The marginal
   * scale is used when the type of the measurement vector componets are of
   * integer type. */
  itkSetGetDecoratedInputMacro(MarginalScale, HistogramMeasurementType);

  /** Methods for setting and getting the Minimum and Maximum values of the
   * histogram bins. */
  itkSetGetDecoratedInputMacro(HistogramBinMinimum, HistogramMeasurementVectorType);
  itkSetGetDecoratedInputMacro(HistogramBinMaximum, HistogramMeasurementVectorType);

  /** Methods for setting and getting the boolean flag that defines whether the
   * minimum and maximum of the histogram are going to be computed
   * automatically from the values of the sample */
  itkSetGetDecoratedInputMacro(AutoMinimumMaximum, bool);

  /** Method that facilitates the use of this filter in the internal
   * pipeline of another filter. */
  virtual void GraftOutput(DataObject *output);

protected:
  ImageToHistogramFilter();
  virtual ~ImageToHistogramFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void BeforeThreadedGenerateData(void) ITK_OVERRIDE;
  void ThreadedGenerateData(const RegionType & inputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;
  void AfterThreadedGenerateData(void) ITK_OVERRIDE;

  /** Method that construct the outputs */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObject::Pointer  MakeOutput(DataObjectPointerArraySizeType) ITK_OVERRIDE;

  virtual void ThreadedComputeMinimumAndMaximum( const RegionType & inputRegionForThread, ThreadIdType threadId, ProgressReporter & progress );
  virtual void ThreadedComputeHistogram( const RegionType & inputRegionForThread, ThreadIdType threadId, ProgressReporter & progress );

  std::vector< HistogramPointer >               m_Histograms;
  std::vector< HistogramMeasurementVectorType > m_Minimums;
  std::vector< HistogramMeasurementVectorType > m_Maximums;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToHistogramFilter);

  void ApplyMarginalScale( HistogramMeasurementVectorType & min, HistogramMeasurementVectorType & max, HistogramSizeType & size );
  typename Barrier::Pointer                     m_Barrier;

};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToHistogramFilter.hxx"
#endif

#endif
