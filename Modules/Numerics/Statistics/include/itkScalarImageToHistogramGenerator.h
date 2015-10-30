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
#ifndef itkScalarImageToHistogramGenerator_h
#define itkScalarImageToHistogramGenerator_h

#include "itkImageToListSampleAdaptor.h"
#include "itkSampleToHistogramFilter.h"
#include "itkHistogram.h"
#include "itkObject.h"

namespace itk
{
namespace Statistics
{
/** \class ScalarImageToHistogramGenerator
 *
 * \brief TODO
 * \ingroup ITKStatistics
 */
template< typename TImageType >
class ScalarImageToHistogramGenerator:public Object
{
public:
  /** Standard typedefs */
  typedef ScalarImageToHistogramGenerator Self;
  typedef Object                          Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarImageToHistogramGenerator, Object);

  /** standard New() method support */
  itkNewMacro(Self);

  typedef TImageType                                             ImageType;
  typedef itk::Statistics::ImageToListSampleAdaptor< ImageType > AdaptorType;
  typedef typename AdaptorType::Pointer                          AdaptorPointer;
  typedef typename ImageType::PixelType                          PixelType;
  typedef typename NumericTraits< PixelType >::RealType          RealPixelType;

  typedef itk::Statistics::Histogram< double >                                   HistogramType;
  typedef itk::Statistics::SampleToHistogramFilter< AdaptorType, HistogramType > GeneratorType;

  typedef typename GeneratorType::Pointer GeneratorPointer;

  typedef typename HistogramType::Pointer      HistogramPointer;
  typedef typename HistogramType::ConstPointer HistogramConstPointer;

public:

  /** Triggers the Computation of the histogram */
  void Compute();

  /** Connects the input image for which the histogram is going to be computed
    */
  void SetInput(const ImageType *);

  /** Return the histogram. o
   \warning This output is only valid after the Compute() method has been invoked
   \sa Compute */
  const HistogramType * GetOutput() const;

  /** Set number of histogram bins */
  void SetNumberOfBins(unsigned int numberOfBins);

  /** Set marginal scale value to be passed to the histogram generator */
  void SetMarginalScale(double marginalScale);

  /** Set the minimum value from which the bins will be computed */
  void SetHistogramMin(RealPixelType minimumValue);

  /** Set the maximum value from which the bins will be computed */
  void SetHistogramMax(RealPixelType maximumValue);

protected:
  ScalarImageToHistogramGenerator();
  virtual ~ScalarImageToHistogramGenerator() {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  AdaptorPointer m_ImageToListSampleAdaptor;

  GeneratorPointer m_HistogramGenerator;

  ScalarImageToHistogramGenerator(const Self &) ITK_DELETE_FUNCTION;
  void operator=(const Self &) ITK_DELETE_FUNCTION;
};
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarImageToHistogramGenerator.hxx"
#endif

#endif
