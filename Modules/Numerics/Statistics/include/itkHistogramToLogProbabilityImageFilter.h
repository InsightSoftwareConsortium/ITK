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
#ifndef __itkHistogramToLogProbabilityImageFilter_h
#define __itkHistogramToLogProbabilityImageFilter_h

#include "itkHistogramToImageFilter.h"

namespace itk
{
/** \class HistogramToLogProbabilityImageFilter
 * \brief The class takes a histogram as an input and gives the log probability
 * image as the output. A pixel, at position I,  in the output image is given by
 *
 * \f[
 * f(I) = \log_2( \frac{q_I}{\sum_{i \in I} q_I} )
 * \f]
 *  where \f$q_I\f$ is the frequency of measurement vector, I.
 *
 * This is the log of the  frequency of a measurement vector by the sum of all
 * frequencies.
 *
 * The output image is of type double.
 *
 * This is useful in plotting the joint histograms during registration.
 *
 *  \sa HistogramToImageFilter, HistogramToProbabilityImageFilter,
 *  HistogramToIntensityImageFilter, HistogramToEntropyImageFilter
 *
 * \ingroup ITKStatistics
 */

namespace Function
{
template< class TInput, class TOutput = double >
class HistogramLogProbabilityFunction
{
public:

  //Probability function = Number of occurrences in each bin /
  //   Total Number of occurrences.
  //
  // Returns pixels of float..
  typedef  TOutput OutputPixelType;

  HistogramLogProbabilityFunction():
    m_TotalFrequency(1) {}

  ~HistogramLogProbabilityFunction() {}

  inline OutputPixelType operator()(const TInput & A) const
  {
    if ( A )
      {
      return static_cast< OutputPixelType >( vcl_log( static_cast< OutputPixelType >( A )
                                                      / static_cast< OutputPixelType >( m_TotalFrequency ) )
                                             / vcl_log(2.0) );
      }
    else
      {   // Check for Log 0. Always assume that the frequency is atleast 1.
      return static_cast< OutputPixelType >( vcl_log( static_cast< OutputPixelType >( A + 1 )
                                                      / static_cast< OutputPixelType >( m_TotalFrequency ) )
                                             / vcl_log(2.0) );
      }
  }

  void SetTotalFrequency(SizeValueType n)
  {
    m_TotalFrequency = n;
  }

  SizeValueType GetTotalFrequency() const
  {
    return m_TotalFrequency;
  }

private:
  SizeValueType m_TotalFrequency;
};
}

template< class THistogram, class TImage=Image< double, 3 > >
class HistogramToLogProbabilityImageFilter:
  public HistogramToImageFilter< THistogram, TImage,
                                 Function::HistogramLogProbabilityFunction< SizeValueType, typename TImage::PixelType > >
{
public:

  /** Standard class typedefs. */
  typedef HistogramToLogProbabilityImageFilter Self;

  /** Standard "Superclass" typedef. */
  typedef HistogramToImageFilter< THistogram, TImage,
                                 Function::HistogramLogProbabilityFunction< SizeValueType, typename TImage::PixelType > >
  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(HistogramToLogProbabilityImageFilter, HistogramToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

protected:
  HistogramToLogProbabilityImageFilter() {}
  virtual ~HistogramToLogProbabilityImageFilter() {}

private:
  HistogramToLogProbabilityImageFilter(const Self &); //purposely not
                                                      // implemented
  void operator=(const Self &);                       //purposely not
                                                      // implemented
};
} // end namespace itk

#endif
