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
#ifndef itkHistogramToImageFilter_h
#define itkHistogramToImageFilter_h

#include "itkImageSource.h"
#include "itkConceptChecking.h"
#include "itkHistogram.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/** \class HistogramToImageFilter
 *  \brief This class takes a histogram as an input and returns an image of
 *  type specified by the functor.
 *
 *  The dimension of the image is equal to the size of each measurement
 *  vector of the histogram. The size in the image along each dimension will be
 *  equal to the number of bins along each dimension of the histogram.
 *
 *  The filter may be used in registration methods to plot the joint histogram
 *  after every iteration. A functor is used since it is customary to plot
 *  p log p    where p is the probability of each measurement vector
 *  p is given by Number of occurrences of the measurement vector / total number
 *  of occurrences of all measurement vectors.
 *
 *  \sa HistogramToProbabilityImageFilter, HistogramToLogProbabilityImageFilter,
 *  HistogramToIntensityImageFilter, HistogramToEntropyImageFilter
 *
 * \ingroup ITKStatistics
 */

template< typename THistogram, typename TImage, typename TFunction >
class ITK_TEMPLATE_EXPORT HistogramToImageFilter:
  public ImageSource< TImage >
{
public:

  /** Standard class typedefs. */
  typedef TFunction                                           FunctorType;
  typedef HistogramToImageFilter                              Self;
  typedef ImageSource< TImage >                               Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  typedef TImage                                              OutputImageType;
  typedef typename Superclass::Pointer                        OutputImagePointer;
  typedef typename OutputImageType::SpacingType               SpacingType;
  typedef typename OutputImageType::PointType                 PointType;
  typedef typename OutputImageType::PixelType                 OutputPixelType;

  // Define an iterator to iterate through the image
  typedef itk::ImageRegionIteratorWithIndex< OutputImageType > ImageIteratorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(HistogramToImageFilter, ImageSource);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef THistogram                                    HistogramType;
  typedef typename HistogramType::MeasurementVectorType MeasurementVectorType;
  typedef typename HistogramType::SizeType              HistogramSizeType;
  typedef typename OutputImageType::SizeType            SizeType;

  /** Determine the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, OutputImageType::ImageDimension);

  /** Set/Get the input of this process object.  */
  using Superclass::SetInput;
  virtual void SetInput(const HistogramType *histogram);

  const HistogramType * GetInput();

  /** Set the functor object.  This replaces the current Functor with a
   * copy of the specified Functor. This allows the user to specify a
   * functor that has ivars set differently than the default functor.
   * This method requires an operator!=() be defined on the functor
   * (or the compiler's default implementation of operator!=() being
   * appropriate). */
  void SetFunctor(const FunctorType & functor)
  {
    m_Functor = functor;
    this->Modified();
  }

  /** Get the functor object.  The functor is returned by reference.
   * (Functors do not have to derive from itk::LightObject, so they do
   * not necessarily have a reference count. So we cannot return a
   * SmartPointer.) */
  FunctorType & GetFunctor() { return m_Functor; }
  const FunctorType & GetFunctor() const { return m_Functor; }

  void SetTotalFrequency(SizeValueType n);

protected:
  HistogramToImageFilter();
  ~HistogramToImageFilter() ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  FunctorType m_Functor;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramToImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogramToImageFilter.hxx"
#endif

#endif
