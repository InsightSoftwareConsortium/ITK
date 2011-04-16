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
#ifndef __itkHistogramToImageFilter_h
#define __itkHistogramToImageFilter_h

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
 *  p is given by Number of occurances of the measurement vector / total number
 *  of occurances of all measurement vectors.
 *
 *  \sa HistogramToProbabilityImageFilter, HistogramToLogProbabilityImageFilter,
 *  HistogramToIntensityImageFilter, HistogramToEntropyImageFilter
 *
 * \ingroup ITK-Statistics
 */

template< class THistogram, unsigned int NDimension, class TFunction >
class ITK_EXPORT HistogramToImageFilter:
  public ImageSource< Image< typename TFunction::OutputPixelType, NDimension > >
{
public:

  /** Standard class typedefs. */
  typedef TFunction                                           FunctorType;
  typedef typename FunctorType::OutputPixelType               OutputPixelType;
  typedef HistogramToImageFilter                              Self;
  typedef ImageSource< Image< OutputPixelType, NDimension > > Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  typedef Image< OutputPixelType, NDimension >  OutputImageType;
  typedef typename Superclass::Pointer          OutputImagePointer;
  typedef typename OutputImageType::SpacingType SpacingType;
  typedef typename OutputImageType::PointType   PointType;

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

  /** Since histograms are not dataobjects, we use the decorator to push
   *  them down the pipeline */
  typedef SimpleDataObjectDecorator< HistogramType * > InputHistogramObjectType;

  /** Determine the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, NDimension);

  /** Set/Get the input of this process object.  */
  virtual void SetInput(const HistogramType *histogram);

  virtual void SetInput(const InputHistogramObjectType *inputObject);

  const InputHistogramObjectType * GetInput(void);

  /** Set the spacing (size of a pixel) of the image.
   *  \sa GetSpacing() */
  itkSetMacro(Spacing, SpacingType);
  virtual void SetSpacing(const double *values);

  /** Get the spacing (size of a pixel) of the image.
   * For ImageBase and Image, the default data spacing is unity. */
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Set the origin of the image.
   * \sa GetOrigin() */
  itkSetMacro(Origin, PointType);
  virtual void SetOrigin(const double *values);

  /** Get the origin of the image.  */
  itkGetConstReferenceMacro(Origin, PointType);

  /** Get the size of the histogram. */
  itkGetMacro(Size, SizeType);

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
  ~HistogramToImageFilter();

  virtual void GenerateOutputInformation();

  virtual void GenerateData();

  FunctorType m_Functor;

  SizeType m_Size;

  SpacingType m_Spacing;

  PointType m_Origin;

  virtual void PrintSelf(std::ostream & os, Indent indent) const;

private:
  HistogramToImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogramToImageFilter.txx"
#endif

#endif
