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
#ifndef itkBinaryProjectionImageFilter_h
#define itkBinaryProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class BinaryProjectionImageFilter
 * \brief Binary projection
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * The original paper can be found at
 *   https://hdl.handle.net/1926/164
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 *  INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template< typename TInputPixel, typename TOutputPixel >
class BinaryAccumulator
{
public:
  BinaryAccumulator( SizeValueType ) {}
  ~BinaryAccumulator(){}

  inline void Initialize()
  {
    m_IsForeground = false;
  }

  inline void operator()(const TInputPixel & input)
  {
    if ( input == m_ForegroundValue )
      {
      m_IsForeground = true;
      }
  }

  inline TOutputPixel GetValue()
  {
    if ( m_IsForeground )
      {
      return (TOutputPixel)m_ForegroundValue;
      }
    else
      {
      return m_BackgroundValue;
      }
  }

  bool m_IsForeground;

  TInputPixel m_ForegroundValue;

  TOutputPixel m_BackgroundValue;
};
} // end namespace Function

template< typename TInputImage, typename TOutputImage >
class BinaryProjectionImageFilter:
  public ProjectionImageFilter< TInputImage, TOutputImage,
                                Functor::BinaryAccumulator<
                                  typename TInputImage::PixelType,
                                  typename TOutputImage::PixelType > >
{
public:
  typedef BinaryProjectionImageFilter Self;
  typedef ProjectionImageFilter< TInputImage, TOutputImage,
                                 Functor::BinaryAccumulator<
                                   typename TInputImage::PixelType,
                                   typename TOutputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(BinaryProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Image typedef support. */
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  typedef typename Superclass::AccumulatorType AccumulatorType;

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. Subclasses may alias this to
   * DilateValue or ErodeValue. */
  itkSetMacro(ForegroundValue, InputPixelType);

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. */
  itkGetConstMacro(ForegroundValue, InputPixelType);

  /** Set the value used as "background".  Any pixel value which is
   * not DilateValue is considered background. BackgroundValue is used
   * for defining boundary conditions. Defaults to
   * NumericTraits<PixelType>::NonpositiveMin(). */
  itkSetMacro(BackgroundValue, OutputPixelType);

  /** Get the value used as "background". Any pixel value which is
   * not DilateValue is considered background. BackgroundValue is used
   * for defining boundary conditions. Defaults to
   * NumericTraits<PixelType>::NonpositiveMin(). */
  itkGetConstMacro(BackgroundValue, OutputPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputPixelTypeGreaterThanComparable,
                   ( Concept::EqualityComparable< InputPixelType > ) );
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  BinaryProjectionImageFilter()
  {
    m_ForegroundValue = NumericTraits< InputPixelType >::max();
    m_BackgroundValue = NumericTraits< OutputPixelType >::NonpositiveMin();
  }

  virtual ~BinaryProjectionImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);

    typedef typename NumericTraits< InputPixelType >::PrintType
    InputPixelPrintType;

    os << indent << "ForegroundValue: "
       << static_cast< InputPixelPrintType >( m_ForegroundValue )
       << std::endl;

    typedef typename NumericTraits< OutputPixelType >::PrintType
    OutputPixelPrintType;

    os << indent << "BackgroundValue: "
       << static_cast< OutputPixelPrintType >( m_BackgroundValue )
       << std::endl;
  }

  virtual AccumulatorType NewAccumulator( SizeValueType size ) const ITK_OVERRIDE
  {
    AccumulatorType accumulator(size);

    accumulator.m_ForegroundValue = m_ForegroundValue;
    accumulator.m_BackgroundValue = m_BackgroundValue;
    return accumulator;
  }

  /** Pixel value to dilate */
  InputPixelType m_ForegroundValue;

  /** Pixel value for background */
  OutputPixelType m_BackgroundValue;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryProjectionImageFilter);
};                                           // end BinaryProjectionImageFilter
} //end namespace itk

#endif
