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
#ifndef __itkSumProjectionImageFilter_h
#define __itkSumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class SumProjectionImageFilter
 * \brief Sum projection
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * The original paper can be found at
 *      http://hdl.handle.net/1926/164
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template< class TInputPixel, class TOuputPixel >
class SumAccumulator
{
public:
  SumAccumulator( SizeValueType ) {}
  ~SumAccumulator(){}

  inline void Initialize()
  {
    m_Sum = NumericTraits< TOuputPixel >::Zero;
  }

  inline void operator()(const TInputPixel & input)
  {
    m_Sum = m_Sum + input;
  }

  inline TOuputPixel GetValue()
  {
    return m_Sum;
  }

  TOuputPixel m_Sum;
};
} // end namespace Function

template< class TInputImage, class TOutputImage >
class ITK_EXPORT SumProjectionImageFilter:
  public
  ProjectionImageFilter< TInputImage, TOutputImage,
                         Functor::SumAccumulator<
                           typename TInputImage::PixelType, typename TOutputImage::PixelType > >
{
public:
  typedef SumProjectionImageFilter Self;
  typedef ProjectionImageFilter< TInputImage, TOutputImage,
                                 Functor::SumAccumulator<
                                   typename TInputImage::PixelType,
                                   typename TOutputImage::PixelType > > Superclass;

  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType InputPixelType;

  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(SumProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputPixelToOutputPixelTypeGreaterAdditiveOperatorCheck,
                   ( Concept::AdditiveOperators< OutputPixelType,
                                                 InputPixelType,
                                                 OutputPixelType > ) );
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  /** End concept checking */
#endif

protected:
  SumProjectionImageFilter() {}
  virtual ~SumProjectionImageFilter() {}

private:
  SumProjectionImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};                                        // end SumProjectionImageFilter
} //end namespace itk

#endif
