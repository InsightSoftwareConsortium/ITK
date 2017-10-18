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
#ifndef itkMaximumProjectionImageFilter_h
#define itkMaximumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class MaximumProjectionImageFilter
 * \brief Maximum projection
 *
 * This class was contributed to the insight journal by Gaetan Lehmann.
 * The original paper can be found at
 *    https://hdl.handle.net/1926/164
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la reproduction,
 *  inra de jouy-en-josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa MedianProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \sa MinimumProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template< typename TInputPixel >
class MaximumAccumulator
{
public:
  MaximumAccumulator( SizeValueType ) {}
  ~MaximumAccumulator(){}

  inline void Initialize()
  {
    m_Maximum = NumericTraits< TInputPixel >::NonpositiveMin();
  }

  inline void operator()(const TInputPixel & input)
  {
    m_Maximum = std::max(m_Maximum, input);
  }

  inline TInputPixel GetValue()
  {
    return m_Maximum;
  }

  TInputPixel m_Maximum;
};
} // end namespace Function

template< typename TInputImage, typename TOutputImage >
class MaximumProjectionImageFilter:
  public ProjectionImageFilter< TInputImage, TOutputImage,
                                Functor::MaximumAccumulator< typename TInputImage::PixelType > >
{
public:
  typedef MaximumProjectionImageFilter Self;
  typedef ProjectionImageFilter< TInputImage, TOutputImage,
                                 Functor::MaximumAccumulator< typename TInputImage::PixelType > > Superclass;

  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType InputPixelType;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(MaximumProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputPixelTypeGreaterThanComparable,
                   ( Concept::GreaterThanComparable< InputPixelType > ) );
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  MaximumProjectionImageFilter() {}
  virtual ~MaximumProjectionImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaximumProjectionImageFilter);
};                                            // end
                                              // MaximumProjectionImageFilter
} //end namespace itk

#endif
