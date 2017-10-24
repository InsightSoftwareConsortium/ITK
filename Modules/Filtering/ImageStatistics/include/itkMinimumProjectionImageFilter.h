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
#ifndef itkMinimumProjectionImageFilter_h
#define itkMinimumProjectionImageFilter_h

#include "itkProjectionImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class MinimumProjectionImageFilter
 * \brief Minimum projection
 *
 * This class was contributed to the Insight Journal by Gaetan Lehmann.
 * The original paper can be found at
 *      https://hdl.handle.net/1926/164
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * \sa ProjectionImageFilter
 * \sa StandardDeviationProjectionImageFilter
 * \sa SumProjectionImageFilter
 * \sa BinaryProjectionImageFilter
 * \sa MaximumProjectionImageFilter
 * \sa MeanProjectionImageFilter
 * \ingroup ITKImageStatistics
 */

namespace Functor
{
template< typename TInputPixel >
class MinimumAccumulator
{
public:
  MinimumAccumulator( SizeValueType ) {}
  ~MinimumAccumulator(){}

  inline void Initialize()
  {
    m_Minimum = NumericTraits< TInputPixel >::max();
  }

  inline void operator()(const TInputPixel & input)
  {
    m_Minimum = std::min(m_Minimum, input);
  }

  inline TInputPixel GetValue()
  {
    return m_Minimum;
  }

  TInputPixel m_Minimum;
};
} // end namespace Function

template< typename TInputImage, typename TOutputImage >
class MinimumProjectionImageFilter:public
  ProjectionImageFilter< TInputImage, TOutputImage,
                         Functor::MinimumAccumulator< typename TInputImage::PixelType > >
{
public:
  typedef MinimumProjectionImageFilter Self;
  typedef ProjectionImageFilter< TInputImage, TOutputImage,
                                 Functor::MinimumAccumulator<
                                   typename TInputImage::PixelType > > Superclass;

  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType InputPixelType;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Runtime information support. */
  itkTypeMacro(MinimumProjectionImageFilter, ProjectionImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputPixelTypeGreaterThanComparable,
                   ( Concept::LessThanComparable< InputPixelType > ) );
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  MinimumProjectionImageFilter() {}
  virtual ~MinimumProjectionImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MinimumProjectionImageFilter);
};                                            // end
                                              // MinimumProjectionImageFilter
} //end namespace itk

#endif
