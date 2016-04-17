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
#ifndef itkMaskedRankImageFilter_hxx
#define itkMaskedRankImageFilter_hxx

#include "itkMaskedRankImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkNumericTraits.h"

#include "itkImageRegionIterator.h"
#include "itkImageLinearConstIteratorWithIndex.h"

#include <iomanip>
#include <sstream>

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 */

namespace itk
{
template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel >
MaskedRankImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel >
::MaskedRankImageFilter()
{
  m_Rank = 0.5;
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel >
void
MaskedRankImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel >
::ConfigureHistogram( HistogramType & histogram )
{
  histogram.SetRank( m_Rank );
}


template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel >
void
MaskedRankImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Rank: " << static_cast< typename NumericTraits< float >::PrintType >( m_Rank ) << std::endl;
}
} // end namespace itk
#endif
