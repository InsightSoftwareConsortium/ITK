/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVotingBinaryIterativeHoleFillingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVotingBinaryIterativeHoleFillingImageFilter_txx
#define _itkVotingBinaryIterativeHoleFillingImageFilter_txx
#include "itkVotingBinaryIterativeHoleFillingImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"

#include <vector>
#include <algorithm>

namespace itk
{

template <class TInputImage >
VotingBinaryIterativeHoleFillingImageFilter<TInputImage >
::VotingBinaryIterativeHoleFillingImageFilter()
{
  m_Radius.Fill(1);
  m_ForegroundValue = NumericTraits<InputPixelType>::max();
  m_BackgroundValue = NumericTraits<InputPixelType>::Zero;
  m_MaximumNumberOfIterations = 10;
  m_CurrentNumberOfIterations = 0;
  m_MajorityThreshold = 1;
  m_NumberOfPixelsChanged = 0;
}


template< class TInputImage >
void
VotingBinaryIterativeHoleFillingImageFilter< TInputImage >
::GenerateData()
{
  
  typename InputImageType::ConstPointer  input  = this->GetInput();
  
  m_NumberOfPixelsChanged = 0;

  typename VotingFilterType::Pointer filter = VotingFilterType::New();


  filter->SetRadius( this->GetRadius() );
  filter->SetBackgroundValue( this->GetBackgroundValue() );
  filter->SetForegroundValue( this->GetForegroundValue() );
  filter->SetMajorityThreshold( this->GetMajorityThreshold() );

  m_CurrentNumberOfIterations = 0;

  typename OutputImageType::Pointer output;

  ProgressReporter progress(this, 0, m_MaximumNumberOfIterations);

  while ( m_CurrentNumberOfIterations < m_MaximumNumberOfIterations )
    {
    filter->SetInput( input );
    filter->Update();
    
    m_CurrentNumberOfIterations++;
    progress.CompletedPixel();   // not really a pixel but an iteration
    this->InvokeEvent( IterationEvent() );
    
    const unsigned int numberOfPixelsChangedInThisIteration =
                              filter->GetNumberOfPixelsChanged();
    m_NumberOfPixelsChanged += numberOfPixelsChangedInThisIteration;

    output = filter->GetOutput();
    output->DisconnectPipeline();
    input = output;
    if( numberOfPixelsChangedInThisIteration == 0 )
      {
      break;
      }
    }
  this->GraftOutput( output );
}



/**
 * Standard "PrintSelf" method
 */
template <class TInputImage >
void
VotingBinaryIterativeHoleFillingImageFilter<TInputImage>
::PrintSelf(
  std::ostream& os, 
  Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "Foreground value : " << static_cast<NumericTraits<InputPixelType>::PrintType>( m_ForegroundValue )<< std::endl;
  os << indent << "Background value : " << static_cast<NumericTraits<InputPixelType>::PrintType>( m_BackgroundValue ) << std::endl;
  os << indent << "Maximum Number of Iterations : " << m_MaximumNumberOfIterations << std::endl;
  os << indent << "Current Number of Iterations : " << m_CurrentNumberOfIterations << std::endl;
  os << indent << "Majority Threshold           : " << m_MajorityThreshold << std::endl;
  os << indent << "Number of Pixels Changed     : " << m_NumberOfPixelsChanged << std::endl;
}

} // end namespace itk

#endif
