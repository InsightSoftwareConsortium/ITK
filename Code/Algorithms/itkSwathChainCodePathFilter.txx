/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSwathChainCodePathFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkSwathChainCodePathFilter_txx
#define _itkSwathChainCodePathFilter_txx

#include "itkSwathChainCodePathFilter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TPath, class TImage>
SwathChainCodePathFilter<TPath, TImage>
::SwathChainCodePathFilter()
{
  // do nothing for now ???
}


/**
 * GenerateData Performs the reflection
 */
template <class TPath, class TImage>
void
SwathChainCodePathFilter<TPath, TImage>
::GenerateData( void )
{
  /*
  OffsetType offset;
  OffsetType tempOffset;
  OffsetType zeroOffset;
  zeroOffset.Fill(0);
  
  InputPathInputType inputPathInput;

  int dimension = OffsetType::GetOffsetDimension();

  typename Superclass::InputPathConstPointer  inputPtr  = this->GetInput();
  typename Superclass::OutputPathPointer      outputPtr = this->GetOutput(0);

  //outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  //outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  //outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  //outputPtr->Allocate();  // Allocate() is an Image function
  
  outputPtr->Clear();
  inputPathInput = inputPtr->StartOfInput();
  outputPtr->SetStart(  inputPtr->EvaluateToIndex( inputPathInput )  );
  
  for(OutputPathInputType outputPathInput=0;;)
    {
    offset  = inputPtr->IncrementInput(inputPathInput);
    if( zeroOffset == offset ) { break; }
    
    if( ! m_MaximallyConnected )
      {
      outputPtr->InsertStep( outputPathInput++, offset );
      }
    else
      {
      for( int d=0; d<dimension; d++ )
        {
        if( offset[d] )
          {
          tempOffset.Fill(0);
          tempOffset[d] = offset[d];
          outputPtr->InsertStep( outputPathInput++, tempOffset );
          }
        }
      }
    }
  */
}

template <class TPath, class TImage>
void
SwathChainCodePathFilter<TPath, TImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
