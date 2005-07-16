/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelVotingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLabelVotingImageFilter_txx
#define _itkLabelVotingImageFilter_txx

#include "itkLabelVotingImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

#include "vnl/vnl_math.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
LabelVotingImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "m_HasLabelForUndecidedPixels = " 
     << this->m_HasLabelForUndecidedPixels << std::endl;
  os << indent << "m_LabelForUndecidedPixels = " 
     << this->m_LabelForUndecidedPixels << std::endl;

}

template< typename TInputImage, typename TOutputImage >
typename LabelVotingImageFilter< TInputImage,TOutputImage>::InputPixelType
LabelVotingImageFilter< TInputImage, TOutputImage >
::ComputeMaximumInputValue()
{
  InputPixelType maxLabel = 0;

  typedef ImageRegionConstIterator< TInputImage > IteratorType;

  // Record the number of input files.
  const unsigned int numberOfInputFiles = this->GetNumberOfInputs();

  for(unsigned int i = 0; i < numberOfInputFiles; ++i)
    {
    const InputImageType * inputImage =  this->GetInput( i );
    IteratorType it( inputImage, inputImage->GetBufferedRegion() );
    for( it.GoToBegin(); !it.IsAtEnd(); ++it )
      {
      maxLabel = vnl_math_max( maxLabel, it.Get() );
      }
    }

  return maxLabel;
}

template< typename TInputImage, typename TOutputImage >
void
LabelVotingImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData ()
{
  Superclass::BeforeThreadedGenerateData();

  // determine the maximum label in all input images
  this->m_TotalLabelCount = this->ComputeMaximumInputValue() + 1;

  if ( ! this->m_HasLabelForUndecidedPixels )
    {
    this->m_LabelForUndecidedPixels = this->m_TotalLabelCount;
    }
  
  // Allocate the output image.
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();
}

template< typename TInputImage, typename TOutputImage >
void
LabelVotingImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{
  typedef ImageRegionConstIterator< TInputImage > IteratorType;
  typedef ImageRegionIterator< TOutputImage> OutIteratorType;

  typename TOutputImage::Pointer output = this->GetOutput();
  
  // Record the number of input files.
  const unsigned int numberOfInputFiles = this->GetNumberOfInputs();

  //  create and initialize all input image iterators
  IteratorType *it = new IteratorType[numberOfInputFiles];
  for ( unsigned int i = 0; i < numberOfInputFiles; ++i)
    {
    it[i] = IteratorType( this->GetInput( i ), 
                          outputRegionForThread );
    }

  unsigned int* votesByLabel = new unsigned int[ this->m_TotalLabelCount ];

  OutIteratorType out = OutIteratorType( output, outputRegionForThread );
  for( out.GoToBegin(); !out.IsAtEnd(); ++out )
    {
    // reset number of votes per label for all labels
    for( InputPixelType l = 0; l < this->m_TotalLabelCount; ++l )
      {
      votesByLabel[l] = 0;
      }

    // count number of votes for the labels
    for( unsigned int i = 0; i < numberOfInputFiles; ++i)
      {
      const InputPixelType label = it[i].Get();
      ++votesByLabel[label];
      ++(it[i]);
      }
    
    // determine the label with the most votes for this pixel
    out.Set( 0 );
    unsigned int maxVotes = votesByLabel[0];
    for( InputPixelType l = 1; l < this->m_TotalLabelCount; ++l )
      {
      if( votesByLabel[l] > maxVotes )
        {
        maxVotes = votesByLabel[l];
        out.Set( l );
        }
      else
        {
        if( votesByLabel[l] == maxVotes )
          {
          out.Set( this->m_LabelForUndecidedPixels );
          }
        }
      }
    }
  
  delete[] it;  
  delete[] votesByLabel;
}

} // end namespace itk

#endif
