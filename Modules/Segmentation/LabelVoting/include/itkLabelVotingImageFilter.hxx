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
#ifndef __itkLabelVotingImageFilter_hxx
#define __itkLabelVotingImageFilter_hxx

#include "itkLabelVotingImageFilter.h"

#include "itkImageRegionIterator.h"

#include "vnl/vnl_math.h"

namespace itk
{


template< typename TInputImage, typename TOutputImage >
LabelVotingImageFilter< TInputImage, TOutputImage >
::LabelVotingImageFilter()
{
  this->m_HasLabelForUndecidedPixels = false;
  this->m_LabelForUndecidedPixels = 0;
  this->m_TotalLabelCount = 0;
}

template< typename TInputImage, typename TOutputImage >
void
LabelVotingImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_HasLabelForUndecidedPixels = "
     << this->m_HasLabelForUndecidedPixels << std::endl;
  os << indent << "m_LabelForUndecidedPixels = "
     << this->m_LabelForUndecidedPixels << std::endl;
}

template< typename TInputImage, typename TOutputImage >
typename LabelVotingImageFilter< TInputImage, TOutputImage >::InputPixelType
LabelVotingImageFilter< TInputImage, TOutputImage >
::ComputeMaximumInputValue()
{
  InputPixelType maxLabel = 0;

  typedef ImageRegionConstIterator< TInputImage > IteratorType;

  // Record the number of input files.
  const unsigned int numberOfInputFiles = this->GetNumberOfIndexedInputs();

  for ( unsigned int i = 0; i < numberOfInputFiles; ++i )
    {
    const InputImageType *inputImage =  this->GetInput(i);
    IteratorType          it( inputImage, inputImage->GetBufferedRegion() );
    for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
      {
      maxLabel = vnl_math_max( maxLabel, it.Get() );
      }
    }

  return maxLabel;
}

template< typename TInputImage, typename TOutputImage >
void
LabelVotingImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  // determine the maximum label in all input images
  this->m_TotalLabelCount =
    static_cast<size_t>(this->ComputeMaximumInputValue()) + 1;

  if ( !this->m_HasLabelForUndecidedPixels )
    {
    if (this->m_TotalLabelCount > itk::NumericTraits<OutputPixelType>::max())
      {
      itkWarningMacro("No new label for undecided pixels, using zero.");
      }
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
::ThreadedGenerateData( const OutputImageRegionType & outputRegionForThread,
                        ThreadIdType itkNotUsed(threadId) )
{
  typedef ImageRegionConstIterator< TInputImage > IteratorType;
  typedef ImageRegionIterator< TOutputImage >     OutIteratorType;

  typename TOutputImage::Pointer output = this->GetOutput();

  // Record the number of input files.
  const unsigned int numberOfInputFiles = this->GetNumberOfIndexedInputs();

  //  create and initialize all input image iterators
  IteratorType *it = new IteratorType[numberOfInputFiles];
  for ( unsigned int i = 0; i < numberOfInputFiles; ++i )
    {
    it[i] = IteratorType(this->GetInput(i),
                         outputRegionForThread);
    }

  unsigned int *votesByLabel = new unsigned int[this->m_TotalLabelCount];

  OutIteratorType out = OutIteratorType(output, outputRegionForThread);
  for ( out.GoToBegin(); !out.IsAtEnd(); ++out )
    {
    // reset number of votes per label for all labels
    std::fill_n( votesByLabel, this->m_TotalLabelCount, 0 );

    // count number of votes for the labels
    for ( unsigned int i = 0; i < numberOfInputFiles; ++i )
      {
      const InputPixelType label = it[i].Get();
      if ( NumericTraits<InputPixelType>::IsNonnegative( label ) )
        {
        ++votesByLabel[label];
        }
      ++( it[i] );
      }

    // determine the label with the most votes for this pixel
    out.Set(0);
    unsigned int maxVotes = votesByLabel[0];
    for ( size_t l = 1; l < this->m_TotalLabelCount; ++l )
      {
      if ( votesByLabel[l] > maxVotes )
        {
        maxVotes = votesByLabel[l];
        out.Set(static_cast<OutputPixelType>(l));
        }
      else
        {
        if ( votesByLabel[l] == maxVotes )
          {
          out.Set(this->m_LabelForUndecidedPixels);
          }
        }
      }
    }

  delete[] it;
  delete[] votesByLabel;
}
} // end namespace itk

#endif
