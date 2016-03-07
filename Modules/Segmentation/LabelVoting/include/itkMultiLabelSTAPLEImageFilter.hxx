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
#ifndef itkMultiLabelSTAPLEImageFilter_hxx
#define itkMultiLabelSTAPLEImageFilter_hxx

#include "itkMultiLabelSTAPLEImageFilter.h"

#include "itkLabelVotingImageFilter.h"

#include "itkMath.h"

namespace itk
{

template < typename TInputImage, typename TOutputImage, typename TWeights>
void
MultiLabelSTAPLEImageFilter< TInputImage, TOutputImage, TWeights >
::PrintSelf(std::ostream& os, Indent  indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "HasLabelForUndecidedPixels = "
     << this->m_HasLabelForUndecidedPixels << std::endl;
  typedef typename NumericTraits< OutputPixelType >::PrintType OutputPixelPrintType;
  os << indent << "LabelForUndecidedPixels = "
     << static_cast<OutputPixelPrintType>(this->m_LabelForUndecidedPixels) << std::endl;
  os << indent << "HasPriorProbabilities = "
     << this->m_PriorProbabilities << std::endl;
  os << indent << "PriorProbabilities = "
     << this->m_PriorProbabilities << std::endl;
  os << indent << "HasMaximumNumberOfIterations = "
     << this->m_HasMaximumNumberOfIterations << std::endl;
  os << indent << "MaximumNumberOfIterations = "
     << this->m_MaximumNumberOfIterations << std::endl;
  os << indent << "m_ElapsedNumberOfIterations = "
     << m_ElapsedNumberOfIterations << std::endl;
  os << indent << "TerminationUpdateThreshold = "
     << this->m_TerminationUpdateThreshold << std::endl;
}

template < typename TInputImage, typename TOutputImage, typename TWeights>
void
MultiLabelSTAPLEImageFilter< TInputImage, TOutputImage, TWeights >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  for ( unsigned int k = 0; k < this->GetNumberOfInputs(); ++k )
    {
    InputImagePointer input = const_cast<InputImageType *>( this->GetInput( k ) );
    input->SetRequestedRegionToLargestPossibleRegion();
    }
}

template < typename TInputImage, typename TOutputImage, typename TWeights>
void
MultiLabelSTAPLEImageFilter< TInputImage, TOutputImage, TWeights >
::EnlargeOutputRequestedRegion( DataObject *data )
{
  Superclass::EnlargeOutputRequestedRegion( data );
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage, typename TWeights >
typename TInputImage::PixelType
MultiLabelSTAPLEImageFilter< TInputImage, TOutputImage, TWeights >
::ComputeMaximumInputValue()
{
  InputPixelType maxLabel = 0;

  // Record the number of input files.
  const size_t numberOfInputs = this->GetNumberOfInputs();

  for ( size_t k = 0; k < numberOfInputs; ++k )
    {
    InputConstIteratorType it( this->GetInput( k ), this->GetInput( k )->GetBufferedRegion() );

    for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
      {
      maxLabel = std::max( maxLabel, it.Get() );
      }
    }

  return maxLabel;
}

template< typename TInputImage, typename TOutputImage, typename TWeights >
void
MultiLabelSTAPLEImageFilter< TInputImage, TOutputImage, TWeights >
::AllocateConfusionMatrixArray()
{
  // we need one confusion matrix for every input
  const ProcessObject::DataObjectPointerArraySizeType numberOfInputs = this->GetNumberOfInputs();

  this->m_ConfusionMatrixArray.clear();
  this->m_UpdatedConfusionMatrixArray.clear();

  // create the confusion matrix and space for updated confusion matrix for
  // each of the input images
  for ( unsigned int k = 0; k < numberOfInputs; ++k )
    {
    // the confusion matrix has as many rows as there are input labels, and
    // one more column to accomodate "reject" classifications by the combined
    // classifier.
    this->m_ConfusionMatrixArray.push_back
      ( ConfusionMatrixType(static_cast<unsigned int>( this->m_TotalLabelCount )+1,
                            static_cast<unsigned int>( this->m_TotalLabelCount ) ) );
    this->m_UpdatedConfusionMatrixArray.push_back
      ( ConfusionMatrixType(static_cast<unsigned int>( this->m_TotalLabelCount )+1,
                            static_cast<unsigned int>( this->m_TotalLabelCount ) ) );
    }
}

template< typename TInputImage, typename TOutputImage, typename TWeights >
void
MultiLabelSTAPLEImageFilter< TInputImage, TOutputImage, TWeights >
::InitializeConfusionMatrixArrayFromVoting()
{
  const unsigned int numberOfInputs = static_cast<const unsigned int>( this->GetNumberOfInputs() );

  typedef LabelVotingImageFilter<TInputImage, TOutputImage> LabelVotingFilterType;
  typedef typename LabelVotingFilterType::Pointer           LabelVotingFilterPointer;

  typename OutputImageType::Pointer votingOutput;

  { // begin scope for local filter allocation
  LabelVotingFilterPointer votingFilter = LabelVotingFilterType::New();

  for ( unsigned int k = 0; k < numberOfInputs; ++k )
    {
    votingFilter->SetInput( k, this->GetInput( k ) );
    }
  votingFilter->Update();
  votingOutput = votingFilter->GetOutput();
  } // begin scope for local filter allocation; de-allocate filter

  OutputIteratorType out =
    OutputIteratorType( votingOutput, votingOutput->GetRequestedRegion() );

  for ( unsigned int k = 0; k < numberOfInputs; ++k )
    {
    this->m_ConfusionMatrixArray[k].Fill( 0.0 );

    InputConstIteratorType in =
      InputConstIteratorType( this->GetInput( k ), votingOutput->GetRequestedRegion() );

    for ( out.GoToBegin(); ! out.IsAtEnd(); ++out, ++in )
      {
      ++(this->m_ConfusionMatrixArray[k][in.Get()][out.Get()]);
      }
    }

  // normalize matrix rows to unit probability sum
  for ( unsigned int k = 0; k < numberOfInputs; ++k )
    {
    for ( InputPixelType inLabel = 0; inLabel < this->m_TotalLabelCount+1; ++inLabel )
      {
      // compute sum over all output labels for given input label
      WeightsType sum = 0;
      for ( OutputPixelType outLabel = 0; outLabel < this->m_TotalLabelCount; ++outLabel )
        {
        sum += this->m_ConfusionMatrixArray[k][inLabel][outLabel];
        }
      // make sure that this input label did in fact show up in the input!!
      if ( sum > 0 )
        {
        // normalize
        for ( OutputPixelType outLabel = 0; outLabel < this->m_TotalLabelCount; ++outLabel )
          {
          this->m_ConfusionMatrixArray[k][inLabel][outLabel] /= sum;
          }
        }
      }
    }
}

template< typename TInputImage, typename TOutputImage, typename TWeights >
void
MultiLabelSTAPLEImageFilter< TInputImage, TOutputImage, TWeights >
::InitializePriorProbabilities()
{
  // test for user-defined prior probabilities and create an estimated one if
  // none exists
  if ( this->m_HasPriorProbabilities )
    {
    if ( this->m_PriorProbabilities.GetSize() < this->m_TotalLabelCount )
      {
      itkExceptionMacro ( "m_PriorProbabilities array has wrong size " << m_PriorProbabilities << "; should be at least " << 1+this->m_TotalLabelCount );
      }
    }
  else
    {
    this->m_PriorProbabilities.SetSize( 1+ static_cast<SizeValueType>( this->m_TotalLabelCount ) );
    this->m_PriorProbabilities.Fill( 0.0 );

    const size_t numberOfInputs = this->GetNumberOfInputs();
    for (size_t k = 0; k < numberOfInputs; ++k )
      {
      InputConstIteratorType in = InputConstIteratorType( this->GetInput( k ), this->GetOutput()->GetRequestedRegion() );
      for ( in.GoToBegin(); ! in.IsAtEnd(); ++in )
        {
        ++(this->m_PriorProbabilities[in.Get()]);
        }
      }

    WeightsType totalProbMass = 0.0;
    for ( InputPixelType l = 0; l < this->m_TotalLabelCount; ++l )
      totalProbMass += this->m_PriorProbabilities[l];
    for ( InputPixelType l = 0; l < this->m_TotalLabelCount; ++l )
      this->m_PriorProbabilities[l] /= totalProbMass;
    }
}

template< typename TInputImage, typename TOutputImage, typename TWeights >
void
MultiLabelSTAPLEImageFilter< TInputImage, TOutputImage, TWeights >
::GenerateData()
{
  // determine the maximum label in all input images
  this->m_TotalLabelCount =
    static_cast<size_t>(this->ComputeMaximumInputValue()) + 1;

  if ( ! this->m_HasLabelForUndecidedPixels )
    {
    this->m_LabelForUndecidedPixels = static_cast<OutputPixelType>( this->m_TotalLabelCount );
    }

  // allocate and initialize the confusion matrices
  this->AllocateConfusionMatrixArray();
  this->InitializeConfusionMatrixArrayFromVoting();

  // test existing or allocate and initialize new array with prior class
  // probabilities
  this->InitializePriorProbabilities();

  // Allocate the output image.
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  // Record the number of input files.
  const size_t numberOfInputs = this->GetNumberOfInputs();

  // create and initialize all input image iterators
  InputConstIteratorType *it = new InputConstIteratorType[numberOfInputs];
  for (size_t k = 0; k < numberOfInputs; ++k )
    {
    it[k] = InputConstIteratorType
      ( this->GetInput( k ), output->GetRequestedRegion() );
    }

  // allocate array for pixel class weights
  WeightsType* W = new WeightsType[ this->m_TotalLabelCount ];

  unsigned int iteration = 0;
  for (; (!this->m_HasMaximumNumberOfIterations) || (iteration < this->m_MaximumNumberOfIterations); ++iteration )
    {
    // reset updated confusion matrix
    for ( unsigned int k = 0; k < numberOfInputs; ++k )
      {
      this->m_UpdatedConfusionMatrixArray[k].Fill( 0.0 );
      }

    // reset all input iterators to start
    for ( unsigned int k = 0; k < numberOfInputs; ++k )
      {
      it[k].GoToBegin();
      }

    // use it[0] as indicator for image pixel count
    while ( ! it[0].IsAtEnd() )
      {
      // the following is the E step
      for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
        {
        W[ci] = this->m_PriorProbabilities[ci];
        }
      for ( unsigned int k = 0; k < numberOfInputs; ++k )
        {
        const InputPixelType j = it[k].Get();
        for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
          {
          W[ci] *= this->m_ConfusionMatrixArray[k][j][ci];
          }
        }

      // the following is the M step
      WeightsType sumW = W[0];
      for ( OutputPixelType ci = 1; ci < this->m_TotalLabelCount; ++ci )
        {
        sumW += W[ci];
        }

      if ( sumW )
        {
        for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
          {
          W[ci] /= sumW;
          }
        }

      for ( unsigned int k = 0; k < numberOfInputs; ++k )
        {
        const InputPixelType j = it[k].Get();
        for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
          {
          this->m_UpdatedConfusionMatrixArray[k][j][ci] += W[ci];
          }

        // we're now done with this input pixel, so update.
        ++(it[k]);
        }
      }

    // Normalize matrix elements of each of the updated confusion matrices
    // with sum over all expert decisions.
    for ( unsigned int k = 0; k < numberOfInputs; ++k )
      {
      // compute sum over all output classifications
      for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
        {
        WeightsType sumW = this->m_UpdatedConfusionMatrixArray[k][0][ci];
        for ( InputPixelType j = 1; j < 1+this->m_TotalLabelCount; ++j )
          {
          sumW += this->m_UpdatedConfusionMatrixArray[k][j][ci];
          }

        // normalize with for each class ci
        if ( sumW )
          {
          for ( InputPixelType j = 0; j < 1+this->m_TotalLabelCount; ++j )
            {
            this->m_UpdatedConfusionMatrixArray[k][j][ci] /= sumW;
            }
          }
        }
      }

    // now we're applying the update to the confusion matrices and compute the
    // maximum parameter change in the process.
    WeightsType maximumUpdate = 0;
    for ( unsigned int k = 0; k < numberOfInputs; ++k )
      {
      for ( InputPixelType j = 0; j < 1+this->m_TotalLabelCount; ++j )
        {
        for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
          {
          const WeightsType thisParameterUpdate =
            itk::Math::abs( this->m_UpdatedConfusionMatrixArray[k][j][ci] -
           this->m_ConfusionMatrixArray[k][j][ci] );

            maximumUpdate = std::max( maximumUpdate, thisParameterUpdate );

          this->m_ConfusionMatrixArray[k][j][ci] =
            this->m_UpdatedConfusionMatrixArray[k][j][ci];
          }
        }
      }

    this->InvokeEvent( IterationEvent() );
    if( this->GetAbortGenerateData() )
      {
      this->ResetPipeline();
      // fake this to cause termination; we could really just break
      maximumUpdate = 0;
      }

    // if all confusion matrix parameters changes by less than the defined
    // threshold, we're done.
    if ( maximumUpdate < this->m_TerminationUpdateThreshold )
      {
      break;
      }
    } // end for ( iteration )

  // now we'll build the combined output image based on the estimated
  // confusion matrices

  // reset all input iterators to start
  for ( unsigned int k = 0; k < numberOfInputs; ++k )
    {
    it[k].GoToBegin();
    }

  // reset output iterator to start
  OutputIteratorType out = OutputIteratorType( output, output->GetRequestedRegion() );
  for ( out.GoToBegin(); !out.IsAtEnd(); ++out )
    {
    // basically, we'll repeat the E step from above
    for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
      {
      W[ci] = this->m_PriorProbabilities[ci];
      }

    for ( unsigned int k = 0; k < numberOfInputs; ++k )
      {
      const InputPixelType j = it[k].Get();
      for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
        {
        W[ci] *= this->m_ConfusionMatrixArray[k][j][ci];
        }
      ++it[k];
      }

    // now determine the label with the maximum W
    OutputPixelType winningLabel = static_cast<OutputPixelType>( this->m_TotalLabelCount );
    WeightsType winningLabelW = 0;
    for ( OutputPixelType ci = 0; ci < this->m_TotalLabelCount; ++ci )
      {
      if ( W[ci] > winningLabelW )
        {
        winningLabelW = W[ci];
        winningLabel = ci;
        }
      else if ( ! ( W[ci] < winningLabelW ) )
        {
        winningLabel = static_cast<OutputPixelType>( this->m_TotalLabelCount );
        }
      }

    out.Set( winningLabel );
    }

  m_ElapsedNumberOfIterations = iteration;

  delete[] W;
  delete[] it;
}

} // end namespace itk

#endif
