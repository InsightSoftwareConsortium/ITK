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
#ifndef __itkSliceBySliceImageFilter_hxx
#define __itkSliceBySliceImageFilter_hxx

#include "itkSliceBySliceImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TInputFilter, class TOutputFilter, class TInternalInputImageType,
          class TInternalOutputImageType >
SliceBySliceImageFilter< TInputImage, TOutputImage, TInputFilter, TOutputFilter, TInternalInputImageType,
                         TInternalOutputImageType >
::SliceBySliceImageFilter()
{
  m_InputFilter = NULL;
  m_OutputFilter = NULL;
  this->m_Dimension = ImageDimension - 1;
  m_SliceIndex = 0;
}

template< class TInputImage, class TOutputImage, class TInputFilter, class TOutputFilter, class TInternalInputImageType,
          class TInternalOutputImageType >
void
SliceBySliceImageFilter< TInputImage, TOutputImage, TInputFilter, TOutputFilter, TInternalInputImageType,
                         TInternalOutputImageType >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  for ( unsigned int i = 0; i < this->GetNumberOfInputs(); i++ )
    {
    InputImagePointer inputPtr = const_cast< InputImageType * >( this->GetInput(i) );

    if ( !inputPtr )
      {
      return;
      }

    inputPtr->SetRequestedRegion( inputPtr->GetLargestPossibleRegion() );
    }
}

template< class TInputImage, class TOutputImage, class TInputFilter, class TOutputFilter, class TInternalInputImageType,
          class TInternalOutputImageType >
void
SliceBySliceImageFilter< TInputImage, TOutputImage, TInputFilter, TOutputFilter, TInternalInputImageType,
                         TInternalOutputImageType >
::EnlargeOutputRequestedRegion(DataObject *)
{
  for ( unsigned int i = 0; i < this->GetNumberOfOutputs(); i++ )
    {
    this->GetOutput(i)->SetRequestedRegion( this->GetOutput(i)->GetLargestPossibleRegion() );
    }
}

template< class TInputImage, class TOutputImage, class TInputFilter, class TOutputFilter, class TInternalInputImageType,
          class TInternalOutputImageType >
void
SliceBySliceImageFilter< TInputImage, TOutputImage, TInputFilter, TOutputFilter, TInternalInputImageType,
                         TInternalOutputImageType >
::SetFilter(InputFilterType *filter)
{
  OutputFilterType *outputFilter = dynamic_cast< OutputFilterType * >( filter );

  if ( outputFilter == NULL && filter != NULL )
    {
    // TODO: can it be replaced by a concept check ?
    itkExceptionMacro(
      "Wrong output filter type. Use SetOutputFilter() and SetInputFilter() instead of SetFilter() when input and output filter types are different.");
    }
  this->SetInputFilter(filter);
  this->SetOutputFilter(outputFilter);
}

template< class TInputImage, class TOutputImage, class TInputFilter, class TOutputFilter, class TInternalInputImageType,
          class TInternalOutputImageType >
void
SliceBySliceImageFilter< TInputImage, TOutputImage, TInputFilter, TOutputFilter, TInternalInputImageType,
                         TInternalOutputImageType >
::SetInputFilter(InputFilterType *filter)
{
  if ( m_InputFilter.GetPointer() != filter )
    {
    this->Modified();
    m_InputFilter = filter;
    // adapt the number of inputs and outputs
    this->SetNumberOfRequiredInputs( filter->GetNumberOfValidRequiredInputs() );
    }
}

template< class TInputImage, class TOutputImage, class TInputFilter, class TOutputFilter, class TInternalInputImageType,
          class TInternalOutputImageType >
void
SliceBySliceImageFilter< TInputImage, TOutputImage, TInputFilter, TOutputFilter, TInternalInputImageType,
                         TInternalOutputImageType >
::SetOutputFilter(OutputFilterType *filter)
{
  if ( m_OutputFilter.GetPointer() != filter )
    {
    this->Modified();
    m_OutputFilter = filter;
    // adapt the number of inputs and outputs
    this->SetNumberOfRequiredOutputs( filter->GetNumberOfOutputs() );
    }
}

template< class TInputImage, class TOutputImage, class TInputFilter, class TOutputFilter, class TInternalInputImageType,
          class TInternalOutputImageType >
void
SliceBySliceImageFilter< TInputImage, TOutputImage, TInputFilter, TOutputFilter, TInternalInputImageType,
                         TInternalOutputImageType >
::GenerateData()
{
  if ( !m_InputFilter )
    {
    itkExceptionMacro("InputFilter must be set.");
    }

  if ( !m_OutputFilter )
    {
    itkExceptionMacro("OutputFilter must be set.");
    }

  for ( unsigned int i = 1; i < this->GetNumberOfInputs(); i++ )
    {
    if ( this->GetInput()->GetRequestedRegion().GetSize() != this->GetInput(i)->GetRequestedRegion().GetSize() )
      {
      itkExceptionMacro(<< "Inputs must have the same size.");
      }
    }

  this->AllocateOutputs();

  RegionType requestedRegion = this->GetOutput()->GetRequestedRegion();
  IndexType  requestedIndex = requestedRegion.GetIndex();
  SizeType   requestedSize = requestedRegion.GetSize();

  InternalRegionType internalRegion;
  InternalSizeType   internalSize;
  InternalIndexType  internalIndex;

  for ( unsigned int i = 0; i < InternalImageDimension; i++ )
    {
    if ( i != this->m_Dimension )
      {
      internalSize[i] = requestedSize[i];
      internalIndex[i] = requestedIndex[i];
      }
    else
      {
      internalSize[i] = requestedSize[ImageDimension - 1];
      internalIndex[i] = requestedIndex[ImageDimension - 1];
      }
    }
  internalRegion.SetSize(internalSize);
  internalRegion.SetIndex(internalIndex);

  ProgressReporter progress(this, 0, requestedSize[m_Dimension]);

  const int sliceRange =
    static_cast< int >( requestedSize[m_Dimension] ) + requestedIndex[m_Dimension];

  for ( int slice = requestedIndex[m_Dimension]; slice < sliceRange; slice++ )
    {
    // say to the user that we are begining a new slice
    m_SliceIndex = slice;
    this->InvokeEvent( IterationEvent() );

    // reallocate the internal input at each slice, so the slice by slice filter
    // can work
    // even if the pipeline is run in place
    typedef typename InternalInputImageType::Pointer InternalInputImagePointer;
    std::vector< InternalInputImagePointer > internalInputs;

    internalInputs.resize( this->GetNumberOfInputs() );

    for ( unsigned int i = 0; i < this->GetNumberOfInputs(); i++ )
      {
      internalInputs[i] = InternalInputImageType::New();
      internalInputs[i]->SetRegions(internalRegion);
      internalInputs[i]->Allocate();
      m_InputFilter->SetInput(i, internalInputs[i]);
      }

    // copy the current slice to the input image
    typedef ImageRegionIterator< InternalInputImageType > InputIteratorType;
    std::vector< InputIteratorType > inputIterators;
    inputIterators.resize( this->GetNumberOfInputs() );

    for ( unsigned int i = 0; i < this->GetNumberOfInputs(); i++ )
      {
      inputIterators[i] = InputIteratorType(internalInputs[i], internalRegion);
      inputIterators[i].GoToBegin();
      }

    while ( !inputIterators[0].IsAtEnd() )
      {
      IndexType               idx;
      const InternalIndexType iidx = inputIterators[0].GetIndex();
      for ( unsigned int i = 0; i < InternalImageDimension; i++ )
        {
        if ( i >= m_Dimension )
          {
          idx[i + 1] = iidx[i];
          }
        else
          {
          idx[i] = iidx[i];
          }
        }
      idx[m_Dimension] = slice;

      for ( unsigned int i = 0; i < this->GetNumberOfInputs(); i++ )
        {
        inputIterators[i].Set( this->GetInput(i)->GetPixel(idx) );
        ++( inputIterators[i] );
        }
      }

    // run the filter on the current slice
    m_InputFilter->Modified();
    m_OutputFilter->Modified(); // should not be needed, but may help in some
                                // cases
    m_OutputFilter->UpdateLargestPossibleRegion();
    progress.CompletedPixel();

    // and copy the output slice to the output image
    typedef ImageRegionConstIterator< InternalOutputImageType > OutputIteratorType;
    std::vector< OutputIteratorType > outputIterators;

    outputIterators.resize( this->GetNumberOfOutputs() );

    for ( unsigned int i = 0; i < this->GetNumberOfOutputs(); i++ )
      {
      outputIterators[i] = OutputIteratorType(m_OutputFilter->GetOutput(i), internalRegion);
      outputIterators[i].GoToBegin();
      }

    while ( !outputIterators[0].IsAtEnd() )
      {
      IndexType               idx;
      const InternalIndexType iidx = outputIterators[0].GetIndex();
      for ( unsigned int i = 0; i < InternalImageDimension; i++ )
        {
        if ( i >= m_Dimension )
          {
          idx[i + 1] = iidx[i];
          }
        else
          {
          idx[i] = iidx[i];
          }
        }

      idx[m_Dimension] = slice;

      for ( unsigned int i = 0; i < this->GetNumberOfOutputs(); i++ )
        {
        this->GetOutput(i)->SetPixel( idx, outputIterators[i].Get() );
        ++( outputIterators[i] );
        }
      }
    }
}

template< class TInputImage, class TOutputImage, class TInputFilter, class TOutputFilter, class TInternalInputImageType,
          class TInternalOutputImageType >
void
SliceBySliceImageFilter< TInputImage, TOutputImage, TInputFilter, TOutputFilter, TInternalInputImageType,
                         TInternalOutputImageType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Dimension: " << this->m_Dimension << std::endl;
  os << indent << "InputFilter: " << this->m_InputFilter->GetNameOfClass()
     << " " << this->m_InputFilter.GetPointer() << std::endl;
  os << indent << "OutputFilter: " << this->m_OutputFilter->GetNameOfClass()
     << " " << this->m_OutputFilter.GetPointer() << std::endl;
  os << indent << "SliceIndex: " << m_SliceIndex << std::endl;
}
}

#endif
