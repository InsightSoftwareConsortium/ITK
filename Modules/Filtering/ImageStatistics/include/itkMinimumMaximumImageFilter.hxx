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
#ifndef itkMinimumMaximumImageFilter_hxx
#define itkMinimumMaximumImageFilter_hxx
#include "itkMinimumMaximumImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

#include <vector>

namespace itk
{
template< typename TInputImage >
MinimumMaximumImageFilter< TInputImage >
::MinimumMaximumImageFilter()
{
  this->SetNumberOfRequiredOutputs(3);
  // first output is a copy of the image, DataObject created by
  // superclass
  //
  // allocate the data objects for the remaining outputs which are
  // just decorators around floating point types
  for ( int i = 1; i < 3; ++i )
    {
    typename PixelObjectType::Pointer output =
      static_cast< PixelObjectType * >( this->MakeOutput(i).GetPointer() );
    this->ProcessObject::SetNthOutput( i, output.GetPointer() );
    }

  this->GetMinimumOutput()->Set( NumericTraits< PixelType >::max() );
  this->GetMaximumOutput()->Set( NumericTraits< PixelType >::NonpositiveMin() );
}

template< typename TInputImage >
DataObject::Pointer
MinimumMaximumImageFilter< TInputImage >
::MakeOutput(DataObjectPointerArraySizeType output)
{
  switch ( output )
    {
    case 0:
      return TInputImage::New().GetPointer();
      break;
    case 1:
    case 2:
      return PixelObjectType::New().GetPointer();
      break;
    default:
      // might as well make an image
      return TInputImage::New().GetPointer();
      break;
    }
}

template< typename TInputImage >
typename MinimumMaximumImageFilter< TInputImage >::PixelObjectType *
MinimumMaximumImageFilter< TInputImage >
::GetMinimumOutput()
{
  return static_cast< PixelObjectType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage >
const typename MinimumMaximumImageFilter< TInputImage >::PixelObjectType *
MinimumMaximumImageFilter< TInputImage >
::GetMinimumOutput() const
{
  return static_cast< const PixelObjectType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage >
typename MinimumMaximumImageFilter< TInputImage >::PixelObjectType *
MinimumMaximumImageFilter< TInputImage >
::GetMaximumOutput()
{
  return static_cast< PixelObjectType * >( this->ProcessObject::GetOutput(2) );
}

template< typename TInputImage >
const typename MinimumMaximumImageFilter< TInputImage >::PixelObjectType *
MinimumMaximumImageFilter< TInputImage >
::GetMaximumOutput() const
{
  return static_cast< const PixelObjectType * >( this->ProcessObject::GetOutput(2) );
}

template< typename TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
      const_cast< typename Superclass::InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::AllocateOutputs()
{
  // Pass the input through as the output
  InputImagePointer image =
    const_cast< TInputImage * >( this->GetInput() );

  this->GraftOutput(image);

  // Nothing that needs to be allocated for the remaining outputs
}

template< typename TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  // Create the thread temporaries
  m_ThreadMin = std::vector< PixelType >( numberOfThreads,
                                          NumericTraits< PixelType >::max() );
  m_ThreadMax = std::vector< PixelType >( numberOfThreads,
                                          NumericTraits< PixelType >::NonpositiveMin() );
}

template< typename TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::AfterThreadedGenerateData()
{
  ThreadIdType i;
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  PixelType minimum, maximum;

  // Find the min/max over all threads
  minimum = NumericTraits< PixelType >::max();
  maximum = NumericTraits< PixelType >::NonpositiveMin();
  for ( i = 0; i < numberOfThreads; i++ )
    {
    if ( m_ThreadMin[i] < minimum )
      {
      minimum = m_ThreadMin[i];
      }
    if ( m_ThreadMax[i] > maximum )
      {
      maximum = m_ThreadMax[i];
      }
    }

  // Set the outputs
  this->GetMinimumOutput()->Set(minimum);
  this->GetMaximumOutput()->Set(maximum);
}

template< typename TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  if ( outputRegionForThread.GetNumberOfPixels() == 0 )
    return;

  PixelType localMin = m_ThreadMin[threadId];
  PixelType localMax = m_ThreadMax[threadId];

  ImageRegionConstIterator< TInputImage > it (this->GetInput(), outputRegionForThread);

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels()/2 );

  // Handle the odd pixel separately
  if ( outputRegionForThread.GetNumberOfPixels()%2 == 1 )
    {
    const PixelType value = it.Get();
    localMin = localMax = value;
    ++it;
    }

  // do the work for the even number of pixels 2 at a time
  while ( !it.IsAtEnd() )
    {
    const PixelType value1 = it.Get();
    ++it;
    const PixelType value2 = it.Get();
    ++it;

    if (value1 > value2)
      {
      localMax = std::max(value1,localMax);
      localMin = std::min(value2,localMin);
      }
    else
      {
      localMax = std::max(value2,localMax);
      localMin = std::min(value1,localMin);
      }
    progress.CompletedPixel();
    }

  m_ThreadMin[threadId] = localMin;
  m_ThreadMax[threadId] = localMax;
}

template< typename TImage >
void
MinimumMaximumImageFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Minimum: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( this->GetMinimum() )
     << std::endl;
  os << indent << "Maximum: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( this->GetMaximum() )
     << std::endl;
}
} // end namespace itk
#endif
