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
#ifndef __itkMinimumMaximumImageFilter_hxx
#define __itkMinimumMaximumImageFilter_hxx
#include "itkMinimumMaximumImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

#include <vector>

namespace itk
{
template< class TInputImage >
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

template< class TInputImage >
DataObject::Pointer
MinimumMaximumImageFilter< TInputImage >
::MakeOutput(unsigned int output)
{
  switch ( output )
    {
    case 0:
      return static_cast< DataObject * >( TInputImage::New().GetPointer() );
      break;
    case 1:
    case 2:
      return static_cast< DataObject * >( PixelObjectType::New().GetPointer() );
      break;
    default:
      // might as well make an image
      return static_cast< DataObject * >( TInputImage::New().GetPointer() );
      break;
    }
}

template< class TInputImage >
typename MinimumMaximumImageFilter< TInputImage >::PixelObjectType *
MinimumMaximumImageFilter< TInputImage >
::GetMinimumOutput()
{
  return static_cast< PixelObjectType * >( this->ProcessObject::GetOutput(1) );
}

template< class TInputImage >
const typename MinimumMaximumImageFilter< TInputImage >::PixelObjectType *
MinimumMaximumImageFilter< TInputImage >
::GetMinimumOutput() const
{
  return static_cast< const PixelObjectType * >( this->ProcessObject::GetOutput(1) );
}

template< class TInputImage >
typename MinimumMaximumImageFilter< TInputImage >::PixelObjectType *
MinimumMaximumImageFilter< TInputImage >
::GetMaximumOutput()
{
  return static_cast< PixelObjectType * >( this->ProcessObject::GetOutput(2) );
}

template< class TInputImage >
const typename MinimumMaximumImageFilter< TInputImage >::PixelObjectType *
MinimumMaximumImageFilter< TInputImage >
::GetMaximumOutput() const
{
  return static_cast< const PixelObjectType * >( this->ProcessObject::GetOutput(2) );
}

template< class TInputImage >
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

template< class TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< class TInputImage >
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

template< class TInputImage >
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

template< class TInputImage >
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

template< class TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  PixelType value;

  ImageRegionConstIterator< TInputImage > it (this->GetInput(), outputRegionForThread);

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // do the work
  while ( !it.IsAtEnd() )
    {
    value = static_cast< PixelType >( it.Get() );
    if ( value < m_ThreadMin[threadId] )
      {
      m_ThreadMin[threadId] = value;
      }
    if ( value > m_ThreadMax[threadId] )
      {
      m_ThreadMax[threadId] = value;
      }
    ++it;
    progress.CompletedPixel();
    }
}

template< class TImage >
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
