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


#include "itkImageScanlineIterator.h"
#include <mutex>

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
  m_ThreadMin = NumericTraits< PixelType >::max();
  m_ThreadMax = NumericTraits< PixelType >::NonpositiveMin();
}

template< typename TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::AfterThreadedGenerateData()
{
  this->GetMinimumOutput()->Set(m_ThreadMin);
  this->GetMaximumOutput()->Set(m_ThreadMax);
}

template< typename TInputImage >
void
MinimumMaximumImageFilter< TInputImage >
::DynamicThreadedGenerateData(const RegionType & regionForThread)
{
  if ( regionForThread.GetNumberOfPixels() == 0 )
    {
    return;
    }

  PixelType localMin = NumericTraits< PixelType >::max();
  PixelType localMax = NumericTraits< PixelType >::NonpositiveMin();

  ImageScanlineConstIterator< TInputImage > it (this->GetInput(),  regionForThread);


  // do the work
  while ( !it.IsAtEnd() )
    {
    // Handle the odd pixel separately
    if ( regionForThread.GetSize(0)%2 == 1 )
      {
      const PixelType value = it.Get();
      localMin = std::min(value, localMin);
      localMax = std::max(value, localMax);
      ++it;
      }

    while ( !it.IsAtEndOfLine() )
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
      }
    it.NextLine();

    }

  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  m_ThreadMin = std::min(localMin, m_ThreadMin);
  m_ThreadMax = std::max(localMax, m_ThreadMax);
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
