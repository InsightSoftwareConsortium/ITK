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
#ifndef itkStatisticsImageFilter_hxx
#define itkStatisticsImageFilter_hxx
#include "itkStatisticsImageFilter.h"


#include "itkImageScanlineIterator.h"
#include <mutex>

namespace itk
{
template< typename TInputImage >
StatisticsImageFilter< TInputImage >
::StatisticsImageFilter()
  :m_ThreadSum(1),
   m_SumOfSquares(1),
   m_Count(1),
   m_ThreadMin(1),
   m_ThreadMax(1)
{
  // first output is a copy of the image, DataObject created by superclass
  //
  // allocate the data objects for the outputs which are
  // just decorators around pixel types
  for ( int i = 1; i < 3; ++i )
    {
    typename PixelObjectType::Pointer output =
      static_cast< PixelObjectType * >( this->MakeOutput(i).GetPointer() );
    this->ProcessObject::SetNthOutput( i, output.GetPointer() );
    }
  // allocate the data objects for the outputs which are
  // just decorators around real types
  for ( int i = 3; i < 8; ++i )
    {
    typename RealObjectType::Pointer output =
      static_cast< RealObjectType * >( this->MakeOutput(i).GetPointer() );
    this->ProcessObject::SetNthOutput( i, output.GetPointer() );
    }

  this->GetMinimumOutput()->Set( NumericTraits< PixelType >::max() );
  this->GetMaximumOutput()->Set( NumericTraits< PixelType >::NonpositiveMin() );
  this->GetMeanOutput()->Set( NumericTraits< RealType >::max() );
  this->GetSigmaOutput()->Set( NumericTraits< RealType >::max() );
  this->GetVarianceOutput()->Set( NumericTraits< RealType >::max() );
  this->GetSumOutput()->Set(NumericTraits< RealType >::ZeroValue());
  this->GetSumOfSquaresOutput()->Set(NumericTraits< RealType >::ZeroValue());
}

template< typename TInputImage >
DataObject::Pointer
StatisticsImageFilter< TInputImage >
::MakeOutput(DataObjectPointerArraySizeType output)
{
  switch ( output )
    {
    case 0:
      return TInputImage::New().GetPointer();
      break;
    case 1:
      return PixelObjectType::New().GetPointer();
      break;
    case 2:
      return PixelObjectType::New().GetPointer();
      break;
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
      return RealObjectType::New().GetPointer();
      break;
    default:
      // might as well make an image
      return TInputImage::New().GetPointer();
      break;
    }
}

template< typename TInputImage >
typename StatisticsImageFilter< TInputImage >::PixelObjectType *
StatisticsImageFilter< TInputImage >
::GetMinimumOutput()
{
  return static_cast< PixelObjectType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage >
const typename StatisticsImageFilter< TInputImage >::PixelObjectType *
StatisticsImageFilter< TInputImage >
::GetMinimumOutput() const
{
  return static_cast< const PixelObjectType * >( this->ProcessObject::GetOutput(1) );
}

template< typename TInputImage >
typename StatisticsImageFilter< TInputImage >::PixelObjectType *
StatisticsImageFilter< TInputImage >
::GetMaximumOutput()
{
  return static_cast< PixelObjectType * >( this->ProcessObject::GetOutput(2) );
}

template< typename TInputImage >
const typename StatisticsImageFilter< TInputImage >::PixelObjectType *
StatisticsImageFilter< TInputImage >
::GetMaximumOutput() const
{
  return static_cast< const PixelObjectType * >( this->ProcessObject::GetOutput(2) );
}

template< typename TInputImage >
typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetMeanOutput()
{
  return static_cast< RealObjectType * >( this->ProcessObject::GetOutput(3) );
}

template< typename TInputImage >
const typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetMeanOutput() const
{
  return static_cast< const RealObjectType * >( this->ProcessObject::GetOutput(3) );
}

template< typename TInputImage >
typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetSigmaOutput()
{
  return static_cast< RealObjectType * >( this->ProcessObject::GetOutput(4) );
}

template< typename TInputImage >
const typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetSigmaOutput() const
{
  return static_cast< const RealObjectType * >( this->ProcessObject::GetOutput(4) );
}

template< typename TInputImage >
typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetVarianceOutput()
{
  return static_cast< RealObjectType * >( this->ProcessObject::GetOutput(5) );
}

template< typename TInputImage >
const typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetVarianceOutput() const
{
  return static_cast< const RealObjectType * >( this->ProcessObject::GetOutput(5) );
}

template< typename TInputImage >
typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetSumOutput()
{
  return static_cast< RealObjectType * >( this->ProcessObject::GetOutput(6) );
}

template< typename TInputImage >
const typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetSumOutput() const
{
  return static_cast< const RealObjectType * >( this->ProcessObject::GetOutput(6) );
}


template< typename TInputImage >
typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetSumOfSquaresOutput()
{
  return static_cast< RealObjectType * >( this->ProcessObject::GetOutput(7) );
}

template< typename TInputImage >
const typename StatisticsImageFilter< TInputImage >::RealObjectType *
StatisticsImageFilter< TInputImage >
::GetSumOfSquaresOutput() const
{
  return static_cast< const RealObjectType * >( this->ProcessObject::GetOutput(7) );
}

template< typename TInputImage >
void
StatisticsImageFilter< TInputImage >
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
StatisticsImageFilter< TInputImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage >
void
StatisticsImageFilter< TInputImage >
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
StatisticsImageFilter< TInputImage >
::BeforeThreadedGenerateData()
{
  // Resize the thread temporaries
  m_Count = NumericTraits< SizeValueType >::ZeroValue();
  m_SumOfSquares = NumericTraits< RealType >::ZeroValue();
  m_ThreadSum = NumericTraits< RealType >::ZeroValue();
  m_ThreadMin = NumericTraits< PixelType >::max();
  m_ThreadMax = NumericTraits< PixelType >::NonpositiveMin();

}

template< typename TInputImage >
void
StatisticsImageFilter< TInputImage >
::AfterThreadedGenerateData()
{
  const SizeValueType count = m_Count;
  const RealType      sumOfSquares(m_SumOfSquares);
  const PixelType     minimum = m_ThreadMin;
  const PixelType     maximum = m_ThreadMax;
  const RealType      sum(m_ThreadSum);

  const RealType  mean = sum / static_cast< RealType >( count );
  const RealType  variance = ( sumOfSquares - ( sum * sum / static_cast< RealType >( count ) ) )
    / ( static_cast< RealType >( count ) - 1 );
  const RealType  sigma = std::sqrt(variance);

  // Set the outputs
  this->GetMinimumOutput()->Set(minimum);
  this->GetMaximumOutput()->Set(maximum);
  this->GetMeanOutput()->Set(mean);
  this->GetSigmaOutput()->Set(sigma);
  this->GetVarianceOutput()->Set(variance);
  this->GetSumOutput()->Set(sum);
  this->GetSumOfSquaresOutput()->Set(sumOfSquares);
}

template< typename TInputImage >
void
StatisticsImageFilter< TInputImage >
::DynamicThreadedGenerateData(const RegionType & regionForThread)
{

  CompensatedSummation<RealType> sum = NumericTraits< RealType >::ZeroValue();
  CompensatedSummation<RealType> sumOfSquares = NumericTraits< RealType >::ZeroValue();
  SizeValueType count = NumericTraits< SizeValueType >::ZeroValue();
  PixelType min = NumericTraits< PixelType >::max();
  PixelType max = NumericTraits< PixelType >::NonpositiveMin();

  ImageScanlineConstIterator< TInputImage > it (this->GetInput(),  regionForThread);

  // do the work
  while ( !it.IsAtEnd() )
    {
    while ( !it.IsAtEndOfLine() )
      {
      const PixelType& value  = it.Get();
      const RealType    realValue = static_cast< RealType >( value );
      min = std::min(min, value);
      max = std::max(max, value);

      sum += realValue;
      sumOfSquares += ( realValue * realValue );
      ++count;
      ++it;
      }
    it.NextLine();

    }

  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  m_ThreadSum += sum;
  m_SumOfSquares += sumOfSquares;
  m_Count += count;
  m_ThreadMin = std::min( min, m_ThreadMin );
  m_ThreadMax = std::max( max, m_ThreadMax );
}

template< typename TImage >
void
StatisticsImageFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Count: " << static_cast< typename NumericTraits<SizeValueType>::PrintType>( this->m_Count ) << std::endl;
  os << indent << "Minimum: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( this->GetMinimum() ) << std::endl;
  os << indent << "Maximum: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( this->GetMaximum() ) << std::endl;
  os << indent << "Sum: "          << this->GetSum() << std::endl;
  os << indent << "Mean: "         << this->GetMean() << std::endl;
  os << indent << "Sigma: "        << this->GetSigma() << std::endl;
  os << indent << "Variance: "     << this->GetVariance() << std::endl;
  os << indent << "SumOfSquares: " << this->GetSumOfSquares() << std::endl;
}
} // end namespace itk
#endif
