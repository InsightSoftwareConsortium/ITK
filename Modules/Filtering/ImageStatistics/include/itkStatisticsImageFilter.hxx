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
#include "itkProgressReporter.h"

namespace itk
{
template< typename TInputImage >
StatisticsImageFilter< TInputImage >
::StatisticsImageFilter():m_ThreadSum(1), m_SumOfSquares(1), m_Count(1), m_ThreadMin(1), m_ThreadMax(1)
{
  // first output is a copy of the image, DataObject created by
  // superclass
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
  for ( int i = 3; i < 7; ++i )
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
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  // Resize the thread temporaries
  m_Count.SetSize(numberOfThreads);
  m_SumOfSquares.SetSize(numberOfThreads);
  m_ThreadSum.SetSize(numberOfThreads);
  m_ThreadMin.SetSize(numberOfThreads);
  m_ThreadMax.SetSize(numberOfThreads);

  // Initialize the temporaries
  m_Count.Fill(NumericTraits< SizeValueType >::ZeroValue());
  m_ThreadSum.Fill(NumericTraits< RealType >::ZeroValue());
  m_SumOfSquares.Fill(NumericTraits< RealType >::ZeroValue());
  m_ThreadMin.Fill( NumericTraits< PixelType >::max() );
  m_ThreadMax.Fill( NumericTraits< PixelType >::NonpositiveMin() );
}

template< typename TInputImage >
void
StatisticsImageFilter< TInputImage >
::AfterThreadedGenerateData()
{
  ThreadIdType    i;
  SizeValueType   count;
  RealType        sumOfSquares;

  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  PixelType minimum;
  PixelType maximum;
  RealType  mean;
  RealType  sigma;
  RealType  variance;
  RealType  sum;

  sum = sumOfSquares = NumericTraits< RealType >::ZeroValue();
  count = 0;

  // Find the min/max over all threads and accumulate count, sum and
  // sum of squares
  minimum = NumericTraits< PixelType >::max();
  maximum = NumericTraits< PixelType >::NonpositiveMin();
  for ( i = 0; i < numberOfThreads; i++ )
    {
    count += m_Count[i];
    sum += m_ThreadSum[i];
    sumOfSquares += m_SumOfSquares[i];

    if ( m_ThreadMin[i] < minimum )
      {
      minimum = m_ThreadMin[i];
      }
    if ( m_ThreadMax[i] > maximum )
      {
      maximum = m_ThreadMax[i];
      }
    }
  // compute statistics
  mean = sum / static_cast< RealType >( count );

  // unbiased estimate
  variance = ( sumOfSquares - ( sum * sum / static_cast< RealType >( count ) ) )
             / ( static_cast< RealType >( count ) - 1 );
  sigma = std::sqrt(variance);

  // Set the outputs
  this->GetMinimumOutput()->Set(minimum);
  this->GetMaximumOutput()->Set(maximum);
  this->GetMeanOutput()->Set(mean);
  this->GetSigmaOutput()->Set(sigma);
  this->GetVarianceOutput()->Set(variance);
  this->GetSumOutput()->Set(sum);
}

template< typename TInputImage >
void
StatisticsImageFilter< TInputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if( size0 == 0)
    {
    return;
    }
  RealType  realValue;
  PixelType value;

  RealType sum = NumericTraits< RealType >::ZeroValue();
  RealType sumOfSquares = NumericTraits< RealType >::ZeroValue();
  SizeValueType count = NumericTraits< SizeValueType >::ZeroValue();
  PixelType min = NumericTraits< PixelType >::max();
  PixelType max = NumericTraits< PixelType >::NonpositiveMin();

  ImageScanlineConstIterator< TInputImage > it (this->GetInput(),  outputRegionForThread);

  // support progress methods/callbacks
  const size_t numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / size0;
  ProgressReporter progress( this, threadId, static_cast<itk::SizeValueType>( numberOfLinesToProcess ) );

  // do the work
  while ( !it.IsAtEnd() )
    {
    while ( !it.IsAtEndOfLine() )
      {
      value = it.Get();
      realValue = static_cast< RealType >( value );
      if ( value < min )
        {
        min = value;
        }
      if ( value > max )
        {
        max  = value;
        }

      sum += realValue;
      sumOfSquares += ( realValue * realValue );
      ++count;
      ++it;
      }
    it.NextLine();
    progress.CompletedPixel();
    }

  m_ThreadSum[threadId] = sum;
  m_SumOfSquares[threadId] = sumOfSquares;
  m_Count[threadId] = count;
  m_ThreadMin[threadId] = min;
  m_ThreadMax[threadId] = max;
}

template< typename TImage >
void
StatisticsImageFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Minimum: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( this->GetMinimum() ) << std::endl;
  os << indent << "Maximum: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( this->GetMaximum() ) << std::endl;
  os << indent << "Sum: "      << this->GetSum() << std::endl;
  os << indent << "Mean: "     << this->GetMean() << std::endl;
  os << indent << "Sigma: "    << this->GetSigma() << std::endl;
  os << indent << "Variance: " << this->GetVariance() << std::endl;
}
} // end namespace itk
#endif
