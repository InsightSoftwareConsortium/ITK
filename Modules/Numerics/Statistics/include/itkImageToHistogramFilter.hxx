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
#ifndef itkImageToHistogramFilter_hxx
#define itkImageToHistogramFilter_hxx

#include "itkImageToHistogramFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
namespace Statistics
{
template< typename TImage >
ImageToHistogramFilter< TImage >
::ImageToHistogramFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );

  // same default values as in the HistogramGenerator

  typename SimpleDataObjectDecorator<HistogramMeasurementType>::Pointer marginalScale =
    SimpleDataObjectDecorator<HistogramMeasurementType>::New();
  marginalScale->Set(100);
  this->ProcessObject::SetInput( "MarginalScale", marginalScale );

  SimpleDataObjectDecorator<bool>::Pointer autoMinMax =
    SimpleDataObjectDecorator<bool>::New();
  if( typeid(ValueType) == typeid(signed char) || typeid(ValueType) == typeid(unsigned char) )
    {
    autoMinMax->Set(false);
    }
  else
    {
    autoMinMax->Set(true);
    }
   this->ProcessObject::SetInput( "AutoMinimumMaximum", autoMinMax );
}

template< typename TImage >
DataObject::Pointer
ImageToHistogramFilter< TImage >
::MakeOutput( DataObjectPointerArraySizeType itkNotUsed(idx) )
{
  return HistogramType::New().GetPointer();
}

template< typename TImage >
const typename ImageToHistogramFilter< TImage >::HistogramType *
ImageToHistogramFilter< TImage >
::GetOutput() const
{
  const HistogramType *output =
    itkDynamicCastInDebugMode< const HistogramType * >( this->ProcessObject::GetOutput(0) );

  return output;
}

template< typename TImage >
typename ImageToHistogramFilter< TImage >::HistogramType *
ImageToHistogramFilter< TImage >
::GetOutput()
{
  HistogramType *output =
    static_cast< HistogramType * >( this->ProcessObject::GetOutput(0) );

  return output;
}

template< typename TImage >
void
ImageToHistogramFilter< TImage >
::GraftOutput(DataObject *graft)
{
  DataObject *output =
    const_cast< HistogramType * >( this->GetOutput() );

  // Call Histogram to copy meta-information, and the container
  output->Graft(graft);
}


template< typename TImage >
void
ImageToHistogramFilter< TImage >
::BeforeThreadedGenerateData()
{
  // find the actual number of threads
  long nbOfThreads = this->GetNumberOfThreads();
  if ( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() != 0 )
    {
    nbOfThreads = std::min( this->GetNumberOfThreads(), itk::MultiThreader::GetGlobalMaximumNumberOfThreads() );
    }
  // number of threads can be constrained by the region size, so call the
  // SplitRequestedRegion
  // to get the real number of threads which will be used
  RegionType splitRegion;  // dummy region - just to call the following method
  nbOfThreads = this->SplitRequestedRegion(0, nbOfThreads, splitRegion);

  // and allocate one histogram per thread
  m_Histograms.resize(nbOfThreads);
  m_Minimums.resize(nbOfThreads);
  m_Maximums.resize(nbOfThreads);
  m_Barrier = Barrier::New();
  m_Barrier->Initialize(nbOfThreads);
}


template< typename TImage >
void
ImageToHistogramFilter< TImage >
::ThreadedGenerateData(const RegionType & inputRegionForThread, ThreadIdType threadId)
{
  long nbOfPixels = inputRegionForThread.GetNumberOfPixels();
  if( this->GetAutoMinimumMaximumInput() && this->GetAutoMinimumMaximum() )
    {
    // we'll have to iterate over all the pixels 2 times
    nbOfPixels *= 2;
    }
  ProgressReporter progress( this, threadId, nbOfPixels );

  if( threadId == 0 )
    {
    // just use the main one
    m_Histograms[threadId] = this->GetOutput();
    }
  else
    {
    m_Histograms[threadId] = HistogramType::New();
    }
  HistogramType * hist = m_Histograms[threadId];
  hist->SetClipBinsAtEnds(true);

  // the parameter needed to initialize the histogram
  unsigned int nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  HistogramSizeType size( nbOfComponents );
  HistogramMeasurementVectorType min( nbOfComponents );
  HistogramMeasurementVectorType max( nbOfComponents );
  if( this->GetHistogramSizeInput() )
    {
    // user provided value
    size = this->GetHistogramSize();
    }
  else
    {
    // use a default value, which must be computed at run time for the VectorImage
    size.Fill(256);
    }

  if( this->GetAutoMinimumMaximumInput() && this->GetAutoMinimumMaximum() )
    {
    // we have to compute the minimum and maximum values
    this->ThreadedComputeMinimumAndMaximum( inputRegionForThread, threadId, progress );

    // wait for the other threads to complete their part
    m_Barrier->Wait();

    // a non multithreaded part
    if( threadId == 0 )
      {
      min = m_Minimums[0];
      max = m_Maximums[0];
      for( unsigned int t=1; t<m_Minimums.size(); t++ )
        {
        for( unsigned int i=0; i<nbOfComponents; i++ )
          {
          min[i] = std::min( min[i], m_Minimums[t][i] );
          max[i] = std::max( max[i], m_Maximums[t][i] );
          }
        }
      this->ApplyMarginalScale( min, max, size );
      // store the values so they can be retreived by the other threads
      m_Minimums[0] = min;
      m_Maximums[0] = max;
      }

    // wait for all the threads to complete
    m_Barrier->Wait();

    // and get the values computed in the main thread
    min = m_Minimums[0];
    max = m_Maximums[0];
    }
  else
    {
    if( this->GetHistogramBinMinimumInput() )
      {
      min = this->GetHistogramBinMinimum();
      }
    else
      {
      min.Fill( NumericTraits<ValueType>::NonpositiveMin() - 0.5 );
      }
    if( this->GetHistogramBinMaximumInput() )
      {
      max = this->GetHistogramBinMaximum();
      }
    else
      {
      max.Fill( NumericTraits<ValueType>::max() + 0.5 );
      // this->ApplyMarginalScale( min, max, size );
      }
    }

  // finally, initialize the histogram
  hist->SetMeasurementVectorSize( nbOfComponents );
  hist->Initialize( size, min, max );

  // now fill the histograms
  this->ThreadedComputeHistogram( inputRegionForThread, threadId, progress );
}


template< typename TImage >
void
ImageToHistogramFilter< TImage >
::AfterThreadedGenerateData()
{
  // group the results in the output histogram
  HistogramType * hist = m_Histograms[0];
  typename HistogramType::IndexType index;
  for( unsigned int i=1; i<m_Histograms.size(); i++ )
    {
    typedef typename HistogramType::ConstIterator         HistogramIterator;

    HistogramIterator hit = m_Histograms[i]->Begin();
    HistogramIterator end = m_Histograms[i]->End();
    while ( hit != end )
      {
      hist->GetIndex( hit.GetMeasurementVector(), index);
      hist->IncreaseFrequencyOfIndex( index, hit.GetFrequency() );
      ++hit;
      }
    }

  // and drop the temporary histograms
  m_Histograms.clear();
  m_Minimums.clear();
  m_Maximums.clear();
  m_Barrier = ITK_NULLPTR;
}


template< typename TImage >
void
ImageToHistogramFilter< TImage >
::ThreadedComputeMinimumAndMaximum(const RegionType & inputRegionForThread, ThreadIdType threadId, ProgressReporter & progress )
{
  unsigned int nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  HistogramMeasurementVectorType min( nbOfComponents );
  HistogramMeasurementVectorType max( nbOfComponents );

  ImageRegionConstIterator< TImage > inputIt( this->GetInput(), inputRegionForThread );
  inputIt.GoToBegin();
  HistogramMeasurementVectorType m( nbOfComponents );

  min.Fill( NumericTraits<ValueType>::max() );
  max.Fill( NumericTraits<ValueType>::NonpositiveMin() );
  while ( !inputIt.IsAtEnd() )
    {
    const PixelType & p = inputIt.Get();
    NumericTraits<PixelType>::AssignToArray( p, m );
    for( unsigned int i=0; i<nbOfComponents; i++ )
      {
      min[i] = std::min( m[i], min[i] );
      max[i] = std::max( m[i], max[i] );
      }
    progress.CompletedPixel();  // potential exception thrown here
    ++inputIt;
    }
  m_Minimums[threadId] = min;
  m_Maximums[threadId] = max;
}

template< typename TImage >
void
ImageToHistogramFilter< TImage >
::ThreadedComputeHistogram(const RegionType & inputRegionForThread, ThreadIdType threadId, ProgressReporter & progress )
{
  unsigned int nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  ImageRegionConstIterator< TImage > inputIt( this->GetInput(), inputRegionForThread );
  inputIt.GoToBegin();
  HistogramMeasurementVectorType m( nbOfComponents );

  typename HistogramType::IndexType index;
  while ( !inputIt.IsAtEnd() )
    {
    const PixelType & p = inputIt.Get();
    NumericTraits<PixelType>::AssignToArray( p, m );
    m_Histograms[threadId]->GetIndex( m, index );
    m_Histograms[threadId]->IncreaseFrequencyOfIndex( index, 1 );
    ++inputIt;
    progress.CompletedPixel();  // potential exception thrown here
    }
}

template< typename TImage >
void
ImageToHistogramFilter< TImage >
::ApplyMarginalScale( HistogramMeasurementVectorType & min, HistogramMeasurementVectorType & max, HistogramSizeType & size )
{
  unsigned int nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  bool clipHistograms = true;
  for ( unsigned int i = 0; i < nbOfComponents; i++ )
    {
    if ( !NumericTraits< HistogramMeasurementType >::is_integer )
      {
      HistogramMeasurementType marginalScale = this->GetMarginalScale();
      const double margin =
        ( static_cast< HistogramMeasurementType >( max[i] - min[i] )
          / static_cast< HistogramMeasurementType >( size[i] ) )
        / static_cast< HistogramMeasurementType >( marginalScale );

      // Now we check if the max[i] value can be increased by
      // the margin value without saturating the capacity of the
      // HistogramMeasurementType
      if ( ( NumericTraits< HistogramMeasurementType >::max() - max[i] ) > margin )
        {
        max[i] = static_cast< HistogramMeasurementType >( max[i] + margin );
        }
      else
        {
        // an overflow would occur if we add 'margin' to the max
        // therefore we just compromise in setting max = max.
        // Histogram measurement type would force the clipping the max
        // value.
        // Therefore we must call the following to include the max value:
        clipHistograms = false;
        // The above function is okay since here we are within the
        // autoMinMax
        // computation and clearly the user intended to include min and max.
        }
      }
    else
      {
      // max[i] = SafeAssign(max[i] + NumericTraits<MeasurementType>::OneValue());
      // if ( max[i] <= max[i] )
      if(max[i] <
          (static_cast<ValueType>
          (NumericTraits<HistogramMeasurementType>::max()) -
          NumericTraits<ValueType>::OneValue()))
        {
        max[i] = static_cast<HistogramMeasurementType>
          (max[i] + NumericTraits<ValueType>::OneValue());
        }
      else
        {
        // an overflow would have occurred, therefore set max to max
        // Histogram measurement type would force the clipping the max
        // value.
        // Therefore we must call the following to include the max value:
        clipHistograms = false;
        // The above function is okay since here we are within the
        // autoMinMax
        // computation and clearly the user intended to include min and max.
        }
      }
    }
  if( clipHistograms == false )
    {
    for( unsigned int i=0; i<m_Histograms.size(); i++ )
      {
      m_Histograms[i]->SetClipBinsAtEnds(false);
      }
    }
}

template< typename TImage >
void
ImageToHistogramFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  // m_HistogramBinMinimum
  os << indent << "HistogramBinMinimum: " << this->GetHistogramBinMinimumInput() << std::endl;
  // m_HistogramBinMaximum
  os << indent << "HistogramBinMaximum: " << this->GetHistogramBinMaximumInput() << std::endl;
  // m_MarginalScale
  os << indent << "MarginalScale: " << this->GetMarginalScaleInput() << std::endl;
  // m_AutoMinimumMaximum
  os << indent << "AutoMinimumMaximum: " << this->GetAutoMinimumMaximumInput() << std::endl;
  // m_HistogramSize
  os << indent << "HistogramSize: " << this->GetHistogramSizeInput() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
