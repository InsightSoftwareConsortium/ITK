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
#ifndef itkFastMarchingImageFilter_hxx
#define itkFastMarchingImageFilter_hxx

#include "itkFastMarchingImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include <algorithm>
#include "itkMath.h"

namespace itk
{
template< typename TLevelSet, typename TSpeedImage >
FastMarchingImageFilter< TLevelSet, TSpeedImage >
::FastMarchingImageFilter():
  m_TrialHeap()
{
  this->ProcessObject::SetNumberOfRequiredInputs(0);

  OutputSizeType outputSize;
  outputSize.Fill(16);
  typename LevelSetImageType::IndexType outputIndex;
  outputIndex.Fill(0);

  m_OutputRegion.SetSize(outputSize);
  m_OutputRegion.SetIndex(outputIndex);

  m_OutputOrigin.Fill(0.0);
  m_OutputSpacing.Fill(1.0);
  m_OutputDirection.SetIdentity();
  m_OverrideOutputInformation = false;

  m_AlivePoints = ITK_NULLPTR;
  m_OutsidePoints = ITK_NULLPTR;
  m_TrialPoints = ITK_NULLPTR;
  m_ProcessedPoints = ITK_NULLPTR;

  m_SpeedConstant = 1.0;
  m_InverseSpeed = -1.0;
  m_LabelImage = LabelImageType::New();

  m_LargeValue    = static_cast< PixelType >( NumericTraits< PixelType >::max() / 2.0 );
  m_StoppingValue = static_cast< double >( m_LargeValue );
  m_CollectPoints = false;

  m_NormalizationFactor = 1.0;
}

template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingImageFilter< TLevelSet, TSpeedImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Alive points: " << m_AlivePoints.GetPointer() << std::endl;
  os << indent << "Trial points: " << m_TrialPoints.GetPointer() << std::endl;
  os << indent << "Speed constant: " << m_SpeedConstant << std::endl;
  os << indent << "Stopping value: " << m_StoppingValue << std::endl;
  os << indent << "Large Value: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_LargeValue )
     << std::endl;
  os << indent << "Normalization Factor: " << m_NormalizationFactor << std::endl;
  os << indent << "Collect points: " << m_CollectPoints << std::endl;
  os << indent << "OverrideOutputInformation: ";
  os << m_OverrideOutputInformation << std::endl;
  os << indent << "OutputRegion: " << m_OutputRegion << std::endl;
  os << indent << "OutputOrigin:  " << m_OutputOrigin << std::endl;
  os << indent << "OutputSpacing: " << m_OutputSpacing << std::endl;
  os << indent << "OutputDirection: " << m_OutputDirection << std::endl;
}

template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingImageFilter< TLevelSet, TSpeedImage >
::GenerateOutputInformation()
{
  // copy output information from input image
  Superclass::GenerateOutputInformation();

  // use user-specified output information
  if ( this->GetInput() == ITK_NULLPTR || m_OverrideOutputInformation )
    {
    LevelSetPointer output = this->GetOutput();
    output->SetLargestPossibleRegion(m_OutputRegion);
    output->SetOrigin(m_OutputOrigin);
    output->SetSpacing(m_OutputSpacing);
    output->SetDirection(m_OutputDirection);
    }
}

template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingImageFilter< TLevelSet, TSpeedImage >
::EnlargeOutputRequestedRegion(
  DataObject *output)
{
  // enlarge the requested region of the output
  // to the whole data set
  TLevelSet *imgData;

  imgData = dynamic_cast< TLevelSet * >( output );
  if ( imgData )
    {
    imgData->SetRequestedRegionToLargestPossibleRegion();
    }
  else
    {
    // Pointer could not be cast to TLevelSet *
    itkWarningMacro( << "itk::FastMarchingImageFilter"
                     << "::EnlargeOutputRequestedRegion cannot cast "
                     << typeid( output ).name() << " to "
                     << typeid( TLevelSet * ).name() );
    }
}

template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingImageFilter< TLevelSet, TSpeedImage >
::Initialize(LevelSetImageType *output)
{
  // allocate memory for the output buffer
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  // cache some buffered region information
  m_BufferedRegion = output->GetBufferedRegion();
  m_StartIndex = m_BufferedRegion.GetIndex();
  m_LastIndex = m_StartIndex + m_BufferedRegion.GetSize();
  typename LevelSetImageType::OffsetType offset;
  offset.Fill(1);
  m_LastIndex -= offset;

  // allocate memory for the PointTypeImage
  m_LabelImage->CopyInformation(output);
  m_LabelImage->SetBufferedRegion(
    output->GetBufferedRegion() );
  m_LabelImage->Allocate();

  // set all output value to infinity
  typedef ImageRegionIterator< LevelSetImageType >
  OutputIterator;

  OutputIterator outIt ( output, output->GetBufferedRegion() );

  PixelType outputPixel;
  outputPixel = m_LargeValue;

  outIt.GoToBegin();

  while( !outIt.IsAtEnd() )
    {
    outIt.Set(outputPixel);
    ++outIt;
    }

  // set all points type to FarPoint
  typedef ImageRegionIterator< LabelImageType > LabelIterator;

  LabelIterator typeIt( m_LabelImage,
                        m_LabelImage->GetBufferedRegion() );


  typeIt.GoToBegin();
  while( !typeIt.IsAtEnd() )
    {
    typeIt.Set(FarPoint);
    ++typeIt;
    }

  // process input alive points
  AxisNodeType node;
  NodeIndexType idx;

  if ( m_AlivePoints )
    {
    typename NodeContainer::ConstIterator pointsIter = m_AlivePoints->Begin();
    typename NodeContainer::ConstIterator pointsEnd = m_AlivePoints->End();

    while( pointsIter != pointsEnd )
      {
      // get node from alive points container
      node = pointsIter.Value();
      idx = node.GetIndex();

      // check if node index is within the output level set
      if ( m_BufferedRegion.IsInside( idx ) )
        {
        // make this an alive point
        m_LabelImage->SetPixel(idx, AlivePoint);

        outputPixel = node.GetValue();
        output->SetPixel(idx, outputPixel);
        }

      ++pointsIter;
      }
    }

  if( m_OutsidePoints )
    {
    typename NodeContainer::ConstIterator pointsIter = m_OutsidePoints->Begin();
    typename NodeContainer::ConstIterator pointsEnd = m_OutsidePoints->End();

    while( pointsIter != pointsEnd )
      {
      // get node from alive points container
      node = pointsIter.Value();
      idx = node.GetIndex();

      // check if node index is within the output level set
      if ( m_BufferedRegion.IsInside( idx ) )
        {
        // make this an alive point
        m_LabelImage->SetPixel(idx, OutsidePoint );

        outputPixel = node.GetValue();
        output->SetPixel(idx, outputPixel);
        }

      ++pointsIter;
      }
    }

  // make sure the heap is empty
  while ( !m_TrialHeap.empty() )
    {
    m_TrialHeap.pop();
    }

  // process the input trial points
  if ( m_TrialPoints )
    {
    typename NodeContainer::ConstIterator pointsIter = m_TrialPoints->Begin();
    typename NodeContainer::ConstIterator pointsEnd = m_TrialPoints->End();

    while( pointsIter != pointsEnd )
      {
      // get node from trial points container
      node = pointsIter.Value();
      idx = node.GetIndex();

      // check if node index is within the output level set
      if ( m_BufferedRegion.IsInside( idx ) )
        {
        // make this an initial trial point
        m_LabelImage->SetPixel(idx, InitialTrialPoint);

        outputPixel = node.GetValue();
        output->SetPixel(idx, outputPixel);

        m_TrialHeap.push(node);
        }
      ++pointsIter;
      }
    }
}

template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingImageFilter< TLevelSet, TSpeedImage >
::GenerateData()
{
  if( m_NormalizationFactor < itk::Math::eps )
    {
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation(ITK_LOCATION);
    err.SetDescription("Normalization Factor is null or negative");
    throw err;
    }

  LevelSetPointer        output      = this->GetOutput();
  SpeedImageConstPointer speedImage  = this->GetInput();

  this->Initialize(output);

  if ( m_CollectPoints )
    {
    m_ProcessedPoints = NodeContainer::New();
    }

  // process points on the heap
  AxisNodeType node;
  double       currentValue;
  double       oldProgress = 0;

  this->UpdateProgress(0.0);   // Send first progress event

  // CACHE
  while ( !m_TrialHeap.empty() )
    {
    // get the node with the smallest value
    node = m_TrialHeap.top();
    m_TrialHeap.pop();

    // does this node contain the current value ?
    currentValue = static_cast< double >( output->GetPixel( node.GetIndex() ) );

    if ( Math::ExactlyEquals(node.GetValue(), currentValue) )
      {
      // is this node already alive ?
      if ( m_LabelImage->GetPixel( node.GetIndex() ) != AlivePoint )
        {
        if ( currentValue > m_StoppingValue )
          {
          this->UpdateProgress(1.0);
          break;
          }

        if ( m_CollectPoints )
          {
          m_ProcessedPoints->InsertElement(m_ProcessedPoints->Size(), node);
          }

        // set this node as alive
        m_LabelImage->SetPixel(node.GetIndex(), AlivePoint);

        // update its neighbors
        this->UpdateNeighbors(node.GetIndex(), speedImage, output);

        // Send events every certain number of points.
        const double newProgress = currentValue / m_StoppingValue;
        if ( newProgress - oldProgress > 0.01 )  // update every 1%
          {
          this->UpdateProgress(newProgress);
          oldProgress = newProgress;
          if ( this->GetAbortGenerateData() )
            {
            this->InvokeEvent( AbortEvent() );
            this->ResetPipeline();
            ProcessAborted e(__FILE__, __LINE__);
            e.SetDescription("Process aborted.");
            e.SetLocation(ITK_LOCATION);
            throw e;
            }
          }
        }
      }
    }
}

template< typename TLevelSet, typename TSpeedImage >
void
FastMarchingImageFilter< TLevelSet, TSpeedImage >
::UpdateNeighbors(
  const IndexType & index,
  const SpeedImageType *speedImage,
  LevelSetImageType *output)
{
  IndexType neighIndex = index;
  unsigned char label;

  for ( unsigned int j = 0; j < SetDimension; j++ )
    {
    // update left neighbor
    if ( index[j] > m_StartIndex[j] )
      {
      neighIndex[j] = index[j] - 1;
      }

    label = m_LabelImage->GetPixel(neighIndex);

    if ( ( label != AlivePoint ) &&
         ( label != InitialTrialPoint ) &&
         ( label != OutsidePoint ) )
      {
      this->UpdateValue(neighIndex, speedImage, output);
      }

    // update right neighbor
    if ( index[j] < m_LastIndex[j] )
      {
      neighIndex[j] = index[j] + 1;
      }

    label = m_LabelImage->GetPixel(neighIndex);

    if ( ( label != AlivePoint ) &&
         ( label != InitialTrialPoint ) &&
         ( label != OutsidePoint ) )
      {
      this->UpdateValue(neighIndex, speedImage, output);
      }

    //reset neighIndex
    neighIndex[j] = index[j];
    }
}

template< typename TLevelSet, typename TSpeedImage >
double
FastMarchingImageFilter< TLevelSet, TSpeedImage >
::UpdateValue(
  const IndexType & index,
  const SpeedImageType *speedImage,
  LevelSetImageType *output)
{
  IndexType neighIndex = index;

  PixelType neighValue;

  // just to make sure the index is initialized (really cautious)
  AxisNodeType node;
  node.SetIndex( index );

  for ( unsigned int j = 0; j < SetDimension; j++ )
    {
    node.SetValue(m_LargeValue);

    // find smallest valued neighbor in this dimension
    for ( int s = -1; s < 2; s = s + 2 )
      {
      neighIndex[j] = index[j] + s;

      // make sure neighIndex is not outside from the image
      if ( ( neighIndex[j] > m_LastIndex[j] ) ||
           ( neighIndex[j] < m_StartIndex[j] ) )
        {
        continue;
        }

      if ( m_LabelImage->GetPixel( neighIndex ) == AlivePoint )
        {
        neighValue = static_cast< PixelType >( output->GetPixel(neighIndex) );

        // let's find the minimum value given a direction j
        if ( node.GetValue() > neighValue )
          {
          node.SetValue(neighValue);
          node.SetIndex(neighIndex);
          }
        }
      }

    // put the minimum neighbor onto the heap
    m_NodesUsed[j] = node;
    m_NodesUsed[j].SetAxis(j);

    // reset neighIndex
    neighIndex[j] = index[j];
    }

  // sort the local list
  std::sort(m_NodesUsed, m_NodesUsed + SetDimension);

  // solve quadratic equation
  double solution = static_cast< double >( m_LargeValue );

  double aa( 0.0 );
  double bb( 0.0 );
  double cc( m_InverseSpeed );

  if ( speedImage )
    {
    cc = static_cast< double >( speedImage->GetPixel(index)  ) / m_NormalizationFactor;
    cc = -1.0 * itk::Math::sqr(1.0 / cc);
    }

  OutputSpacingType spacing = /* this->GetOutput() */ output->GetSpacing();

  double discrim;

  for ( unsigned int j = 0; j < SetDimension; j++ )
    {
    node = m_NodesUsed[j];
    const double value = static_cast< double >( node.GetValue() );

    if ( solution >= value )
      {
      const int    axis = node.GetAxis();
      // spaceFactor = \frac{1}{spacing[axis]^2}
      const double spaceFactor = itk::Math::sqr(1.0 / spacing[axis]);
      aa += spaceFactor;
      bb += value * spaceFactor;
      cc += itk::Math::sqr(value) * spaceFactor;

      discrim = itk::Math::sqr(bb) - aa * cc;
      if ( discrim < 0.0 )
        {
        // Discriminant of quadratic eqn. is negative
        ExceptionObject err(__FILE__, __LINE__);
        err.SetLocation(ITK_LOCATION);
        err.SetDescription("Discriminant of quadratic equation is negative");
        throw err;
        }

      solution = ( std::sqrt(discrim) + bb ) / aa;
      }
    else
      {
      break;
      }
    }

  if ( solution < m_LargeValue )
    {
    // write solution to m_OutputLevelSet
    PixelType outputPixel = static_cast< PixelType >( solution );
    output->SetPixel(index, outputPixel);

    // insert point into trial heap
    m_LabelImage->SetPixel(index, TrialPoint);
    node.SetValue( outputPixel );
    node.SetIndex( index );
    m_TrialHeap.push(node);
    }

  return solution;
}
} // namespace itk

#endif
