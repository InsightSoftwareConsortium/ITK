/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkFastMarchingImageFilter_txx
#define _itkFastMarchingImageFilter_txx


#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "vnl/vnl_math.h"
#include <algorithm>

namespace itk
{

/**
 *
 */
template <class TLevelSet, class TSpeedImage>
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::FastMarchingImageFilter()
{

  this->ProcessObject::SetNumberOfRequiredInputs(0);

  for ( int j = 0; j < SetDimension; j++ )
    {
    m_OutputSize[j] = 16;
    }

  m_AlivePoints = NULL;
  m_TrialPoints = NULL;

  m_SpeedConstant = 1.0;
  m_InverseSpeed = -1.0;
  m_LabelImage = LabelImageType::New();

  typedef typename LevelSetImageType::PixelType PixelType;
  m_LargeValue = NumericTraits<PixelType>::max();
  m_StoppingValue =  static_cast<double>( m_LargeValue );
  m_CollectPoints = false;

}


/**
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Alive points: " << m_AlivePoints.GetPointer() << std::endl;
  os << indent << "Trial points: " << m_TrialPoints.GetPointer() << std::endl;
  os << indent << "Speed constant: " << m_SpeedConstant << std::endl;
  os << indent << "Stopping value: " << m_StoppingValue << std::endl;
  os << indent << "Collect points: " << m_CollectPoints << std::endl;
  os << indent << "Output size: " << m_OutputSize << std::endl;
}


/**
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::SetSpeedImage(
SpeedImageType * ptr )
{
  this->SetInput( ptr );
}


/**
 *
 */
template <class TLevelSet, class TSpeedImage>
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::SpeedImagePointer
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::GetSpeedImage()
{
  return this->GetInput();
}


/**
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::GenerateOutputInformation()
{

  // make the output of the size specified by m_OutputSize
  LevelSetPointer output = this->GetOutput();
 
  typename TLevelSet::RegionType region;
  region.SetSize( m_OutputSize );
  region.SetIndex( IndexType::ZeroIndex );

  output->SetLargestPossibleRegion( region );

}


/**
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::EnlargeOutputRequestedRegion(
DataObject *output )
{
  // enlarge the requested region of the output
  // to the whole data set
  TLevelSet * imgData;

  imgData = dynamic_cast<TLevelSet*>( output );
  imgData->SetRequestedRegionToLargestPossibleRegion();

}



/**
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::Initialize( LevelSetImageType * output )
{

  // allocate memory for the output buffer
  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();


  // allocate memory for the PointTypeImage
  m_LabelImage->SetLargestPossibleRegion( 
    output->GetLargestPossibleRegion() );
  m_LabelImage->SetBufferedRegion( 
    output->GetBufferedRegion() );
  m_LabelImage->Allocate();


  // set all output value to infinity
  typedef ImageRegionIterator<LevelSetImageType>
    OutputIterator;
  
  OutputIterator outIt ( output, output->GetBufferedRegion() );

  PixelType outputPixel;
  outputPixel = m_LargeValue;
  
  for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
    {
    outIt.Set( outputPixel );
    }


  // set all points type to FarPoint
  typedef ImageRegionIterator< LabelImageType > 
    LabelIterator;

  LabelIterator typeIt( m_LabelImage,
    m_LabelImage->GetBufferedRegion() );

  for ( typeIt.GoToBegin(); !typeIt.IsAtEnd(); ++typeIt )
    {
    typeIt.Set( FarPoint );
    }


  // process input alive points
  typename NodeContainer::ConstIterator pointsIter;
  typename NodeContainer::ConstIterator pointsEnd;
  NodeType node;

  if ( m_AlivePoints ) 
    {
    pointsIter = m_AlivePoints->Begin();
    pointsEnd = m_AlivePoints->End();

    for ( ; pointsIter != pointsEnd; ++pointsIter )
      {

      // get node from alive points container
      node = pointsIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for ( int j = 0; j < SetDimension; j++ )
        {
        if ( node.index[j] > (signed long) m_OutputSize[j] )
          {
          inRange = false;
          break;
          }
        }
      if ( !inRange ) continue;

      // make this an alive point
      m_LabelImage->SetPixel( node.index, AlivePoint );

      outputPixel = node.value;
      output->SetPixel( node.index, outputPixel );

      }
    }


  // make sure the heap is empty
  while ( !m_TrialHeap.empty() )
    {
    m_TrialHeap.pop();
    }


  // process the input trail points
  if ( m_TrialPoints )
    {
    pointsIter = m_TrialPoints->Begin();
    pointsEnd = m_TrialPoints->End();

    for ( ; pointsIter != pointsEnd; ++pointsIter )
      {
      
      // get node from trail points container
      node = pointsIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for ( int j = 0; j < SetDimension; j++ )
      {
       if( node.index[j] > (signed long) m_OutputSize[j] )
        {
        inRange = false;
        break;
        }
      }
      if ( !inRange ) continue;

      // make this a trail point
      m_LabelImage->SetPixel( node.index, TrialPoint );

      outputPixel = node.value;
      output->SetPixel( node.index, outputPixel );

      m_TrialHeap.push( node );

      }
    }
      
}


/**
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::GenerateData()
{

  LevelSetPointer output = this->GetOutput();
  SpeedImagePointer speedImage = this->GetInput();

  this->Initialize( output );

  if ( m_CollectPoints )
    {
    m_ProcessedPoints = NodeContainer::New();
    }
  
  // process points on the heap
  NodeType node;
  double currentValue;
  double oldValue = -m_LargeValue;

  unsigned long NumPoints = 0;
  unsigned long InvalidPoints = 0;

  while ( !m_TrialHeap.empty() )
    {
    // get the node with the smallest value
    node = m_TrialHeap.top();
    m_TrialHeap.pop();

    if ( this->GetDebug() ) { NumPoints++; }

    // does this node contain the current value ?
    currentValue = (double) output->GetPixel( node.index );

    if ( node.value != currentValue )
      {
      if( this->GetDebug() ) { InvalidPoints++; }
      continue;
      } 

    // is this node already alive ?
    if ( m_LabelImage->GetPixel( node.index ) != TrialPoint ) 
      {
      if( this->GetDebug() ) { InvalidPoints++; }
      continue;
      }

    if ( currentValue > m_StoppingValue )
      {
      itkDebugMacro(<< "stopping value reached");
      break;
      }

    if ( this->GetDebug() && currentValue < oldValue) 
      {
      itkDebugMacro(<< "error value decrease at:" << node.index );
      }
    oldValue = currentValue;

    if ( m_CollectPoints )
      {
      m_ProcessedPoints->InsertElement( m_ProcessedPoints->Size(), node );
      }
    
    // set this node as alive
    m_LabelImage->SetPixel( node.index, AlivePoint );

    // update its neighbors
    this->UpdateNeighbors( node.index, speedImage, output );

    }
  
  itkDebugMacro(<< "No. points processed: " << NumPoints);
  itkDebugMacro(<< "No. invalid points: " << InvalidPoints);
}

/**
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::UpdateNeighbors(
IndexType& index,
SpeedImageType * speedImage,
LevelSetImageType * output )
{
  IndexType neighIndex = index;

  for ( int j = 0; j < SetDimension; j++ )
  {
    // update left neighbor
    if( index[j] > 0 )
    {
      neighIndex[j] = index[j] - 1;
    }
    if ( m_LabelImage->GetPixel( neighIndex ) != AlivePoint )
    {
      this->UpdateValue( neighIndex, speedImage, output );
    }

    // updaet right neighbor
    if ( index[j] < (signed long) m_OutputSize[j] - 1 )
    {
      neighIndex[j] = index[j] + 1;
    }
    if ( m_LabelImage->GetPixel( neighIndex ) != AlivePoint )
    {
      this->UpdateValue( neighIndex, speedImage, output );
    }

    //reset neighIndex
    neighIndex[j] = index[j];
      
  }

}


/**
 *
 */
template <class TLevelSet, class TSpeedImage>
double
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::UpdateValue(
IndexType& index,
SpeedImageType * speedImage,
LevelSetImageType * output )
{

  IndexType neighIndex = index;
  typename TLevelSet::PixelType neighValue;
  PixelType outputPixel;
  NodeType node;

  for ( int j = 0; j < SetDimension; j++ )
  {
    node.value = m_LargeValue;

    // find smallest valued neighbor in this dimension
    for( int s = -1; s < 2; s = s + 2 )
    {
      neighIndex[j] = index[j] + s;

      if( neighIndex[j] > (signed long) m_OutputSize[j] - 1 || 
          neighIndex[j] < 0 )
      {
        continue;
      }

      if ( m_LabelImage->GetPixel( neighIndex ) == AlivePoint )
      {
        outputPixel = output->GetPixel( neighIndex );
        neighValue = outputPixel ;

        if( node.value > neighValue )
        {
          node.value = neighValue;
          node.index = neighIndex;
        }
      }

    }

    // put the minimum neighbor onto the heap
    m_NodesUsed[j] = node;
    
    // reset neighIndex
    neighIndex[j] = index[j];

  }

  // sort the local list
  std::sort( m_NodesUsed, m_NodesUsed + SetDimension );

  // solve quadratic equation
  double aa, bb, cc;
  double solution = m_LargeValue;
  
  aa = 0.0;
  bb = 0.0;
  if ( speedImage )
  {
    typedef typename SpeedImageType::PixelType SpeedPixelType;
    cc = (double) speedImage->GetPixel( index ) ;
    cc = -1.0 * vnl_math_sqr( 1.0 / cc );
  }
  else 
  {
    cc = m_InverseSpeed;
  }

  double discrim;

  for ( int j = 0; j < SetDimension; j++ )
  {
    node = m_NodesUsed[j];

    if ( solution >= node.value )
    {
      aa += 1.0;
      bb += node.value;
      cc += vnl_math_sqr( node.value );

      discrim = vnl_math_sqr( bb ) - aa * cc;
      if ( discrim < 0.0 )
      {
        // Discriminant of quadratic eqn. is negative
        ExceptionObject err(__FILE__, __LINE__);
        err.SetLocation( "UpdateValue" );
        err.SetDescription( "Discriminant of quadratic equation is negative" );
        throw err;
      }
    
      solution = ( vnl_math_sqrt( discrim ) + bb ) / aa;
    }
    else
    {
      break;
    }
  }

  if ( solution < m_LargeValue )
  {
  // write solution to m_OutputLevelSet
  outputPixel = solution;
  output->SetPixel( index, outputPixel );

    // insert point into trial heap
  m_LabelImage->SetPixel( index, TrialPoint );
  node.value = solution;
  node.index = index;
  m_TrialHeap.push( node );
  }

  return solution;

}

} // namespace itk


#endif
