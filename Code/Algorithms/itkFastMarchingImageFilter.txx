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

  for( int j = 0; j < SetDimension; j++ )
    {
    m_OutputSize[j] = 16;
    }

  m_AlivePoints = NULL;
  m_TrialPoints = NULL;
  m_SpeedImage  = NULL;

  m_SpeedConstant = 1.0;
  m_InverseSpeed = -1.0;
  m_LabelImage = LabelImageType::New();

  typedef typename LevelSetImageType::PixelType PixelType;
  m_LargeValue = NumericTraits<PixelType>::max();
  m_StoppingValue =  (double) m_LargeValue;
  m_CollectPoints = false;

  m_NodesUsed.resize( SetDimension );

  m_DebugOn = false;

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
  os << indent << "Fast Marching" << std::endl;
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
  // check if anything needs to be done
  if( m_SpeedImage == ptr )
    {
    return;
    }

  m_SpeedImage = ptr;
  this->ProcessObject::SetNthInput(0, ptr );
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
::Initialize()
{
  // get the filter output
  m_OutputLevelSet = this->GetOutput();


  // allocate memory for the output buffer
  m_OutputLevelSet->SetBufferedRegion( 
    m_OutputLevelSet->GetRequestedRegion() );
  m_OutputLevelSet->Allocate();


  // allocate memory for the PointTypeImage
  m_LabelImage->SetLargestPossibleRegion( 
    m_OutputLevelSet->GetLargestPossibleRegion() );
  m_LabelImage->SetBufferedRegion( 
    m_OutputLevelSet->GetBufferedRegion() );
  m_LabelImage->Allocate();


  // set all output value to infinity
  typedef ImageRegionIterator<LevelSetImageType>
    OutputIterator;
  
  OutputIterator outIt ( m_OutputLevelSet,
    m_OutputLevelSet->GetBufferedRegion() );

  PixelType outputPixel;
  outputPixel = m_LargeValue;
  
  for( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
    {
    outIt.Set( outputPixel );
    }


  // set all points type to FarPoint
  typedef ImageRegionIterator< LabelImageType > 
    LabelIterator;

  LabelIterator typeIt( m_LabelImage,
    m_LabelImage->GetBufferedRegion() );

  for( typeIt.GoToBegin(); !typeIt.IsAtEnd(); ++typeIt )
    {
    typeIt.Set( FarPoint );
    }


  // process input alive points
  typename NodeContainer::ConstIterator pointsIter;
  typename NodeContainer::ConstIterator pointsEnd;
  NodeType node;

  if( m_AlivePoints ) 
    {
    pointsIter = m_AlivePoints->Begin();
    pointsEnd = m_AlivePoints->End();

    for( ; pointsIter != pointsEnd; ++pointsIter )
      {

      // get node from alive points container
      node = pointsIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for( int j = 0; j < SetDimension; j++ )
        {
        if( node.index[j] > (signed long) m_OutputSize[j] )
          {
          inRange = false;
          break;
          }
        }
      if( !inRange ) continue;

      // make this an alive point
      m_LabelImage->SetPixel( node.index, AlivePoint );

      outputPixel = node.value;
      m_OutputLevelSet->SetPixel( node.index, outputPixel );

      }
    }


  // make sure the heap is empty
  while( !m_TrialHeap.empty() )
    {
    m_TrialHeap.pop();
    }


  // process the input trail points
  if( m_TrialPoints )
    {
    pointsIter = m_TrialPoints->Begin();
    pointsEnd = m_TrialPoints->End();

    for( ; pointsIter != pointsEnd; ++pointsIter )
      {
      
      // get node from trail points container
      node = pointsIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for( int j = 0; j < SetDimension; j++ )
      {
       if( node.index[j] > (signed long) m_OutputSize[j] )
        {
          inRange = false;
          break;
        }
      }
      if( !inRange ) continue;

      // make this a trail point
      m_LabelImage->SetPixel( node.index, TrialPoint );

      outputPixel = node.value;
      m_OutputLevelSet->SetPixel( node.index, outputPixel );

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

  this->Initialize();

  if( m_CollectPoints )
    {
    m_ProcessedPoints = NodeContainer::New();
    }
  
  // process points on the heap
  NodeType node;
  double currentValue;
  double oldValue = -m_LargeValue;

  unsigned long NumPoints = 0;
  unsigned long InvalidPoints = 0;

  while( !m_TrialHeap.empty() )
    {
    // get the node with the smallest value
    node = m_TrialHeap.top();
    m_TrialHeap.pop();

    if( m_DebugOn ) { NumPoints++; }

    // does this node contain the current value ?
    currentValue = (double) m_OutputLevelSet->GetPixel( node.index );

    if( node.value != currentValue )
      {
      if( m_DebugOn ) { InvalidPoints++; }
      continue;
      } 

    // is this node already alive ?
    if( m_LabelImage->GetPixel( node.index ) != TrialPoint ) 
      {
      if( m_DebugOn) { InvalidPoints++; }
      continue;
      }

    if( currentValue > m_StoppingValue )
      {
      if( m_DebugOn ) 
        {
        std::cout << "stopping value reached" << std::endl;
        }
      break;
      }

    if( m_DebugOn && currentValue < oldValue) 
      {
       std::cout << "error value decrease at:" << node.index << std::endl;
      }
    oldValue = currentValue;

    if( m_CollectPoints )
      {
      m_ProcessedPoints->InsertElement( m_ProcessedPoints->Size(), node );
      }
    
    // set this node as alive
    m_LabelImage->SetPixel( node.index, AlivePoint );

    // update its neighbors
    this->UpdateNeighbors( node.index );

    }

  if( m_DebugOn ) 
    {
    std::cout << "No. points processed: " << NumPoints << std::endl;
    std::cout << "No. invalid points: " << InvalidPoints << std::endl;
    }

}


/**
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::UpdateNeighbors(
IndexType& index )
{
  IndexType neighIndex = index;

  for( int j = 0; j < SetDimension; j++ )
  {
    // update left neighbor
    if( index[j] > 0 )
    {
      neighIndex[j] = index[j] - 1;
    }
    if( m_LabelImage->GetPixel( neighIndex ) != AlivePoint )
    {
      this->UpdateValue( neighIndex );
    }

    // updaet right neighbor
    if( index[j] < (signed long) m_OutputSize[j] - 1 )
    {
      neighIndex[j] = index[j] + 1;
    }
    if( m_LabelImage->GetPixel( neighIndex ) != AlivePoint )
    {
      this->UpdateValue( neighIndex );
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
IndexType& index )
{

  IndexType neighIndex = index;
  typename TLevelSet::PixelType neighValue;
  PixelType outputPixel;
  NodeType node;

  for( int j = 0; j < SetDimension; j++ )
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

      if( m_LabelImage->GetPixel( neighIndex ) == AlivePoint )
      {
        outputPixel = m_OutputLevelSet->GetPixel( neighIndex );
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
  std::sort( m_NodesUsed.begin(), m_NodesUsed.end());

  // solve quadratic equation
  double aa, bb, cc;
  double solution = m_LargeValue;
  
  aa = 0.0;
  bb = 0.0;
  if( m_SpeedImage )
  {
    typedef typename SpeedImageType::PixelType SpeedPixelType;
    cc = (double) m_SpeedImage->GetPixel( index ) ;
    cc = -1.0 * vnl_math_sqr( 1.0 / cc );
  }
  else 
  {
    cc = m_InverseSpeed;
  }

  double discrim;

  for( int j = 0; j < SetDimension; j++ )
  {
    node = m_NodesUsed[j];

    if( solution >= node.value )
    {
      aa += 1.0;
      bb += node.value;
      cc += vnl_math_sqr( node.value );

      discrim = vnl_math_sqr( bb ) - aa * cc;
      if( discrim < 0.0 )
      {
        // discriminant zero
        throw ExceptionObject(__FILE__, __LINE__);
      }
    
      solution = ( vnl_math_sqrt( discrim ) + bb ) / aa;
    }
    else
    {
      break;
    }
  }

  if( solution < m_LargeValue )
  {
  // write solution to m_OutputLevelSet
  outputPixel = solution;
  m_OutputLevelSet->SetPixel( index, outputPixel );

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
