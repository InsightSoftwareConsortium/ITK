/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFastMarchingImageFilter_txx
#define _itkFastMarchingImageFilter_txx

#include "itkFastMarchingImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "vnl/vnl_math.h"
#include <algorithm>


namespace itk
{

/*
 *
 */
template <class TLevelSet, class TSpeedImage>
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::FastMarchingImageFilter()
  : m_TrialHeap( )
{

  this->ProcessObject::SetNumberOfRequiredInputs(0);

  m_OutputSize.Fill( 16 ); 

  m_AlivePoints = NULL;
  m_TrialPoints = NULL;
  m_ProcessedPoints = NULL;

  m_SpeedConstant = 1.0;
  m_InverseSpeed = -1.0;
  m_LabelImage = LabelImageType::New();

  typedef typename LevelSetImageType::PixelType PixelType;
  m_LargeValue = NumericTraits<PixelType>::max() / 2.0;
  m_StoppingValue =  static_cast<double>( m_LargeValue );
  m_CollectPoints = false;

}


/*
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

/*
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::GenerateOutputInformation()
{

  // make the output of the size specified by m_OutputSize
  LevelSetPointer output = this->GetOutput();
 
  typedef typename TLevelSet::RegionType RegionType;
  RegionType region;

  typename RegionType::IndexType index;
  index.Fill(0);

  region.SetSize( m_OutputSize );
  region.SetIndex( index );

  output->SetLargestPossibleRegion( region );

}


/*
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
  if ( imgData )
    {
    imgData->SetRequestedRegionToLargestPossibleRegion();
    }
  else
    {
    // Pointer could not be cast to TLevelSet *
    itkWarningMacro(<< "itk::FastMarchingImageFilter" <<
              "::EnlargeOutputRequestedRegion cannot cast "
              << typeid(output).name() << " to "
              << typeid(TLevelSet*).name() );    
    }

}



/*
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
      for ( unsigned int j = 0; j < SetDimension; j++ )
        {
        if ( node.GetIndex()[j] > (signed long) m_OutputSize[j] )
          {
          inRange = false;
          break;
          }
        }
      if ( !inRange ) continue;

      // make this an alive point
      m_LabelImage->SetPixel( node.GetIndex(), AlivePoint );

      outputPixel = node.GetValue();
      output->SetPixel( node.GetIndex(), outputPixel );

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
    pointsIter = m_TrialPoints->Begin();
    pointsEnd = m_TrialPoints->End();

    for ( ; pointsIter != pointsEnd; ++pointsIter )
      {
      
      // get node from trial points container
      node = pointsIter.Value();

      // check if node index is within the output level set
      bool inRange = true;
      for ( unsigned int j = 0; j < SetDimension; j++ )
      {
       if( node.GetIndex()[j] > (signed long) m_OutputSize[j] )
        {
        inRange = false;
        break;
        }
      }
      if ( !inRange ) continue;

      // make this a trial point
      m_LabelImage->SetPixel( node.GetIndex(), TrialPoint );

      outputPixel = node.GetValue();
      output->SetPixel( node.GetIndex(), outputPixel );

      m_TrialHeap.push( node );

      }
    }
      
}


/*
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::GenerateData()
{

  LevelSetPointer         output      = this->GetOutput();
  SpeedImageConstPointer  speedImage  = this->GetInput();

  this->Initialize( output );

  if ( m_CollectPoints )
    {
    m_ProcessedPoints = NodeContainer::New();
    }
  
  // process points on the heap
  NodeType node;
  double currentValue;
  double oldValue = -m_LargeValue;

  double oldProgress = 0;

  unsigned long NumPoints = 0;
  unsigned long InvalidPoints = 0;

  while ( !m_TrialHeap.empty() )
    {
    // get the node with the smallest value
    node = m_TrialHeap.top();
    m_TrialHeap.pop();

    if ( this->GetDebug() ) { NumPoints++; }

    // does this node contain the current value ?
    currentValue = (double) output->GetPixel( node.GetIndex() );

    if ( node.GetValue() != currentValue )
      {
      if( this->GetDebug() ) { InvalidPoints++; }
      continue;
      } 

    // is this node already alive ?
    if ( m_LabelImage->GetPixel( node.GetIndex() ) != TrialPoint ) 
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
      itkDebugMacro(<< "error value decrease at:" << node.GetIndex() );
      }
    oldValue = currentValue;

    if ( m_CollectPoints )
      {
      m_ProcessedPoints->InsertElement( m_ProcessedPoints->Size(), node );
      }
    
    // set this node as alive
    m_LabelImage->SetPixel( node.GetIndex(), AlivePoint );

    // update its neighbors
    this->UpdateNeighbors( node.GetIndex(), speedImage, output );


    // Send events every certain number of points.
    const double newProgress = currentValue / m_StoppingValue;
    if( newProgress - oldProgress > 0.01 )  // update every 1%
      {
      this->UpdateProgress( newProgress );
      oldProgress = newProgress;
      }

    }
  
  itkDebugMacro(<< "No. points processed: " << NumPoints);
  itkDebugMacro(<< "No. invalid points: " << InvalidPoints);
}

/*
 *
 */
template <class TLevelSet, class TSpeedImage>
void
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::UpdateNeighbors(
const IndexType& index,
const SpeedImageType * speedImage,
LevelSetImageType * output )
{
  IndexType neighIndex = index;

  for ( unsigned int j = 0; j < SetDimension; j++ )
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


/*
 *
 */
template <class TLevelSet, class TSpeedImage>
double
FastMarchingImageFilter<TLevelSet,TSpeedImage>
::UpdateValue(
const IndexType& index,
const SpeedImageType * speedImage,
LevelSetImageType * output )
{

  IndexType neighIndex = index;
  typename TLevelSet::PixelType neighValue;
  PixelType outputPixel;
  NodeType node;

  for ( unsigned int j = 0; j < SetDimension; j++ )
  {
    node.SetValue( m_LargeValue );

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

        if( node.GetValue() > neighValue )
        {
          node.SetValue( neighValue );
          node.SetIndex( neighIndex );
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

  for ( unsigned int j = 0; j < SetDimension; j++ )
  {
    node = m_NodesUsed[j];

    if ( solution >= node.GetValue() )
    {
      aa += 1.0;
      bb += node.GetValue();
      cc += vnl_math_sqr( node.GetValue() );

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
  node.SetValue( solution );
  node.SetIndex( index );
  m_TrialHeap.push( node );
  }

  return solution;

}

} // namespace itk


#endif
