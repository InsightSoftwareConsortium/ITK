/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLevelSetImageFilter_txx
#define _itkLevelSetImageFilter_txx

#include "itkLevelSetImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkIndex.h"

namespace itk
{

/**
 *
 */
template <class TLevelSet>
LevelSetImageFilter<TLevelSet>
::LevelSetImageFilter()
{
  m_TimeStepSize = 0.5;
  m_NarrowBandwidth = 12.0;
  m_NarrowBanding = false;
  m_InputNarrowBand = NULL;

  m_NumberOfIterations = 10;

  m_InputBuffer = NULL;
  m_OutputBuffer = NULL;

}


/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Time step size: " << m_TimeStepSize << std::endl;
  os << indent << "Narrowbanding: " << m_NarrowBanding << std::endl;
  os << indent << "Narrow bandwidth: " << m_NarrowBandwidth << std::endl;
  os << indent << "Input narrow band: " << m_InputNarrowBand.GetPointer() << std::endl;
  os << indent << "No. iterations: " << m_NumberOfIterations << std::endl;  
}


/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
::SetInputNarrowBand(NodeContainer * ptr )
{
  if( m_InputNarrowBand != ptr )
    {
    m_InputNarrowBand = ptr;
    this->Modified();
    }

}


/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
::GenerateInputRequestedRegion()
{

  // use the default implementation.
  this->Superclass::GenerateInputRequestedRegion();

}


/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
::EnlargeOutputRequestedRegion(DataObject *output )
{

  // this filter requires the all of the output image to be in
  // the buffer
  TLevelSet *imgData;
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


/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
::AllocateBuffers(bool outputOnly)
{
  LevelSetConstPointer inputPtr = this->GetInput();
 
  if( !m_InputBuffer && !outputOnly) 
    { 
    m_InputBuffer = TLevelSet::New();
    }
  if( !m_OutputBuffer )
    {
    m_OutputBuffer = TLevelSet::New();
    }

  typedef Index<TLevelSet::ImageDimension> IndexType;

  const typename TLevelSet::RegionType largestRegion = 
    inputPtr->GetLargestPossibleRegion();
  const typename TLevelSet::RegionType bufferedRegion =
    inputPtr->GetBufferedRegion();

  if( !outputOnly )
    {
    m_InputBuffer->SetLargestPossibleRegion( largestRegion );
    m_InputBuffer->SetBufferedRegion( bufferedRegion );
    m_InputBuffer->Allocate();
    }

  m_OutputBuffer->SetLargestPossibleRegion( largestRegion );
  m_OutputBuffer->SetBufferedRegion( bufferedRegion );
  m_OutputBuffer->Allocate();

}

/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
::SwapBuffers()
{
  typename TLevelSet::Pointer temp;
  temp = m_OutputBuffer;
  m_OutputBuffer = m_InputBuffer;
  m_InputBuffer = temp;
}

/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
::CopyInputToInputBuffer()
{

  // Using iterators for copying is inefficient.
  // Replace this by a "DeepCopy" function when
  // available.
  LevelSetConstPointer inputPtr = this->GetInput();

  // Define iterators
  typedef
    ImageRegionIterator<LevelSetImageType> IteratorType;
  typedef
    ImageRegionConstIterator<LevelSetImageType> ConstIteratorType;

  ConstIteratorType inIt = ConstIteratorType( 
    inputPtr, inputPtr->GetBufferedRegion() );
  IteratorType inBuffIt = IteratorType( 
    m_InputBuffer, m_InputBuffer->GetBufferedRegion() );
  
  while( !inIt.IsAtEnd() )
    {
    inBuffIt.Set( inIt.Get() );
    ++inBuffIt;
    ++inIt;
    }

}


/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
::CopyOutputBufferToOutput()
{

  // Using iterators for copying is inefficient.
  // Replace this by a "DeepCopy" function when
  // available.
  LevelSetPointer outputPtr = this->GetOutput();

  // Define iterators
  typedef
     ImageRegionIterator<LevelSetImageType> IteratorType;
  typedef
     ImageRegionConstIterator<LevelSetImageType> ConstIteratorType;

  IteratorType outIt = IteratorType( 
    outputPtr, outputPtr->GetBufferedRegion() );
  ConstIteratorType outBuffIt = ConstIteratorType( 
    m_OutputBuffer, m_OutputBuffer->GetBufferedRegion() );

  while( !outIt.IsAtEnd() )
    {
    outIt.Set( outBuffIt.Get() );
    ++outIt;
    ++outBuffIt;
    }

}



} // namespace itk

#endif
