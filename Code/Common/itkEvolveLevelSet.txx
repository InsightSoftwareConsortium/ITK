/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEvolveLevelSet.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImageRegionIterator.h"
#include "itkIndex.h"

namespace itk
{

/**
 *
 */
template <class TLevelSet>
EvolveLevelSet<TLevelSet>
::EvolveLevelSet()
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
EvolveLevelSet<TLevelSet>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Evolve level set:" << std::endl;
  os << indent << "Time step size:" << m_TimeStepSize << std::endl;
  os << indent << "Narrowbanding: " << m_NarrowBanding << std::endl;
}

/**
 *
 */
template <class TLevelSet>
void
EvolveLevelSet<TLevelSet>
::SetInputNarrowBand(
NodeContainer * ptr )
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
EvolveLevelSet<TLevelSet>
::GenerateInputRequestedRegion()
{

  // this filter requires all of the input image to be in
  // the buffer
  LevelSetPointer inputPtr = this->GetInput();
  inputPtr->SetRequestedRegionToLargestPossibleRegion();

}


/**
 *
 */
template <class TLevelSet>
void
EvolveLevelSet<TLevelSet>
::EnlargeOutputRequestedRegion(
DataObject *output )
{

  // this filter requires the all of the output image to be in
  // the buffer
  TLevelSet *imgData;
  imgData = dynamic_cast<TLevelSet*>( output );
  imgData->SetRequestedRegionToLargestPossibleRegion();

}


/**
 *
 */
template <class TLevelSet>
void
EvolveLevelSet<TLevelSet>
::AllocateBuffers(
bool outputOnly)
{
  LevelSetPointer inputPtr = this->GetInput();
 
  if( !inputPtr )
    { 
    throw ExceptionObject();  
    }

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
EvolveLevelSet<TLevelSet>
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
EvolveLevelSet<TLevelSet>
::CopyInputToInputBuffer()
{

  // Using iterators for copying is inefficient.
  // Replace this by a "DeepCopy" function when
  // available.
  LevelSetPointer inputPtr = this->GetInput();

  if( !inputPtr )
    { 
    throw ExceptionObject();  
    }

  // Define iterators
  typedef
    ImageRegionIterator<PixelType,SetDimension> IteratorType;

  IteratorType inIt = IteratorType( 
    inputPtr, inputPtr->GetBufferedRegion() );
  IteratorType inBuffIt = IteratorType( 
    m_InputBuffer, m_InputBuffer->GetBufferedRegion() );
  inIt = inIt.Begin();
  inBuffIt = inBuffIt.Begin();
  
  while( !inIt.IsAtEnd() )
    {
    *inBuffIt = *inIt;
    ++inBuffIt;
    ++inIt;
    }

}


/**
 *
 */
template <class TLevelSet>
void
EvolveLevelSet<TLevelSet>
::CopyOutputBufferToOutput()
{

  // Using iterators for copying is inefficient.
  // Replace this by a "DeepCopy" function when
  // available.
  LevelSetPointer outputPtr = this->GetOutput();

  if( !outputPtr )
    { 
    throw ExceptionObject();  
    }

  // Define iterators
  typedef
     ImageRegionIterator<PixelType,SetDimension> IteratorType;

  IteratorType outIt = IteratorType( 
    outputPtr, outputPtr->GetBufferedRegion() );
  IteratorType outBuffIt = IteratorType( 
    m_OutputBuffer, m_OutputBuffer->GetBufferedRegion() );

  outIt = outIt.Begin();
  outBuffIt = outBuffIt.Begin();
  
  while( !outIt.IsAtEnd() )
    {
    *outIt = *outBuffIt;
    ++outIt;
    ++outBuffIt;
    }

}



} // namespace itk
