/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetImageFilter.txx
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
#ifndef _itkLevelSetImageFilter_txx
#define _itkLevelSetImageFilter_txx

#include "itkImageRegionIterator.h"
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

  os << indent << "Evolve level set:" << std::endl;
  os << indent << "Time step size:" << m_TimeStepSize << std::endl;
  os << indent << "Narrowbanding: " << m_NarrowBanding << std::endl;
}

/**
 *
 */
template <class TLevelSet>
void
LevelSetImageFilter<TLevelSet>
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
LevelSetImageFilter<TLevelSet>
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
LevelSetImageFilter<TLevelSet>
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
LevelSetImageFilter<TLevelSet>
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
  LevelSetPointer inputPtr = this->GetInput();

  if( !inputPtr )
    { 
    throw ExceptionObject();  
    }

  // Define iterators
  typedef
    ImageRegionIterator<LevelSetImageType> IteratorType;

  IteratorType inIt = IteratorType( 
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

  if( !outputPtr )
    { 
    throw ExceptionObject();  
    }

  // Define iterators
  typedef
     ImageRegionIterator<LevelSetImageType> IteratorType;

  IteratorType outIt = IteratorType( 
    outputPtr, outputPtr->GetBufferedRegion() );
  IteratorType outBuffIt = IteratorType( 
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
