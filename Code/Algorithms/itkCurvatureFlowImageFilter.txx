/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureFlowImageFilter.txx
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
#ifndef _itkCurvatureFlowImageFilter_txx
#define _itkCurvatureFlowImageFilter_txx

#include "itkPixelTraits.h"
#include "itkImageRegionIterator.h"
#include "itkLevelSetCurvatureFunction.h"

namespace itk
{

/**
 *
 */
template <class TLevelSet>
CurvatureFlowImageFilter<TLevelSet>
::CurvatureFlowImageFilter()
{
  m_DebugOn = false;

}


/**
 *
 */
template <class TLevelSet>
void
CurvatureFlowImageFilter<TLevelSet>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Curvature flow" << std::endl;
}


/**
 *
 */
template <class TLevelSet>
void
CurvatureFlowImageFilter<TLevelSet>
::Initialize()
{
  // allocate the output image buffer
  LevelSetPointer outputPtr = this->GetOutput();
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();
}


/**
 *
 */
template <class TLevelSet>
void
CurvatureFlowImageFilter<TLevelSet>
::GenerateData()
{

  if( !this->GetInput() ) return;

  this->Initialize();
  this->AllocateBuffers();
  this->CopyInputToInputBuffer();

  int numIterations = this->GetNumberOfIterations();
  double timeStep = this->GetTimeStepSize();

  // Define a level set curvature calculator
  typedef
    LevelSetCurvatureFunction<LevelSetImageType> CurvatureType;
  CurvatureType::Pointer inCurvature = CurvatureType::New();

  for( int k = 0; k < numIterations; k++ )
    {

    if( m_DebugOn ) 
      {
      std::cout << "iteration: " << k << std::endl;
      }

    LevelSetPointer outputBuffer = this->GetOutputBuffer();
    LevelSetPointer inputBuffer = this->GetInputBuffer();

    // setup curvature calculator
    inCurvature->SetInputImage( inputBuffer );

    // Define/declare an iterator that will walk the output region
    typedef
      ImageRegionIterator<LevelSetImageType> IteratorType;

    IteratorType outIt = IteratorType( outputBuffer, 
      outputBuffer->GetBufferedRegion() );
    IteratorType inIt = IteratorType( inputBuffer,
      inputBuffer->GetBufferedRegion() );
  
    IndexType inputIndex;
    PixelType outputPixel;
    double curvature;
    double magnitude;
    double value;

    // walk the output level set
    for (outIt = outIt.Begin(), inIt = inIt.Begin(); 
      outIt != outIt.End(); ++outIt, ++inIt)
      {

      value = (double) ScalarTraits<PixelType>:: GetScalar(inIt.Get());

      inputIndex = outIt.GetIndex();

      curvature = inCurvature->Evaluate( inputIndex );
      magnitude = inCurvature->GetMagnitude();

      value += timeStep * curvature * magnitude;

      ScalarTraits<PixelType>::SetScalar(outputPixel, value );
      outIt.Set(outputPixel);

      }

    this->SwapBuffers();

    }

  this->SwapBuffers();
  this->CopyOutputBufferToOutput();

}


} // namespace itk

#endif
