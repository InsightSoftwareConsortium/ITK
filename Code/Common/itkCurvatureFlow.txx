/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureFlow.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkPixelTraits.h"
#include "itkImageRegionIterator.h"
#include "itkLevelSetCurvatureFunction.h"

namespace itk
{

/**
 *
 */
template <class TLevelSet>
CurvatureFlow<TLevelSet>
::CurvatureFlow()
{
  m_DebugOn = false;

}


/**
 *
 */
template <class TLevelSet>
void
CurvatureFlow<TLevelSet>
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
CurvatureFlow<TLevelSet>
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
CurvatureFlow<TLevelSet>
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
      ImageRegionIterator<PixelType,SetDimension> IteratorType;

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

      value = (double) ScalarTraits<PixelType>:: GetScalar(*inIt);

      inputIndex = outIt.GetIndex();

      curvature = inCurvature->Evaluate( inputIndex );
      magnitude = inCurvature->GetMagnitude();

      value += timeStep * curvature * magnitude;

      ScalarTraits<PixelType>::SetScalar(outputPixel, value );
      *outIt = outputPixel;

      }

    this->SwapBuffers();

    }

  this->SwapBuffers();
  this->CopyOutputBufferToOutput();

}


} // namespace itk
