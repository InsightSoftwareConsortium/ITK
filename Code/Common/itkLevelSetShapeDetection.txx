/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetShapeDetection.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkLevelSetCurvatureFunction.h"
#include "itkEntropyDerivativeFunction.h"
#include "itkImageRegionIterator.h"

#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template <class TLevelSet, class TEdgeImage>
LevelSetShapeDetection<TLevelSet,TEdgeImage>
::LevelSetShapeDetection()
{
  m_EdgeImage = NULL;
  m_LengthPenaltyStrength = 0.05;

  m_OutputNarrowBand = NULL;
  m_Extender = ExtenderType::New();

  m_DebugOn = false;

}

/**
 *
 */
template <class TLevelSet, class TEdgeImage>
void
LevelSetShapeDetection<TLevelSet,TEdgeImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Level set shape detection" << std::endl;
}


/**
 *
 */
template <class TLevelSet, class TEdgeImage>
void
LevelSetShapeDetection<TLevelSet,TEdgeImage>
::SetEdgeImage(TEdgeImage * ptr)
{
  if( !ptr ) return;

  m_EdgeImage = ptr;
  this->ProcessObject::SetNthInput( 1, ptr );

}


/**
 *
 */
template <class TLevelSet, class TEdgeImage>
void
LevelSetShapeDetection<TLevelSet,TEdgeImage>
::AllocateOutput()
{
  // allocate memory for the output image
  LevelSetPointer outputPtr = this->GetOutput();
  outputPtr->SetBufferedRegion( 
    outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

}


/**
 *
 */
template <class TLevelSet, class TEdgeImage>
void
LevelSetShapeDetection<TLevelSet,TEdgeImage>
::GenerateInputRequestedRegion()
{
  // call the superclass implementation
  this->Superclass::GenerateInputRequestedRegion();

  // this filter requires all of the input image to
  // be in the buffer
  EdgeImagePointer imgPtr = this->GetInput(1);
  imgPtr->SetRequestedRegionToLargestPossibleRegion();

}


/**
 *
 */
template <class TLevelSet, class TEdgeImage>
void
LevelSetShapeDetection<TLevelSet,TEdgeImage>
::GenerateData()
{
  if ( !m_EdgeImage )
    {
    throw ExceptionObject();
    }

  LevelSetPointer inputPtr = this->GetInput();
  if ( !inputPtr )
    {
    throw ExceptionObject();
    }

  this->AllocateOutput();

  if( this->GetNarrowBanding() ) 
    {
    this->GenerateDataNarrowBand();
    }
  else
    {
    this->GenerateDataFull();
    }

}


/**
 *
 */
template <class TLevelSet, class TEdgeImage>
void
LevelSetShapeDetection<TLevelSet,TEdgeImage>
::GenerateDataFull()
{

  this->AllocateBuffers();
  this->CopyInputToInputBuffer();

  unsigned int numberOfIterations = this->GetNumberOfIterations();
  double timeStepSize = this->GetTimeStepSize();

  for( unsigned int k = 0; k < numberOfIterations; k++ )
    {
    if( m_DebugOn ) 
      {
      std::cout << "iteration: " << k << std::endl;
      }
    LevelSetPointer inputBuffer = this->GetInputBuffer();
    LevelSetPointer outputBuffer = this->GetOutputBuffer();

    // Setup the extender
    m_Extender->SetVelocityImage( m_EdgeImage );
    m_Extender->SetInput( inputBuffer );
    m_Extender->Update();
    typename TEdgeImage::Pointer extVelPtr = m_Extender->GetVelocityImage();

    // Define a level set curvature calculator
    typedef
      LevelSetCurvatureFunction<LevelSetImageType> CurvatureType;
    CurvatureType::Pointer inCurvature = CurvatureType::New();
    inCurvature->SetInputImage( inputBuffer );

    // Define a entropy-satisfying derivative calculator
    typedef
      EntropyDerivativeFunction<LevelSetImageType> DerivativeType;
    DerivativeType::Pointer inEntropy = DerivativeType::New();
    inEntropy->SetInputImage( inputBuffer );

      // Define iterators
    typedef
      ImageRegionIterator<PixelType,SetDimension> IteratorType;
  
    IteratorType inBuffIt = IteratorType( inputBuffer, 
      inputBuffer->GetBufferedRegion() );
    IteratorType outBuffIt = IteratorType( outputBuffer, 
      outputBuffer->GetBufferedRegion() );

    typedef
      ImageRegionIterator<EdgePixelType,SetDimension> 
        SpeedIteratorType;

    SpeedIteratorType speedIt = SpeedIteratorType( extVelPtr, 
      extVelPtr->GetBufferedRegion() );

    typedef Index<TLevelSet::ImageDimension> IndexType;

    IndexType index;
    double curvature;
    double magnitude;
    double updateValue;
    double speed;
    double value;

    outBuffIt = outBuffIt.Begin();
    inBuffIt = inBuffIt.Begin();
    speedIt = speedIt.Begin();

    while( !outBuffIt.IsAtEnd() )
      {
      index = outBuffIt.GetIndex();

      magnitude = inEntropy->Evaluate( index );
      updateValue = -1.0 * magnitude;

      curvature = inCurvature->Evaluate( index );
      magnitude = inCurvature->GetMagnitude();
      updateValue += m_LengthPenaltyStrength * curvature * magnitude;

      speed = (double) ScalarTraits<EdgePixelType>::
        GetScalar( *speedIt );

      updateValue *= timeStepSize * speed; 
    
      value = (double) ScalarTraits<PixelType>::GetScalar( *inBuffIt );
      value += updateValue;

      ScalarTraits<PixelType>::SetScalar( *outBuffIt, value );

      ++outBuffIt;
      ++inBuffIt;
      ++speedIt;

      }

    this->SwapBuffers();
    }

  this->SwapBuffers();
  this->CopyOutputBufferToOutput();
                                        
}

/**
 *
 */
template <class TLevelSet, class TEdgeImage>
void
LevelSetShapeDetection<TLevelSet,TEdgeImage>
::GenerateDataNarrowBand()
{

  this->AllocateBuffers(true);

  LevelSetPointer outputPtr = this->GetOutputBuffer();
  LevelSetPointer inputPtr = this->GetInput();

  double narrowBandwidth = this->GetNarrowBandwidth();

  // copy input to output
  typedef
     ImageRegionIterator<PixelType,SetDimension> IteratorType;
  
  IteratorType inIt = IteratorType( inputPtr, 
    inputPtr->GetBufferedRegion() );
  IteratorType outIt = IteratorType( outputPtr, 
    outputPtr->GetBufferedRegion() );

  inIt = inIt.Begin();
  outIt = outIt.Begin();
  
  while( !inIt.IsAtEnd() )
    {
    *outIt = *inIt;
    ++inIt;
    ++outIt;
    }

  // Setup the extender
  m_Extender->SetVelocityImage( m_EdgeImage );
  m_Extender->NarrowBandingOn();
  m_Extender->SetNarrowBandwidth( narrowBandwidth );

  NodeContainerPointer inputNarrowBand = this->GetInputNarrowBand();
  unsigned int numberOfIterations = this->GetNumberOfIterations();
  double timeStepSize = this->GetTimeStepSize();

  for( unsigned int k = 0; k < numberOfIterations; k++ )
    {
    if( m_DebugOn ) 
      {
      std::cout << "iteration: " << k << std::endl;
      }
    m_Extender->SetInput( outputPtr );
    m_Extender->SetInputNarrowBand( inputNarrowBand );
    m_Extender->Update();

    typename TEdgeImage::Pointer extVelPtr = m_Extender->GetVelocityImage();

    inputNarrowBand = m_Extender->GetOutputNarrowBand();
    inputPtr = m_Extender->GetOutput();

    // Define a level set curvature calculator
    typedef
      LevelSetCurvatureFunction<LevelSetImageType> CurvatureType;
    CurvatureType::Pointer inCurvature = CurvatureType::New();
    inCurvature->SetInputImage( inputPtr );

    // Define a entropy-satisfying derivative calculator
    typedef
      EntropyDerivativeFunction<LevelSetImageType> DerivativeType;
    DerivativeType::Pointer inEntropy = DerivativeType::New();
    inEntropy->SetInputImage( inputPtr );

    typename NodeContainer::ConstIterator pointsIt;
    typename NodeContainer::ConstIterator pointsEnd;
  
    pointsIt = inputNarrowBand->Begin();
    pointsEnd = inputNarrowBand->End();

    double maxValue = narrowBandwidth / 2.0;
    NodeType node;
    PixelType lsetPixel;
    double curvature;
    double magnitude;
    double updateValue;
    double speed;
    double value;

    for( ; pointsIt != pointsEnd; ++pointsIt )
      {
      node = pointsIt.Value();

      if( vnl_math_abs( node.value ) <= maxValue )
        {
        
        magnitude = inEntropy->Evaluate( node.index );
        updateValue = -1.0 * magnitude;

        curvature = inCurvature->Evaluate( node.index );
        magnitude = inCurvature->GetMagnitude();
        updateValue += m_LengthPenaltyStrength * curvature * magnitude;

        speed = (double) ScalarTraits<EdgePixelType>::
          GetScalar( extVelPtr->GetPixel(node.index) );

        updateValue *= timeStepSize * speed; 
    
        value = (double) ScalarTraits<PixelType>::
          GetScalar( inputPtr->GetPixel( node.index ) );
        value += updateValue;

        ScalarTraits<PixelType>::SetScalar( lsetPixel, value );
        outputPtr->SetPixel( node.index, lsetPixel );

        }

      } // end while loop

    } // end iteration loop

  m_OutputNarrowBand = inputNarrowBand;
  this->CopyOutputBufferToOutput();
                                        
}


} // namespace itk
