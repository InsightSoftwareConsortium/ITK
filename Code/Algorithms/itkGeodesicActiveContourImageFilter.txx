/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkGeodesicActiveContourImageFilter_txx
#define _itkGeodesicActiveContourImageFilter_txx

#include "itkLevelSetCurvatureFunction.h"
#include "itkEntropyPreservingGradientMagnitudeImageFunction.h"
#include "itkUpwindDerivativeImageFunction.h"
#include "itkImageRegionIterator.h"

#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::GeodesicActiveContourImageFilter()
{
  m_Extender = ExtenderType::New();

  for( unsigned int j = 0; j < SetDimension; j++ )
    {
    m_DerivImages[j] = NULL;  
    }

  m_DebugOn = false;

}


/**
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Geodesic active contours" << std::endl;

}

/**
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::SetDerivativeImage(
TDerivImage * ptr,
unsigned int dim )
{
  if( !ptr || dim > SetDimension - 1) return;

  m_DerivImages[dim] = ptr;
  this->ProcessObject::SetNthInput( dim+1, ptr );

}


/**
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::GenerateInputRequestedRegion()
{
  //call the superclass implementation of this method
  this->Superclass::GenerateInputRequestedRegion();

  // this filter requires all of the input images to
  // be in the buffer
  for( unsigned int k = 0; k < SetDimension; k++ )
    {
    DerivImagePointer ptr = this->GetInput( k+1 );
    ptr->SetRequestedRegionToLargestPossibleRegion();
    }

}


/**
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::GenerateDataFull()
{
  
  for( unsigned int j = 0; j < SetDimension; j++ )
    {
    if ( !m_DerivImages[j] )
      {
        throw ExceptionObject();
      }
    }

  this->AllocateBuffers();
  this->CopyInputToInputBuffer();

  EdgeImagePointer edgeImage = this->GetEdgeImage();

  unsigned int numberOfIterations = this->GetNumberOfIterations();
  double timeStepSize = this->GetTimeStepSize();
  bool propagateOutwards = this->GetPropagateOutwards();

  // Define a level set curvature calculator
  typedef
    LevelSetCurvatureFunction<LevelSetImageType> CurvatureType;
  CurvatureType::Pointer inCurvature = CurvatureType::New();

  // Define a entropy-satisfying derivative calculator
  typedef
    EntropyPreservingGradientMagnitudeImageFunction<LevelSetImageType> DerivativeType;
  DerivativeType::Pointer inEntropy = DerivativeType::New();
  if( propagateOutwards )
    {
    inEntropy->SetSpeed( 1.0 );
    }
  else
    {
    inEntropy->SetSpeed( -1.0 );
    }

  // Define a upwind-derivative calculator
  typedef
    UpwindDerivativeImageFunction<LevelSetImageType> UpwindType;
  UpwindType::Pointer inUpwind = UpwindType::New();


  for( unsigned int k = 0; k < numberOfIterations; k++ )
    {
    if( m_DebugOn ) 
      {
      std::cout << "iteration: " << k << std::endl;
      }

    LevelSetPointer inputBuffer = this->GetInputBuffer();
    LevelSetPointer outputBuffer = this->GetOutputBuffer();

    typedef Index<TLevelSet::ImageDimension> IndexType;

    inCurvature->SetInputImage( inputBuffer );
    inEntropy->SetInputImage( inputBuffer );
    inUpwind->SetInputImage( inputBuffer );

    // Define iterators
    typedef
       ImageRegionIterator<LevelSetImageType> IteratorType;
  
    IteratorType inIt = IteratorType( inputBuffer, 
      inputBuffer->GetBufferedRegion() );
    IteratorType outIt = IteratorType( outputBuffer, 
      outputBuffer->GetBufferedRegion() );

    typedef
      ImageRegionIterator<EdgeImageType> 
        SpeedIteratorType;

    SpeedIteratorType speedIt = SpeedIteratorType( edgeImage, 
      edgeImage->GetBufferedRegion() );

    typedef
      ImageRegionIterator<DerivImageType> 
        DerivIteratorType;
    DerivIteratorType derivIt[SetDimension];

    for( unsigned int j = 0; j < SetDimension; j++ )
      {
      derivIt[j] = DerivIteratorType( m_DerivImages[j], 
        m_DerivImages[j]->GetBufferedRegion() );
      }

    IndexType index;
    double curvature;
    double magnitude;
    double updateValue;
    double speed;
    double deriv;
    double value;

    outIt = outIt.Begin();
    inIt = inIt.Begin();
    speedIt = speedIt.Begin();
    for( unsigned int j = 0; j < SetDimension; j++ )
      {
      derivIt[j] = derivIt[j].Begin();
      }

    while( !outIt.IsAtEnd() )
      {
      index = inIt.GetIndex();
    
      magnitude = inEntropy->Evaluate( index );
      updateValue = m_InflationStrength * magnitude;
      if( propagateOutwards )
      {
        updateValue *= -1.0;
      }

      curvature = inCurvature->Evaluate( index );
      magnitude = inCurvature->GetMagnitude();
      updateValue += curvature * magnitude;

      speed = (double) ScalarTraits<EdgePixelType>::
        GetScalar( speedIt.Get() );
      updateValue *= speed;

      for( unsigned int j = 0; j < SetDimension; j++ )
        {
        deriv = (double) ScalarTraits<DerivPixelType>::
          GetScalar( derivIt[j].Get() );

        inUpwind->SetSpeed( -1.0 * deriv );

        updateValue += deriv * inUpwind->Evaluate( index, j );
        }

      updateValue *= timeStepSize; 
    
      value = (double) ScalarTraits<PixelType>::GetScalar( inIt.Get() );
      value += updateValue;

      ScalarTraits<PixelType>::SetScalar( outIt.Get(), value );

      ++outIt;
      ++inIt;
      ++speedIt;
      for( unsigned int j = 0; j < SetDimension; j++ )
        {
        ++(derivIt[j]);
        }

      } // end while loop

    this->SwapBuffers();

    } // end iteration loop

  this->SwapBuffers();
  this->CopyOutputBufferToOutput();
                                        
}

/**
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::GenerateDataNarrowBand()
{

  for( unsigned int j = 0; j < SetDimension; j++ )
    {
    if ( !m_DerivImages[j] )
      {
        throw ExceptionObject();
      }
    }

  this->AllocateBuffers(true);
  
  LevelSetPointer outputPtr = this->GetOutputBuffer();
  LevelSetPointer inputPtr = this->GetInput();
  EdgeImagePointer edgeImage = this->GetEdgeImage();

  // copy input to output
  typedef
     ImageRegionIterator<LevelSetImageType> IteratorType;
  
  IteratorType inIt = IteratorType( 
    inputPtr, inputPtr->GetBufferedRegion() );
  IteratorType outIt = IteratorType( 
    outputPtr, outputPtr->GetBufferedRegion() );

  inIt = inIt.Begin();
  outIt = outIt.Begin();
  
  while( !inIt.IsAtEnd() )
    {
    outIt.Set( inIt.Get() );
    ++inIt;
    ++outIt;
    }

  double narrowBandwidth = this->GetNarrowBandwidth();

  // Setup the extender
  m_Extender->NarrowBandingOn();
  m_Extender->SetNarrowBandwidth( narrowBandwidth );

  NodeContainerPointer inputNarrowBand = this->GetInputNarrowBand();
  unsigned int numberOfIterations = this->GetNumberOfIterations();
  double timeStepSize = this->GetTimeStepSize();
  bool propagateOutwards = this->GetPropagateOutwards();

  // Define a level set curvature calculator
  typedef
    LevelSetCurvatureFunction<LevelSetImageType> CurvatureType;
  CurvatureType::Pointer inCurvature = CurvatureType::New();

  // Define a entropy-satisfying derivative calculator
  typedef
    EntropyPreservingGradientMagnitudeImageFunction<LevelSetImageType> DerivativeType;
  DerivativeType::Pointer inEntropy = DerivativeType::New();
  if( propagateOutwards )
    {
    inEntropy->SetSpeed( 1.0 );
    }
  else
    {
    inEntropy->SetSpeed( -1.0 );
    }

  // Define a upwind-derivative calculator
  typedef
    UpwindDerivativeImageFunction<LevelSetImageType> UpwindType;
  UpwindType::Pointer inUpwind = UpwindType::New();
 
  for( unsigned int k = 0; k < numberOfIterations; k++ )
    {
    if( m_DebugOn ) 
      {
      std::cout << "iteration: " << k << std::endl;
      }

    m_Extender->SetInput( outputPtr );
    m_Extender->SetInputNarrowBand( inputNarrowBand );
    m_Extender->Update();

    inputNarrowBand = m_Extender->GetOutputNarrowBand();
    inputPtr = m_Extender->GetOutput();

    inCurvature->SetInputImage( inputPtr );
    inEntropy->SetInputImage( inputPtr );
    inUpwind->SetInputImage( inputPtr );

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
    double deriv;

    for( ; pointsIt != pointsEnd; ++pointsIt )
      {
      node = pointsIt.Value();

      if( vnl_math_abs( node.value ) <= maxValue )
        {

        magnitude = inEntropy->Evaluate( node.index );
        updateValue = m_InflationStrength * magnitude;
        if( propagateOutwards )
          {
          updateValue *= -1.0;
          }

        curvature = inCurvature->Evaluate( node.index );
        magnitude = inCurvature->GetMagnitude();
        updateValue += curvature * magnitude;

        typedef typename TEdgeImage::PixelType EdgePixelType;
        speed = (double) ScalarTraits<EdgePixelType>::
          GetScalar( edgeImage->GetPixel(node.index) );

        updateValue *= speed;

        for( unsigned int j = 0; j < SetDimension; j++ )
          {
          typedef typename TDerivImage::PixelType DerivPixelType;
          deriv = (double) ScalarTraits<DerivPixelType>::
            GetScalar( m_DerivImages[j]->GetPixel( node.index ) );

          inUpwind->SetSpeed( -1.0 * deriv );
          
          updateValue += deriv * inUpwind->Evaluate( node.index, j );

          }

        updateValue *= timeStepSize; 
    
        value = (double) ScalarTraits<PixelType>::
          GetScalar( inputPtr->GetPixel( node.index ) );
        value += updateValue;

        ScalarTraits<PixelType>::SetScalar( lsetPixel, value );
        outputPtr->SetPixel( node.index, lsetPixel );

        }

      } // end while loop

    } // end iteration loop

  this->SetOutputNarrowBand( inputNarrowBand );
  this->CopyOutputBufferToOutput();
                                          
} 


} // namespace itk

#endif
