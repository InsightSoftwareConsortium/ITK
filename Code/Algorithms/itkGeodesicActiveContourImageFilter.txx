/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGeodesicActiveContourImageFilter_txx
#define _itkGeodesicActiveContourImageFilter_txx
#include "itkGeodesicActiveContourImageFilter.h"

#include "itkLevelSetCurvatureFunction.h"
#include "itkEntropyPreservingGradientMagnitudeImageFunction.h"
#include "itkUpwindDerivativeImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

#include "vnl/vnl_math.h"

namespace itk
{

/*
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::GeodesicActiveContourImageFilter()
{

  this->ProcessObject::SetNumberOfRequiredInputs( 3 );
  m_Extender = ExtenderType::New();
  m_InflationStrength = 0.0;

}


/*
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Inflation strength: " << m_InflationStrength;
  os << std::endl;
}

/*
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::SetDerivativeImage( const TDerivImage * ptr )
{
  this->ProcessObject::SetNthInput( 2, 
                          const_cast<TDerivImage *>(ptr) );
}


/*
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
typename GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::DerivImageType *
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::GetDerivativeImage()
{
  if ( this->GetNumberOfInputs() < 3 )
    {
    return NULL;
    }

  return static_cast<TDerivImage *>(
                        this->ProcessObject::GetInput( 2 ) );

}


/*
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
  this->GetDerivativeImage()->SetRequestedRegionToLargestPossibleRegion();

}


/*
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::GenerateDataFull()
{
  
  this->AllocateBuffers();
  this->CopyInputToInputBuffer();

  typename Superclass::EdgeImageConstPointer edgeImage = this->GetEdgeImage();

  const unsigned int numberOfIterations = this->GetNumberOfIterations();
  const double       timeStepSize       = this->GetTimeStepSize();
  const bool         propagateOutwards  = this->GetPropagateOutwards();

  // Define a level set curvature calculator
  typedef
    LevelSetCurvatureFunction<LevelSetImageType> CurvatureType;
  typename CurvatureType::Pointer inCurvature = CurvatureType::New();

  // Define a entropy-satisfying derivative calculator
  typedef
    EntropyPreservingGradientMagnitudeImageFunction<LevelSetImageType> DerivativeType;
  typename DerivativeType::Pointer inEntropy = DerivativeType::New();
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
  typename UpwindType::Pointer inUpwind = UpwindType::New();


  for( unsigned int k = 0; k < numberOfIterations; k++ )
    {
      
    // Update progress.
    if ( numberOfIterations < 100 || !(k % 10) )
      {
      this->UpdateProgress( (float) k / (float) numberOfIterations );
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
      ImageRegionConstIterator<EdgeImageType> 
        SpeedIteratorType;

    SpeedIteratorType speedIt = 
                SpeedIteratorType( edgeImage, 
                                   edgeImage->GetBufferedRegion() );

    typedef ImageRegionIterator<DerivImageType> DerivIteratorType;

    DerivIteratorType derivIt = 
      DerivIteratorType( this->GetDerivativeImage(), 
                         this->GetDerivativeImage()->GetBufferedRegion() );

    IndexType index;
    double curvature;
    double magnitude;
    double updateValue;
    double speed;
    double value;
    DerivPixelType deriv;

    outIt.GoToBegin();
    inIt.GoToBegin();
    speedIt.GoToBegin();
    derivIt.GoToBegin();

    while( !outIt.IsAtEnd() )
      {
      index = inIt.GetIndex();
    
      magnitude = inEntropy->EvaluateAtIndex( index );
      updateValue = m_InflationStrength * magnitude;
      if( propagateOutwards )
      {
        updateValue *= -1.0;
      }

      curvature = inCurvature->EvaluateAtIndex( index );
      magnitude = inCurvature->GetMagnitude();
      updateValue += curvature * magnitude;

      speed = (double) speedIt.Get();
      updateValue *= speed;

      deriv = derivIt.Get();

      for( unsigned int j = 0; j < SetDimension; j++ )
        {

          const double derivComponent = 
                         static_cast<double>( deriv[j] );

        inUpwind->SetSpeed( -1.0 * derivComponent );

        updateValue += derivComponent * 
          inUpwind->EvaluateNthDerivativeAtIndex( index, j );
        }

      updateValue *= timeStepSize; 
    
      value = (double) inIt.Get();
      value += updateValue;

      outIt.Set( value );

      ++outIt;
      ++inIt;
      ++speedIt;
      ++derivIt;

      } // end while loop

    this->SwapBuffers();

    } // end iteration loop

  this->SwapBuffers();
  this->CopyOutputBufferToOutput();
                                        
}

/*
 *
 */
template <class TLevelSet, class TEdgeImage, class TDerivImage>
void
GeodesicActiveContourImageFilter<TLevelSet,TEdgeImage,TDerivImage>
::GenerateDataNarrowBand()
{

  this->AllocateBuffers(true);
  
  LevelSetPointer outputPtr = this->GetOutputBuffer();
  typename Superclass::LevelSetConstPointer inputPtr = this->GetInput();
  typename Superclass::EdgeImageConstPointer edgeImage = this->GetEdgeImage();

  const DerivImageType * derivImage = this->GetDerivativeImage();


  // copy input to output
  typedef
     ImageRegionIterator<LevelSetImageType> IteratorType;
  typedef
     ImageRegionConstIterator<LevelSetImageType> ConstIteratorType;
  
  ConstIteratorType inIt = ConstIteratorType( 
    inputPtr, inputPtr->GetBufferedRegion() );
  IteratorType outIt = IteratorType( 
    outputPtr, outputPtr->GetBufferedRegion() );

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

  NodeContainerPointer inputNarrowBand  = this->GetInputNarrowBand();
  const unsigned int numberOfIterations = this->GetNumberOfIterations();
  const double       timeStepSize       = this->GetTimeStepSize();
  const bool         propagateOutwards  = this->GetPropagateOutwards();

  // Define a level set curvature calculator
  typedef
    LevelSetCurvatureFunction<LevelSetImageType> CurvatureType;
  typename CurvatureType::Pointer inCurvature = CurvatureType::New();

  // Define a entropy-satisfying derivative calculator
  typedef
    EntropyPreservingGradientMagnitudeImageFunction<LevelSetImageType> DerivativeType;
  typename DerivativeType::Pointer inEntropy = DerivativeType::New();
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
  typename UpwindType::Pointer inUpwind = UpwindType::New();
 
  for( unsigned int k = 0; k < numberOfIterations; k++ )
    {

    // Update progress.
    if ( numberOfIterations < 100 || !(k % 10) )
      {
      this->UpdateProgress( (float) k / (float) numberOfIterations );
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
    DerivPixelType deriv;

    for( ; pointsIt != pointsEnd; ++pointsIt )
      {
      node = pointsIt.Value();

      if( vnl_math_abs( node.GetValue() ) <= maxValue )
        {

        magnitude = inEntropy->EvaluateAtIndex( node.GetIndex() );
        updateValue = m_InflationStrength * magnitude;
        if( propagateOutwards )
          {
          updateValue *= -1.0;
          }

        curvature = inCurvature->EvaluateAtIndex( node.GetIndex() );
        magnitude = inCurvature->GetMagnitude();
        updateValue += curvature * magnitude;

        typedef typename TEdgeImage::PixelType EdgePixelType;
        speed = (double) edgeImage->GetPixel(node.GetIndex());

        updateValue *= speed;

        deriv = derivImage->GetPixel( node.GetIndex() );

        for( unsigned int j = 0; j < SetDimension; j++ )
          {

          const double derivComponent = 
                           static_cast<double>( deriv[j] );

          inUpwind->SetSpeed( -1.0 * derivComponent );
          
          updateValue += derivComponent * 
            inUpwind->EvaluateNthDerivativeAtIndex( node.GetIndex(), j );

          }

        updateValue *= timeStepSize; 
    
        value = (double) inputPtr->GetPixel( node.GetIndex() );
        value += updateValue;

        lsetPixel =  value;
        outputPtr->SetPixel( node.GetIndex(), lsetPixel );

        }

      } // end while loop

    } // end iteration loop

  this->SetOutputNarrowBand( inputNarrowBand );
  this->CopyOutputBufferToOutput();
                                          
} 


} // namespace itk

#endif
