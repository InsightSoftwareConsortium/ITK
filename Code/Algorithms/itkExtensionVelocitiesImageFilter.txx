/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtensionVelocitiesImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkExtensionVelocitiesImageFilter_txx
#define _itkExtensionVelocitiesImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkIndex.h"


namespace itk
{

/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::ExtensionVelocitiesImageFilter()
{

  m_Locator = LocatorType::New();

  m_Marcher = FastMarchingImageFilterType::New();

  this->ProcessObject::SetNumberOfRequiredInputs(VAuxDimension + 1);
  this->ProcessObject::SetNumberOfRequiredOutputs(VAuxDimension + 1);

  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    AuxImagePointer ptr;
    ptr = AuxImageType::New();
    m_OutputAuxImage[k] = ptr;
    this->ProcessObject::AddOutput( ptr.GetPointer() );
    }

}

/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Extension velocities" << std::endl;
}

/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::SetVelocityImage(
AuxImageType * ptr,
unsigned int idx )
{
  if( !ptr || idx >= VAuxDimension )
    {
    return;
    }

  m_InputAuxImage[idx] = ptr;
  this->ProcessObject::SetNthInput( idx+1, ptr );

}


/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::GenerateInputRequestedRegion()
{
  // call the superclass implemenation of this method
  this->Superclass::GenerateInputRequestedRegion();

  // this filter requires all of the input images to be
  // in the buffer
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    m_InputAuxImage[k]->
      SetRequestedRegionToLargestPossibleRegion();
    }

}


/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::EnlargeOutputRequestedRegion( DataObject * output )
{
  // call the superclass implemenation of this method
  this->Superclass::EnlargeOutputRequestedRegion( output );

  // set the requested region for all output to be the
  // same as the primary input
  LevelSetPointer primaryOutput = this->GetOutput(0);
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    m_OutputAuxImage[k]->SetRequestedRegion(
      primaryOutput->GetRequestedRegion() );
    }

}


/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::AllocateOutput()
{
  this->Superclass::AllocateOutput();

  // allocate memory for the output images
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    m_OutputAuxImage[k]->SetBufferedRegion( 
      m_OutputAuxImage[k]->GetRequestedRegion() );
    m_OutputAuxImage[k]->Allocate();
    }

  // set the marcher output size
  LevelSetPointer outputPtr = this->GetOutput();
  m_Marcher->SetOutputSize( 
    outputPtr->GetRequestedRegion().GetSize() );

}

/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::GenerateDataFull()
{

  LevelSetPointer inputPtr = this->GetInput();
  LevelSetPointer outputPtr = this->GetOutput();
  LevelSetPointer tempLevelSet = m_Marcher->GetOutput();

  double levelSetValue = this->GetLevelSetValue();

  // define iterators
  typedef 
    ImageRegionIterator<LevelSetType::LevelSetImageType> IteratorType;

  IteratorType inputIt( inputPtr,
    inputPtr->GetBufferedRegion() );
  IteratorType outputIt( outputPtr,
    outputPtr->GetBufferedRegion() );

  IteratorType tempIt;

  typedef
    ImageRegionIterator<AuxImageType> AuxIteratorType;

  AuxIteratorType auxTempIt[VAuxDimension];
  AuxIteratorType auxOutputIt[VAuxDimension];

  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    AuxImagePointer ptr;
    ptr = m_OutputAuxImage[k];
    auxOutputIt[k] = AuxIteratorType( ptr,
      ptr->GetBufferedRegion() );
    }
 
  // locate the level set
  m_Locator->SetInput( inputPtr );
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    m_Locator->SetAuxImage( m_InputAuxImage[k], k );
    }
  m_Locator->SetLevelSetValue( levelSetValue );
  m_Locator->Locate();


  // march outward
  m_Marcher->SetTrialPoints( m_Locator->GetOutsidePoints() );
  m_Marcher->SetAuxiliaryTrialValues( m_Locator->GetAuxOutsideValues() );
  m_Marcher->Update();

  tempIt = IteratorType( tempLevelSet,
    tempLevelSet->GetBufferedRegion() );

  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    AuxImagePointer ptr;
    ptr = m_Marcher->GetAuxiliaryImage(k);
    auxTempIt[k] = AuxIteratorType( ptr,
                               ptr->GetBufferedRegion() );
    }

  double value;

  inputIt = inputIt.Begin();
  outputIt = outputIt.Begin();
  tempIt = tempIt.Begin();
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    auxOutputIt[k] = auxOutputIt[k].Begin();
    auxTempIt[k] = auxTempIt[k].Begin();
    }

  while( !inputIt.IsAtEnd() )
    {
    value = (double) ScalarTraits<PixelType>::GetScalar( inputIt.Get() );
    if( value - levelSetValue > 0 )
      {
      outputIt.Set( tempIt.Get() );

      for( unsigned int k = 0; k < VAuxDimension; k++ )
        {
        auxOutputIt[k].Set( auxTempIt[k].Get() );
        }

     }

    ++inputIt;
    ++outputIt;
    ++tempIt;
    for( unsigned int k = 0; k < VAuxDimension; k++ )
      {
      ++(auxTempIt[k]);
      ++(auxOutputIt[k]);
      }
    }

  // march inward
  m_Marcher->SetTrialPoints( m_Locator->GetInsidePoints() );
  m_Marcher->SetAuxiliaryTrialValues( m_Locator->GetAuxInsideValues() );
  m_Marcher->Update();

  inputIt = inputIt.Begin();
  outputIt = outputIt.Begin();
  tempIt = tempIt.Begin();
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    auxOutputIt[k] = auxOutputIt[k].Begin();
    auxTempIt[k] = auxTempIt[k].Begin();
    }

  while( !inputIt.IsAtEnd() )
    {
    value = (double) ScalarTraits<PixelType>::GetScalar( inputIt.Get() );
    if( value - levelSetValue <= 0 )
      {
      value = (double) ScalarTraits<PixelType>::GetScalar( tempIt.Get() );
      outputIt.Set( -1.0 * value );

      for( unsigned int k = 0; k < VAuxDimension; k++ )
        {
        auxOutputIt[k].Set( auxTempIt[k].Get() );
        }
      }

    ++inputIt;
    ++outputIt;
    ++tempIt;
    for( unsigned int k = 0; k < VAuxDimension; k++ )
      {
      ++(auxTempIt[k]);
      ++(auxOutputIt[k]);
      }
    }

}
  

/**
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::GenerateDataNarrowBand()
{

  LevelSetPointer inputPtr = this->GetInput();
  LevelSetPointer outputPtr = this->GetOutput();
  LevelSetPointer tempLevelSet = m_Marcher->GetOutput();

  double levelSetValue = this->GetLevelSetValue();
  double outputBandwidth = this->GetOutputNarrowBandwidth();
  double inputBandwidth = this->GetInputNarrowBandwidth();

  // define iterators
  typedef 
    ImageRegionIterator<LevelSetType::LevelSetImageType> IteratorType;

  IteratorType inputIt( inputPtr,
    inputPtr->GetBufferedRegion() );

  IteratorType outputIt( outputPtr,
    outputPtr->GetBufferedRegion() );

  PixelType posInfinity;
  PixelType negInfinity;
  typedef typename TLevelSet::ScalarValueType ScalarValueType;
  ScalarTraits<PixelType>::SetScalar(posInfinity, 
    NumericTraits<ScalarValueType>::max());
  ScalarTraits<PixelType>::SetScalar(negInfinity, 
    NumericTraits<ScalarValueType>::NonpositiveMin());

  // set all internal pixels to minus infinity and 
  // all external pixels to positive infinity
  double value;

  inputIt = inputIt.Begin();
  outputIt = outputIt.Begin();

  while( !inputIt.IsAtEnd() )
    {
    value = (double) ScalarTraits<PixelType>::GetScalar( inputIt.Get() );
    if( value - levelSetValue <= 0 )
      {
      outputIt.Set( negInfinity );
      }
    else
      {
      outputIt.Set( posInfinity );
      }

    ++inputIt;
    ++outputIt;
    }

  // set all auxiliary images to zero
  TAuxValue zeroPixel;
  ScalarTraits<TAuxValue>::SetScalar(zeroPixel, 0.0 );

  typedef
    ImageRegionIterator<AuxImageType> AuxIteratorType;

  AuxIteratorType auxOutputIt[VAuxDimension];

  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    AuxImagePointer ptr;
    ptr = m_OutputAuxImage[k];
    auxOutputIt[k] = AuxIteratorType( ptr,
      ptr->GetBufferedRegion() );
    auxOutputIt[k] = auxOutputIt[k].Begin();
    }
  while( !auxOutputIt[0].IsAtEnd() )
    {
    for( unsigned int k = 0; k < VAuxDimension; k++ )
      {
      auxOutputIt[k].Set(zeroPixel);
      ++(auxOutputIt[k]);

      }
    }

  AuxImagePointer tempAuxImage[VAuxDimension];
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    tempAuxImage[k] = m_Marcher->GetAuxiliaryImage(k);
    }

  // create a new output narrowband container
  NodeContainerPointer outputNB = NodeContainer::New();
  this->SetOutputNarrowBand( outputNB );

  // locate the level set
  m_Locator->SetInput( inputPtr );
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    m_Locator->SetAuxImage( m_InputAuxImage[k], k );
    }
  m_Locator->SetLevelSetValue( levelSetValue );

  if( this->GetNarrowBanding() && this->GetInputNarrowBand() )
    {
    m_Locator->NarrowBandingOn();
    m_Locator->SetNarrowBandwidth( inputBandwidth );
    m_Locator->SetInputNarrowBand( this->GetInputNarrowBand() );
    }
  else
    { 
    m_Locator->NarrowBandingOff();
    }

  m_Locator->Locate();

  // march outward
  double stoppingValue = ( outputBandwidth / 2.0 ) + 2.0;
  m_Marcher->SetStoppingValue( stoppingValue );
  m_Marcher->CollectPointsOn();
  m_Marcher->SetTrialPoints( m_Locator->GetOutsidePoints() );
  m_Marcher->SetAuxiliaryTrialValues( m_Locator->GetAuxOutsideValues() );
  m_Marcher->Update();

  NodeContainerPointer procPoints = m_Marcher->GetProcessedPoints();
  
  typename NodeContainer::ConstIterator pointsIt;
  typename NodeContainer::ConstIterator pointsEnd;
  
  pointsIt = procPoints->Begin();
  pointsEnd = procPoints->End();

  NodeType node;
  PixelType inPixel;

  for( ; pointsIt != pointsEnd; ++pointsIt )
    {
    node = pointsIt.Value();
    inPixel = inputPtr->GetPixel( node.index );
    
    value = (double) ScalarTraits<PixelType>::GetScalar( inPixel );
    if( value - levelSetValue > 0 )
      {
      inPixel = tempLevelSet->GetPixel( node.index );
      outputPtr->SetPixel( node.index, inPixel );
      outputNB->InsertElement( outputNB->Size(), node );

      for( unsigned int k = 0; k < VAuxDimension; k++ )
        { 
        m_OutputAuxImage[k]->SetPixel( node.index, 
          tempAuxImage[k]->GetPixel( node.index ) );
        }

      }

    }

  // march inward
  m_Marcher->SetTrialPoints( m_Locator->GetInsidePoints() );
  m_Marcher->SetAuxiliaryTrialValues( m_Locator->GetAuxInsideValues() );
  m_Marcher->Update();

  procPoints = m_Marcher->GetProcessedPoints();
  pointsIt = procPoints->Begin();
  pointsEnd = procPoints->End();

  for( ; pointsIt != pointsEnd; ++pointsIt )
    {
    node = pointsIt.Value();
    inPixel = inputPtr->GetPixel( node.index );
    
    value = (double) ScalarTraits<PixelType>::GetScalar( inPixel );
    if( value - levelSetValue <= 0 )
      {
      inPixel = tempLevelSet->GetPixel( node.index );
      value = (double) ScalarTraits<PixelType>::GetScalar( inPixel );
      ScalarTraits<PixelType>::SetScalar( inPixel, -1.0 * value );
      outputPtr->SetPixel( node.index, inPixel );
      node.value *= -1.0;
      outputNB->InsertElement( outputNB->Size(), node );

      for( unsigned int k = 0; k < VAuxDimension; k++ )
        { 
        m_OutputAuxImage[k]->SetPixel( node.index, 
          tempAuxImage[k]->GetPixel( node.index ) );
        }

      }

    }

}


} //namespace itk

#endif
