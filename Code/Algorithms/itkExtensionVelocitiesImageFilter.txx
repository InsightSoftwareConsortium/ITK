/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtensionVelocitiesImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkExtensionVelocitiesImageFilter_txx
#define _itkExtensionVelocitiesImageFilter_txx

#include "itkExtensionVelocitiesImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkIndex.h"


namespace itk
{

/*
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
    this->ProcessObject::SetNthOutput( k+1, ptr.GetPointer() );
    }

}

/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::SetInputVelocityImage(
  const AuxImageType * ptr,
  unsigned int idx )
{
  if( idx >= VAuxDimension )
    {
    return;
    }

  this->ProcessObject::SetNthInput( idx+1, const_cast< AuxImageType * >( ptr ) );

}


/*
 * 
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension>
const typename ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::AuxImageType *
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::GetInputVelocityImage( unsigned int idx )
{

  if ( idx >= VAuxDimension || this->GetNumberOfInputs() < idx + 2 )
    {
    return NULL;
    }

  return dynamic_cast<AuxImageType *>(
    this->ProcessObject::GetInput(idx+1) );

}


/*
 * 
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension>
typename ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::AuxImageType *
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::GetOutputVelocityImage( unsigned int idx )
{

  if ( idx >= VAuxDimension || this->GetNumberOfOutputs() < idx + 2 )
    {
    return NULL;
    }

  return static_cast<AuxImageType*>(
    this->ProcessObject::GetOutput(idx+1) );

}


/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) )
{

  for ( unsigned int j = 0; j < this->GetNumberOfOutputs(); j++ )
    {

    // Set all output requested region to largest possible
    if ( j == 0 ) 
      {
      this->Superclass::EnlargeOutputRequestedRegion( this->GetOutput(j) );
      }
    else if ( j <= VAuxDimension )
      {
      AuxImageType * imgData;
      imgData = dynamic_cast<AuxImageType*>( this->GetOutput(j) );
      if (imgData)
        {
        imgData->SetRequestedRegionToLargestPossibleRegion();
        }
      else
        {
        // Pointer could not be cast to AuxImageType *
        itkWarningMacro(<< "itk::ExtensionVelocitiesImageFilter" <<
                        "::EnlargeOutputRequestedRegion cannot cast "
                        << typeid(this->GetOutput(j)).name() << " to "
                        << typeid(AuxImageType*).name() );    

        }
      }
    }

}


/*
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
    AuxImagePointer output = this->GetOutputVelocityImage(k);
    output->SetBufferedRegion( output->GetRequestedRegion() );
    output->Allocate();
    }

  // set the marcher output size
  LevelSetPointer outputPtr = this->GetOutput();
  m_Marcher->SetOutputSize( 
    outputPtr->GetRequestedRegion().GetSize() );

}

/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::GenerateDataFull()
{

  LevelSetConstPointer inputPtr = this->GetInput();
  LevelSetPointer outputPtr = this->GetOutput();
  LevelSetPointer tempLevelSet = m_Marcher->GetOutput();

  double levelSetValue = this->GetLevelSetValue();

  // define iterators
  typedef typename LevelSetType::LevelSetImageType LocalLevelSetImageType;
  typedef 
    ImageRegionIterator<LocalLevelSetImageType> IteratorType;
  typedef 
    ImageRegionConstIterator<LocalLevelSetImageType> ConstIteratorType;

  ConstIteratorType inputIt( inputPtr,
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
    AuxImagePointer ptr = this->GetOutputVelocityImage(k);
    auxOutputIt[k] = AuxIteratorType( ptr,
                                      ptr->GetBufferedRegion() );
    }
 
  this->UpdateProgress( 0.0 );

  // locate the level set
  m_Locator->SetInputLevelSet( inputPtr );
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    m_Locator->SetAuxImage( this->GetInputVelocityImage(k), k );
    }
  m_Locator->SetLevelSetValue( levelSetValue );
  m_Locator->Locate();


  this->UpdateProgress( 0.33 );

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

  inputIt.GoToBegin();
  outputIt.GoToBegin();
  tempIt.GoToBegin();
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    auxOutputIt[k].GoToBegin();
    auxTempIt[k].GoToBegin();
    }

  while( !inputIt.IsAtEnd() )
    {
    value = (double) inputIt.Get();
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

  this->UpdateProgress( 0.66 );

  // march inward
  m_Marcher->SetTrialPoints( m_Locator->GetInsidePoints() );
  m_Marcher->SetAuxiliaryTrialValues( m_Locator->GetAuxInsideValues() );
  m_Marcher->Update();

  inputIt.GoToBegin();
  outputIt.GoToBegin();
  tempIt.GoToBegin();
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    auxOutputIt[k].GoToBegin();
    auxTempIt[k].GoToBegin();
    }

  while( !inputIt.IsAtEnd() )
    {
    value = (double) inputIt.Get();
    if( value - levelSetValue <= 0 )
      {
      value = (double) tempIt.Get();
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
  

/*
 *
 */
template <class TLevelSet, class TAuxValue, unsigned int VAuxDimension >
void
ExtensionVelocitiesImageFilter<TLevelSet,TAuxValue,VAuxDimension>
::GenerateDataNarrowBand()
{

  LevelSetConstPointer inputPtr = this->GetInput();
  LevelSetPointer outputPtr = this->GetOutput();
  LevelSetPointer tempLevelSet = m_Marcher->GetOutput();

  double levelSetValue = this->GetLevelSetValue();
  double outputBandwidth = this->GetOutputNarrowBandwidth();
  double inputBandwidth = this->GetInputNarrowBandwidth();

  // define iterators
  typedef typename LevelSetType::LevelSetImageType LocalLevelSetImageType;
  typedef 
    ImageRegionIterator<LocalLevelSetImageType> IteratorType;
  typedef 
    ImageRegionConstIterator<LocalLevelSetImageType> ConstIteratorType;

  ConstIteratorType inputIt( inputPtr,
                             inputPtr->GetBufferedRegion() );

  IteratorType outputIt( outputPtr,
                         outputPtr->GetBufferedRegion() );

  PixelType posInfinity;
  PixelType negInfinity;

  posInfinity = NumericTraits<PixelType>::max();
  negInfinity = NumericTraits<PixelType>::NonpositiveMin();

  // set all internal pixels to minus infinity and 
  // all external pixels to positive infinity
  double value;

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while( !inputIt.IsAtEnd() )
    {
    value = (double) inputIt.Get();
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
  zeroPixel = 0.0;

  typedef
    ImageRegionIterator<AuxImageType> AuxIteratorType;

  AuxIteratorType auxOutputIt[VAuxDimension];

  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    AuxImagePointer ptr = this->GetOutputVelocityImage(k);
    auxOutputIt[k] = AuxIteratorType( ptr,
                                      ptr->GetBufferedRegion() );
    auxOutputIt[k].GoToBegin();
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
  AuxImagePointer outputAuxImage[VAuxDimension];
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    tempAuxImage[k] = m_Marcher->GetAuxiliaryImage(k);
    outputAuxImage[k] = this->GetOutputVelocityImage(k);
    }

  // create a new output narrowband container
  NodeContainerPointer outputNB = NodeContainer::New();
  this->SetOutputNarrowBand( outputNB );

  this->UpdateProgress( 0.0 );

  // locate the level set
  m_Locator->SetInputLevelSet( inputPtr );
  for( unsigned int k = 0; k < VAuxDimension; k++ )
    {
    m_Locator->SetAuxImage( this->GetInputVelocityImage(k), k );
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

  this->UpdateProgress( 0.33 );


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
    inPixel = inputPtr->GetPixel( node.GetIndex() );
    
    value = (double) inPixel;
    if( value - levelSetValue > 0 )
      {
      inPixel = tempLevelSet->GetPixel( node.GetIndex() );
      outputPtr->SetPixel( node.GetIndex(), inPixel );
      outputNB->InsertElement( outputNB->Size(), node );

      for( unsigned int k = 0; k < VAuxDimension; k++ )
        { 
        outputAuxImage[k]->SetPixel( node.GetIndex(), 
                                     tempAuxImage[k]->GetPixel( node.GetIndex() ) );
        }

      }

    }

  this->UpdateProgress( 0.66 );

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
    inPixel = inputPtr->GetPixel( node.GetIndex() );
    
    value = (double) inPixel;
    if( value - levelSetValue <= 0 )
      {
      inPixel = tempLevelSet->GetPixel( node.GetIndex() );
      value = (double) inPixel;
      inPixel = -1.0 * value;
      outputPtr->SetPixel( node.GetIndex(), inPixel );
      node.SetValue( node.GetValue() * -1.0 );
      outputNB->InsertElement( outputNB->Size(), node );

      for( unsigned int k = 0; k < VAuxDimension; k++ )
        { 
        outputAuxImage[k]->SetPixel( node.GetIndex(), 
                                     tempAuxImage[k]->GetPixel( node.GetIndex() ) );
        }

      }

    }

}


} //namespace itk

#endif
