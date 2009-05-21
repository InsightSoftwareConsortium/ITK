/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiphaseDenseFiniteDifferenceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMultiphaseDenseFiniteDifferenceImageFilter_txx
#define __itkMultiphaseDenseFiniteDifferenceImageFilter_txx

#include "itkMultiphaseDenseFiniteDifferenceImageFilter.h"

namespace itk
{

template < class TInputImage,
  class TOutputImage,
  class TFunction,
  typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage, TFunction, TIdCell >
::CopyInputToOutput()
{
  OutputImagePointer output = this->GetOutput();

  for( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    InputImagePointer input = this->m_LevelSet[i];
    InputPointType origin = input->GetOrigin();
    InputSpacingType spacing = input->GetSpacing();
    InputSizeType size = input->GetLargestPossibleRegion().GetSize();

    OutputIndexType start;

    // FIXME: Review pixel centering policy here !!!
    // Probably the PhysicalPointToIndex method should be used here...
    //
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      start[j] = static_cast<OutputIndexValueType>( origin[j]/spacing[j] );
      }

    OutputRegionType region;
    region.SetSize( size );
    region.SetIndex( start );

    if ( !input || !output )
      {
      itkExceptionMacro ( << "Either input and/or output is NULL." );
      }

    ImageRegionIterator< InputImageType > in( input, input->GetLargestPossibleRegion() );
    ImageRegionIterator< OutputImageType > out( output, region );

    // Fill the output pointer
    OutputPixelType p = static_cast<OutputPixelType> ( this->m_Lookup[i] );

    in.GoToBegin();
    out.GoToBegin();

    while( !out.IsAtEnd() )
      {
      if( in.Get() < 0 )
        {
        out.Value() =  p;
        }
      else
        {
        out.Value() = 0;
        }
      ++in;
      ++out;
      }
  }
}

template < class TInputImage,
  class TOutputImage,
  class TFunction,
  typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage, TFunction, TIdCell >
::AllocateUpdateBuffer()
{
  for( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    InputImagePointer input = this->m_LevelSet[i];
    InputRegionType region = input->GetLargestPossibleRegion();

    m_UpdateBuffers[i]->CopyInformation( input );
    m_UpdateBuffers[i]->SetRegions( region );
    m_UpdateBuffers[i]->Allocate();
    }
}

template < class TInputImage,
  class TOutputImage,
  class TFunction,
  typename TIdCell >
typename
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage, TFunction, TIdCell >::TimeStepType
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage, TFunction, TIdCell >
::CalculateChange()
{
  TimeStepType timeStep = NumericTraits< TimeStepType >::max();

  for( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    OutputImagePointer levelset = this->m_LevelSet[i];

    // Get the FiniteDifferenceFunction to use in calculations.
    const FiniteDifferenceFunctionPointer df = this->m_DifferenceFunctions[i];

    const OutputSizeType radius = df->GetRadius();

    // Break the input into a series of regions.  The first region is free
    // of boundary conditions, the rest with boundary conditions.  We operate
    // on the levelset region because input has been copied to output.
    FaceCalculatorType faceCalculator;
    FaceListType faceList =
      faceCalculator ( levelset, levelset->GetLargestPossibleRegion(), radius );

    void *globalData;

    // Ask the function object for a pointer to a data structure it
    // will use to manage any global values it needs.  We'll pass this
    // back to the function object at each calculation and then
    // again so that the function object can use it to determine a
    // time step for this iteration.
    globalData = df->GetGlobalDataPointer();

    typename FaceListType::iterator fIt;
    for ( fIt = faceList.begin(); fIt != faceList.end(); ++fIt )
      {
      // Process the non-boundary region.
      NeighborhoodIteratorType nD ( radius, levelset, *fIt );
      UpdateIteratorType nU ( m_UpdateBuffers[i], *fIt );

      nD.GoToBegin();
      nU.GoToBegin();

      while( !nD.IsAtEnd() )
        {
        nU.Value() = df->ComputeUpdate ( nD, globalData );
        ++nD;
        ++nU;
        }
      }

    // Ask the finite difference function to compute the time step for
    // this iteration.  We give it the global data pointer to use, then
    // ask it to free the global data memory.
    TimeStepType dt = df->ComputeGlobalTimeStep ( globalData );
    df->ReleaseGlobalDataPointer ( globalData );

    if ( dt < timeStep )
      {
      timeStep = dt;
      }
    }

  timeStep = 0.08;  // FIXME !!! After all this work, assign a constant !!! Why ??

  return timeStep;
}

template< class TInputImage,
  class TOutputImage,
  class TFunction,
  typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage, TFunction, TIdCell >
::SetFunctionCount( const IdCellType& n )
{
  this->Superclass::SetFunctionCount( n );

  this->m_UpdateBuffers.resize( n, 0 );

  for( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    this->m_UpdateBuffers[i] = UpdateBufferType::New();
    }
}

template< class TInputImage,
  class TOutputImage,
  class TFunction,
  typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage, TFunction, TIdCell >
::ApplyUpdate ( TimeStepType dt )
{
  double rms_change_accumulator = 0;
  double den = 0;
  IdCellType i;

  for( i = 0;  i < this->m_FunctionCount; i++ )
    {
    const double img_size = this->m_LevelSet[i]->GetLargestPossibleRegion().GetNumberOfPixels();
    den += img_size;
    }

  // this must never occur!
  if( den < vnl_math::eps )
    {
    itkExceptionMacro( "den = 0." );
    }

  // Updating the output image
  for ( i = 0;  i < this->m_FunctionCount; i++ )
    {
    //NOTE: here this->m_LevelSet[i]->GetRequestedRegion() is used and previously
    // it is this->m_LevelSet[i]->GetLargestPossibleRegion()
    InputRegionType region = this->m_LevelSet[i]->GetRequestedRegion();

    ImageRegionIterator<UpdateBufferType> u ( m_UpdateBuffers[i], region );
    ImageRegionIterator<OutputImageType>  o ( this->m_LevelSet[i], region );

    u.GoToBegin();
    o.GoToBegin();

    while( !u.IsAtEnd() )
      {
      u.Set( o.Value() + static_cast<OutputPixelType> ( dt ) * u.Get() );
      ++u;
      ++o;
      }

    if ( this->GetElapsedIterations() % this->m_ReinitializeCounter == 0 )
      {
      ThresholdFilterPointer thresh = ThresholdFilterType::New();
      thresh->SetLowerThreshold( NumericTraits< OutputPixelType >::NonpositiveMin() );
      thresh->SetUpperThreshold( 0 );
      thresh->SetInsideValue( 1 );
      thresh->SetOutsideValue( 0 );
      thresh->SetInput( m_UpdateBuffers[i] );
      thresh->Update();

      MaurerPointer maurer = MaurerType::New();
      maurer->SetInput( thresh->GetOutput() );
      maurer->SetSquaredDistance( 0 );
      maurer->SetUseImageSpacing( this->m_UseImageSpacing );
      maurer->SetInsideIsPositive( 0 );
      maurer->Update();

      ImageRegionIterator< OutputImageType >  it ( maurer->GetOutput(), region );

      o.GoToBegin();
      it.GoToBegin();

      OutputPixelType val;

      while( !o.IsAtEnd() )
        {
        val = it.Value();
        rms_change_accumulator += static_cast<double> ( vnl_math_sqr( o.Value() - val ) );
        o.Set( val );
        ++o;
        ++it;
        }
      }
    }

  this->SetRMSChange( vcl_sqrt(rms_change_accumulator / den ) );
}

template< class TInputImage,
  class TOutputImage,
  class TFunction,
  typename TIdCell >
void
MultiphaseDenseFiniteDifferenceImageFilter< TInputImage,
  TOutputImage, TFunction, TIdCell >
::PostProcessOutput()
{
  this->CopyInputToOutput();
}

}// end namespace itk

#endif
