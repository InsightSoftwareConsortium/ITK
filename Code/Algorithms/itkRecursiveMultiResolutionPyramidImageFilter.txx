/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveMultiResolutionPyramidImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRecursiveMultiResolutionPyramidImageFilter_txx
#define _itkRecursiveMultiResolutionPyramidImageFilter_txx

#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkCastImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkExceptionObject.h"

#include "vnl/vnl_math.h"

namespace itk
{


/*
 * Constructor
 */
template <class TInputImage, class TOutputImage>
RecursiveMultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::RecursiveMultiResolutionPyramidImageFilter()
{
}


/*
 * GenerateData
 */
template <class TInputImage, class TOutputImage>
void
RecursiveMultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::GenerateData()
{

  if( !this->IsScheduleDownwardDivisible( m_Schedule ) )
    {
    // use the Superclass implemenation
    this->Superclass::GenerateData();
    return;
    }

  // Get the input and output pointers
  InputImageConstPointer  inputPtr = this->GetInput();

  // Create caster, smoother and shrinker filters
  typedef CastImageFilter<TInputImage, TOutputImage> CasterType;
  typedef CastImageFilter<TOutputImage, TOutputImage> CopierType;
  typedef DiscreteGaussianImageFilter<TOutputImage, TOutputImage> SmootherType;
  typedef ShrinkImageFilter<TOutputImage,TOutputImage> ShrinkerType;

  typename CasterType::Pointer   caster   = CasterType::New();
  typename CopierType::Pointer   copier   = CopierType::New();
  typename SmootherType::Pointer smoother = SmootherType::New();
  typename ShrinkerType::Pointer shrinker = ShrinkerType::New();

  int ilevel;
  unsigned int idim;
  unsigned int factors[ImageDimension];
  double       variance[ImageDimension];
  bool         allOnes;
  OutputImagePointer   outputPtr;
  OutputImagePointer swapPtr;
  typename TOutputImage::RegionType  LPRegion;

  smoother->SetMaximumError( m_MaximumError );
  shrinker->SetInput( smoother->GetOutput() );

  
  // recursively compute outputs starting from the last one
  for( ilevel = m_NumberOfLevels - 1; ilevel > -1; ilevel--)
    {

    this->UpdateProgress( 1.0 - static_cast<float>( 1 + ilevel ) /
      static_cast<float>( m_NumberOfLevels ) );

    // Allocate memory for each output
    outputPtr = this->GetOutput( ilevel );
    outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
    outputPtr->Allocate();
   
    // cached a copy of the largest possible region
    LPRegion = outputPtr->GetLargestPossibleRegion();

    // Check shrink factors and compute variances
    allOnes = true;
    for( idim = 0; idim < ImageDimension; idim++ )
      {
      if( ilevel == static_cast<int>(m_NumberOfLevels) - 1)
        {
        factors[idim] = m_Schedule[ilevel][idim];
        }
      else
        {
        factors[idim] = m_Schedule[ilevel][idim] /
          m_Schedule[ilevel+1][idim];
        }
      variance[idim] = vnl_math_sqr( 0.5 * 
        static_cast<float>( factors[idim] ) );
      if( factors[idim] != 1 ) 
        { 
        allOnes = false; 
        }
      else
        {
        variance[idim] = 0.0;
        }
      }


    if( allOnes && ilevel == static_cast<int>(m_NumberOfLevels) - 1 )
      {
      // just copy the input over
      caster->SetInput( inputPtr );
      caster->GraftOutput( outputPtr );
      // ensure only the requested region is updated
      caster->UpdateOutputInformation();
      caster->GetOutput()->SetRequestedRegion(outputPtr->GetRequestedRegion());
      caster->GetOutput()->PropagateRequestedRegion();
      caster->GetOutput()->UpdateOutputData();

      swapPtr = caster->GetOutput();


      }
    else if( allOnes )
      {
      // just copy the data over
      copier->SetInput( swapPtr );
      copier->GraftOutput( outputPtr );
      // ensure only the requested region is updated
      copier->GetOutput()->UpdateOutputInformation();
      copier->GetOutput()->SetRequestedRegion(outputPtr->GetRequestedRegion());
      copier->GetOutput()->PropagateRequestedRegion();
      copier->GetOutput()->UpdateOutputData();

      swapPtr = copier->GetOutput();
        
      }
    else
      {
      if( ilevel == static_cast<int>(m_NumberOfLevels) - 1 )
        {
        // use caster -> smoother -> shrinker piepline
        caster->SetInput( inputPtr );
        smoother->SetInput( caster->GetOutput() );
        }
      else
        {
        // use smoother -> shrinker pipeline
        smoother->SetInput( swapPtr );
        }

      smoother->SetVariance( variance );

      shrinker->SetShrinkFactors( factors );
      shrinker->GraftOutput( outputPtr );
      // ensure only the requested region is updated
      shrinker->GetOutput()->UpdateOutputInformation();
      shrinker->GetOutput()->SetRequestedRegion(outputPtr->GetRequestedRegion());
      shrinker->GetOutput()->PropagateRequestedRegion();
      shrinker->GetOutput()->UpdateOutputData();

      swapPtr = shrinker->GetOutput();

      }

    // graft pipeline output back onto this filter's output
    swapPtr->SetLargestPossibleRegion( LPRegion );
    this->GraftNthOutput( ilevel, swapPtr );

    // disconnect from pipeline to stop cycle
    swapPtr->DisconnectPipeline();

    }

}


/*
 * PrintSelf method
 */
template <class TInputImage, class TOutputImage>
void
RecursiveMultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


/* 
 * GenerateOutputRequestedRegion
 */
template <class TInputImage, class TOutputImage>
void
RecursiveMultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::GenerateOutputRequestedRegion(DataObject * ptr )
{

  // call the superclass's implementation of this method
  Superclass::GenerateOutputRequestedRegion( ptr );

  TOutputImage * refOutputPtr = static_cast<TOutputImage*>( ptr );
  if( !refOutputPtr )
    {
    itkExceptionMacro( << "Could not cast ptr to TOutputImage*." );
    }

  // find the index for this output
  unsigned int refLevel = refOutputPtr->GetSourceOutputIndex();
  refLevel = refOutputPtr->GetSourceOutputIndex();

  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef GaussianOperator<OutputPixelType,ImageDimension> OperatorType;

  OperatorType * oper = new OperatorType;
  oper->SetMaximumError( m_MaximumError );

  typedef typename OutputImageType::SizeType    SizeType;
  typedef typename SizeType::SizeValueType      SizeValueType;
  typedef typename OutputImageType::IndexType   IndexType;
  typedef typename IndexType::IndexValueType    IndexValueType;
  typedef typename OutputImageType::RegionType  RegionType;

  int ilevel, idim;
  unsigned int factors[ImageDimension];
  unsigned long radius[ImageDimension];
  RegionType requestedRegion;
  SizeType   requestedSize;
  IndexType  requestedIndex;

  // compute requested regions for lower levels
  for( ilevel = refLevel + 1; ilevel < static_cast<int>(m_NumberOfLevels); 
    ilevel++ )
    {

    requestedRegion = this->GetOutput( ilevel - 1 )->GetRequestedRegion();
    requestedSize = requestedRegion.GetSize();
    requestedIndex = requestedRegion.GetIndex();

    for( idim = 0; idim < static_cast<int>(ImageDimension); idim++ )
      {
      factors[idim] = m_Schedule[ilevel-1][idim] / m_Schedule[ilevel][idim];

      // take into account shrink component
      requestedSize[idim] *= static_cast<SizeValueType>(factors[idim]);
      requestedIndex[idim] *= static_cast<IndexValueType>(factors[idim]);
      
      // take into account smoothing component
      if( factors[idim] > 1 )
        {
        oper->SetDirection( idim );
        oper->SetVariance( vnl_math_sqr( 0.5 * 
          static_cast<float>( factors[idim] ) ) );
        oper->CreateDirectional();
        radius[idim] = oper->GetRadius()[idim];
        }
       else
        {
        radius[idim] = 0;
        }
      }

    requestedRegion.SetSize( requestedSize );
    requestedRegion.SetIndex( requestedIndex );
    requestedRegion.PadByRadius( radius );
    requestedRegion.Crop( this->GetOutput(ilevel)->
      GetLargestPossibleRegion() );

    this->GetOutput(ilevel)->SetRequestedRegion( requestedRegion );

    }


  // compute requested regions for higher levels
  for( ilevel = refLevel - 1; ilevel > -1; ilevel-- )
    {
    requestedRegion = this->GetOutput( ilevel + 1 )->GetRequestedRegion();
    requestedSize = requestedRegion.GetSize();
    requestedIndex = requestedRegion.GetIndex();

    for( idim = 0; idim < static_cast<int>(ImageDimension); idim++ )
      {

      factors[idim] = m_Schedule[ilevel][idim] / m_Schedule[ilevel+1][idim];
      
      // take into account smoothing component
      if( factors[idim] > 1 )
        {
        oper->SetDirection( idim );
        oper->SetVariance( vnl_math_sqr( 0.5 * 
          static_cast<float>( factors[idim] ) ) );
        oper->CreateDirectional();
        radius[idim] = oper->GetRadius()[idim];
        }
       else
        {
        radius[idim] = 0;
        }

      requestedSize[idim] -= static_cast<SizeValueType>( 
        2 * radius[idim] );
      requestedIndex[idim] += radius[idim];
      
      // take into account shrink component
      requestedSize[idim] = static_cast<SizeValueType>( floor(
        static_cast<double>(requestedSize[idim]) / 
        static_cast<double>(factors[idim]) ) );
      if( requestedSize[idim] < 1 )
        {
        requestedSize[idim] = 1;
        }
      requestedIndex[idim] = static_cast<IndexValueType>( ceil(
        static_cast<double>(requestedIndex[idim]) /
        static_cast<double>(factors[idim]) ) );

      }

    requestedRegion.SetSize( requestedSize );
    requestedRegion.SetIndex( requestedIndex );
    requestedRegion.Crop( this->GetOutput(ilevel)->
      GetLargestPossibleRegion() );

    this->GetOutput(ilevel)->SetRequestedRegion( requestedRegion );

    }

  // clean up
  delete oper;

}


/* 
 * GenerateInputRequestedRegion
 */
template <class TInputImage, class TOutputImage>
void
RecursiveMultiResolutionPyramidImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{

  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
      const_cast< InputImageType *>( this->GetInput() );
  if ( !inputPtr )
    {
    itkExceptionMacro( << "Input has not been set." );
    }

  // compute baseIndex and baseSize
  typedef typename OutputImageType::SizeType    SizeType;
  typedef typename SizeType::SizeValueType      SizeValueType;
  typedef typename OutputImageType::IndexType   IndexType;
  typedef typename IndexType::IndexValueType    IndexValueType;
  typedef typename OutputImageType::RegionType  RegionType;

  unsigned int refLevel = m_NumberOfLevels - 1;
  SizeType baseSize = this->GetOutput(refLevel)->GetRequestedRegion().GetSize();
  IndexType baseIndex = this->GetOutput(refLevel)->GetRequestedRegion().GetIndex();
  RegionType baseRegion;

  unsigned int idim;
  for( idim = 0; idim < ImageDimension; idim++ )
    {
    unsigned int factor = m_Schedule[refLevel][idim];
    baseIndex[idim] *= static_cast<IndexValueType>( factor );
    baseSize[idim]  *= static_cast<SizeValueType>( factor );    
    }
  baseRegion.SetIndex( baseIndex );
  baseRegion.SetSize( baseSize );

  // compute requirements for the smoothing part
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef GaussianOperator<OutputPixelType,ImageDimension> OperatorType;

  OperatorType *oper = new OperatorType;
  unsigned long radius[ImageDimension];
  RegionType inputRequestedRegion = baseRegion;
  refLevel = 0;

  for( idim = 0; idim < TInputImage::ImageDimension; idim++ )
    {
    oper->SetDirection(idim);
    oper->SetVariance( vnl_math_sqr( 0.5 * static_cast<float>(
      m_Schedule[refLevel][idim] ) ) );
    oper->SetMaximumError( m_MaximumError );
    oper->CreateDirectional();
    radius[idim] = oper->GetRadius()[idim];
    if( m_Schedule[refLevel][idim] <= 1 )
      {
      radius[idim] = 0;
      }
      
    }
  delete oper;

  inputRequestedRegion.PadByRadius( radius );

  // make sure the requested region is within the largest possible
  inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );
  
  // set the input requested region
  inputPtr->SetRequestedRegion( inputRequestedRegion );

}


} // namespace itk

#endif
