/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSignedDanielssonDistanceMapImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSignedDanielssonDistanceMapImageFilter_txx
#define _itkSignedDanielssonDistanceMapImageFilter_txx

#include "itkSignedDanielssonDistanceMapImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

/**
 *    Constructor
 */
template <class TInputImage,class TOutputImage>
SignedDanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::SignedDanielssonDistanceMapImageFilter()
{
  this->SetNumberOfRequiredOutputs( 3 );

  OutputImagePointer distanceMap = OutputImageType::New();
  this->SetNthOutput( 0, distanceMap.GetPointer() );

  OutputImagePointer voronoiMap = OutputImageType::New();
  this->SetNthOutput( 1, voronoiMap.GetPointer() );

  VectorImagePointer distanceVectors = VectorImageType::New();
  this->SetNthOutput( 2, distanceVectors.GetPointer() );

  //Default values
  this->m_SquaredDistance     = false;  //Should we remove this ?
                                        //doesn't make sense in a SignedDaniel
  this->m_UseImageSpacing     = false;
  this->m_InsideIsPositive    = false;
}



/**
 *  Return the distance map Image pointer
 */
template <class TInputImage,class TOutputImage>
typename SignedDanielssonDistanceMapImageFilter<
  TInputImage,TOutputImage>::OutputImageType * 
SignedDanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::GetDistanceMap(void)
{
  return  dynamic_cast< OutputImageType * >(
    this->ProcessObject::GetOutput(0) );
}


/**
 *  Return Closest Points Map
 */
template <class TInputImage,class TOutputImage>
typename
SignedDanielssonDistanceMapImageFilter<TInputImage,TOutputImage>::OutputImageType*
SignedDanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::GetVoronoiMap(void)
{
  return  dynamic_cast< OutputImageType * >(
    this->ProcessObject::GetOutput(1) );
}


/**
 *  Return the distance vectors
 */
template <class TInputImage,class TOutputImage>
typename SignedDanielssonDistanceMapImageFilter<
  TInputImage,TOutputImage>::VectorImageType * 
SignedDanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::GetVectorDistanceMap(void)
{
  return  dynamic_cast< VectorImageType * >(
    this->ProcessObject::GetOutput(2) );
}


/**
 *  Compute Distance and Voronoi maps by calling 
 * DanielssonDistanceMapImageFilter twice.
 */
template <class TInputImage,class TOutputImage>
void SignedDanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::GenerateData() 
{
  //Set up mini pipeline filter
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);  
  
  typedef DanielssonDistanceMapImageFilter<
    InputImageType, OutputImageType >  FilterType;
  typename FilterType::Pointer filter1= FilterType::New();
  typename FilterType::Pointer filter2= FilterType::New();

  filter1->SetInputIsBinary( true );  // Force signed distance map to work on 
  filter2->SetInputIsBinary( true );  // binary images
  filter1->SetUseImageSpacing( m_UseImageSpacing );
  filter2->SetUseImageSpacing( m_UseImageSpacing );
  filter1->SetSquaredDistance( m_SquaredDistance );
  filter2->SetSquaredDistance( m_SquaredDistance );
    
  //Invert input image for second Danielsson filter
  typedef typename InputImageType::PixelType InputPixelType;
  typedef Functor::InvertIntensityFunctor< InputPixelType >  FunctorType;
  
  typedef UnaryFunctorImageFilter< InputImageType, 
                                   InputImageType,
                                   FunctorType >    InverterType;

  typename InverterType::Pointer inverter = InverterType::New();

  inverter->SetInput(this->GetInput());
 
  //Dilate the inverted image by 1 pixel to give it the same boundary
  //as the univerted input.
  
  typedef BinaryBallStructuringElement< 
                     InputPixelType, 
                     InputImageDimension  > StructuringElementType;  

  typedef BinaryDilateImageFilter< 
                         InputImageType, 
                         InputImageType, 
                         StructuringElementType >     DilatorType; 

  typename DilatorType::Pointer dilator = DilatorType::New();

  StructuringElementType  structuringElement;
  structuringElement.SetRadius( 1 );  // 3x3 structuring element
  structuringElement.CreateStructuringElement();
  dilator->SetKernel(  structuringElement );
  dilator->SetDilateValue(1);

  filter1->SetInput( this->GetInput() );
  dilator->SetInput( inverter->GetOutput() );
  filter2->SetInput( dilator->GetOutput() );

  //Subtract Distance maps results of the two Danielsson filters
  typedef SubtractImageFilter< OutputImageType, OutputImageType, 
    OutputImageType > SubtracterType;

  typename SubtracterType::Pointer subtracter = SubtracterType::New();

  if ( m_InsideIsPositive )
    {
    subtracter->SetInput1(filter2->GetOutput(0));
    subtracter->SetInput2(filter1->GetOutput(0));
    }
  else
    {
    subtracter->SetInput2(filter2->GetOutput(0));
    subtracter->SetInput1(filter1->GetOutput(0));
    }
    
  subtracter->Update();
  filter1->Update();
  filter2->Update();
  
  // Register progress
  progress->RegisterInternalFilter(filter1,.5f);
  
  // Graft outputs
  this->GraftNthOutput( 0, subtracter->GetOutput(0) );
  this->GraftNthOutput( 1, filter1->GetOutput(1) );
  this->GraftNthOutput( 2, filter1->GetOutput(2) );
  
} 
// end GenerateData()



/**
 *  Print Self
 */
template <class TInputImage,class TOutputImage>
void SignedDanielssonDistanceMapImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Signed Danielson Distance: " << std::endl;
  os << indent << "Use Image Spacing : " << m_UseImageSpacing << std::endl;
  os << indent << "Squared Distance  : " << m_SquaredDistance << std::endl;
  os << indent << "Inside is positive  : " << m_InsideIsPositive << std::endl;
}


} // end namespace itk

#endif

