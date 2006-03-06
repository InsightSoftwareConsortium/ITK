/*=========================================================================
  
  Filter: Automatic Threshold Image Filter
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdMaximumConnectedComponentsImageFilter.txx
  Language:  C++
  Date:      15 August 2006
  Version:   Revision: 1.00

  Copyright (c) Ken Urish 2005. All rights reserved.
  
  Portions of this code are covered under the ITK and VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.


     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _ThresholdMaximumConnectedComponentsImageFilter_txx
#define _ThresholdMaximumConnectedComponentsImageFilter_txx

#include "itkThresholdMaximumConnectedComponentsImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkCastImageFilter.h"



namespace itk
{

/** Constructor
 *
 */
template <class TInputImage>
ThresholdMaximumConnectedComponentsImageFilter<TInputImage>
::ThresholdMaximumConnectedComponentsImageFilter()
{

  const PixelType maxLabel = NumericTraits<PixelType>::max();
  const PixelType minLabel = NumericTraits<PixelType>::min(); 
  
  m_MinimumObjectSizeInPixels = 0; //Default. Use ITK set macro "SetMinimumObjectSizeInPixels" to change

  m_ThresholdValue = ( maxLabel - minLabel ) / 2; 
  
  // Initialize values for the theshold filters
  m_OutsideValue = minLabel; // Default. Use ITK set macro "SetOutsideValue" to change
  m_InsideValue  = maxLabel; // Default. Use ITK set macro "SetInsideValue" to change

  m_LowerBoundary = m_ThresholdValue;
  m_UpperBoundary = maxLabel;  // Default. Use ITK set macro "SetUpperBoundary" to change
  
 
  // Initialize the counter for the number of connected components (objects) in the image.
  m_NumberOfObjects = 0;
  
} // end of the constructor


/**
 * 
 */
template <class TInputImage>
void ThresholdMaximumConnectedComponentsImageFilter<TInputImage>
::ComputeConnectedComponents()
{

  // Get the input and output pointers
  typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();

  // Convert input pixel to internal pixel type
  // This is necessary for images with a pixel type that has a maximum value that is less than the 
  // number of connected components in the image. For example, an unsigned char image has a maximum
  // pixel value of 255, however if the image has 300 objects, the connected components filter 
  // generates an error message. This converts any pixel type to the internal filter type. At the 
  // end of this filter, the output image is converted back to the input pixel type.
  //
  typedef CastImageFilter < InputImageType, FilterImageType >  InputToFilterCastFilterType;
  typename InputToFilterCastFilterType::Pointer inputToFilterCastFilter= InputToFilterCastFilterType::New();

  inputToFilterCastFilter->SetInput( inputPtr );
  
  //
  // Binary Threshold Filter
  //
  typedef BinaryThresholdImageFilter< FilterImageType, InputImageType >  ThresholdFilterType;
  typename ThresholdFilterType::Pointer thresholdFilter = ThresholdFilterType::New();

  thresholdFilter->SetInput( inputToFilterCastFilter->GetOutput() );
  thresholdFilter->SetOutsideValue( m_OutsideValue );
  thresholdFilter->SetInsideValue( m_InsideValue );
  thresholdFilter->SetLowerThreshold( m_ThresholdValue );
  thresholdFilter->SetUpperThreshold( m_UpperBoundary );

  // 
  // Connected Components Filter  
  //
  typedef ConnectedComponentImageFilter< InputImageType, FilterImageType > ConnectedFilterType;
  typename ConnectedFilterType::Pointer connectedComponent = ConnectedFilterType::New();

  connectedComponent->SetInput( thresholdFilter->GetOutput() );
  
  //
  // Relabeled Components Filter    
  //
  typedef RelabelComponentImageFilter< FilterImageType, FilterImageType > RelabelFilterType;
  typename RelabelFilterType::Pointer labeledComponent = RelabelFilterType::New();

  labeledComponent->SetInput( connectedComponent->GetOutput() );
  labeledComponent->Update();
  
  const unsigned long totalNumberOfConnectedComponents = labeledComponent->GetNumberOfObjects();
  
  //
  // Count Valid Connected Components 
  // This removes any cc's that are below the input minimum pixel area
  // 
  m_NumberOfConnectedComponentsInThisIteration = 0;

  for( int i=0; i < totalNumberOfConnectedComponents; i++ )
    {

    const unsigned int connectedComponentSize = labeledComponent->GetSizeOfObjectsInPixels()[i];
    
    if( connectedComponentSize > m_MinimumObjectSizeInPixels )
      {
      m_NumberOfConnectedComponentsInThisIteration++;
      }
    } // end of for loop       

}  //  end of ComputeConnectedComponents()



/**
 * This is the meat of the filter. It essentially uses a bisection method
 * to search for the threshold setPt that maximizes the number of connected
 * components in the image. The "ComputeConnectedComponents" does the threshold
 * and then a connected components object count. Its removed from "GenerateData" 
 * to make this all easier to read.
 *
 * Remove the comments on the output statements to see how the search strategy works.   
 */
template <class TInputImage>
void ThresholdMaximumConnectedComponentsImageFilter<TInputImage>::GenerateData( void )
{

  const PixelType maxLabel = NumericTraits<PixelType>::max();
  const PixelType minLabel = NumericTraits<PixelType>::min(); 
  
  //Initial values to maximize search strategy
  PixelType lowerBound = minLabel;
  PixelType upperBound = maxLabel;
  
  PixelType midpoint = ( maxLabel - minLabel ) / 2;
  
  PixelType midpointL = ( lowerBound + ( midpoint - lowerBound ) / 2 );
  PixelType midpointR = ( upperBound - ( upperBound - midpoint ) / 2 );
 
  unsigned long iterationCounter = 0;

  while ( ( upperBound - lowerBound ) > 2 ) 
    {  
    
    m_ThresholdValue = midpointR;
    
    this->ComputeConnectedComponents();
    
    const unsigned long connectedComponentsRight = m_NumberOfConnectedComponentsInThisIteration;
    m_ThresholdValue = midpointL;
    
    this->ComputeConnectedComponents();
    
    const unsigned long connectedComponentsLeft = m_NumberOfConnectedComponentsInThisIteration;
    
    if( connectedComponentsRight > connectedComponentsLeft ) 
      {
      lowerBound = midpoint;
      midpoint   = midpointR;
      m_NumberOfObjects = connectedComponentsRight;
      } 
    else 
      { 
      upperBound = midpoint;
      midpoint   = midpointL;
      m_NumberOfObjects = connectedComponentsLeft;
      }
      
    itkDebugMacro("lowerbound:" << lowerBound << " midpoint:" << midpoint << " upperBound:" << upperBound );
    itkDebugMacro("Objects at Lt point:" << connectedComponentsLeft << "; at Rt point: " << connectedComponentsRight );

    //
    // Set up values for next iteration
    //
    midpointL = ( lowerBound + ( midpoint - lowerBound ) / 2 );
    midpointR = ( upperBound - ( upperBound - midpoint ) / 2 );

    itkDebugMacro("new midpointL:" << midpointL << "new midpoint:" << midpoint << "new midpointR:" << midpointR );
    itkDebugMacro("Iteration # :" << iterationCounter );
    
    iterationCounter++; 
    
  } // end of the thresholdloop

  //
  //  The two ouput values
  //
  m_ThresholdValue = midpoint;
  
  //
  //  Setup pointers for to get input image and send info to ouput image
  //
  typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput(0);
  
  outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  outputPtr->Allocate();  
  
  //
  //Binary Threshold Filter
  typedef BinaryThresholdImageFilter<InputImageType, InputImageType>  ThresholdFilterType;
  typename ThresholdFilterType::Pointer thresholdFilter = ThresholdFilterType::New();
  thresholdFilter->SetInput( inputPtr );
  thresholdFilter->SetOutsideValue( m_OutsideValue );
  thresholdFilter->SetInsideValue( m_InsideValue );
  thresholdFilter->SetLowerThreshold( m_ThresholdValue );
  thresholdFilter->SetUpperThreshold( m_UpperBoundary );
  thresholdFilter->Update();
   
  //
  // Writes to Output Image Pointer
  // This converts the image from the filter pixel type back to the input pixel type.
  //
  //  REVIEW COMMENT: Why not just grafting the output image ???
  //
  typename Superclass::OutputImagePointer tempOutputPtr = thresholdFilter->GetOutput();
  typedef    ImageRegionConstIterator< InputImageType >    InputIterator;
  typedef    ImageRegionIterator< OutputImageType >        OutputIterator;
  
  InputIterator  it1( tempOutputPtr, inputPtr->GetLargestPossibleRegion() );
  OutputIterator it2( outputPtr, outputPtr->GetLargestPossibleRegion() );
  inputPtr = thresholdFilter->GetOutput();
  
  it1.GoToBegin();
  it2.GoToBegin();
  while (!it1.IsAtEnd()) 
    {
    it2.Set ( it1.Get() );
    ++it1;
    ++it2;
    }    
  
} // end of GenerateData Process



/** Standard Run of the mill PrintSelf
 *  
 */
template <class TInputImage>
void 
ThresholdMaximumConnectedComponentsImageFilter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "OutsideValue: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>( m_OutsideValue )
     << std::endl;
  os << indent << "Lower: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>( m_LowerBoundary )
     << std::endl;
  os << indent << "Upper: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>( m_UpperBoundary )
     << std::endl;
}



} // end namespace itk

#endif

