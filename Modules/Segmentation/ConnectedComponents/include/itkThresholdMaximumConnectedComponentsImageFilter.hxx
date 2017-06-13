/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkThresholdMaximumConnectedComponentsImageFilter_hxx
#define itkThresholdMaximumConnectedComponentsImageFilter_hxx

#include "itkThresholdMaximumConnectedComponentsImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkObjectFactory.h"

namespace itk
{
/** Constructor
 *
 */
template< typename TInputImage, typename TOutputImage >
ThresholdMaximumConnectedComponentsImageFilter< TInputImage, TOutputImage >
::ThresholdMaximumConnectedComponentsImageFilter()
{
  m_ThresholdFilter = ThresholdFilterType::New();

  m_ConnectedComponent = ConnectedFilterType::New();

  m_LabeledComponent = RelabelFilterType::New();

  m_MinMaxCalculator = MinMaxCalculatorType::New();

  //
  // Connecting the internal pipeline.
  //
  m_ConnectedComponent->SetInput( m_ThresholdFilter->GetOutput() );
  m_LabeledComponent->SetInput( m_ConnectedComponent->GetOutput() );

  const typename NumericTraits< PixelType >::AccumulateType maxLabel =
    NumericTraits< PixelType >::max();
  const typename  NumericTraits< PixelType >::AccumulateType minLabel =
    NumericTraits< PixelType >::NonpositiveMin();

  //Default. Use ITK set macro "SetMinimumObjectSizeInPixels" to change
  m_MinimumObjectSizeInPixels = 0;

  m_ThresholdValue = static_cast< PixelType >( ( maxLabel + minLabel ) / 2 );

  // Initialize values for the threshold filters
  // Default. Use ITK set macro "SetOutsideValue" to change
  m_OutsideValue = static_cast< OutputPixelType >( minLabel );

  // Default. Use ITK set macro "SetInsideValue" to change
  m_InsideValue  = static_cast< OutputPixelType >( maxLabel );

  m_LowerBoundary = m_ThresholdValue;

  // Default. Use ITK set macro "SetUpperBoundary" to change
  m_UpperBoundary = static_cast< PixelType >( maxLabel );

  // Initialize the counter for the number of connected components
  // (objects) in the image.
  m_NumberOfObjects = 0;
} // end of the constructor

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
SizeValueType
ThresholdMaximumConnectedComponentsImageFilter< TInputImage, TOutputImage >
::ComputeConnectedComponents()
{
  m_ThresholdFilter->SetLowerThreshold(m_ThresholdValue);

  m_LabeledComponent->SetMinimumObjectSize(m_MinimumObjectSizeInPixels);
  m_LabeledComponent->Update();

  return m_LabeledComponent->GetNumberOfObjects();
}  //  end of ComputeConnectedComponents()

/**
 * This is the meat of the filter. It essentially uses a bisection
 * method to search for the threshold setPt that maximizes the number
 * of connected components in the image. The
 * "ComputeConnectedComponents" does the threshold and then a
 * connected components object count. It is removed from "GenerateData"
 * to make this all easier to read.
 *
 * Remove the comments on the output statements to see how the search
 * strategy works.
 */
template< typename TInputImage, typename TOutputImage >
void ThresholdMaximumConnectedComponentsImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  //
  //  Setup pointers to get input image and send info to output image
  //
  typename Superclass::InputImageConstPointer inputPtr  = this->GetInput();

  // Find the min and max of the image.
  m_MinMaxCalculator->SetImage( this->GetInput() );
  m_MinMaxCalculator->Compute();
  // Initial values to maximize search strategy
  // These are set to the smallest and largest image values so that
  // there is no chance that the found threshold is outside of this range.
  PixelType lowerBound = m_MinMaxCalculator->GetMinimum();
  PixelType upperBound = m_MinMaxCalculator->GetMaximum();

  // If the upper boundary is higher than the calculated maximum image
  // value, clamp it to this value.  This saves computation time
  // because there is no reason to search for values higher than the
  // max image value.
  upperBound = std::min( upperBound, m_UpperBoundary );

  m_ThresholdFilter->SetInput(inputPtr);
  m_ThresholdFilter->SetOutsideValue(m_OutsideValue);
  m_ThresholdFilter->SetInsideValue(m_InsideValue);
  m_ThresholdFilter->SetUpperThreshold(m_UpperBoundary);

  PixelType midpoint = ( upperBound - lowerBound ) / 2;
  PixelType midpointL = ( lowerBound + ( midpoint - lowerBound ) / 2 );
  PixelType midpointR = ( upperBound - ( upperBound - midpoint ) / 2 );

#ifndef NDEBUG
  SizeValueType iterationCounter = 0;
#endif

  while ( ( upperBound - lowerBound ) > 2 )
    {
    m_ThresholdValue = midpointR;

    const SizeValueType connectedComponentsRight =
      this->ComputeConnectedComponents();

    m_ThresholdValue = midpointL;

    const SizeValueType connectedComponentsLeft =
      this->ComputeConnectedComponents();

    // If the two thresholds give equal number of connected
    // components, we choose the lower threshold.
    if ( connectedComponentsRight > connectedComponentsLeft )
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

    itkDebugMacro("lowerbound: " << lowerBound
                                 << "\t midpoint:" << midpoint
                                 << "\t upperBound:" << upperBound);
    itkDebugMacro("Number of objects at left point: " << connectedComponentsLeft
                                                      << "; at right point: " << connectedComponentsRight);

    //
    // Set up values for next iteration
    //
    midpointL = ( lowerBound + ( midpoint - lowerBound ) / 2 );
    midpointR = ( upperBound - ( upperBound - midpoint ) / 2 );

#ifndef NDEBUG
    itkDebugMacro("new midpointL: " << midpointL
                                    << "\t new midpoint:" << midpoint
                                    << "\t new midpointR:" << midpointR << std::endl);
    itkDebugMacro("Iteration #:" << iterationCounter);

    iterationCounter++;
#endif
    } // end of the thresholdloop

  //
  //  The two output values
  //
  m_ThresholdValue = midpoint;

  m_ThresholdFilter->SetLowerThreshold(m_ThresholdValue);
  m_ThresholdFilter->Update();

  //
  // Graft the output of the thresholding filter to the output of this filter.
  //
  this->GraftOutput( m_ThresholdFilter->GetOutput() );
} // end of GenerateData Process

/** Standard Run of the mill PrintSelf
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ThresholdMaximumConnectedComponentsImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "InsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >(
    m_InsideValue ) << std::endl;
  os << indent << "OutsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >(
    m_OutsideValue ) << std::endl;
  os << indent << "Lower: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >(
    m_LowerBoundary ) << std::endl;
  os << indent << "Upper: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >(
    m_UpperBoundary ) << std::endl;
  os << indent << "Threshold Value: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >(
    m_ThresholdValue ) << std::endl;
  os << indent << "Number of Objects: " << m_NumberOfObjects << std::endl;
  os << indent << "Minimum Object Size in Pixels: "
     <<  m_MinimumObjectSizeInPixels << std::endl;
}
} // end namespace itk

#endif
