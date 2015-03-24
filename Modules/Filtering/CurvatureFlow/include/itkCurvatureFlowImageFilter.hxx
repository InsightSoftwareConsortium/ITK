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
#ifndef itkCurvatureFlowImageFilter_hxx
#define itkCurvatureFlowImageFilter_hxx

#include "itkCurvatureFlowImageFilter.h"
#include "itkMacro.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
CurvatureFlowImageFilter< TInputImage, TOutputImage >
::CurvatureFlowImageFilter()
{
  this->SetNumberOfIterations(0);
  m_TimeStep   = 0.05f;

  typename CurvatureFlowFunctionType::Pointer cffp;
  cffp = CurvatureFlowFunctionType::New();

  this->SetDifferenceFunction( static_cast< FiniteDifferenceFunctionType * >(
                                 cffp.GetPointer() ) );
}

/**
 * Standard PrintSelf method.
 */
template< typename TInputImage, typename TOutputImage >
void
CurvatureFlowImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Time step: " << m_TimeStep;
  os << std::endl;
}

/**
 * Initialize the state of filter and equation before each iteration.
 */
template< typename TInputImage, typename TOutputImage >
void
CurvatureFlowImageFilter< TInputImage, TOutputImage >
::InitializeIteration()
{
  // update variables in the equation object
  CurvatureFlowFunctionType *f =
    dynamic_cast< CurvatureFlowFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !f )
    {
    itkExceptionMacro(<< "DifferenceFunction not of type CurvatureFlowFunction");
    }

  f->SetTimeStep(m_TimeStep);

  // call superclass's version
  this->Superclass::InitializeIteration();

  // progress feedback
  if ( this->GetNumberOfIterations() != 0 )
    {
    this->UpdateProgress( ( (float)( this->GetElapsedIterations() ) )
                          / ( (float)( this->GetNumberOfIterations() ) ) );
    }
}

/**
 * GenerateInputRequestedRegion
 */
template< typename TInputImage, typename TOutputImage >
void
CurvatureFlowImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr  =
    const_cast< InputImageType * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // set the input requested region to be the same as
  // the output requested region
  inputPtr->SetRequestedRegion(
    outputPtr->GetRequestedRegion() );
}

/**
 * EnlargeOutputRequestedRegion
 */
template< typename TInputImage, typename TOutputImage >
void
CurvatureFlowImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(
  DataObject *ptr)
{
  // convert DataObject pointer to OutputImageType pointer
  OutputImageType *outputPtr;

  outputPtr = dynamic_cast< OutputImageType * >( ptr );

  // get input image pointer
  typename Superclass::InputImagePointer inputPtr  =
    const_cast< InputImageType * >( this->GetInput() );
  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Get the size of the neighborhood on which we are going to operate.  This
  // radius is supplied by the difference function we are using.
  typename FiniteDifferenceFunctionType::RadiusType radius =
    this->GetDifferenceFunction()->GetRadius();

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    radius[j] *= this->GetNumberOfIterations();
    }

  /**
   * NewOutputRequestedRegion = OldOutputRequestedRegion +
   * radius * m_NumberOfIterations padding on each edge
   */
  typename OutputImageType::RegionType outputRequestedRegion =
    outputPtr->GetRequestedRegion();

  outputRequestedRegion.PadByRadius(radius);
  outputRequestedRegion.Crop( outputPtr->GetLargestPossibleRegion() );

  outputPtr->SetRequestedRegion(outputRequestedRegion);
}
} // end namespace itk

#endif
