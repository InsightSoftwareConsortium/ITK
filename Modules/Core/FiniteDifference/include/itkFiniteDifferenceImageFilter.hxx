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
#ifndef itkFiniteDifferenceImageFilter_hxx
#define itkFiniteDifferenceImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkMacro.h"
#include "itkEventObject.h"
#include "itkFiniteDifferenceImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
FiniteDifferenceImageFilter< TInputImage, TOutputImage >
::FiniteDifferenceImageFilter()
{
  m_UseImageSpacing    = true;
  m_ElapsedIterations  = 0;
  m_DifferenceFunction = ITK_NULLPTR;
  m_NumberOfIterations = NumericTraits< IdentifierType >::max();
  m_MaximumRMSError = 0.0;
  m_RMSChange = 0.0;
  m_IsInitialized = false;
  m_ManualReinitialization = false;
  this->InPlaceOff();
}

template< typename TInputImage, typename TOutputImage >
FiniteDifferenceImageFilter< TInputImage, TOutputImage >
::~FiniteDifferenceImageFilter()
{}

template< typename TInputImage, typename TOutputImage >
void
FiniteDifferenceImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Test whether the output pixel type (or its components) are not of type
  // float or double:
  if ( NumericTraits< OutputPixelValueType >::is_integer )
    {
    itkWarningMacro("Output pixel type MUST be float or double to prevent computational errors");
    }

  if ( !m_IsInitialized )
    {
    // Allocate the output image
    this->AllocateOutputs();

    // Copy the input image to the output image.  Algorithms will operate
    // directly on the output image and the update buffer.
    this->CopyInputToOutput();

    // Set the coefficients of the Function and consider the use of images
    // spacing.
    this->InitializeFunctionCoefficients();

    // Perform any other necessary pre-iteration initialization.
    this->Initialize();

    // Allocate the internal update buffer.  This takes place entirely within
    // the subclass, since this class cannot define an update buffer type.
    this->AllocateUpdateBuffer();

    m_IsInitialized = true;
    m_ElapsedIterations = 0;
    }

  // Iterative algorithm
  while ( !this->Halt() )
    {
    this->InitializeIteration(); // An optional method for precalculating
                                 // global values, or otherwise setting up
                                 // for the next iteration

    TimeStepType dt = this->CalculateChange();

    this->ApplyUpdate(dt);
    ++m_ElapsedIterations;

    // Invoke the iteration event.
    this->InvokeEvent( IterationEvent() );
    if ( this->GetAbortGenerateData() )
      {
      this->InvokeEvent( IterationEvent() );
      this->ResetPipeline();
      throw ProcessAborted(__FILE__, __LINE__);
      }
    }

  if ( !m_ManualReinitialization )
    {
    // Reset the state once execution is completed
    m_IsInitialized = false;
    }
  // Any further processing of the solution can be done here.
  this->PostProcessOutput();
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
FiniteDifferenceImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input
  typename Superclass::InputImagePointer inputPtr  =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }

  // Get the size of the neighborhood on which we are going to operate.  This
  // radius is supplied by the difference function we are using.
  RadiusType radius = this->GetDifferenceFunction()->GetRadius();

  // Try to set up a buffered region that will accommodate our
  // neighborhood operations.  This may not be possible and we
  // need to be careful not to request a region outside the largest
  // possible region, because the pipeline will give us whatever we
  // ask for.

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(radius);

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< typename TInputImage, typename TOutputImage >
typename FiniteDifferenceImageFilter< TInputImage, TOutputImage >::TimeStepType
FiniteDifferenceImageFilter< TInputImage, TOutputImage >
::ResolveTimeStep(const std::vector< TimeStepType >& timeStepList,
                  const std::vector< bool >& valid ) const
{
  TimeStepType oMin = NumericTraits< TimeStepType >::ZeroValue();
  bool         flag = false;

  // grab first valid value
  typename std::vector< TimeStepType >::const_iterator t_it = timeStepList.begin();
  typename std::vector< TimeStepType >::const_iterator t_end = timeStepList.end();

  typename std::vector< bool >::const_iterator v_it = valid.begin();

  while( t_it != t_end )
    {
    if( *v_it )
      {
      oMin = *t_it;
      flag = true;
      break;
      }
    ++t_it;
    ++v_it;
    }

  if ( !flag )
    {
    // no values!
    itkGenericExceptionMacro( << "there is no satisfying value" );
    }

  t_it = timeStepList.begin();
  v_it = valid.begin();

  // find minimum value
   while( t_it != t_end )
    {
    if( *v_it && ( *t_it < oMin ) )
      {
      oMin = *t_it;
      }
    ++t_it;
    ++v_it;
    }

  return oMin;
}

template< typename TInputImage, typename TOutputImage >
bool
FiniteDifferenceImageFilter< TInputImage, TOutputImage >
::Halt()
{
  if ( m_NumberOfIterations != 0 )
    {
    this->UpdateProgress( static_cast< float >( this->GetElapsedIterations() )
                          / static_cast< float >( m_NumberOfIterations ) );
    }

  if ( this->GetElapsedIterations() >= m_NumberOfIterations )
    {
    return true;
    }
  else if ( this->GetElapsedIterations() == 0 )
    {
    return false;
    }
  else if ( this->GetMaximumRMSError() > m_RMSChange )
    {
    return true;
    }
  else
    {
    return false;
    }
}

template< typename TInputImage, typename TOutputImage >
void
FiniteDifferenceImageFilter< TInputImage, TOutputImage >
::InitializeFunctionCoefficients()
{
  // Set the coefficients for the derivatives
  double coeffs[ImageDimension];

  if ( this->m_UseImageSpacing )
    {
    const TOutputImage *outputImage =  this->GetOutput();
    if ( outputImage == ITK_NULLPTR )
      {
      itkExceptionMacro("Output image is ITK_NULLPTR");
      }

    typedef typename TOutputImage::SpacingType SpacingType;
    const SpacingType & spacing = outputImage->GetSpacing();

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      coeffs[i] = 1.0 / static_cast< double >( spacing[i] );
      }
    }
  else
    {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      coeffs[i] = 1.0;
      }
    }
  m_DifferenceFunction->SetScaleCoefficients(coeffs);
}

template< typename TInputImage, typename TOutputImage >
void
FiniteDifferenceImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ElapsedIterations: " << m_ElapsedIterations << std::endl;
  os << indent << "UseImageSpacing: " << ( m_UseImageSpacing ? "On" : "Off" ) << std::endl;
  os << indent << "State: " << ( m_IsInitialized ? "INITIALIZED" : "UNINITIALIZED" ) << std::endl;
  os << indent << "MaximumRMSError: " << m_MaximumRMSError << std::endl;
  os << indent << "NumberOfIterations: " << m_NumberOfIterations << std::endl;
  os << indent << "ManualReinitialization: " << m_ManualReinitialization << std::endl;
  os << indent << "RMSChange: " << m_RMSChange << std::endl;
  os << std::endl;
  if ( m_DifferenceFunction )
    {
    os << indent << "DifferenceFunction: " << std::endl;
    m_DifferenceFunction->Print( os, indent.GetNextIndent() );
    }
  else
    {
    os << indent << "DifferenceFunction: " << "(None)" << std::endl;
    }
  os << std::endl;
}
} // end namespace itk

#endif
