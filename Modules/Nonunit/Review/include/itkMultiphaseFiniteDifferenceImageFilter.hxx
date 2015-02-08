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
#ifndef itkMultiphaseFiniteDifferenceImageFilter_hxx
#define itkMultiphaseFiniteDifferenceImageFilter_hxx

#include "itkMultiphaseFiniteDifferenceImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkExceptionObject.h"
#include "itkEventObject.h"

namespace itk
{
template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFiniteDifferenceFunction,
          typename TIdCell >
void
MultiphaseFiniteDifferenceImageFilter< TInputImage,
                                       TFeatureImage,
                                       TOutputImage,
                                       TFiniteDifferenceFunction,
                                       TIdCell >
::GenerateData()
{
  if ( !this->m_FunctionCount )
    {
    itkExceptionMacro("Number of level set functions not specified. "
                      << "Please set using SetFunctionCount()");
    }

  if ( !this->m_InitializedState )
    {
    // Set the coefficients for the deriviatives
    double       coeffs[ImageDimension];
    unsigned int i;
    if ( m_UseImageSpacing )
      {
      for ( i = 0; i < ImageDimension; i++ )
        {
        coeffs[i] = 1.0 / m_LevelSet[0]->GetSpacing()[i];
        }
      }
    else
      {
      for ( i = 0; i < ImageDimension; i++ )
        {
        coeffs[i] = 1.0;
        }
      }

    for ( IdCellType id = 0; id <  this->m_FunctionCount; id++ )
      {
      this->m_DifferenceFunctions[id]->SetScaleCoefficients(coeffs);
      }

    // Allocate the output image -- inherited method
    this->AllocateOutputs();

    // Copy the input image to the output image.  Algorithms will operate
    // directly on the output image and the update buffer.
    this->CopyInputToOutput();

    // Perform any other necessary pre-iteration initialization.
    this->Initialize();

    // Allocate the internal update buffer.  This takes place entirely within
    // the subclass, since this class cannot define an update buffer type.
    this->AllocateUpdateBuffer();

    this->SetInitializedState(true);
    }

  // Iterative algorithm
  TimeStepType dt;

  // An optional method for precalculating global values, or setting
  // up for the next iteration
  this->InitializeIteration();
  this->m_RMSChange = NumericTraits< double >::max();

  while ( !this->Halt() )
    {
    dt = this->CalculateChange();

    this->ApplyUpdate(dt);

    this->m_ElapsedIterations++;

    // Invoke the iteration event.
    this->InvokeEvent( IterationEvent() );

    if ( this->GetAbortGenerateData() )
      {
      this->InvokeEvent( IterationEvent() );
      this->ResetPipeline();
      throw ProcessAborted(__FILE__, __LINE__);
      }

    this->InitializeIteration();
    }

  // Reset the state once execution is completed
  if ( !this->m_ManualReinitialization )
    {
    this->SetInitializedState(true);
    }

  // Any further processing of the solution can be done here.
  this->PostProcessOutput();
}

/**
 *
 */
template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFiniteDifferenceFunction,
          typename TIdCell >
void
MultiphaseFiniteDifferenceImageFilter< TInputImage,
                                       TFeatureImage,
                                       TOutputImage,
                                       TFiniteDifferenceFunction,
                                       TIdCell >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input
  FeatureImagePointer inputPtr  =
    const_cast< FeatureImageType * >( this->GetInput(0) );

  if ( inputPtr.IsNull() )
    {
    return;
    }

  if ( this->m_DifferenceFunctions[0].IsNull() )
    {
    return;
    }

  // Get the size of the neighborhood on which we are going to operate.  This
  // radius is supplied by the difference function we are using.
  RadiusType radius = this->m_DifferenceFunctions[0]->GetRadius();

  // Try to set up a buffered region that will accommodate our
  // neighborhood operations.  This may not be possible and we
  // need to be careful not to request a region outside the largest
  // possible region, because the pipeline will give us whatever we
  // ask for.

  // get a copy of the input requested region (should equal the output
  // requested region)
  FeatureRegionType inputRequestedRegion;
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
    e.SetDescription("Requested region is (at least partially) outside the "
                     "largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFiniteDifferenceFunction,
          typename TIdCell >
typename MultiphaseFiniteDifferenceImageFilter< TInputImage, TFeatureImage,
                                                TOutputImage, TFiniteDifferenceFunction, TIdCell >::TimeStepType
MultiphaseFiniteDifferenceImageFilter< TInputImage,
                                       TFeatureImage,
                                       TOutputImage,
                                       TFiniteDifferenceFunction,
                                       TIdCell >
::ResolveTimeStep(const TimeStepVectorType & timeStepList,
                  const std::vector< bool > & valid)
{
  TimeStepType oMin = NumericTraits< TimeStepType >::ZeroValue();
  const SizeValueType size = timeStepList.size();

  if ( size == valid.size() )
    {
    bool   flag = false;
    SizeValueType k = 0;
    SizeValueType i;

    for ( i = 0; i < size; ++i )
      {
      if ( valid[i] )
        {
        oMin = timeStepList[i];
        k = i;
        flag = true;
        break;
        }
      }

    if ( !flag )
      {
      itkExceptionMacro("No Values");
      }

    // find minimum value
    for ( i = k; i < size; ++i )
      {
      if ( valid[i] && ( timeStepList[i] < oMin ) )
        {
        oMin = timeStepList[i];
        }
      }
    }

  return oMin;
}

template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFiniteDifferenceFunction,
          typename TIdCell >
bool
MultiphaseFiniteDifferenceImageFilter< TInputImage,
                                       TFeatureImage,
                                       TOutputImage,
                                       TFiniteDifferenceFunction,
                                       TIdCell >
::Halt()
{
  float progress = 0.;

  if ( this->m_NumberOfIterations != 0 )
    {
    progress = static_cast< float >( this->GetElapsedIterations() )
               / static_cast< float >( this->m_NumberOfIterations );
    }
  this->UpdateProgress(progress);

  return ( ( this->GetElapsedIterations() >= this->m_NumberOfIterations )
           || ( this->GetMaximumRMSError() >= this->m_RMSChange ) );
}

template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFiniteDifferenceFunction,
          typename TIdCell >
void
MultiphaseFiniteDifferenceImageFilter< TInputImage,
                                       TFeatureImage,
                                       TOutputImage,
                                       TFiniteDifferenceFunction,
                                       TIdCell >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ElapsedIterations: " << this->m_ElapsedIterations << std::endl;
  os << indent << "UseImageSpacing: " << ( m_UseImageSpacing ? "On" : "Off" ) << std::endl;
  os << indent << "State: " <<  this->m_InitializedState << std::endl;
  os << indent << "MaximumRMSError: " << m_MaximumRMSError << std::endl;
  os << indent << "NumberOfIterations: " << this->m_NumberOfIterations << std::endl;
  os << indent << "ManualReinitialization: " << this->m_ManualReinitialization << std::endl;
  os << indent << "RMSChange: " << m_RMSChange << std::endl;
  os << std::endl;

  if (  this->m_FunctionCount )
    {
    if ( this->m_DifferenceFunctions[0] )
      {
      os << indent << "DifferenceFunction: " << std::endl;
      for ( IdCellType i = 0; i <  this->m_FunctionCount; ++i )
        {
        this->m_DifferenceFunctions[i]->Print( os, indent.GetNextIndent() );
        }
      }
    }
  else
    {
    os << indent << "DifferenceFunction: " << "(None)" << std::endl;
    }

  os << std::endl;
}
} // end namespace itk

#endif
