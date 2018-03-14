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
#ifndef itkPhaseCorrelationOptimizer_hxx
#define itkPhaseCorrelationOptimizer_hxx

#include "itkPhaseCorrelationOptimizer.h"

namespace itk
{


template < typename TImage >
PhaseCorrelationOptimizer<TImage>
::PhaseCorrelationOptimizer()
{
  this->SetNumberOfRequiredInputs( 1 );
  this->SetNumberOfRequiredOutputs( 1 );  // for the parameters

  m_Offset.Fill( 0 );

  OffsetOutputPointer offsetDecorator =
      static_cast< OffsetOutputType * >( this->MakeOutput(0).GetPointer() );
  this->ProcessObject::SetNthOutput( 0, offsetDecorator.GetPointer() );
  itkDebugMacro( "output is " << this->GetOutput()->Get() );
}


template < typename TImage >
void
PhaseCorrelationOptimizer<TImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Offset: " << m_Offset << std::endl;
}


template < typename TImage >
void
PhaseCorrelationOptimizer<TImage>
::GenerateData()
{
  if (!m_Updating)
    {
    this->Update();
    }
  else
    {
    OffsetType empty;
    empty.Fill( 0 );
    try
      {
      this->ComputeOffset();
      }
    catch( ExceptionObject& err )
      {
      itkDebugMacro( "exception called while computing offset - passing" );

      m_Offset = empty;

      // pass exception to caller
      throw err;
      }
    }

  // write the result to the output
  OffsetOutputType * output =
      static_cast< OffsetOutputType * >( this->ProcessObject::GetOutput(0) );
  output->Set(m_Offset);
}


template < typename TImage >
void
PhaseCorrelationOptimizer<TImage>
::SetInput( const ImageType * image )
{
  itkDebugMacro("setting input image to " << image );
  if ( this->GetInput(0) != image )
    {
    this->ProcessObject::SetNthInput(0, const_cast< ImageType * >( image ) );

    this->Modified();
    }
}


template < typename TImage >
const typename PhaseCorrelationOptimizer<TImage>::OffsetOutputType *
PhaseCorrelationOptimizer<TImage>
::GetOutput() const
{
  return static_cast< const OffsetOutputType * >(
                                          this->ProcessObject::GetOutput(0) );
}


template < typename TImage >
DataObject::Pointer
PhaseCorrelationOptimizer<TImage>
::MakeOutput( DataObjectPointerArraySizeType output )
{
  switch (output)
    {
    case 0:
      return static_cast<DataObject*>(OffsetOutputType::New().GetPointer());
      break;
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
    }
}

} //end namespace itk

#endif
