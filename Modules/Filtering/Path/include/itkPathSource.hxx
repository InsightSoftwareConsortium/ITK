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
#ifndef itkPathSource_hxx
#define itkPathSource_hxx

#include "itkPathSource.h"

namespace itk
{
/**
 *
 */
template< typename TOutputPath >
PathSource< TOutputPath >
::PathSource()
{
  // Create the output. We use static_cast<> here because we know the default
  // output must be of type TOutputPath
  OutputPathPointer output =
    static_cast< TOutputPath * >( this->MakeOutput(0).GetPointer() );

  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );

  // Initialize PathSource member data
}

/**
 *
 */
template< typename TOutputPath >
typename PathSource< TOutputPath >::OutputPathType *
PathSource< TOutputPath >
::GetOutput(void)
{
  return itkDynamicCastInDebugMode< TOutputPath * >( this->GetPrimaryOutput() );
}

/**
 *
 */
template< typename TOutputPath >
typename PathSource< TOutputPath >::OutputPathType *
PathSource< TOutputPath >
::GetOutput(unsigned int idx)
{
  return itkDynamicCastInDebugMode< TOutputPath * >
         ( this->ProcessObject::GetOutput(idx) );
}

/**
 *
 */
template< typename TOutputPath >
void
PathSource< TOutputPath >
::GraftOutput(TOutputPath *graft)
{
  this->GraftNthOutput(0, graft);
}

/**
 *
 */
template< typename TOutputPath >
void
PathSource< TOutputPath >
::GraftNthOutput(unsigned int idx, TOutputPath *graft)
{
  if ( idx < this->GetNumberOfIndexedOutputs() )
    {
    OutputPathType *output = this->GetOutput(idx);

    if ( output && graft )
      {
      // Paths do not have a generic pointer to their bulk data
      itkWarningMacro(<< "Warning:  GraftNthOutput() is broken");

      // possible VERY WRONG KLUDGE that should enable mini-pipelining:
      // Completely copy the path to graft over the current output path,
      // but RESTORE the original Source ivars to preserve pipeline routing.
      //
      // ProcessObject *source = output->GetSource();
      // *output = *graft;
      // output->DisconnectSource( graft->GetSource(),
      //                           graft->GetSourceOutputIndex() );
      // output->ConnectSource( source, idx );

      // grab a handle to the bulk data of the specified data object
      // output->SetPixelContainer( graft->GetPixelContainer() );

      // copy the region ivars of the specified data object
      // output->SetRequestedRegion( graft->GetRequestedRegion() );
      // output->SetLargestPossibleRegion( graft->GetLargestPossibleRegion() );
      // output->SetBufferedRegion( graft->GetBufferedRegion() );

      // copy the meta-information
      //output->CopyInformation( graft );
      }
    }
}

/**
 *
 */
template< typename TOutputPath >
typename PathSource< TOutputPath >::DataObjectPointer
PathSource< TOutputPath >
::MakeOutput(DataObjectPointerArraySizeType)
{
  return itkDynamicCastInDebugMode< DataObject * >( TOutputPath::New().GetPointer() );
}

/**
 *
 */
template< typename TOutputPath >
void
PathSource< TOutputPath >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
