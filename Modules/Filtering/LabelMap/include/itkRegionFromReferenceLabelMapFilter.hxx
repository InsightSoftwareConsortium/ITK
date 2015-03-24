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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkRegionFromReferenceLabelMapFilter_hxx
#define itkRegionFromReferenceLabelMapFilter_hxx
#include "itkRegionFromReferenceLabelMapFilter.h"

namespace itk
{
template< typename TInputImage >
void
RegionFromReferenceLabelMapFilter< TInputImage >
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  this->SetRegion( this->GetReferenceImage()->GetLargestPossibleRegion() );
  this->GetOutput()->SetLargestPossibleRegion( this->GetRegion() );
}

template< typename TInputImage >
void
RegionFromReferenceLabelMapFilter< TInputImage >
::SetReferenceImage(const ReferenceImageType *image)
{
  itkDebugMacro("setting input ReferenceImage to " << image);
  if ( image != static_cast< const ReferenceImageType * >( this->GetInput(1) ) )
    {
    this->ProcessObject::SetNthInput( 1, const_cast< ReferenceImageType * >( image ) );
    this->Modified();
    }
}

template< typename TInputImage >
const typename RegionFromReferenceLabelMapFilter< TInputImage >::ReferenceImageType *
RegionFromReferenceLabelMapFilter< TInputImage >
::GetReferenceImage() const
{
  Self *surrogate = const_cast< Self * >( this );

  return itkDynamicCastInDebugMode<const ReferenceImageType *>(surrogate->ProcessObject::GetInput(1));
}

template< typename TInputImage >
void
RegionFromReferenceLabelMapFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
