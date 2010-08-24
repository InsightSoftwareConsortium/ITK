/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionFromReferenceLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegionFromReferenceLabelMapFilter_txx
#define __itkRegionFromReferenceLabelMapFilter_txx
#include "itkRegionFromReferenceLabelMapFilter.h"

namespace itk
{
template< class TInputImage >
void
RegionFromReferenceLabelMapFilter< TInputImage >
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  this->SetRegion( this->GetReferenceImage()->GetLargestPossibleRegion() );
  this->GetOutput()->SetLargestPossibleRegion( this->GetRegion() );
}

template< class TInputImage >
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

template< class TInputImage >
const typename RegionFromReferenceLabelMapFilter< TInputImage >::ReferenceImageType *
RegionFromReferenceLabelMapFilter< TInputImage >
::GetReferenceImage() const
{
  Self *surrogate = const_cast< Self * >( this );

  const DataObject *input = surrogate->ProcessObject::GetInput(1);

  const ReferenceImageType *referenceImage = static_cast< const ReferenceImageType * >( input );

  return referenceImage;
}

template< class TInputImage >
void
RegionFromReferenceLabelMapFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
