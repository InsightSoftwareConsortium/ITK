/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedEquivalenceRelabeler.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedEquivalenceRelabeler_txx
#define __itkWatershedEquivalenceRelabeler_txx
#include "itkWatershedEquivalenceRelabeler.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
namespace itk
{
namespace watershed
{
template< class TScalarType, unsigned int TImageDimension >
void EquivalenceRelabeler< TScalarType, TImageDimension >
::GenerateData()
{
  typename ImageType::ConstPointer input  = this->GetInputImage();
  typename ImageType::Pointer output = this->GetOutputImage();

  typename EquivalencyTableType::Pointer eqT = this->GetEquivalencyTable();

  output->SetBufferedRegion( output->GetRequestedRegion() );
  output->Allocate();

  //
  // Copy input to output
  //
  ImageRegionConstIterator< ImageType > it_a( input, output->GetRequestedRegion() );
  ImageRegionIterator< ImageType >      it_b( output, output->GetRequestedRegion() );

  it_a = it_a.Begin();
  it_b = it_b.Begin();
  while ( !it_a.IsAtEnd() )
    {
    it_b.Set( it_a.Get() );
    ++it_a;
    ++it_b;
    }

  eqT->Flatten();
  SegmenterType::RelabelImage(output, output->GetRequestedRegion(), eqT);
}

template< class TScalarType, unsigned int VImageDimension >
void EquivalenceRelabeler< TScalarType, VImageDimension >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  ImageType *inputPtr  = const_cast< ImageType * >( this->GetInputImage() );
  ImageType *outputPtr  = this->GetOutputImage();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  //
  // FOR NOW WE'LL JUST SET THE INPUT REGION TO THE OUTPUT REGION
  // FOR STREAMING WITHIN THE PIPELINE NEED TO FIX THIS
  //
  inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
}

template< class TScalarType, unsigned int TImageDimension >
void EquivalenceRelabeler< TScalarType, TImageDimension >
::GenerateOutputRequestedRegion(DataObject *output)
{
  // Only the Image output need to be propagated through.
  // No choice but to use RTTI here.
  // All Image outputs set to the same RequestedRegion  other
  // outputs ignored.
  ImageBase< ImageDimension > *imgData;
  ImageBase< ImageDimension > *op;
  imgData = dynamic_cast< ImageBase< ImageDimension > * >( output );

  if ( imgData )
    {
    std::vector< ProcessObject::DataObjectPointer >::size_type idx;
    for ( idx = 0; idx < this->GetOutputs().size(); ++idx )
      {
      if ( this->GetOutputs()[idx] && this->GetOutputs()[idx] != output )
        {
        op = dynamic_cast< ImageBase< ImageDimension > * >( this->GetOutputs()[idx].GetPointer() );
        if ( op ) { this->GetOutputs()[idx]->SetRequestedRegion(output); }
        }
      }
    }
}

template< class TScalarType, unsigned int TImageDimension >
void EquivalenceRelabeler< TScalarType, TImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< class TScalarType, unsigned int TImageDimension >
typename EquivalenceRelabeler< TScalarType, TImageDimension >::DataObjectPointer
EquivalenceRelabeler< TScalarType, TImageDimension >
::MakeOutput(unsigned int)
{
  return static_cast< DataObject * >( ImageType::New().GetPointer() );
}
} // end namespace watershed
} // end namespace itk

#endif
