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
#ifndef __itkWatershedEquivalenceRelabeler_hxx
#define __itkWatershedEquivalenceRelabeler_hxx
#include "itkWatershedEquivalenceRelabeler.h"
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
