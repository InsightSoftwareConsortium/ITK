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
#ifndef itkWatershedEquivalenceRelabeler_hxx
#define itkWatershedEquivalenceRelabeler_hxx
#include "itkWatershedEquivalenceRelabeler.h"
#include "itkImageRegionIterator.h"
namespace itk
{
namespace watershed
{
template< typename TScalar, unsigned int TImageDimension >
void EquivalenceRelabeler< TScalar, TImageDimension >
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

  it_a.GoToBegin();
  it_b.GoToBegin();
  while ( !it_a.IsAtEnd() )
    {
    it_b.Set( it_a.Get() );
    ++it_a;
    ++it_b;
    }

  eqT->Flatten();
  SegmenterType::RelabelImage(output, output->GetRequestedRegion(), eqT);
}

template< typename TScalar, unsigned int VImageDimension >
void EquivalenceRelabeler< TScalar, VImageDimension >
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

template< typename TScalar, unsigned int TImageDimension >
void EquivalenceRelabeler< TScalar, TImageDimension >
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
    for ( idx = 0; idx < this->GetNumberOfIndexedOutputs(); ++idx )
      {
      if ( this->GetOutput(idx) && this->GetOutput(idx) != output )
        {
        op = dynamic_cast< ImageBase< ImageDimension > * >( this->GetOutput(idx) );
        if ( op ) { this->GetOutput(idx)->SetRequestedRegion(output); }
        }
      }
    }
}

template< typename TScalar, unsigned int TImageDimension >
void EquivalenceRelabeler< TScalar, TImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TScalar, unsigned int TImageDimension >
typename EquivalenceRelabeler< TScalar, TImageDimension >::DataObjectPointer
EquivalenceRelabeler< TScalar, TImageDimension >
::MakeOutput(DataObjectPointerArraySizeType)
{
  return ImageType::New().GetPointer();
}
} // end namespace watershed
} // end namespace itk

#endif
