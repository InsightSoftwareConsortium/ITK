/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedEquivalenceRelabeler.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
template <class TScalarType, unsigned int TImageDimension>
void EquivalenceRelabeler<TScalarType, TImageDimension>
::GenerateData()
{ 
  typename ImageType::ConstPointer input  = this->GetInputImage();
  typename ImageType::Pointer output = this->GetOutputImage();

  typename EquivalencyTableType::Pointer eqT = this->GetEquivalencyTable();
  
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  //
  // Copy input to output
  //
  ImageRegionConstIterator<ImageType> it_a(input, output->GetRequestedRegion());
  ImageRegionIterator<ImageType> it_b(output, output->GetRequestedRegion());

  it_a = it_a.Begin();
  it_b = it_b.Begin();
  while (! it_a.IsAtEnd() )
    {
    it_b.Set(it_a.Get());
    ++it_a;
    ++it_b;
    }

  eqT->Flatten();
  SegmenterType::RelabelImage(output, output->GetRequestedRegion(), eqT); 
}

template <class TScalarType, unsigned int ImageDimension>
void EquivalenceRelabeler<TScalarType, ImageDimension>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  ImageType  *inputPtr  = const_cast<ImageType *> (this->GetInputImage());
  ImageType *outputPtr  = this->GetOutputImage();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  /*
    // we need to compute the input requested region (size and start index)
    int i;
    const typename OutputImageType::SizeType& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
    const typename OutputImageType::IndexType& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();
    
    typename InputImageType::SizeType  inputRequestedRegionSize;
    typename InputImageType::IndexType inputRequestedRegionStartIndex;
    
    const typename InputImageType::SizeType  inputLargestPossibleRegionSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
    const typename InputImageType::IndexType inputLargestPossibleRegionStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();
    
    //  typename InputImageType::RegionType reg1 = inputPtr->GetRequestedRegion();
    //  reg1.PadByRadius(1);
    ///  
    //  exit(0);
    
    long crop=0;
    for (i = 0; i < TInputImage::ImageDimension; i++)
    {
    
    // Calculate a new region that is padded by 1 on each face
    inputRequestedRegionSize[i]
    = outputRequestedRegionSize[i] + 2;
    inputRequestedRegionStartIndex[i]
    = outputRequestedRegionStartIndex[i] - 1;
    
    // crop the requested region to the largest possible region
    
    
    // first check the start index
    if (inputRequestedRegionStartIndex[i]
    < inputLargestPossibleRegionStartIndex[i])
    {
    // how much do we need to adjust
    crop = inputLargestPossibleRegionStartIndex[i]
    - inputRequestedRegionStartIndex[i];
    
    // adjust the start index and the size of the requested region
    inputRequestedRegionStartIndex[i] += crop;
    inputRequestedRegionSize[i] -= crop;
    }
    // now check the final size
    if (inputRequestedRegionStartIndex[i] + inputRequestedRegionSize[i] 
    > inputLargestPossibleRegionStartIndex[i]
    + inputLargestPossibleRegionSize[i])
    {
    // how much do we need to adjust
    crop = inputRequestedRegionStartIndex[i] + inputRequestedRegionSize[i] 
    - inputLargestPossibleRegionStartIndex[i]
    - inputLargestPossibleRegionSize[i];
    
    // adjust the size
    inputRequestedRegionSize[i] -= crop;
    }
    }
    
    typename TInputImage::RegionType inputRequestedRegion;
    inputRequestedRegion.SetSize( inputRequestedRegionSize );
    inputRequestedRegion.SetIndex( inputRequestedRegionStartIndex );
    
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
  */

  // 
  // FOR NOW WE'LL JUST SET THE INPUT REGION TO THE OUTPUT REGION
  // FOR STREAMING WITHIN THE PIPELINE NEED TO FIX THIS
  //
  inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
}
  
template <class TScalarType, unsigned int TImageDimension>
void EquivalenceRelabeler<TScalarType, TImageDimension>
::GenerateOutputRequestedRegion(DataObject *output)
{
  // Only the Image output need to be propagated through.
  // No choice but to use RTTI here.
  // All Image outputs set to the same RequestedRegion  other
  // outputs ignored.
  ImageBase<ImageDimension> *imgData;
  ImageBase<ImageDimension> *op;
  imgData = dynamic_cast<ImageBase<ImageDimension> * >(output);

  if (imgData)
    {
    std::vector<ProcessObject::DataObjectPointer>::size_type idx;
    for (idx = 0; idx < this->GetOutputs().size(); ++idx)
      {
      if (this->GetOutputs()[idx] && this->GetOutputs()[idx] != output)
        {
        op = dynamic_cast<ImageBase<ImageDimension> *>(this->GetOutputs()[idx].GetPointer());
        if (op) this->GetOutputs()[idx]->SetRequestedRegion(output);
        }
      }
    }
}

template <class TScalarType, unsigned int TImageDimension>
void EquivalenceRelabeler<TScalarType, TImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

template <class TScalarType, unsigned int TImageDimension>
typename EquivalenceRelabeler<TScalarType, TImageDimension>::DataObjectPointer
EquivalenceRelabeler<TScalarType, TImageDimension>
::MakeOutput(unsigned int )
{
  return static_cast<DataObject*>(ImageType::New().GetPointer());
}
  


}// end namespace watershed
}// end namespace itk

#endif
