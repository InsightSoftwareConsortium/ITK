/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedRelabeler.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedRelabeler_txx
#define __itkWatershedRelabeler_txx

#include "itkImageRegionIterator.h"

namespace itk
{
namespace watershed
{

template <class TScalarType, unsigned int TImageDimension>
Relabeler<TScalarType, TImageDimension>::
Relabeler() : m_FloodLevel(0.0)
{
  typename ImageType::Pointer img
    = static_cast<ImageType*>(this->MakeOutput(0).GetPointer());
  this->SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, img.GetPointer());
}
  
template <class TScalarType, unsigned int TImageDimension>
typename Relabeler<TScalarType, TImageDimension>::DataObjectPointer
Relabeler<TScalarType, TImageDimension>
::MakeOutput(unsigned int idx)
{
  return static_cast<DataObject*>(ImageType::New().GetPointer());
}
  
template <class TScalarType, unsigned int TImageDimension>
void Relabeler<TScalarType, TImageDimension>
::GenerateData()
{
  this->UpdateProgress(0.0);
  typename ImageType::Pointer input  = this->GetInputImage();
  typename ImageType::Pointer output  = this->GetOutputImage();

  typename SegmentTreeType::Pointer tree = this->GetInputSegmentTree();
  typename SegmentTreeType::Iterator it;
  EquivalencyTable::Pointer eqT = EquivalencyTable::New();

  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();
  //
  // Copy input to output
  //
  ImageRegionIterator<ImageType> it_a(input, output->GetRequestedRegion());
  ImageRegionIterator<ImageType> it_b(output, output->GetRequestedRegion());
  it_a = it_a.Begin();
  it_b = it_b.Begin();
  while (! it_a.IsAtEnd() )
    {
      it_b.Set(it_a.Get());
      ++it_a;
      ++it_b;
    }

  this->UpdateProgress(0.1);
  //
  // Extract the merges up the requested level
  //
  if (tree->Empty() == true )
    {
      // itkWarningMacro("Empty input.  No relabeling was done.");
      return;
    }
  ScalarType max = tree->Back().saliency;
  ScalarType mergeLimit = m_FloodLevel * max;

  this->UpdateProgress(0.5);
  
  it = tree->Begin();
  while ( it != tree->End() && (*it).saliency <= mergeLimit )
    {
      eqT->Add((*it).from, (*it).to);
      it++;      
    } 

  SegmenterType::RelabelImage(output, output->GetRequestedRegion(), eqT);
  this->UpdateProgress(1.0);
}

template <class TScalarType, unsigned int ImageDimension>
void Relabeler<TScalarType, ImageDimension>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  typename ImageType::Pointer  inputPtr  = this->GetInputImage();
  typename ImageType::Pointer outputPtr = this->GetOutputImage();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  /*
    // we need to compute the input requested region (size and start index)
    int i;
    const typename ImageType::SizeType& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
    const typename ImageType::IndexType& outputRequestedRegionStartIndex
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
    // std::cout << reg1 << std::endl;
    
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
  //
  inputPtr->SetRequestedRegion( outputPtr->GetRequestedRegion() );
}
  
template <class TScalarType, unsigned int TImageDimension>
void Relabeler<TScalarType, TImageDimension>
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

/*
 * 
 */
template <class TScalarType, unsigned int TImageDimension>
void Relabeler<TScalarType, TImageDimension>
::GraftOutput(ImageType *graft)
{
  this->GraftNthOutput(0, graft);
}

/*
 * 
 */
template <class TScalarType, unsigned int TImageDimension>
void Relabeler<TScalarType, TImageDimension>
::GraftNthOutput(unsigned int idx, ImageType *graft)
{
  typedef typename ImageType::Pointer OutputImagePointer;
  
  if (idx < this->GetNumberOfOutputs())
    {
    OutputImagePointer output = this->GetOutputImage();

    if (output && graft)
      {
      // grab a handle to the bulk data of the specified data object
      output->SetPixelContainer( graft->GetPixelContainer() );
      
      // copy the region ivars of the specified data object
      output->SetRequestedRegion( graft->GetRequestedRegion() );
      output->SetLargestPossibleRegion( graft->GetLargestPossibleRegion() );
      output->SetBufferedRegion( graft->GetBufferedRegion() );
      
      // copy the meta-information
      output->CopyInformation( graft );
      }
    }
}

template<class TScalarType, unsigned int TImageDimension>
void
Relabeler<TScalarType, TImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

}// end namespace watershed
}// end namespace itk

#endif
