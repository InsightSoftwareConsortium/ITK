/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMorphologyImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMorphologyImageFilter_txx
#define __itkMorphologyImageFilter_txx

#include <limits.h>

#include "itkConstantBoundaryCondition.h"
#include "itkNumericTraits.h"
#include "itkMorphologyImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::MorphologyImageFilter()
  : m_Kernel()
{
}
  
template <class TInputImage, class TOutputImage, class TKernel>
void 
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  typename Superclass::InputImagePointer  inputPtr = 
    const_cast< TInputImage * >( this->GetInput() );
  
  if ( !inputPtr )
    {
    return;
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( m_Kernel.GetRadius() );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()) )
    {
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    OStringStream msg;
    msg << static_cast<const char *>(this->GetNameOfClass())
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}


template<class TInputImage, class TOutputImage, class TKernel>
void
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId) 
{
  // Is this the right boundary condition for all morphology operations?
  ConstantBoundaryCondition<TInputImage> BC;
  BC.SetConstant( NumericTraits<PixelType>::Zero );

  // Neighborhood iterators
  NeighborhoodIteratorType b_iter;

  // Find the boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType> fC;
  faceList = fC(this->GetInput(), outputRegionForThread, m_Kernel.GetRadius());

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit;

  ImageRegionIterator<TOutputImage> o_iter;
 
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  // Process the boundary faces, these are N-d regions which border the
  // edge of the buffer

  const KernelIteratorType kernelBegin = m_Kernel.Begin();
  const KernelIteratorType kernelEnd = m_Kernel.End();
  
  for (fit = faceList.begin(); fit != faceList.end(); ++fit)
    { 
    b_iter = NeighborhoodIteratorType(m_Kernel.GetRadius(),
                                      this->GetInput(), *fit);
    
    o_iter = ImageRegionIterator<OutputImageType>(this->GetOutput(), *fit);
    b_iter.OverrideBoundaryCondition(&BC);
    b_iter.GoToBegin();
    
    while ( ! b_iter.IsAtEnd() )
      {
      o_iter.Set( this->Evaluate(b_iter, kernelBegin, kernelEnd) );
      ++b_iter;
      ++o_iter;
      progress.CompletedPixel();
      }
    }
  
}



template<class TInputImage, class TOutputImage, class TKernel>
void
MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
}

}// end namespace itk
#endif
