/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectMorphologyImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkObjectMorphologyImageFilter_txx
#define __itkObjectMorphologyImageFilter_txx

#include <limits.h>

#include "itkConstantBoundaryCondition.h"
#include "itkNumericTraits.h"
#include "itkObjectMorphologyImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIterator.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::ObjectMorphologyImageFilter()
  : m_Kernel()
{
  m_ObjectValue = NumericTraits<PixelType>::One;
}
  
template <class TInputImage, class TOutputImage, class TKernel>
void 
ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
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
    e.SetDescription("Requested region is outside largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}


template<class TInputImage, class TOutputImage, class TKernel>
void
ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId) 
{
  ImageRegionConstIterator<TInputImage> iRegIter;
  ImageRegionIterator<TOutputImage> oRegIter;
  iRegIter = ImageRegionConstIterator<InputImageType>(this->GetInput(),
                                                    outputRegionForThread);
  oRegIter = ImageRegionIterator<OutputImageType>(this->GetOutput(),
                                                    outputRegionForThread);
  /* Copy the input image to the output image - then only boundary pixels
   * need to be changed in the output image */
  iRegIter.GoToBegin();
  oRegIter.GoToBegin();
  while(!oRegIter.IsAtEnd())
    {
    oRegIter.Set(iRegIter.Get());
    ++oRegIter;
    ++iRegIter;
    }

  // Is this the right boundary condition for all morphology operations?
  ConstantBoundaryCondition<TInputImage> BC;
  BC.SetConstant( NumericTraits<PixelType>::Zero );

  // Find the boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>
                                ::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType> fC;
  faceList = fC(this->GetInput(), outputRegionForThread, m_Kernel.GetRadius());

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>
                                ::FaceListType::iterator fit;

  // Setup the kernel that spans the immediate neighbors of the current
  // input pixel - used to determine if that pixel abuts a non-object
  // pixel, i.e., is a boundary pixel
  RadiusType bKernelSize;
  bKernelSize.Fill(1);
 
  ProgressReporter progress(this, threadId,
                            outputRegionForThread.GetNumberOfPixels());

  OutputNeighborhoodIteratorType oSNIter;
  InputNeighborhoodIteratorType iSNIter;
  for (fit = faceList.begin(); fit != faceList.end(); ++fit)
    { 
    oSNIter = OutputNeighborhoodIteratorType(m_Kernel.GetRadius(),
                                             this->GetOutput(), *fit);
    oSNIter.OverrideBoundaryCondition(&BC);
    oSNIter.GoToBegin();

    iSNIter = InputNeighborhoodIteratorType(bKernelSize,
                                            this->GetInput(), *fit);
    iSNIter.OverrideBoundaryCondition(&BC);
    iSNIter.GoToBegin();
    
    while ( ! iSNIter.IsAtEnd() )
      {
      if (iSNIter.GetCenterPixel() == m_ObjectValue)
        {
        if(this->IsObjectPixelOnBoundary(iSNIter))
          {
          this->Evaluate(oSNIter, m_Kernel);
          }
        }
      ++iSNIter;
      ++oSNIter;
      progress.CompletedPixel();
      }
    }
  
}

// Use neighborhood iter to determine if pixel touches a non-object pixel
template<class TInputImage, class TOutputImage, class TKernel>
bool
ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::IsObjectPixelOnBoundary(const InputNeighborhoodIteratorType &iNIter)
  {
  static const unsigned int s =
             (unsigned int)pow(static_cast<float>(3),
                               static_cast<float>(ImageDimension));

  unsigned int i;
  for(i=0; i<s; i++)
    {
    if(iNIter.GetPixel(i) != m_ObjectValue)
      {
      return true;
      }
    }

  return false;
  }

template<class TInputImage, class TOutputImage, class TKernel>
void
ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ObjectValue: " << m_ObjectValue << std::endl;
  os << indent << "Kernel: " << m_Kernel << std::endl;
}

}// end namespace itk
#endif
