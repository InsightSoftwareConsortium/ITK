/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptMorphologyImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
MorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::MorphologyImageFilter()
{
  m_DefaultBoundaryCondition.SetConstant(NumericTraits< PixelType >::Zero);
  m_BoundaryCondition = &m_DefaultBoundaryCondition;
}

template< class TInputImage, class TOutputImage, class TKernel >
void
MorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       int threadId)
{
  // Neighborhood iterators
  NeighborhoodIteratorType b_iter;

  // Find the boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > fC;
  faceList = fC( this->GetInput(), outputRegionForThread, this->GetKernel().GetRadius() );

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType::iterator fit;

  ImageRegionIterator< TOutputImage > o_iter;

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Process the boundary faces, these are N-d regions which border the
  // edge of the buffer

  const KernelIteratorType kernelBegin = this->GetKernel().Begin();
  const KernelIteratorType kernelEnd = this->GetKernel().End();

  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    b_iter = NeighborhoodIteratorType(this->GetKernel().GetRadius(),
                                      this->GetInput(), *fit);

    o_iter = ImageRegionIterator< OutputImageType >(this->GetOutput(), *fit);
    b_iter.OverrideBoundaryCondition(m_BoundaryCondition);
    b_iter.GoToBegin();

    while ( !o_iter.IsAtEnd() )
      {
      o_iter.Set( this->Evaluate(b_iter, kernelBegin, kernelEnd) );
      ++b_iter;
      ++o_iter;
      progress.CompletedPixel();
      }
    }
}

template< class TInputImage, class TOutputImage, class TKernel >
void
MorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Boundary condition: " << typeid( *m_BoundaryCondition ).name() << std::endl;
}
} // end namespace itk
#endif
