/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNearestNeighborInterpolateImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorNearestNeighborInterpolateImageFunction_txx
#define __itkVectorNearestNeighborInterpolateImageFunction_txx

#include "itkVectorNearestNeighborInterpolateImageFunction.h"

#include "vnl/vnl_math.h"

namespace itk
{
/**
 * Constructor
 */
template<class TInputImage, class TCoordRep>
VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::VectorNearestNeighborInterpolateImageFunction()
{
}


/**
 * PrintSelf
 */
template<class TInputImage, class TCoordRep>
void
VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 * Evaluate at image index position, no bounds checking....  
 * use IsInsideBuffer() if needed
 */

/**
 * The nearest neighbour is assumed to be the pixel that has the largest
* overlap with the pixel that is centered on the indicated point (defined
* by ContinuousIndexType& index.
* 
* We also achieve speedup by noting that if overlap is > 1/2 it
* is the nearest neighbour and if < 1/m_Neighbors, it isn't.
* If all overlaps are the same, we choose the last neighbour.
*/
template<class TInputImage, class TCoordRep>
typename VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::OutputType
VectorNearestNeighborInterpolateImageFunction< TInputImage, TCoordRep >
::EvaluateAtContinuousIndex(
  const ContinuousIndexType& index) const 
{
  IndexType nindex;
  this->ConvertContinuousIndexToNearestIndex(index, nindex);
  return static_cast<OutputType>( this->GetInputImage()->GetPixel( nindex ) );
}

} // end namespace itk

#endif
