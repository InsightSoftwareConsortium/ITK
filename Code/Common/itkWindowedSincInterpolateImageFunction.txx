/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWindowedSincInterpolateImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWindowedSincInterpolateImageFunction_txx
#define __itkWindowedSincInterpolateImageFunction_txx

#include "itkWindowedSincInterpolateImageFunction.h"

#include "vnl/vnl_math.h"

namespace itk
{

/* Constant definitions for functions */
namespace Function {

template<unsigned int VRadius, class TInput, class TOutput>
const double
CosineWindowFunction<VRadius, TInput, TOutput>
::m_Factor = vnl_math::pi / ( 2 * VRadius );

template<unsigned int VRadius, class TInput, class TOutput>
const double
HammingWindowFunction<VRadius, TInput, TOutput>
::m_Factor = vnl_math::pi / VRadius;

template<unsigned int VRadius, class TInput, class TOutput>
const double
WelchWindowFunction<VRadius, TInput, TOutput>
::m_Factor = 1.0 / ( VRadius * VRadius );

template<unsigned int VRadius, class TInput, class TOutput>
const double
LanczosWindowFunction<VRadius, TInput, TOutput>
::m_Factor = vnl_math::pi / VRadius;

template<unsigned int VRadius, class TInput, class TOutput>
const double
BlackmanWindowFunction<VRadius, TInput, TOutput>
::m_Factor1 = vnl_math::pi / VRadius;

template<unsigned int VRadius, class TInput, class TOutput>
const double
BlackmanWindowFunction<VRadius, TInput, TOutput>
::m_Factor2 = 2.0 * vnl_math::pi / VRadius;




} // end namespace Function


/**
 * Window size constant
 */
  
template<class TInputImage, unsigned int VRadius,  
  class TWindowFunction, class TBoundaryCondition, class TCoordRep>
const unsigned int
WindowedSincInterpolateImageFunction<TInputImage,VRadius,
  TWindowFunction,TBoundaryCondition,TCoordRep>
::m_WindowSize = VRadius << 1;

/**
 * Constructor
 */
template<class TInputImage, unsigned int VRadius,  
  class TWindowFunction, class TBoundaryCondition, class TCoordRep>
WindowedSincInterpolateImageFunction<TInputImage,VRadius,
  TWindowFunction,TBoundaryCondition,TCoordRep>
::WindowedSincInterpolateImageFunction()
{
  unsigned int dim;
  
  // Compute the offset table size
  m_OffsetTableSize = 1;
  for(dim=0;dim<ImageDimension;dim++)
    {
    m_OffsetTableSize *= m_WindowSize;
    }

  // Allocate the offset table
  m_OffsetTable = new unsigned int[m_OffsetTableSize];

  // Allocate the weights tables
  m_WeightOffsetTable = new unsigned int *[m_OffsetTableSize];
  for(unsigned int i=0;i<m_OffsetTableSize;i++)
    {
    m_WeightOffsetTable[i] = new unsigned int[ImageDimension];
    }
}

/**
 * Destructor
 */
template<class TInputImage, unsigned int VRadius,  
  class TWindowFunction, class TBoundaryCondition, class TCoordRep>
WindowedSincInterpolateImageFunction<TInputImage,VRadius,
  TWindowFunction,TBoundaryCondition,TCoordRep>
::~WindowedSincInterpolateImageFunction()
{
  // Clear the offset table
  delete [] m_OffsetTable;

  // Clear the weights tables
  for(unsigned int i=0; i < m_OffsetTableSize; i++)
    {
    delete [] m_WeightOffsetTable[i];
    }
  delete[] m_WeightOffsetTable;
}

template<class TInputImage, unsigned int VRadius,  
  class TWindowFunction, class TBoundaryCondition, class TCoordRep>
void
WindowedSincInterpolateImageFunction<TInputImage,VRadius,
  TWindowFunction,TBoundaryCondition,TCoordRep>
::SetInputImage(const ImageType *image)
{
  unsigned int dim;
  
  // Call the parent implementation
  Superclass::SetInputImage(image);

  if( image == NULL )
    {
    return;
    }

  // Set the radius for the neighborhood
  Size<ImageDimension> radius;
  radius.Fill(VRadius);

  // Initialize the neighborhood
  IteratorType it = IteratorType(radius, image, image->GetBufferedRegion());

  // Compute the offset tables (we ignore all the zero indices
  // in the neighborhood)
  unsigned int iOffset = 0;
  int empty = VRadius;
  for(unsigned int iPos = 0; iPos < it.Size(); iPos++)
    {
    // Get the offset (index)
    typename IteratorType::OffsetType off = it.GetOffset(iPos);

    // Check if the offset has zero weights
    bool nonzero = true;
    for(dim = 0; dim < ImageDimension; dim++)
      {
      if(off[dim] == -empty) 
        {
        nonzero = false;
        break;
        }
      }

    // Only use offsets with non-zero indices
    if(nonzero)
      {
      // Set the offset index
      m_OffsetTable[iOffset] = iPos;

      // Set the weight table indices
      for(dim = 0; dim < ImageDimension; dim++)
        {
        m_WeightOffsetTable[iOffset][dim] = off[dim] + VRadius - 1;
        }

      // Increment the index
      iOffset++;
      }
    }
}

/**
 * PrintSelf
 */
template<class TInputImage, unsigned int VRadius,  
  class TWindowFunction, class TBoundaryCondition, class TCoordRep>
void
WindowedSincInterpolateImageFunction<TInputImage,VRadius,
  TWindowFunction,TBoundaryCondition,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 * Evaluate at image index position
 */
template<class TInputImage, unsigned int VRadius,  
  class TWindowFunction, class TBoundaryCondition, class TCoordRep>
typename WindowedSincInterpolateImageFunction<TInputImage,VRadius,
  TWindowFunction,TBoundaryCondition,TCoordRep>
::OutputType
WindowedSincInterpolateImageFunction<TInputImage,VRadius,
  TWindowFunction,TBoundaryCondition,TCoordRep>
::EvaluateAtContinuousIndex(
  const ContinuousIndexType& index) const
{
  unsigned int dim;
  Index<ImageDimension> baseIndex;
  double distance[ImageDimension];

  // Compute the integer index based on the continuous one by 
  // 'flooring' the index
  for( dim = 0; dim < ImageDimension; dim++ )
    {
    // The following "if" block is equivalent to the following line without
    // having to call floor.
    //    baseIndex[dim] = (long) floor( index[dim] );
    if (index[dim] >= 0.0)
      {
      baseIndex[dim] = (long) index[dim];
      }
    else
      {
      long tIndex = (long) index[dim];
      if (double(tIndex) != index[dim])
        {
        tIndex--;
        }
      baseIndex[dim] = tIndex;
      }
    
    distance[dim] = index[dim] - double( baseIndex[dim] );
    }
  
  // cout << "Sampling at index " << index << " discrete " << baseIndex << endl;

  // Position the neighborhood at the index of interest
  Size<ImageDimension> radius;
  radius.Fill(VRadius);
  IteratorType nit = IteratorType( radius, this->GetInputImage(), this->GetInputImage()->GetBufferedRegion());
  nit.SetLocation( baseIndex );
  
  // Compute the sinc function for each dimension
  double xWeight[ImageDimension][2 * VRadius];
  for( dim = 0; dim < ImageDimension; dim++ )
    {
    // x is the offset, hence the parameter of the kernel
    double x = distance[dim] + VRadius;

    // If distance is zero, i.e. the index falls precisely on the
    // pixel boundary, the weights form a delta function.
    if(distance[dim] == 0.0)
      {
      for( unsigned int i = 0; i < m_WindowSize; i++)
        {
        xWeight[dim][i] = 
          static_cast<int>(i) == VRadius - 1 ? 1 : 0;
        }
      }
    else
      {
      // i is the relative offset in dimension dim.
      for( unsigned int i = 0; i < m_WindowSize; i++)
        {
        // Increment the offset, taking it through the range
        // (dist + rad - 1, ..., dist - rad), i.e. all x
        // such that abs(x) <= rad
        x -= 1.0;

        // Compute the weight for this m
        xWeight[dim][i] = m_WindowFunction(x) * Sinc(x);
        }
      }
    }

  // Iterate over the neighborhood, taking the correct set
  // of weights in each dimension 
  double xPixelValue = 0.0;
  for(unsigned int j = 0; j < m_OffsetTableSize; j++)
    {
    // Get the offset for this neighbor
    unsigned int off = m_OffsetTable[j];
    
    // Get the intensity value at the pixel
    double xVal = nit.GetPixel(off);

    // Multiply the intensity by each of the weights. Gotta hope
    // that the compiler will unwrap this loop and pipeline this!
    for(dim = 0; dim < ImageDimension; dim++)
      {
      xVal *= xWeight[ dim ][ m_WeightOffsetTable[j][dim] ];
      }

    // Increment the pixel value
    xPixelValue += xVal;
    }
  
  // Return the interpolated value
  return static_cast<OutputType>(xPixelValue);
}

} // namespace itk

#endif
