/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkNeighborhoodAlgorithm_h
#define __itkNeighborhoodAlgorithm_h

#include "itkNeighborhoodOperator.h"
#include "itkNeighborhoodOperatorList.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkImage.h"

namespace itk
{

namespace NeighborhoodAlgorithm
{

/**
 *
 */
template < class TPixel, unsigned long VDimension>
void
DoInnerProductOverRegion(Image<TPixel, VDimension> *,
                         Image<TPixel, VDimension> *,
                         Neighborhood<TPixel, VDimension> &);

/**
 *
 */  
template < class TPixel, unsigned long VDimension>
void
DoInnerProductOverRegion(Image<TPixel, VDimension> *,
                         Image<TPixel, VDimension> *,
                         NeighborhoodOperatorList<TPixel, VDimension> &);

} // end namespace NeighborhoodAlgorithm
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodAlgorithm.txx"
#endif

#endif


