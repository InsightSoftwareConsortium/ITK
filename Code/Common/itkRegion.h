/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegion_h
#define __itkRegion_h

#include "itkObject.h"

namespace itk
{

/** \class Region
 * \brief A region represents some portion or piece of data.
 *
 * Region is an abstract class that represents some portion or
 * piece of a DataObject. A region is used by the pipeline when
 * processing just a portion of the data, either because 1) memory
 * limits dictate that the pipeline cannot fit the entire dataset
 * into memory; 2) the user has requested that only a piece of the
 * dataset is to be processed; or 3) parallel (multi-threaded)
 * processing of the data is required.
 *
 * There are two types of regions in itk: a structured region that
 * specifies a rectangular piece of an image (ImageRegion), and a
 * unstructured region that specifies piece i of N total pieces
 * (MeshRegion). Depending on the filter (its input and output
 * types, and its position in the pipeline), ImageRegion or MeshRegion
 * will be used to describe the region.
 *
 * \sa ImageRegion
 * \sa MeshRegion
 */
class ITK_EXPORT Region
{
public:
  /** 
   * Standard "Self" typedef.
   */
  typedef Region              Self;

  /**
   * Enums used to describe the extent types.
   */
  typedef enum {ITK_UNSTRUCTURED_REGION,ITK_STRUCTURED_REGION} RegionType;
  
  /**
   * Subclasses must return a region type describing whether the region
   * is structured or unstructured.
   */
  virtual int GetRegionType() const = 0;

protected:

private:

};
  
} // end namespace itk

#endif

