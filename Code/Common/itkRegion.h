/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * Region is a light-weight object and not reference counted. This 
 * means that is behaves differently than ITK classes that are
 * reference counted. For example, smart pointer access is not
 * provided, and the (subclasses') constructor, destructor, 
 * copy constructor and operator= are all public.
 *
 * \sa ImageRegion
 * \sa MeshRegion
 * \ingroup DataRepresentation
 */
class ITKCommon_EXPORT Region
{
public:
  /** Standard class typedefs. */
  typedef Region              Self;

  /** Enums used to describe the extent types. */
  enum RegionType {ITK_UNSTRUCTURED_REGION,ITK_STRUCTURED_REGION};

  /** Standard part of all itk objects. */
  itkTypeMacro(Region, None);

  /** Subclasses must return a region type describing whether the region
   * is structured or unstructured. */
  virtual RegionType GetRegionType() const = 0;

  /** Print the region. */
  virtual void Print(std::ostream& os) const;
  
protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  virtual void PrintHeader(std::ostream& os, Indent indent) const;
  virtual void PrintTrailer(std::ostream& os, Indent indent) const;
  
private:

};
  
} // end namespace itk

#endif

