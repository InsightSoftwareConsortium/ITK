/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkRegion_h
#define itkRegion_h

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
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT Region
{
public:
  /** Standard class typedefs. */
  typedef Region Self;

  /** Enums used to describe the extent types. */
  enum RegionType { ITK_UNSTRUCTURED_REGION, ITK_STRUCTURED_REGION };

  /** Standard part of all itk objects. */
  itkTypeMacroNoParent(Region);

  /** Subclasses must return a region type describing whether the region
   * is structured or unstructured. */
  virtual RegionType GetRegionType() const = 0;

  /** Print the region. */
  virtual void Print(std::ostream & os, Indent indent = 0) const;

  Region() {}
  virtual ~Region() {}

protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  virtual void PrintHeader(std::ostream & os, Indent indent) const;

  virtual void PrintTrailer(std::ostream & os, Indent indent) const;

private:
};
} // end namespace itk

#endif
