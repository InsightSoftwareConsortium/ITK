/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

