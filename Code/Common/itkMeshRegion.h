/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshRegion.h
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
#ifndef __itkMeshRegion_h
#define __itkMeshRegion_h

#include "itkRegion.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"

namespace itk
{

/** \class MeshRegion
 * \brief A mesh region represents an unstructured region of data.
 *
 * MeshRegion is an class that represents some unstructured portion or
 * piece of a Mesh. The MeshRegion is described as piece i out of N 
 * total pieces.
 *
 * \sa Region
 * \sa ImageRegion
 */
class ITK_EXPORT MeshRegion: public Region
{
public:
  /** 
   * Standard "Self" typedef.
   */
  typedef MeshRegion              Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Region  Superclass;

  /** 
   * Standard part of all itk objects.
   */
  itkTypeMacro(MeshRegion, Region);

  /**
   * Constructor.  MeshRegion is a lightweight object and is not reference
   * counted.
   */
  MeshRegion();

  /**
   * Destructor.  MeshRegion is a lightweight object and is not reference
   * counted.
   */
  virtual ~MeshRegion();

  /** 
   * Return the region type. Meshes are described with unstructured regions.
   */
  virtual int GetRegionType() const
    {return Superclass::ITK_UNSTRUCTURED_REGION;}

  /** 
   * Get the number of regions.
   */
  unsigned long GetNumberOfRegions() const
  { return m_NumberOfRegions; };

  /** 
   * Set the number of regions.
   */
  void SetNumberOfRegions(unsigned long num)
  { if ((num >= 1) && (num <= NumericTraits<unsigned long>::max()))
    { m_NumberOfRegions = num; } };

  /** 
   * Get the current region.
   */
  unsigned long GetRegion() const
  { return m_Region; };

  /** 
   * Set the number of regions.
   */
  void SetRegion(unsigned long region)
  { if ((region >= 1) && (region <= NumericTraits<unsigned long>::max()))
    { m_Region = region; } };

protected:

private:
  // The maximum number of regions possible.
  unsigned long int    m_NumberOfRegions;

  // The specified region.
  unsigned long int    m_Region;

};
  
} // end namespace itk

#endif

