/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshRegion.h
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
 *
 * \ingroup MeshObjects
 */
class ITKCommon_EXPORT MeshRegion: public Region
{
public:
  /** Standard class typedefs. */
  typedef MeshRegion              Self;
  typedef Region  Superclass;
  
  /** Standard part of all itk objects. */
  itkTypeMacro(MeshRegion, Region);

  /** Constructor.  MeshRegion is a lightweight object and is not reference
   * counted. */
  MeshRegion();

  /** Destructor.  MeshRegion is a lightweight object and is not reference
   * counted. */
  virtual ~MeshRegion();

  /** Return the region type. Meshes are described with unstructured regions. */
  virtual RegionType GetRegionType() const
    {return Superclass::ITK_UNSTRUCTURED_REGION;}

  /** Get the number of regions. */
  unsigned long GetNumberOfRegions() const
    { return m_NumberOfRegions; };

  /** Set the number of regions. */
  void SetNumberOfRegions(unsigned long num)
    { if ((num >= 1) && (num <= NumericTraits<unsigned long>::max()))
      { m_NumberOfRegions = num; } };

  /** Get the current region. */
  unsigned long GetRegion() const
    { return m_Region; };

  /** Set the number of regions. */
  void SetRegion(unsigned long region)
    { if ((region >= 1) && (region <= NumericTraits<unsigned long>::max()))
      { m_Region = region; } };

private:
  // The maximum number of regions possible.
  unsigned long int    m_NumberOfRegions;

  // The specified region.
  unsigned long int    m_Region;

};
  
} // end namespace itk

#endif

