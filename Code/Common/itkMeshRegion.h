/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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

