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
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Standard part of all itk objects.
   */
  itkTypeMacro(MeshRegion, Region);

  /** 
   * Return the region type. Meshes are described with unstructured regions.
   */
  virtual int GetRegionType()
    {return Superclass::ITK_UNSTRUCTURED_REGION;}

  /** 
   * Get the number of regions.
   */
  itkGetMacro(NumberOfRegions,unsigned long);

  /** 
   * Set the number of regions.
   */
  itkSetClampMacro(NumberOfRegions,unsigned long,
		   1, NumericTraits<unsigned long>::max());

  /** 
   * Get the current region.
   */
  itkGetMacro(Region,unsigned long);

  /** 
   * Set the number of regions.
   */
  itkSetClampMacro(Region,unsigned long,
		   0, NumericTraits<unsigned long>::max());

protected:
  MeshRegion(); 
  virtual ~MeshRegion(); 
  MeshRegion(const Self&) {}
  void operator=(const Self&) {}

  virtual void PrintSelf(std::ostream& os, Indent indent);

private:
  // The maximum number of regions possible.
  unsigned long int    m_NumberOfRegions;

  // The specified region.
  unsigned long int    m_Region;

};
  
} // end namespace itk

#endif

