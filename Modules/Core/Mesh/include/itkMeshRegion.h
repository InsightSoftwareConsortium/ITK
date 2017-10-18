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
#ifndef itkMeshRegion_h
#define itkMeshRegion_h

#include "itkRegion.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"
#include "itkIntTypes.h"
#include "ITKMeshExport.h"

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
 * \ingroup ITKMesh
 */
class ITKMesh_EXPORT MeshRegion:public Region
{
public:
  /** Standard class typedefs. */
  typedef MeshRegion Self;
  typedef Region     Superclass;

  /** Standard part of all itk objects. */
  itkTypeMacro(MeshRegion, Region);

  /** Constructor.  MeshRegion is a lightweight object and is not reference
   * counted. */
  MeshRegion();

  /** Destructor.  MeshRegion is a lightweight object and is not reference
   * counted. */
  virtual ~MeshRegion() ITK_OVERRIDE;

  /** Return the region type. Meshes are described with unstructured regions. */
  virtual RegionType GetRegionType() const ITK_OVERRIDE
  { return Superclass::ITK_UNSTRUCTURED_REGION; }

  /** Get the number of regions. */
  SizeValueType GetNumberOfRegions() const
  { return m_NumberOfRegions; }

  /** Set the number of regions. */
  void SetNumberOfRegions(SizeValueType num)
  {
    if ( ( num >= 1 ) && ( num <= NumericTraits< SizeValueType >::max() ) )
              { m_NumberOfRegions = num; } }

  /** Get the current region. */
  SizeValueType GetRegion() const
  { return m_Region; }

  /** Set the number of regions. */
  void SetRegion(SizeValueType region)
  {
    if ( ( region >= 1 ) && ( region <= NumericTraits< SizeValueType >::max() ) )
              { m_Region = region; } }

private:
  // The maximum number of regions possible.
  SizeValueType m_NumberOfRegions;

  // The specified region.
  SizeValueType m_Region;
};
} // end namespace itk

#endif
