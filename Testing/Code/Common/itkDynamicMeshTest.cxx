/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkDynamicMeshTest.cxx
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
#include "itkMesh.h"
#include "itkTetrahedronCell.h"
#include "itkHexahedronCell.h"
#include "itkBoundingBox.h"
#include "itkDefaultDynamicMeshTraits.h"

#include <iostream>
#include <string>

/**
 * Some typedefs to make things easier.
 */

/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itk::DefaultDynamicMeshTraits< int,
                                       2,
                                       2,
                                       float,
                                       float
                                       >  DynamicTraits;
                                 
typedef itk::Mesh< DynamicTraits::PixelType,
                   DynamicTraits::PointDimension,
                   DynamicTraits
                   >  MeshType;
                   

typedef MeshType::CellTraits  CellTraits;


/**
 * The type of point stored in the mesh. Because mesh was instantiated
 * with defaults (itkDefaultStaticMeshTraits), the point dimension is 3 and
 * the coordinate representation is float.
 */
typedef MeshType::PointType  PointType;

typedef MeshType::Pointer        MeshPointer;
typedef MeshType::ConstPointer   MeshConstPointer;

typedef MeshType::PointType      PointType;


// typedef MeshType::PointsContainer::iterator           PointsIterator;
// typedef MeshType::PointDataContainer::iterator        CellsIterator;

typedef MeshType::PointsContainer::Iterator           PointsIterator;
typedef MeshType::PointDataContainer::Iterator        CellsIterator;



int main(void)
{
  
  /**
   * Create the mesh through its object factory.
   */
  MeshType::Pointer mesh(MeshType::New());  

  PointsIterator point    = mesh->GetPoints()->Begin();
  PointsIterator endpoint = mesh->GetPoints()->End();

  while( point != endpoint )
    {
//    point->second;
    point.Value();
    point++;
    }


  return 0;  

}

