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
                                       >  MeshTraits;
                                 
typedef itk::Mesh< MeshTraits::PixelType,
                   MeshTraits::PointDimension,
                   MeshTraits
                   >  MeshType;
                   

typedef MeshType::CellTraits  CellTraits;


/**
 * The type of point stored in the mesh. Because mesh was instantiated
 * with defaults (itkDefaultDynamicMeshTraits), the point dimension is 3 and
 * the coordinate representation is float.
 */
typedef MeshType::PointType             PointType;
typedef PointType::VectorType           VectorType;

typedef MeshType::Pointer               MeshPointer;
typedef MeshType::ConstPointer          MeshConstPointer;

typedef MeshType::PointType             PointType;

typedef MeshType::PointsContainer       PointsContainer;
typedef MeshType::PointDataContainer    PointDataContainer;

typedef PointsContainer::Iterator       PointsIterator;
typedef PointDataContainer::Iterator    CellsIterator;



int main(void)
{
  
  /**
   * Create the mesh through its object factory.
   */
  MeshType::Pointer mesh(MeshType::New());  

  PointType pointA;
  PointType pointB;
  PointType pointC;
  PointType pointD;

  VectorType displacement;

  displacement[0] = 2;
  displacement[1] = 5;

  pointA.Fill( 0.0 );
  pointB = pointA + displacement;
  pointC = pointB + displacement;
  pointD = pointC + displacement;
  
  PointsContainer::Pointer pointsContainter = mesh->GetPoints();

  pointsContainter->SetElement( 0, pointA );
  pointsContainter->SetElement( 1, pointB );
  pointsContainter->SetElement( 2, pointC );
  pointsContainter->SetElement( 3, pointD );


  std::cout << "Number of Points = " << mesh->GetNumberOfPoints() << std::endl;

  PointsIterator point    = pointsContainter->Begin();
  PointsIterator endpoint = pointsContainter->End();

  while( point != endpoint )
    {
    std::cout << point.Index() << " = " << point.Value() << std::endl;
    point++;
    }


  return 0;  

}

