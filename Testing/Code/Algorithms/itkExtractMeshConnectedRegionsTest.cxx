/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkExtractMeshConnectedRegionsTest.cxx
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
#include "itkConnectedRegionsMeshFilter.h"

#include <iostream>
#include <string>

/**
 * Some typedefs to make things easier.
 */

// A mesh with no pixel data.
typedef itk::Mesh<int>  Mesh;
typedef itk::ConnectedRegionsMeshFilter<Mesh,Mesh> Connect;
typedef itk::Point<float,3> Point;

/*
 * Test the mesh connectivity class.
 */
int main(void)
{
  // Define a simple mesh of three connected pieces. The mesh consists
  // of several different cell types.
  //
  Mesh::Pointer inMesh(Mesh::New());
  Mesh::Pointer outMesh(Mesh::New());
  
  // Pass the mesh through the filter in a variety of ways.
  //
  Point p; p = 1,2,3;
  Connect::Pointer connect(Connect::New());
  connect->SetInput(inMesh);
  connect->SetClosestPoint(p);
  connect->AddSeed(0);
  connect->InitializeSeedList();
  connect->AddSeed(1);
  connect->AddSeed(2);
  connect->DeleteSeed(1);

  return 0;  
}

