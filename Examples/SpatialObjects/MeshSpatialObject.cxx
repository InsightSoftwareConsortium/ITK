/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkDefaultDynamicMeshTraits.h"
#include "itkTetrahedronCell.h"

// Software Guide : BeginLatex
//
// \index{itk::MeshSpatialObject}
//
// A \doxygen{MeshSpatialObject} contains a pointer to an \doxygen{Mesh} but
// adds the notion of spatial transformations and parent-child hierarchy. This
// example shows how to create an \doxygen{MeshSpatialObject}, use it to form
// a binary image, and write the mesh to disk.
//
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSpatialObjectToImageFilter.h"
#include "itkMeshSpatialObject.h"
#include "itkSpatialObjectReader.h"
#include "itkSpatialObjectWriter.h"
// Software Guide : EndCodeSnippet

int
main(int, char *[])
{
  // Software Guide : BeginLatex
  //
  // The \code{MeshSpatialObject} wraps an \doxygen{Mesh}, therefore we first
  // create a mesh.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using MeshTrait = itk::DefaultDynamicMeshTraits<float, 3, 3>;
  using MeshType = itk::Mesh<float, 3, MeshTrait>;
  using CellTraits = MeshType::CellTraits;
  using CellInterfaceType = itk::CellInterface<float, CellTraits>;
  using TetraCellType = itk::TetrahedronCell<CellInterfaceType>;
  using PointType = MeshType::PointType;
  using CellType = MeshType::CellType;
  using CellAutoPointer = CellType::CellAutoPointer;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  MeshType::Pointer myMesh = MeshType::New();

  MeshType::CoordRepType testPointCoords[4][3] = {
    { 0, 0, 0 }, { 9, 0, 0 }, { 9, 9, 0 }, { 0, 0, 9 }
  };

  MeshType::PointIdentifier tetraPoints[4] = { 0, 1, 2, 4 };

  int i;
  for (i = 0; i < 4; ++i)
  {
    myMesh->SetPoint(i, PointType(testPointCoords[i]));
  }

  myMesh->SetCellsAllocationMethod(
    itk::MeshEnums::MeshClassCellsAllocationMethod::
      CellsAllocatedDynamicallyCellByCell);
  CellAutoPointer testCell1;
  testCell1.TakeOwnership(new TetraCellType);
  testCell1->SetPointIds(tetraPoints);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  myMesh->SetCell(0, testCell1);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We then create a \code{MeshSpatialObject} which is templated over the
  // type of mesh previously defined...
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using MeshSpatialObjectType = itk::MeshSpatialObject<MeshType>;
  MeshSpatialObjectType::Pointer myMeshSpatialObject =
    MeshSpatialObjectType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // ... and pass the Mesh pointer to the \code{MeshSpatialObject}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  myMeshSpatialObject->SetMesh(myMesh);
  myMeshSpatialObject->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The actual pointer to the passed mesh can be retrieved using the
  // \code{GetMesh()} function, just like any other SpatialObjects.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  myMeshSpatialObject->GetMesh();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{GetBoundingBoxInWorldSpace()}, \code{ValueAtInWorldSpace()},
  // \code{IsInsideInWorldSpace()}, and related functions in ObjectSpace
  // can be used to access important information.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout
    << "Mesh bounds : "
    << myMeshSpatialObject->GetMyBoundingBoxInWorldSpace()->GetBounds()
    << std::endl;
  MeshSpatialObjectType::PointType myPhysicalPoint;
  myPhysicalPoint.Fill(1);
  std::cout << "Is my physical point inside? : "
            << myMeshSpatialObject->IsInsideInWorldSpace(myPhysicalPoint)
            << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Now that we have defined the \code{MeshSpatialObject}, we can save the
  // actual mesh using the \doxygen{SpatialObjectWriter}. In order to do so,
  // we need to specify the type of Mesh we are writing.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using WriterType = itk::SpatialObjectWriter<3, float, MeshTrait>;
  WriterType::Pointer writer = WriterType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then we set the mesh spatial object and the name of the file and call the
  // the \code{Update()} function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  writer->SetInput(myMeshSpatialObject);
  writer->SetFileName("myMesh.meta");
  writer->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Reading the saved mesh is done using the \doxygen{SpatialObjectReader}.
  // Once again we need to specify the type of mesh we intend to read.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using ReaderType = itk::SpatialObjectReader<3, float, MeshTrait>;
  ReaderType::Pointer reader = ReaderType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We set the name of the file we want to read and call update
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->SetFileName("myMesh.meta");
  reader->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Next, we show how to create a binary image of a \code{MeshSpatialObject}
  // using the \doxygen{SpatialObjectToImageFilter}. The resulting image
  // will have ones inside and zeros outside the mesh.
  // First we define and instantiate the SpatialObjectToImageFilter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using ImageType = itk::Image<unsigned char, 3>;
  using GroupType = itk::GroupSpatialObject<3>;
  using SpatialObjectToImageFilterType =
    itk::SpatialObjectToImageFilter<GroupType, ImageType>;
  SpatialObjectToImageFilterType::Pointer imageFilter =
    SpatialObjectToImageFilterType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Then we pass the output of the reader, i.e the \code{MeshSpatialObject},
  // to the filter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  imageFilter->SetInput(reader->GetGroup());
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally we trigger the execution of the filter by calling the
  //  \code{Update()} method. Note that depending on the size of the mesh,
  //  the computation time can increase significantly.
  //  \index{itk::SpatialObjectToImageFilter!Update()}
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  imageFilter->Update();
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then we can get the resulting binary image using the \code{GetOutput()}
  //  function.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  ImageType::Pointer myBinaryMeshImage = imageFilter->GetOutput();
  //  Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
