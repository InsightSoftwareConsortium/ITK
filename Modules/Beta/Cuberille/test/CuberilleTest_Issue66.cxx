/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <itkImage.h>
#include <itkQuadEdgeMesh.h>
#include <itkCuberilleImageToMeshFilter.h>
#include <itkMeshFileWriter.h>
#include <itkImageFileWriter.h>

const unsigned int Dimension = 3;
using TPixel = unsigned char;
using TImage = itk::Image<TPixel, Dimension>;
// using TMesh = itk::Mesh<double, 3>;
using TMesh = itk::QuadEdgeMesh<double, 3>;
using TExtract = itk::CuberilleImageToMeshFilter<TImage, TMesh>;
using TMeshWriter = itk::MeshFileWriter<TMesh>;

/* This is description from https://github.com/InsightSoftwareConsortium/ITKCuberille/issues/66

Imagine an image with only two foreground pixels, offset by {1, 1, 0}. When extracting the surface of this image as a
mesh, it is necessary to duplicate the coincident edge, creating two disconnected cubes, in order to ensure that the
result is manifold. A slice through the z plane would show:

*---*---*
| A |   |
*---*---*
|   | B |
*---*---*
Now imagine that these foreground pixels are still flanked by background pixels in the z plain, but are "connected" into
the same connected component by foreground pixels in the two adjacent z plains:

   Z-1          Z          Z+1
*---*---*   *---*---*   *---*---*
| X | X |   | A |   |   | X | X |
*---*---*   *---*---*   *---*---*
|   | X |   |   | B |   |   | X |
*---*---*   *---*---*   *---*---*
Here, the coincident edge of pixels A and B must again be duplicated; but unlike the first example, the two distinct
edges actually connect the same two vertices. Quite reasonably, when asked to create a face which would require an edge
between two vertices, itk::QuadEdgeMesh checks:
https://github.com/InsightSoftwareConsortium/ITK/blob/v5.4.5/Modules/Core/QuadEdgeMesh/include/itkQuadEdgeMesh.hxx#L1212-L1221
to see whether there is an existing edge, and refuses to create a new
edge if one already exists.

Most of the time, this is what we want--but in the case described above, it acts as a bug. It's not immediately obvious
to me how to detect when this check is and is not necessary; as I work through this problem, I thought I would post it
as an issue in case anyone else had any insight--pun intended. ;-D

*/

int
CuberilleTest_Issue66(int argc, char * argv[])
{
  const auto         image = TImage::New();
  TImage::RegionType region({ { 0, 0, 0 } }, { { 5, 4, 4 } });
  image->SetBufferedRegion(region);
  image->Allocate();
  image->FillBuffer(0);

  image->SetPixel({ { 1, 1, 1 } }, 1); // A
  image->SetPixel({ { 2, 1, 1 } }, 1); // B

  image->SetPixel({ { 3, 1, 1 } }, 1); // C
  image->SetPixel({ { 1, 2, 1 } }, 1); // D
  image->SetPixel({ { 3, 2, 1 } }, 1); // E

  image->SetPixel({ { 1, 2, 2 } }, 1); // F
  image->SetPixel({ { 2, 2, 2 } }, 1); // G
  image->SetPixel({ { 3, 2, 2 } }, 1); // H

  itk::WriteImage(image, "input_issue66.nrrd");

  const auto extract = TExtract::New();
  extract->SetInput(image);
  extract->SavePixelAsCellDataOn();
  extract->GenerateTriangleFacesOff();
  extract->ProjectVerticesToIsoSurfaceOff();

  const auto m_writer = TMeshWriter::New();
  m_writer->SetInput(extract->GetOutput());
  m_writer->SetFileName("mesh.obj");
  m_writer->Update();

  const auto n_cell = extract->GetOutput()->GetNumberOfCells();
  const auto n_data = extract->GetOutput()->GetCellData()->Size();

  if (n_cell != n_data)
  {
    std::cout << "This fails if using itk::QuadEdgeMesh." << std::endl;
    std::cout << "n_cell: " << n_cell << std::endl;
    std::cout << "n_data: " << n_data << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "This succeeds if using itk::Mesh." << std::endl;

  return EXIT_SUCCESS;
}
