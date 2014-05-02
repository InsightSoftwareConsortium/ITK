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

/**
 *
 *  This program illustrates the use of rasterization algorithm
 *  using a sphere of center (50,50,50) and radius 10 and
 *  creates a binary image of size (100,100,100)
 *
 */

#include "itkRegularSphereMeshSource.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkImageFileWriter.h"
#include "itkTriangleMeshToBinaryImageFilter.h"

int itkTriangleMeshToBinaryImageFilterTest(int argc, char * argv [] )
{
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3> TriangleMeshTraits;
  typedef itk::Mesh<double,3, TriangleMeshTraits>     TriangleMeshType;

  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType                 PointType;
  typedef SphereMeshSourceType::VectorType                VectorType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center;
  center[0] = 50;
  center[1] = 50;
  center[2] = 50;
  PointType::ValueType scaleInit[3] = {10,10,10};
  VectorType scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(3);
  mySphereMeshSource->SetScale(scale);
  mySphereMeshSource->Update();

  std::cout << "Triangle mesh created. " << std::endl;
  std::cout << "with " << mySphereMeshSource->GetOutput()->GetNumberOfPoints() << " points";
  std::cout << "and " << mySphereMeshSource->GetOutput()->GetNumberOfCells() << " cells." << std::endl;

  std::cout << "Sending triangle mesh to rasterization algorithm. " << std::endl;
  typedef itk::Image<unsigned char,3> ImageType;

  typedef itk::TriangleMeshToBinaryImageFilter<TriangleMeshType,ImageType> TriangleMeshToBinaryImageFilterType;
  TriangleMeshToBinaryImageFilterType::Pointer imageFilter = TriangleMeshToBinaryImageFilterType::New();
  imageFilter->SetInput(mySphereMeshSource->GetOutput());
  ImageType::SizeType size;

  size[0]=100;
  size[1]=100;
  size[2]=100;
  imageFilter->SetSize(size);

  const double dspacing[3] = { 2.0, 2.0, 2.0 };
  const float fspacing[3] = { 3.0, 3.0, 3.0 };
  const double defaultSpacing[3] = { 1.0, 1.0, 1.0 };
  imageFilter->SetSpacing(dspacing);
  imageFilter->SetSpacing(fspacing);
  imageFilter->SetSpacing(defaultSpacing);

  const double dorigin[3] = { 2.0, 2.0, 2.0 };
  const float forigin[3] = { 3.0, 3.0, 3.0 };
  const double defaultOrigin[3] = { 0.0, 0.0, 0.0 };
  imageFilter->SetOrigin(dorigin);
  imageFilter->SetOrigin(forigin);
  imageFilter->SetOrigin(defaultOrigin);

  imageFilter->Update();

  ImageType::Pointer im = ImageType::New();
  ImageType::SizeType imSize; imSize[0] = imSize[1] = imSize[2] = 100;
  im->SetRegions(imSize);
  im->Allocate();

  imageFilter->SetInfoImage(im);
  imageFilter->Update();

  std::cout << "[PASSED]" << std::endl;


  if( argc > 1 )
    {
    typedef itk::ImageFileWriter<ImageType > WriterType;

    WriterType::Pointer ImageWriter = WriterType::New();
    ImageWriter->SetInput(imageFilter->GetOutput() );
    ImageWriter->SetFileName( argv[1] );
    ImageWriter->Update();
    }

  std::cout << "Test [DONE]" << std::endl;

  return EXIT_SUCCESS;
}
