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

#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkSimplexMeshToTriangleMeshFilter.h"
#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkImageFileWriter.h"

int itkTriangleMeshToBinaryImageFilterTest2( int argc, char * argv [] )
{

  // Declare the type of the input and output mesh
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double>
                                                        TriangleMeshTraits;
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double>
                                                        SimplexMeshTraits;
  typedef itk::Mesh<double,3,TriangleMeshTraits>        TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;

  typedef itk::Image<unsigned char, 3> ImageType;
  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType> SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType                PointType;
  typedef SphereMeshSourceType::VectorType               VectorType;

  // Declare the type of the gradient image
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>  SimplexFilterType;

  typedef itk::SimplexMeshToTriangleMeshFilter<SimplexMeshType,TriangleMeshType>  TriangleFilterType;
  typedef TriangleMeshType::Pointer                                               TriangleMeshPointer;
  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center; center.Fill(50);
  PointType::ValueType scaleInit[3] = {10,10,10};
  VectorType scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(3);
  mySphereMeshSource->SetScale(scale);

  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );

  TriangleFilterType::Pointer backFilter = TriangleFilterType::New();
  backFilter->SetInput( simplexFilter->GetOutput() );
  backFilter->Update();

  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  TriangleMeshPointer originalTriangleMesh = mySphereMeshSource->GetOutput();

  std::cout << " Number of Points and Cells in Original Triangle Mesh" << std::endl;
  std::cout << originalTriangleMesh->GetNumberOfPoints() << std::endl;
  std::cout << originalTriangleMesh->GetNumberOfCells() << std::endl;
  std::cout << "Original triangle mesh: " << std::endl;
  std::cout << originalTriangleMesh << std::endl;


  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;
  TriangleMeshType::Pointer triangleMesh = backFilter->GetOutput();

   std::cout << " Number of Points and Cells in Back Filtered Triangle Mesh" << std::endl;
   std::cout << triangleMesh->GetNumberOfPoints() << std::endl;
   std::cout << triangleMesh->GetNumberOfCells() << std::endl;

   std::cout << "Back filtered Triangle Mesh: " << triangleMesh << std::endl;


  triangleMesh->DisconnectPipeline();

  typedef  itk::TriangleMeshToBinaryImageFilter<TriangleMeshType, ImageType>  TriangleImageType;

  TriangleImageType::Pointer imageFilter = TriangleImageType::New();

  imageFilter->SetInput(triangleMesh);

  ImageType::SizeType size;

  size[0]=100;
  size[1]=100;
  size[2]=100;
  imageFilter->SetSize(size);

  std::cout << "[PASSED]" << std::endl;

  // Testing PrintSelf
  std::cout << imageFilter <<std::endl;

  //Update the filter
  imageFilter->Update();

  if( argc > 1 )
    {
    typedef itk::ImageFileWriter<ImageType > WriterType;

    WriterType::Pointer ImageWriter = WriterType::New();
    ImageWriter->SetInput(imageFilter->GetOutput() );
    ImageWriter->SetFileName( argv[1] );
    ImageWriter->Update();
    }

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}
