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
//Algorithms
#include "itkImageFileReader.h"

#include "itkBalloonForceFilter.h"
#include "itkDeformableMesh3DFilter.h"

//BasicFilters
#include "itkGradientToMagnitudeImageFilter.h"

//IO
#include "itkAnalyzeImageIO.h"

int main (int , char* [])
{
  typedef itk::Image<float,2>          OutputType;

  typedef itk::Mesh<double>  MeshType;

  typedef itk::Vector<float,2>      VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;

  itk::BalloonForceFilter<MeshType,MeshType>::Pointer BalloonForceFilterObj =
    itk::BalloonForceFilter<MeshType,MeshType>::New();
  std:: cout << "-------------BalloonForceFilter " << BalloonForceFilterObj;

  itk::DeformableMesh3DFilter<MeshType,MeshType>::Pointer DeformableMesh3DFilterObj =
    itk::DeformableMesh3DFilter<MeshType,MeshType>::New();
  std:: cout << "-------------DeformableMesh3DFilter " << DeformableMesh3DFilterObj;

  itk::GradientToMagnitudeImageFilter<VectorImageType,OutputType>::Pointer GradientToMagnitudeImageFilterObj =
    itk::GradientToMagnitudeImageFilter<VectorImageType,OutputType>::New();
  std::cout << "-------------GradientToMagnitudeImageFilter" << GradientToMagnitudeImageFilterObj;

  typedef itk::Image<unsigned char,2> ImageType;
  itk::ImageFileReader<ImageType>::Pointer reader =
    itk::ImageFileReader<ImageType>::New();

  itk::AnalyzeImageIO::Pointer Analyzeio;
  Analyzeio = itk::AnalyzeImageIO::New();
  reader->SetImageIO(Analyzeio);
  std::cout << "---------------Analyze" << reader;


  return EXIT_SUCCESS;
}
