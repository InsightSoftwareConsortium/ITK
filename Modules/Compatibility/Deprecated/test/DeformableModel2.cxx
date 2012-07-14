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

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{DeformableMesh3DFilter}.
// An initial mesh is created using the \doxygen{SphereMeshSource} filter.
//
// \index{Deformable Models}
// \index{DeformableMesh3DFilter}
// \index{SphereMeshSource}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

#include "itkDeformableMesh3DFilter.h"

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"

#include "itkSphereMeshSource.h"

#include "itkImageFileReader.h"

int main(int argc, char * argv [] )
{

  if( argc < 7 )
    {
    std::cerr << "Usage: DeformableModel2 inputImage sigma " << std::endl;
    std::cerr << " numberOfIterations timeStep externalFoceScale stiffness" << std::endl;
    return 1;
    }
  const char *inputFileName      =       argv[1];
  const float sigma              = atof( argv[2] );  // Suggested value = 5 * pixel spacing
  const int   numberOfIterations = atoi( argv[3] );  // Suggested value = 100
  const float timeStep           = atof( argv[4] );  // Suggested value = 0.1
  const float externalForceScale = atof( argv[5] );   // Suggested value = 10  (linked to stiffness)
  const float stiffness          = atof( argv[6] );   // Suggested value = 0.1 (linked to force scale)

  typedef double                            MeshPixelType;
  typedef itk::Mesh< MeshPixelType >        MeshType;

  unsigned const int Dimension = 3;

  typedef   float                               PixelType;
  typedef itk::Image<PixelType, Dimension>      ImageType;

  typedef itk::GradientRecursiveGaussianImageFilter<
                                        ImageType
                                           > GradientFilterType;

  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<
                                        ImageType,ImageType>
                                                      GradientMagnitudeFilterType;

  typedef itk::DeformableMesh3DFilter<MeshType,MeshType>  DeformableFilterType;

  typedef itk::ImageFileReader< ImageType       >  ReaderType;

  ReaderType::Pointer       imageReader   =  ReaderType::New();

  imageReader->SetFileName( inputFileName );

  GradientMagnitudeFilterType::Pointer  gradientMagnitudeFilter
                      = GradientMagnitudeFilterType::New();

  try
    {
    imageReader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Problem found while reading file " << inputFileName << std::endl;
    std::cerr << "Exception Caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  ImageType::ConstPointer inputImage = imageReader->GetOutput();

  gradientMagnitudeFilter->SetInput( inputImage );

  gradientMagnitudeFilter->SetSigma( sigma );

  GradientFilterType::Pointer gradientMapFilter = GradientFilterType::New();

  gradientMapFilter->SetInput( gradientMagnitudeFilter->GetOutput());
  gradientMapFilter->SetSigma( sigma );

  gradientMapFilter->Update();

  DeformableFilterType::Pointer deformableModelFilter =
                                     DeformableFilterType::New();

  typedef itk::SphereMeshSource< MeshType >        MeshSourceType;

  MeshSourceType::Pointer meshSource = MeshSourceType::New();

  // Set the initial sphere in the center of the image
  //
  const ImageType::SpacingType spacing = inputImage->GetSpacing();
  ImageType::PointType         origin  = inputImage->GetOrigin();
  ImageType::SizeType          size    = inputImage->GetBufferedRegion().GetSize();

  MeshType::PointType center;
  center[0] = origin[0] + spacing[0] * size[0] / 2.0;
  center[1] = origin[1] + spacing[1] * size[1] / 2.0;
  center[2] = origin[2] + spacing[2] * size[2] / 2.0;
  meshSource->SetCenter( center );

  MeshType::PointType radius;
  radius[0] = spacing[0] * size[0] / 4.0;
  radius[1] = spacing[1] * size[1] / 4.0;
  radius[2] = spacing[2] * size[2] / 4.0;
  meshSource->SetScale( radius );

  meshSource->SetResolutionX( 50 );
  meshSource->SetResolutionY( 50 );
  meshSource->Update();

  deformableModelFilter->SetInput(    meshSource->GetOutput()        );

  deformableModelFilter->SetGradient( gradientMapFilter->GetOutput() );

  typedef itk::CovariantVector<double, 2>           StiffnessType;

  StiffnessType stiffnessVector;
  stiffnessVector[0] = stiffness;
  stiffnessVector[1] = stiffness;

  deformableModelFilter->SetTimeStep( timeStep );
  deformableModelFilter->SetStiffness( stiffnessVector );
  deformableModelFilter->SetStepThreshold( numberOfIterations );
  deformableModelFilter->SetGradientMagnitude( externalForceScale );

  try
    {
    deformableModelFilter->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception Caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  return 0;
}

// Software Guide : EndCodeSnippet
