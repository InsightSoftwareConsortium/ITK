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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkRescaleIntensityImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"

//  Software Guide : BeginLatex
//
// The finite element (FEM) library within the Insight Toolkit can be
// used to solve deformable image registration problems.  The first step in
// implementing a FEM-based registration is to include the appropriate
// header files.
//
//  \index{Registration!Finite Element-Based}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkFEMRegistrationFilter.h"

// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Next, we use \code{typedef}s to instantiate all necessary classes.  We
//  define the image and element types we plan to use to solve a
//  two-dimensional registration problem.  We define multiple element
//  types so that they can be used without recompiling the code.
//
//  Software Guide : EndLatex


//  Software Guide : BeginCodeSnippet
typedef itk::Image<unsigned char, 2>                       DiskImageType;
typedef itk::Image<float, 2>                               ImageType;
typedef itk::fem::Element2DC0LinearQuadrilateralMembrane   ElementType;
typedef itk::fem::Element2DC0LinearTriangularMembrane      ElementType2;
typedef itk::fem::FEMObject<2>                             FEMObjectType;
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Note that in order to solve a three-dimensional registration
//  problem, we would simply define 3D image and element types in lieu
//  of those above.  The following declarations could be used for a 3D
//  problem:
//
//  SoftwareGuide : EndLatex


//  SoftwareGuide : BeginCodeSnippet
typedef itk::Image<unsigned char, 3>                    fileImage3DType;
typedef itk::Image<float, 3>                            Image3DType;
typedef itk::fem::Element3DC0LinearHexahedronMembrane   Element3DType;
typedef itk::fem::Element3DC0LinearTetrahedronMembrane  Element3DType2;
typedef itk::fem::FEMObject<3>                          FEMObject3DType;
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Once all the necessary components have been instantiated, we can
//  instantiate the \doxygen{FEMRegistrationFilter}, which depends on the
//  image input and output types.
//
//  Software Guide : EndLatex


//  Software Guide : BeginCodeSnippet
typedef itk::fem::FEMRegistrationFilter<ImageType,ImageType,FEMObjectType>
                                                             RegistrationType;
//  Software Guide : EndCodeSnippet


int main(int argc, char *argv[])
{
  const char *fixedImageName, *movingImageName;
  if ( argc < 2 )
  {
    std::cout << "Image file names missing" << std::endl;
    std::cout << "Usage: " << argv[0] << " fixedImageFile movingImageFile"
              << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    fixedImageName = argv[1];
    movingImageName = argv[2];
  }


//  Software Guide : BeginLatex
//
//  In order to begin the registration, we declare an instance of the
//  FEMRegistrationFilter and set its parameters.  For simplicity, we will call
//  it \code{registrationFilter}.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
  RegistrationType::Pointer registrationFilter = RegistrationType::New();
  registrationFilter->SetMaxLevel(1);
  registrationFilter->SetUseNormalizedGradient( true );
  registrationFilter->ChooseMetric( 0 );

  unsigned int maxiters = 20;
  float        E = 100;
  float        p = 1;
  registrationFilter->SetElasticity(E, 0);
  registrationFilter->SetRho(p, 0);
  registrationFilter->SetGamma(1., 0);
  registrationFilter->SetAlpha(1.);
  registrationFilter->SetMaximumIterations( maxiters, 0 );
  registrationFilter->SetMeshPixelsPerElementAtEachResolution(4, 0);
  registrationFilter->SetWidthOfMetricRegion(1, 0);
  registrationFilter->SetNumberOfIntegrationPoints(2, 0);
  registrationFilter->SetDoLineSearchOnImageEnergy( 0 );
  registrationFilter->SetTimeStep(1.);
  registrationFilter->SetEmployRegridding(false);
  registrationFilter->SetUseLandmarks(false);
//  Software Guide : EndCodeSnippet


  // Read the image files
  typedef itk::ImageFileReader< DiskImageType > FileSourceType;

  FileSourceType::Pointer movingfilter = FileSourceType::New();
  movingfilter->SetFileName( movingImageName );
  FileSourceType::Pointer fixedfilter = FileSourceType::New();
  fixedfilter->SetFileName( fixedImageName );
  std::cout << " reading moving " << movingImageName << std::endl;
  std::cout << " reading fixed " << fixedImageName << std::endl;


  try
  {
    movingfilter->Update();
  }
  catch( itk::ExceptionObject & e )
  {
    std::cerr << "Exception caught during reference file reading " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }
  try
  {
    fixedfilter->Update();
  }
  catch( itk::ExceptionObject & e )
  {
    std::cerr << "Exception caught during target file reading " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }


  // Rescale the image intensities so that they fall between 0 and 255
  typedef itk::RescaleIntensityImageFilter<DiskImageType,ImageType> FilterType;
  FilterType::Pointer movingrescalefilter = FilterType::New();
  FilterType::Pointer fixedrescalefilter = FilterType::New();

  movingrescalefilter->SetInput(movingfilter->GetOutput());
  fixedrescalefilter->SetInput(fixedfilter->GetOutput());

  const double desiredMinimum =  0.0;
  const double desiredMaximum =  255.0;

  movingrescalefilter->SetOutputMinimum( desiredMinimum );
  movingrescalefilter->SetOutputMaximum( desiredMaximum );
  movingrescalefilter->UpdateLargestPossibleRegion();
  fixedrescalefilter->SetOutputMinimum( desiredMinimum );
  fixedrescalefilter->SetOutputMaximum( desiredMaximum );
  fixedrescalefilter->UpdateLargestPossibleRegion();


  // Histogram match the images
  typedef itk::HistogramMatchingImageFilter<ImageType,ImageType> HEFilterType;
  HEFilterType::Pointer IntensityEqualizeFilter = HEFilterType::New();

  IntensityEqualizeFilter->SetReferenceImage( fixedrescalefilter->GetOutput() );
  IntensityEqualizeFilter->SetInput( movingrescalefilter->GetOutput() );
  IntensityEqualizeFilter->SetNumberOfHistogramLevels( 100);
  IntensityEqualizeFilter->SetNumberOfMatchPoints( 15);
  IntensityEqualizeFilter->ThresholdAtMeanIntensityOn();
  IntensityEqualizeFilter->Update();

  // Set the images for registration filter
  registrationFilter->SetFixedImage(fixedrescalefilter->GetOutput());
  registrationFilter->SetMovingImage(IntensityEqualizeFilter->GetOutput());


  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  std::string ofn="fixed.mha";
  writer->SetFileName(ofn.c_str());
  writer->SetInput(registrationFilter->GetFixedImage() );

  try
  {
    writer->Write();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  ofn="moving.mha";
  itk::ImageFileWriter<ImageType>::Pointer writer2;
  writer2 =  itk::ImageFileWriter<ImageType>::New();
  writer2->SetFileName(ofn.c_str());
  writer2->SetInput(registrationFilter->GetMovingImage() );

  try
  {
    writer2->Write();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

//  Software Guide : BeginLatex
//
//  In order to initialize the mesh of elements, we must first create
//  ``dummy'' material and element objects and assign them to the
//  registration filter.  These objects are subsequently used to
//  either read a predefined mesh from a file or generate a mesh using
//  the software.  The values assigned to the fields within the
//  material object are arbitrary since they will be replaced with
//  those specified earlier.  Similarly, the element
//  object will be replaced with those from the desired mesh.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
  // Create the material properties
  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(0);
  // Young's modulus of the membrane
  m->SetYoungsModulus(registrationFilter->GetElasticity());
  m->SetCrossSectionalArea(1.0);  // Cross-sectional area
  m->SetThickness(1.0);           // Thickness
  m->SetMomentOfInertia(1.0);     // Moment of inertia
  m->SetPoissonsRatio(0.);        // Poisson's ratio -- DONT CHOOSE 1.0!!
  m->SetDensityHeatProduct(1.0);  // Density-Heat capacity product

  // Create the element type
  ElementType::Pointer e1=ElementType::New();
  e1->SetMaterial(m.GetPointer());
  registrationFilter->SetElement(e1.GetPointer());
  registrationFilter->SetMaterial(m);
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Now we are ready to run the registration:
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
  registrationFilter->RunRegistration();
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  To output the image resulting from the registration, we can call
//  \code{GetWarpedImage()}.  The image is written in floating point
//  format.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
  itk::ImageFileWriter<ImageType>::Pointer warpedImageWriter;
  warpedImageWriter = itk::ImageFileWriter<ImageType>::New();
  warpedImageWriter->SetInput( registrationFilter->GetWarpedImage() );
  warpedImageWriter->SetFileName("warpedMovingImage.mha");
  try
  {
    warpedImageWriter->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
//  Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  We can also output the displacement field resulting from the
//  registration; we can call \code{GetDisplacementField()} to get the
//  multi-component image.
//
//  Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter<RegistrationType::FieldType> DispWriterType;
  DispWriterType::Pointer dispWriter = DispWriterType::New();
  dispWriter->SetInput( registrationFilter->GetDisplacementField() );
  dispWriter->SetFileName("displacement.mha");
  try
  {
    dispWriter->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
//  Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
