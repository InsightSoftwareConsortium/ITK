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

#include "itkFEMRegistrationFilter.h"


/* Example of FEM-base deformable registration in 3D */


const unsigned int Dimension = 3;
typedef itk::Image<unsigned char, Dimension>            FileImageType;
typedef itk::Image<float, Dimension>                    ImageType;

typedef itk::fem::Element3DC0LinearHexahedronMembrane   ElementType;
typedef itk::fem::Element3DC0LinearTetrahedronMembrane  ElementType2;
typedef itk::fem::FEMObject<Dimension>                  FEMObjectType;

typedef itk::fem::FEMRegistrationFilter<ImageType,ImageType,FEMObjectType> RegistrationType;


int main(int argc, char *argv[])
{
  const char *fixedImageName, *movingImageName;
  if ( argc < 2 )
  {
    std::cout << "Image file names missing" << std::endl;
    std::cout << "Usage: " << argv[0] << " fixedImageFile movingImageFile" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    fixedImageName = argv[1];
    movingImageName = argv[2];
  }

  // Setup registration parameters
  RegistrationType::Pointer registrationFilter = RegistrationType::New();
  registrationFilter->SetMaxLevel(1);
  registrationFilter->SetUseNormalizedGradient( true );
  registrationFilter->ChooseMetric( 0 );

  unsigned int maxiters = 20;
  float        E = 10;
  float        p = 1;
  registrationFilter->SetElasticity(E, 0);
  registrationFilter->SetRho(p, 0);
  registrationFilter->SetGamma(1., 0);
  registrationFilter->SetAlpha(1.);
  registrationFilter->SetMaximumIterations( maxiters, 0 );
  registrationFilter->SetMeshPixelsPerElementAtEachResolution(4, 0);
  registrationFilter->SetWidthOfMetricRegion(1, 0);
  registrationFilter->SetNumberOfIntegrationPoints(2, 0);
  registrationFilter->SetDescentDirectionMinimize();
  registrationFilter->SetDoLineSearchOnImageEnergy( 0 );
  registrationFilter->SetTimeStep(1.);
  registrationFilter->SetEmployRegridding(false);
  registrationFilter->SetUseLandmarks(false);

  // Read the image files
  typedef itk::ImageFileReader< FileImageType > FileSourceType;
  FileSourceType::Pointer movingfilter = FileSourceType::New();
  movingfilter->SetFileName( movingImageName );
  FileSourceType::Pointer fixedfilter = FileSourceType::New();
  fixedfilter->SetFileName( fixedImageName );

  std::cout << " reading moving ";
  std::cout << movingImageName << std::endl;
  std::cout << " reading fixed ";
  std::cout << fixedImageName << std::endl;


  try
  {
    movingfilter->Update();
  }
  catch( itk::ExceptionObject & e )
  {
    std::cerr << "Exception caught during reference file reading ";
    std::cerr << std::endl << e << std::endl;
    return EXIT_FAILURE;
  }
  try
  {
    fixedfilter->Update();
  }
  catch( itk::ExceptionObject & e )
  {
    std::cerr << "Exception caught during target file reading ";
    std::cerr << std::endl << e << std::endl;
    return EXIT_FAILURE;
  }


  // Rescale the image intensities so that they fall between 0 and 255
  typedef itk::RescaleIntensityImageFilter<
                        FileImageType, ImageType > FilterType;

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

  registrationFilter->SetFixedImage(fixedrescalefilter->GetOutput());
  registrationFilter->SetMovingImage(IntensityEqualizeFilter->GetOutput());


  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();

  writer->SetFileName("fixed.mhd");
  writer->SetInput(registrationFilter->GetFixedImage() );
  writer->Write();

  itk::ImageFileWriter<ImageType>::Pointer writer2;
  writer2 =  itk::ImageFileWriter<ImageType>::New();
  writer2->SetFileName("moving.mhd");
  writer2->SetInput(registrationFilter->GetMovingImage() );
  writer2->Write();


  // Create the material properties
  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->SetGlobalNumber(0);
  m->SetYoungsModulus(registrationFilter->GetElasticity()); // Young's modulus used in the membrane
  m->SetCrossSectionalArea(1.0);                            // Cross-sectional area
  m->SetThickness(1.0);                                     // Thickness
  m->SetMomentOfInertia(1.0);                               // Moment of inertia
  m->SetPoissonsRatio(0.);                                  // Poisson's ratio -- DONT CHOOSE 1.0!!
  m->SetDensityHeatProduct(1.0);                            // Density-Heat capacity product

  // Create the element type
  ElementType::Pointer e1=ElementType::New();
  e1->SetMaterial(m.GetPointer());
  registrationFilter->SetElement(e1.GetPointer());
  registrationFilter->SetMaterial(m);

  // Run registration
  registrationFilter->RunRegistration();

  // Warp the moving image and write it to a file.
  writer->SetFileName("warpedMovingImage.mhd");
  writer->SetInput( registrationFilter->GetWarpedImage() );
  try
  {
    writer->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // output the displacement field
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

  return EXIT_SUCCESS;
}
