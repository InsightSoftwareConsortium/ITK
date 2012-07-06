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

#include <iostream>

#include "itkVTKTetrahedralMeshReader.h"
#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkPhysicsBasedNonRigidRegistrationMethod.h"
#include "itkWarpImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImageFileWriter.h"


int itkPhysicsBasedNonRigidRegistrationMethodTest(int argc, char *argv[] )
{

  const unsigned int ImageDimension = 3;

  if ( argc < 6)
    {
    std::cerr << "Four arguments are required: fixed, moving, mask, mesh, warped." << std::endl;
    return EXIT_FAILURE;
    }

  enum { FIXED_IMG = 1, MOVING_IMG, MASK_IMG, MESH, WARPED_IMG };

  // read fixed
  typedef short                                          InputPixelType;
  typedef itk::Image< InputPixelType,  ImageDimension >  InputImageType;
  typedef itk::ImageFileReader< InputImageType >         ImageReaderType;

  ImageReaderType::Pointer readerFixed = ImageReaderType::New();
  readerFixed->SetFileName( argv[FIXED_IMG] );

  // read moving
  ImageReaderType::Pointer readerMoving = ImageReaderType::New();
  readerMoving->SetFileName( argv[MOVING_IMG] );

  // read mask
  ImageReaderType::Pointer readerMask = ImageReaderType::New();
  readerMask->SetFileName( argv[MASK_IMG] );

  // read mesh
  typedef itk::Mesh< float, ImageDimension >          MeshType;
  typedef itk::VTKTetrahedralMeshReader< MeshType >   MeshReaderType;

  MeshReaderType::Pointer readerMesh = MeshReaderType::New();
  readerMesh->SetFileName( argv[MESH] );
  try
    {
    readerFixed->Update();
    readerMoving->Update();
    readerMask->Update();
    readerMesh->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Error while reading inputs: " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // main filter
  typedef itk::Image< itk::Vector< float, ImageDimension >, ImageDimension >  DeformationFieldType;
  typedef itk::fem::PhysicsBasedNonRigidRegistrationMethod<InputImageType, InputImageType, InputImageType, MeshType, DeformationFieldType> PBNRRFilterType;

  PBNRRFilterType::Pointer filter = PBNRRFilterType::New();
  filter->SetFixedImage( readerFixed->GetOutput() );
  filter->SetMovingImage( readerMoving->GetOutput() );
  filter->SetMaskImage( readerMask->GetOutput() );
  filter->SetMesh( readerMesh->GetOutput() );
  filter->SetSelectFraction( 0.05 );

  std::cout << "Filter: " << filter << std::endl;

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Error during filter->Update(): " << e << std::endl;
    return EXIT_FAILURE;
    }

  DeformationFieldType::Pointer deformationField = filter->GetOutput();

  // warp image
  typedef itk::WarpImageFilter< InputImageType, InputImageType, DeformationFieldType > WarpFilterType;
  WarpFilterType::Pointer warpFilter = WarpFilterType::New();

  typedef itk::LinearInterpolateImageFunction< InputImageType, double > InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  warpFilter->SetInterpolator( interpolator );

  warpFilter->SetInput( readerMoving->GetOutput() );
  warpFilter->SetOutputSpacing( deformationField->GetSpacing() );
  warpFilter->SetOutputOrigin( deformationField->GetOrigin() );
  warpFilter->SetDisplacementField( deformationField );

  try
    {
    warpFilter->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Error during filter->Update(): " << e << std::endl;
    return EXIT_FAILURE;
    }


  // write warped image to file
  typedef itk::ImageFileWriter< InputImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[WARPED_IMG] );
  writer->SetInput( warpFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Error during writer->Update(): " << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
