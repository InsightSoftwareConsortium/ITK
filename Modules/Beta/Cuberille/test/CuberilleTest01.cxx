/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    $RCSfile: CuberilleTest01.cxx,v $
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

   This software is distributed WITHOUT ANY WARRANTY; without even 
   the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
   PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#define _SCL_SECURE_NO_WARNINGS

#define USE_BSPLINE_INTERPOLATOR 0
#define USE_MARCHING_CUBES 0
#define USE_QUAD_EDGE_MESH 0
#define USE_DECIMATION 0

#include <iostream>
#include <sstream>

#ifndef NO_TESTING
#include "itkTestMain.h"
#endif

#include "itkTimeProbe.h"
#include "itkImage.h"
#include "itkMesh.h"
#include "itkQuadEdgeMesh.h"
#include "itkCuberilleImageToMeshFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVTKPolyDataWriter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkQuadEdgeMeshQuadricDecimation.h"
#include "itkQuadEdgeMeshDecimationCriteria.h"

#ifndef NO_TESTING
void RegisterTests()
{
REGISTER_TEST( Test01 );
}
#endif

int Test01(int argc, char * argv [])
{
try
  {
  // Typedefs
  const unsigned int Dimension = 3;
  typedef unsigned char PixelType;
  //typedef signed short PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;
#if USE_QUAD_EDGE_MESH | USE_DECIMATION
  typedef itk::QuadEdgeMesh< PixelType, Dimension > MeshType;
#else
  typedef itk::Mesh< PixelType, Dimension > MeshType;
#endif
  typedef itk::ImageFileReader< ImageType > ImageFileReaderType;
  typedef itk::VTKPolyDataWriter< MeshType > MeshFileWriterType;
#if USE_BSPLINE_INTERPOLATOR
  typedef itk::BSplineInterpolateImageFunction< ImageType, float, float > InterpolatorType;
#else
  typedef itk::LinearInterpolateImageFunction< ImageType > InterpolatorType;
#endif
  typedef itk::CuberilleImageToMeshFilter< ImageType, MeshType, InterpolatorType > CuberilleType;
  typedef itk::BinaryMask3DMeshSource< ImageType, MeshType > MarchingCubesType;
  typedef itk::BinaryThresholdImageFilter< ImageType, ImageType > BinaryThresholdFilterType;

  // Read command-line parameters
  if ( argc < 6 )
    {
      std::cout << "USAGE: " << argv[0];
      std::cout << "InputImage OutputMesh IsoSurfaceValue ExpectedNumberOfPoints ExpectedNumberOfCells";
      std::cout << "[GenerateTriangleFaces] [ProjectToIsoSurface] ";
      std::cout << "[SurfaceDistanceThreshold] [StepLength] [StepLengthRelax] [MaximumNumberOfSteps]";
      std::cout << std::endl;
      return EXIT_FAILURE;
    }
  int arg = 1;
  char * FilenameInputImage = argv[arg++];
  char * FilenameOutputMesh = argv[arg++];
  PixelType IsoSurfaceValue = atoi( argv[arg++] );
  unsigned int ExpectedNumberOfPoints = atoi( argv[arg++] );
  unsigned int ExpectedNumberOfCells = atoi( argv[arg++] );
  bool GenerateTriangleFaces = true;
  if (argc > arg) GenerateTriangleFaces = atoi( argv[arg++] );
  bool ProjectToIsoSurface = true;
  if (argc > arg) ProjectToIsoSurface = atoi( argv[arg++] );
  double SurfaceDistanceThreshold = 0.5;
  if (argc > arg) SurfaceDistanceThreshold = atof( argv[arg++] );
  double StepLength = 0.25;
  if (argc > arg) StepLength = atof( argv[arg++] );
  double StepLengthRelax = 0.95;
  if (argc > arg) StepLengthRelax = atof( argv[arg++] );
  unsigned int MaximumNumberOfSteps = 50;
  if (argc > arg) MaximumNumberOfSteps = atoi( argv[arg++] );

  // Read input image
  std::cout << "Reading input image: " << FilenameInputImage << std::endl;
  ImageFileReaderType::Pointer reader = ImageFileReaderType::New();
  reader->SetFileName( FilenameInputImage );
  reader->UpdateLargestPossibleRegion();
  ImageType::Pointer input = reader->GetOutput();
  input->DisconnectPipeline();

  // Create output mesh
  MeshType::Pointer outputMesh = NULL;
  itk::TimeProbe time;
#if USE_MARCHING_CUBES

  // Create marching cubes
  std::cout << "Creating marching cubes mesh..." << std::endl;
  BinaryThresholdFilterType::Pointer threshold = BinaryThresholdFilterType::New();
  threshold->SetInput( input );
  threshold->SetLowerThreshold( IsoSurfaceValue );
  threshold->SetUpperThreshold( itk::NumericTraits<PixelType>::max() );
  threshold->SetInsideValue( itk::NumericTraits<PixelType>::One );
  threshold->SetOutsideValue( itk::NumericTraits<PixelType>::Zero );
  threshold->UpdateLargestPossibleRegion();
  MarchingCubesType::Pointer marching = MarchingCubesType::New();
  marching->SetInput( threshold->GetOutput() );
  time.Start();
  marching->Update();
  time.Stop();
  outputMesh = marching->GetOutput();
  outputMesh->DisconnectPipeline();
#else

  // Create cuberille mesh filter
  std::cout << "Creating cuberille mesh..." << std::endl;
  CuberilleType::Pointer cuberille = CuberilleType::New();
  cuberille->SetInput( input );
  cuberille->SetIsoSurfaceValue( IsoSurfaceValue );
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
#if USE_BSPLINE_INTERPOLATOR
  interpolator->SetSplineOrder( 3 );
#endif
  cuberille->SetInterpolator( interpolator );
  cuberille->SetGenerateTriangleFaces( GenerateTriangleFaces );
  cuberille->SetProjectVerticesToIsoSurface( ProjectToIsoSurface );
  cuberille->SetProjectVertexSurfaceDistanceThreshold( SurfaceDistanceThreshold );
  cuberille->SetProjectVertexStepLength( StepLength );
  cuberille->SetProjectVertexStepLengthRelaxationFactor( StepLengthRelax );
  cuberille->SetProjectVertexMaximumNumberOfSteps( MaximumNumberOfSteps );
  time.Start();
  cuberille->Update();
  time.Stop();
  outputMesh = cuberille->GetOutput();
  outputMesh->DisconnectPipeline();
#endif

#if USE_DECIMATION
  // Decimation
  typedef itk::NumberOfFacesCriterion< MeshType > DecimationCriterionType;
  DecimationCriterionType::Pointer decimateCriterion = DecimationCriterionType::New();
  decimateCriterion->SetTopologicalChange( false );
  decimateCriterion->SetNumberOfElements( 2000 );
  typedef itk::QuadEdgeMeshQuadricDecimation< MeshType, MeshType, DecimationCriterionType > DecimationType;
  DecimationType::Pointer decimate = DecimationType::New();
  decimate->SetInput( outputMesh );
  decimate->SetCriterion( decimateCriterion );
  decimate->Update();
#endif

  // Write mesh
  std::cout << "Writing output mesh: " << FilenameOutputMesh << std::endl;
  MeshFileWriterType::Pointer writer = MeshFileWriterType::New();
#if USE_DECIMATION
  writer->SetInput( decimate->GetOutput() );
#else
  writer->SetInput( outputMesh );
#endif
  writer->SetFileName( FilenameOutputMesh );
  writer->Update();

  // Assert number of points/cells
  std::cout << "Polygonization took " << time.GetMeanTime() << " seconds" << std::endl;
  std::cout << "Mesh has " << outputMesh->GetNumberOfPoints() << " vertices ";
  std::cout << "and " << outputMesh->GetNumberOfCells() << " cells" << std::endl;
  if ( ExpectedNumberOfPoints > 0 && outputMesh->GetNumberOfPoints() != ExpectedNumberOfPoints )
    {
    std::cerr << "ERROR: Expected mesh with " << ExpectedNumberOfPoints
              << " points, but found " << outputMesh->GetNumberOfPoints() << std::endl;
    return EXIT_FAILURE;
    }
  if ( ExpectedNumberOfCells > 0 && outputMesh->GetNumberOfCells() != ExpectedNumberOfCells )
    {
    std::cerr << "ERROR: Expected mesh with " << ExpectedNumberOfCells
              << " cells, but found " << outputMesh->GetNumberOfCells() << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
  }
catch (itk::ExceptionObject & err)
  {
  std::cerr << "ExceptionObject caught !" << std::endl; 
  std::cerr << err << std::endl; 
  return EXIT_FAILURE;
  }
}
