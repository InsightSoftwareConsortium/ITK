/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkCurvatureFlowTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkCurvatureFlowImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkPhysicalImage.h"
#include "itkScalar.h"
#include "itkVTKImageWriter.h"


int main()
{
  /* -------------------------------------------------
   * Create a random image of size 64 x 64
   */
  typedef itk::Scalar<float> PixelType;
  typedef itk::PhysicalImage<PixelType,2> ImageType;
  
  typedef itk::RandomImageSource<ImageType> SourceType;

  SourceType::Pointer source = SourceType::New();
  
  unsigned long size[2] = {64,64};
  source->SetSize( size );
  source->SetMin(0.0);
  source->SetMax(1.0);

  /* ---------------------------------------------
   * Create a curvature flow object
   */
  typedef itk::CurvatureFlowImageFilter<ImageType> DenoiserType;
  DenoiserType::Pointer denoiser = DenoiserType::New();

  denoiser->SetInput( source->GetOutput() );
  denoiser->SetTimeStepSize( 1.0 );
  denoiser->SetNumberOfIterations( 8 );
  denoiser->SetDebugOn( true );

  /* ------------------------------------------
   * Write output to file
   */
  typedef itk::VTKImageWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( denoiser->GetOutput() );
  writer->SetFileName("CurvatureFlowImageFilterImage.vtk");
  writer->Write();
  
  return EXIT_SUCCESS;

}
