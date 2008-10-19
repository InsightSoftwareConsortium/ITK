/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiscreteHessianGaussianImageFunctionTest.cxx
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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkDiscreteHessianGaussianImageFunction.h"

template < int VDimension >
int itkDiscreteHessianGaussianImageFunctionTest( int argc, char* argv[] )
{

  // Verify the number of parameters in the command line
  if( argc < 4 )
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "inputFileName outputFileName sigma (maximum_error) (maximum_kernel_width)" << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  const unsigned int Dimension = VDimension;
  typedef float                             PixelType;
  typedef itk::Image<PixelType, Dimension>  ImageType;

  // Read input
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return EXIT_FAILURE;
    }

  // Create images for storing result
  std::vector<typename ImageType::Pointer> outputs;
  for( unsigned int i=0; i<Dimension; i++ )
    {
    typename ImageType::Pointer output = ImageType::New();
    output->SetSpacing( reader->GetOutput()->GetSpacing() );
    output->SetOrigin( reader->GetOutput()->GetOrigin() );
    output->SetDirection( reader->GetOutput()->GetDirection() );
    output->SetLargestPossibleRegion( reader->GetOutput()->GetLargestPossibleRegion() );
    output->SetRequestedRegion( reader->GetOutput()->GetRequestedRegion() );
    output->SetBufferedRegion( reader->GetOutput()->GetBufferedRegion() );
    output->Allocate();
    output->FillBuffer( itk::NumericTraits<PixelType>::Zero );
    outputs.push_back( output );
    }

  // Setup operator parameters
  double variance = atof( argv[3] );
  variance *= variance;

  double maxError = 0.001;
  unsigned int maxKernelWidth = 100;
  if( argc == 5 )
    {
    maxError = atof( argv[4] );
    }
  else if( argc > 5 )
    {
    maxError = atof( argv[4] );
    maxKernelWidth = atoi( argv[5] );
    }

  // Create function
  typedef itk::DiscreteHessianGaussianImageFunction< ImageType, PixelType >
    HessianGaussianImageFunctionType;
  typename HessianGaussianImageFunctionType::TensorType hessian;
  typename HessianGaussianImageFunctionType::TensorType::EigenValuesArrayType eigenValues;
  typename HessianGaussianImageFunctionType::Pointer function =
    HessianGaussianImageFunctionType::New();
  function->SetInputImage( reader->GetOutput() );
  function->SetMaximumError( maxError );
  function->SetMaximumKernelWidth( maxKernelWidth );
  function->SetVariance( variance );
  function->SetNormalizeAcrossScale( true );
  function->SetUseImageSpacing( true );

  // Step over input and output images
  typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;
  typedef itk::ImageRegionIterator< ImageType >      IteratorType;

  ConstIteratorType it ( reader->GetOutput(), reader->GetOutput()->GetRequestedRegion() );
  it.GoToBegin();
  std::vector< IteratorType > outs;
  for( unsigned int i=0; i<Dimension; i++ )
    {
    IteratorType out( outputs[i], outputs[i]->GetRequestedRegion() );
    out.GoToBegin();
    outs.push_back( out );
    }

  while( !it.IsAtEnd() )
    {
    hessian = function->EvaluateAtIndex( it.GetIndex() );
    hessian.ComputeEigenValues( eigenValues );

    for( unsigned int i=0; i<Dimension; i++ )
      {
      outs[i].Set( eigenValues[i] );
      ++outs[i];
      }
    ++it;
    }

  // Write outputs
  typedef itk::ImageFileWriter< ImageType > WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  for( unsigned int i=0; i<Dimension; i++ )
    {
    try
      {
      char filename[255];
      sprintf( filename, argv[2], i );
      writer->SetFileName( filename );
      writer->SetInput( outputs[i] );
      writer->Update();
      outputs[i]->DisconnectPipeline( );
      }
    catch ( itk::ExceptionObject &err)
      {
      std::cout << "ExceptionObject caught !" << std::endl; 
      std::cout << err << std::endl; 
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}

int itkDiscreteHessianGaussianImageFunctionTest2D(int argc, char* argv[] )
{
  return itkDiscreteHessianGaussianImageFunctionTest< 2 >( argc, argv );
}

int itkDiscreteHessianGaussianImageFunctionTest3D(int argc, char* argv[] )
{
  return itkDiscreteHessianGaussianImageFunctionTest< 3 >( argc, argv );
}
