/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureFlowTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkCurvatureFlowImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkVTKImageIO.h"
#include "itkTextOutput.h"
#include "itkCommand.h"
#include "itkCastImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkImageRegionIterator.h"


namespace
{
// The following three classes are used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};
}


int itkCurvatureFlowTest(int argc, char* argv[] )
{

  if( argc < 2 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  outputFile" << std::endl;
    return 1;
    }

   itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());


  typedef float PixelType;
  enum { ImageDimension = 2 };
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  std::cout << "Create input image using RandomImageSource" << std::endl;
  typedef itk::RandomImageSource<ImageType> SourceType;

  SourceType::Pointer source = SourceType::New();

  unsigned long size[ImageDimension] = {64,64};
  source->SetSize( size );
  source->SetMin(0.0);
  source->SetMax(1.0);
  source->Update();

  std::cout << "Run CurvatureFlowImageFiler with progress cout's" << std::endl;
  typedef itk::CurvatureFlowImageFilter<ImageType,ImageType> DenoiserType;
  DenoiserType::Pointer denoiser = DenoiserType::New();

  denoiser->SetInput( source->GetOutput() );
  denoiser->SetTimeStep( 0.05 );
  denoiser->SetNumberOfIterations( 8 );

  ShowProgressObject progressWatch(denoiser);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  denoiser->AddObserver( itk::ProgressEvent(), command);

  denoiser->Update();

  std::cout << "Run CurvatureFlowImageFilter using streamer" << std::endl;
  typedef itk::CastImageFilter<ImageType,ImageType> CasterType;
  CasterType::Pointer caster = CasterType::New();
  caster->SetInput( denoiser->GetInput() );

  DenoiserType::Pointer denoiser2 = DenoiserType::New();
  denoiser2->SetInput( caster->GetOutput() );
  denoiser2->SetTimeStep( denoiser->GetTimeStep() );
  denoiser2->SetNumberOfIterations( denoiser->GetNumberOfIterations() );

  typedef itk::StreamingImageFilter<ImageType,ImageType> StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput( denoiser2->GetOutput() );
  streamer->SetNumberOfStreamDivisions( 3 );
  streamer->Update();

  std::cout << "Compare stand-alone and streamer outputs" << std::endl;
  typedef itk::ImageRegionIterator<ImageType> IteratorType;
  IteratorType it1( denoiser->GetOutput(),
    denoiser->GetOutput()->GetBufferedRegion() );
  IteratorType it2( streamer->GetOutput(),
    streamer->GetOutput()->GetBufferedRegion() );

  bool testPass = true;
  while( !it1.IsAtEnd() )
    {
    if( it1.Get() != it2.Get() )
      { testPass = false; }
    ++it1;
    ++it2;
    }

  if( !testPass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise other member functions here
  denoiser->Print( std::cout );
 
  itk::VTKImageIO::Pointer vtkIO;
  vtkIO = itk::VTKImageIO::New();
  typedef itk::ImageFileWriter<ImageType> WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( streamer->GetOutput() );
  writer->SetFileName(argv[1]);
  writer->SetImageIO(vtkIO);
  writer->Write();


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
