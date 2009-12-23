/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJoinSeriesImageFilterStreamingTest.cxx
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
#include "itkJoinSeriesImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkImageRegionIterator.h"
#include "../IO/itkPipelineMonitorImageFilter.h"

int itkJoinSeriesImageFilterStreamingTest(int argc, char* argv[] )
{
  typedef itk::Image< unsigned char, 3> ImageType;
  typedef itk::Image< unsigned char, 2> SliceImageType;
  
  typedef itk::ImageFileReader<ImageType> ImageFileReaderType;
  typedef itk::ExtractImageFilter<ImageType,SliceImageType> SliceExtractorFilterType;  
  typedef itk::JoinSeriesImageFilter<SliceImageType, ImageType> JoinSeriesFilterType;  
  typedef itk::ImageFileWriter<ImageType> ImageFileWriterType;


  if ( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage" << std::endl;
    return EXIT_FAILURE;
    }
  

  std::string inputFileName = argv[1];
  std::string outputFileName = argv[2];

  ImageFileReaderType::Pointer reader = ImageFileReaderType::New();
  reader->SetFileName( inputFileName );
  reader->UpdateOutputInformation();

  
  const unsigned int numberOfSlices = itk::Math::CastWithRangeCheck<unsigned int>(reader->GetOutput()->GetLargestPossibleRegion().GetSize(2));
  
  
  itk::PipelineMonitorImageFilter<ImageType>::Pointer monitor1 = itk::PipelineMonitorImageFilter<ImageType>::New();
  monitor1->SetInput( reader->GetOutput() ) ;

  std::vector<itk::ProcessObject::Pointer> savedPointers;

  JoinSeriesFilterType::Pointer joinSeries = JoinSeriesFilterType::New();
  joinSeries->SetOrigin( reader->GetOutput()->GetOrigin()[2] );
  joinSeries->SetSpacing( reader->GetOutput()->GetSpacing()[2] );
  
  for ( ImageType::SizeValueType z = 0; z < numberOfSlices; ++z ) 
    {
    
    SliceExtractorFilterType::Pointer extractor = SliceExtractorFilterType::New();

    SliceExtractorFilterType::InputImageRegionType slice( reader->GetOutput()->GetLargestPossibleRegion() );
    slice.SetSize( 2, 0 );
    slice.SetIndex( 2, z );    

    extractor->SetExtractionRegion( slice );
    extractor->SetInput( monitor1->GetOutput() ); 
    extractor->ReleaseDataFlagOn();

    savedPointers.push_back( extractor.GetPointer() );

    joinSeries->PushBackInput( extractor->GetOutput() );

    }


  itk::PipelineMonitorImageFilter<ImageType>::Pointer monitor2 = itk::PipelineMonitorImageFilter<ImageType>::New();
  monitor2->SetInput( joinSeries->GetOutput() ) ;

  ImageFileWriterType::Pointer writer = ImageFileWriterType::New();
  writer->SetInput( monitor2->GetOutput() );
  writer->SetFileName( outputFileName );
  writer->SetNumberOfStreamDivisions( numberOfSlices );


  try 
    {
    writer->Update();
    } 
  catch (...) 
    {
    std::cerr << "Exception while trying to stream write file." << std::endl;
    throw;
    }

  std::cout << "Number of Updates: " << monitor1->GetNumberOfUpdates() << std::endl;
  std::cout << "Verifying ImageFileReader to ExtractImageFilter pipeline interaction" << std::endl;

  // We can not use one of the standard verify all methods due to
  // multiple filters connected to the output of the reader
  if ( !(monitor1->VerifyInputFilterExecutedStreaming( numberOfSlices ) && 
        monitor1->VerifyInputFilterMatchedUpdateOutputInformation()) )
    {
    std::cerr << monitor1;
    return EXIT_FAILURE;
    }

  std::cout << "Verifying JoinSeriesImageFilter to ImageFileWriter pipeline interaction" << std::endl;
  if ( !monitor2->VerifyAllInputCanStream( numberOfSlices ) )
    {
    std::cerr << monitor2;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}


