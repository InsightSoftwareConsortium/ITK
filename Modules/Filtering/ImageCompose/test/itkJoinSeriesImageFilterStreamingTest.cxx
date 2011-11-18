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
#include "itkJoinSeriesImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkPipelineMonitorImageFilter.h"

int itkJoinSeriesImageFilterStreamingTest(int argc, char* argv[] )
{
  typedef itk::Image< unsigned char, 3> ImageType;
  typedef itk::Image< unsigned char, 2> SliceImageType;

  typedef itk::ImageFileReader<ImageType>                       ImageFileReaderType;
  typedef itk::ExtractImageFilter<ImageType,SliceImageType>     SliceExtractorFilterType;
  typedef itk::JoinSeriesImageFilter<SliceImageType, ImageType> JoinSeriesFilterType;
  typedef itk::ImageFileWriter<ImageType>                       ImageFileWriterType;


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
  monitor1->SetInput( reader->GetOutput() );

  std::vector<itk::ProcessObject::Pointer> savedPointers;

  JoinSeriesFilterType::Pointer joinSeries = JoinSeriesFilterType::New();
  joinSeries->SetOrigin( reader->GetOutput()->GetOrigin()[2] );
  joinSeries->SetSpacing( reader->GetOutput()->GetSpacing()[2] );

  for ( ImageType::SizeValueType z = 0; z < numberOfSlices; ++z )
    {

    SliceExtractorFilterType::Pointer extractor = SliceExtractorFilterType::New();
    extractor->SetDirectionCollapseToSubmatrix();

    SliceExtractorFilterType::InputImageRegionType slice( reader->GetOutput()->GetLargestPossibleRegion() );
    slice.SetSize( 2, 0 );
    slice.SetIndex( 2, z );

    extractor->SetExtractionRegion( slice );
    extractor->SetInput( monitor1->GetOutput() );
    extractor->InPlaceOn();
    extractor->ReleaseDataFlagOn();

    savedPointers.push_back( extractor.GetPointer() );

    joinSeries->PushBackInput( extractor->GetOutput() );

    }


  itk::PipelineMonitorImageFilter<ImageType>::Pointer monitor2 = itk::PipelineMonitorImageFilter<ImageType>::New();
  monitor2->SetInput( joinSeries->GetOutput() );

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
