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
#include "itkBinaryImageToLevelSetImageAdaptor.h"

int itkBinaryImageToWhitakerSparseLevelSetAdaptorTest( int argc, char* argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char InputPixelType;
  typedef double        OutputPixelType;

  typedef itk::Image< InputPixelType, Dimension >     InputImageType;

  typedef itk::ImageFileReader< InputImageType >      InputReaderType;
  InputReaderType::Pointer reader = InputReaderType::New();
  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  InputImageType::Pointer input = reader->GetOutput();
  std::cout << "Input image read" << std::endl;

  typedef itk::WhitakerSparseLevelSetImage< OutputPixelType, Dimension >
      LevelSetType;

  typedef itk::BinaryImageToLevelSetImageAdaptor< InputImageType,
      LevelSetType > BinaryToSparseAdaptorType;

  BinaryToSparseAdaptorType::Pointer adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage( input );
  adaptor->Initialize();

  std::cout << "Finished converting to sparse format" << std::endl;

  typedef LevelSetType::LayerIdType             LayerIdType;
  LevelSetType::Pointer sparseLevelSet = adaptor->GetModifiableLevelSet();

  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
  OutputImageType::Pointer output = OutputImageType::New();
  output->SetRegions( input->GetLargestPossibleRegion() );
  output->CopyInformation( input );
  output->Allocate();
  output->FillBuffer( 0.0 );

  typedef itk::Image< signed char, Dimension > StatusImageType;
  StatusImageType::Pointer statusImage = StatusImageType::New();
  statusImage->SetRegions( input->GetLargestPossibleRegion() );
  statusImage->CopyInformation( input );
  statusImage->Allocate();
  statusImage->FillBuffer( 0 );

  typedef itk::ImageRegionIteratorWithIndex< OutputImageType > OutputIteratorType;
  OutputIteratorType oIt( output, output->GetLargestPossibleRegion() );
  oIt.GoToBegin();

  typedef itk::ImageRegionIteratorWithIndex< StatusImageType > StatusIteratorType;
  StatusIteratorType sIt( statusImage, statusImage->GetLargestPossibleRegion() );
  sIt.GoToBegin();

  StatusImageType::IndexType idx;

  while( !oIt.IsAtEnd() )
    {
    idx = oIt.GetIndex();
    oIt.Set( sparseLevelSet->Evaluate( idx ) );
    sIt.Set( sparseLevelSet->Status( idx ) );
    ++oIt;
    ++sIt;
    }

  typedef itk::ImageFileWriter< OutputImageType >     OutputWriterType;
  OutputWriterType::Pointer outputWriter = OutputWriterType::New();
  outputWriter->SetFileName( argv[2] );
  outputWriter->SetInput( output );

  try
    {
    outputWriter->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter< StatusImageType >     StatusWriterType;
  StatusWriterType::Pointer statusWriter = StatusWriterType::New();
  statusWriter->SetFileName( argv[3] );
  statusWriter->SetInput( statusImage );

  try
    {
    statusWriter->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    }

  for( LayerIdType lyr = sparseLevelSet->MinusTwoLayer();
      lyr <= sparseLevelSet->PlusTwoLayer(); lyr ++ )
    {
    LevelSetType::LayerType layer = sparseLevelSet->GetLayer( lyr );
    LevelSetType::LayerIterator lIt = layer.begin();

    std::cout << "*** " << static_cast< int >( lyr ) << " ***" <<std::endl;

    while( lIt != layer.end() )
      {
      std::cout << lIt->first << ' ' << lIt->second << std::endl;
      ++lIt;
      }
    std::cout << std::endl;
    }

  typedef itk::LabelObject< unsigned long, 2 >  LabelObjectType;
  typedef LabelObjectType::Pointer              LabelObjectPointer;

  LabelObjectPointer labelObject = LabelObjectType::New();
  LabelObjectPointer labelObjectSrc = sparseLevelSet->GetAsLabelObject<unsigned long>();
  labelObject->CopyAllFrom<LabelObjectType>( labelObjectSrc );
  labelObject->SetLabel( sparseLevelSet->PlusOneLayer() );

  labelObject->Optimize();
  std::cout << labelObject->Size() << std::endl;

  return EXIT_SUCCESS;
}
