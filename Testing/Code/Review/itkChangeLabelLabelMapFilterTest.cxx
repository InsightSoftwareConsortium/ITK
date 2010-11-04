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

#include "itkLabelImageToLabelMapFilter.h"
#include "itkChangeLabelLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int itkChangeLabelLabelMapFilterTest( int argc, char * argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "usage: " << argv[0] << " inputLabelImage outputLabelImage ";
    std::cerr << "  old_label_1 new_label_1" << std::endl;
    std::cerr << " [old_label_2 new_label_2]" << std::endl;
    std::cerr << " [old_label_3 new_label_3]" << std::endl;
    std::cerr << " ...                      " << std::endl;
    std::cerr << " [old_label_n new_label_n]" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char    ImagePixelType;
  typedef unsigned char    LabelPixelType;

  typedef itk::Image< ImagePixelType, Dimension > ImageType;

  typedef itk::LabelObject< LabelPixelType, Dimension >  LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >               LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::ChangeLabelLabelMapFilter< LabelMapType > ChangeLabelFilterType;
  ChangeLabelFilterType::Pointer changeFilter = ChangeLabelFilterType::New();

  changeFilter->SetInput( i2l->GetOutput() );

  const unsigned int numberOfArgumentsBeforeLabels = 3;
  const unsigned int numberOfArguments = argc;

  typedef itk::NumericTraits< LabelPixelType >::PrintType   LabelPrintType;

  for( unsigned int i = numberOfArgumentsBeforeLabels; i < numberOfArguments; i += 2 )
    {
    const LabelPixelType  oldLabel = atoi( argv[i]   );
    const LabelPixelType  newLabel = atoi( argv[i+1] );

    std::cout << "Label pair : ";
    std::cout << static_cast< LabelPrintType >( oldLabel ) << " -> ";
    std::cout << static_cast< LabelPrintType >( newLabel ) << std::endl;
    changeFilter->SetChange( oldLabel, newLabel );
    }

  itk::SimpleFilterWatcher watcher6(changeFilter, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( changeFilter->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  std::cout << "GetNameOfClass() = " << changeFilter->GetNameOfClass() << std::endl;

  changeFilter->Print( std::cout );

  const ChangeLabelFilterType::ChangeMapType & mapOfLabel = changeFilter->GetChangeMap();

  const unsigned int numberOfLabelsToReplace =
    ( numberOfArguments - numberOfArgumentsBeforeLabels ) / 2;

  if( mapOfLabel.size() != numberOfLabelsToReplace )
    {
    std::cerr << "Error in SetChange() or in GetChangeMap() " << std::endl;
    std::cerr << "numberOfLabelsToReplace = " << numberOfLabelsToReplace << std::endl;
    std::cerr << "mapOfLabel.size() = " << mapOfLabel.size() << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
