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
#include "itkSimpleFilterWatcher.h"


#include "itkLabelImageToLabelMapFilter.h"
#include "itkAttributeRelabelLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkAttributeRelabelLabelMapFilterTest1(int argc, char * argv[])
{
  if( argc != 4 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input output";
    std::cerr << " reverseOrdering(0/1)";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 3;

  typedef uint8_t PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::AttributeLabelObject< PixelType, dim, int >      LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >                      LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );


  // The next step is made outside the pipeline model, so we call Update() now.
  i2l->Update();

  // Now we will valuate the attributes. The attribute will be the object position
  // in the label map
  LabelMapType::Pointer labelMap = i2l->GetOutput();

  int pos = 0;
  for( LabelMapType::Iterator it(labelMap); !it.IsAtEnd(); ++it )
    {
    LabelObjectType * labelObject = it.GetLabelObject();
    labelObject->SetAttribute( pos++ );
    }


  typedef itk::AttributeRelabelLabelMapFilter< LabelMapType > LabelRelabelType;
  LabelRelabelType::Pointer relabel = LabelRelabelType::New();

  //testing get and set macros for ReverseOrdering
  //testing boolean macro for ReverseOrdering
  relabel->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, relabel->GetReverseOrdering() );

  relabel->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, relabel->GetReverseOrdering() );

  bool reverseOrdering = atoi( argv[3] );
  relabel->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , relabel->GetReverseOrdering() );

  relabel->SetInput( labelMap );

  itk::SimpleFilterWatcher watcher(relabel, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( relabel->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
