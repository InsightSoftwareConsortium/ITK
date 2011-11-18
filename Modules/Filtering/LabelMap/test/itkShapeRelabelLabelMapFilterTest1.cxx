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


#include "itkShapeRelabelLabelMapFilter.h"
#include "itkLabelImageToShapeLabelMapFilter.h"

#include "itkTestingMacros.h"

int itkShapeRelabelLabelMapFilterTest1(int argc, char * argv[])
{

  if( argc != 5)
    {
    std::cerr << "usage: " << argv[0] << " input output";
    std::cerr << "background reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ShapeLabelObject< PixelType, dim >           ShapeLabelObjectType;
  typedef itk::LabelMap< ShapeLabelObjectType >             LabelMapType;

  //Reading Image File
  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  //Converting LabelImage to ShapeLabelMap
  typedef itk::LabelImageToShapeLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::ShapeRelabelLabelMapFilter< LabelMapType > RelabelType;
  RelabelType::Pointer relabel = RelabelType::New();

  //testing get and set macros for ReverseOrdering
  bool reverseOrdering = atoi( argv[3] );
  relabel->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , relabel->GetReverseOrdering() );

  //testing boolean macro for ReverseOrdering
  relabel->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, relabel->GetReverseOrdering() );

  relabel->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, relabel->GetReverseOrdering() );

  //testing get and set macros for Attribute
  unsigned int attribute = atoi( argv[4] );
  relabel->SetAttribute( attribute );
  TEST_SET_GET_VALUE( attribute, relabel->GetAttribute() );

  std::string attributeName  = ShapeLabelObjectType::GetNameFromAttribute( attribute );
  relabel->SetAttribute( attributeName );

  relabel->SetInput( i2l->GetOutput() );

  itk::SimpleFilterWatcher watcher(relabel, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2ImageType;
  L2ImageType::Pointer l2i = L2ImageType::New();
  l2i->SetInput( relabel->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
