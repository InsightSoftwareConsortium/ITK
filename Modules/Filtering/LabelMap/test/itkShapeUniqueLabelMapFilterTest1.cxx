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


#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkShapeUniqueLabelMapFilter.h"

#include "itkTestingMacros.h"

int itkShapeUniqueLabelMapFilterTest1(int argc, char * argv[])
{
  if( argc != 5 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input output";
    std::cerr << " reverseOrdering(0/1) attribute";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  constexpr unsigned int dim = 3;

  using PixelType = unsigned char;

  using ImageType = itk::Image< PixelType, dim >;

  using ShapeLabelObjectType = itk::ShapeLabelObject< PixelType, dim >;
  using LabelMapType = itk::LabelMap< ShapeLabelObjectType >;

  using ReaderType = itk::ImageFileReader< ImageType >;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  using I2LType = itk::LabelImageToShapeLabelMapFilter< ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  using LabelUniqueType = itk::ShapeUniqueLabelMapFilter< LabelMapType >;
  LabelUniqueType::Pointer Unique = LabelUniqueType::New();

  //testing get and set macros for ReverseOrdering
  bool reverseOrdering = std::stoi( argv[3] );
  Unique->SetReverseOrdering( reverseOrdering );
  ITK_TEST_SET_GET_VALUE( reverseOrdering , Unique->GetReverseOrdering() );

  //testing boolean macro for ReverseOrdering
  Unique->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE( true, Unique->GetReverseOrdering() );

  Unique->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE( false, Unique->GetReverseOrdering() );

  //testing get and set macros for Attribute
  LabelUniqueType::AttributeType attribute = std::stoi( argv[4] );
  Unique->SetAttribute( attribute );
  ITK_TEST_SET_GET_VALUE( attribute, Unique->GetAttribute() );

  Unique->SetInput( i2l->GetOutput() );

  itk::SimpleFilterWatcher watcher(Unique, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter< LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( Unique->GetOutput() );

  using WriterType = itk::ImageFileWriter< ImageType >;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
