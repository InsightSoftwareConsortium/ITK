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
#include "itkLabelMapMaskImageFilter.h"
#include "itkTestingMacros.h"


int itkLabelMapMaskImageFilterTest( int argc, char * argv[] )
{

  if( argc != 9 )
    {
    std::cerr << "usage: " << argv[0] << " labelImage input output label bg neg crop cropBorder" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 3;

  // Declare the pixel type
  typedef unsigned char PixelType;

  // Declare the input image type
  typedef itk::Image< PixelType, Dimension > ImageType;

  // And the label object type to use. The input image is a label image, so the
  // type of the label can be the same type than the pixel type. itk::LabelObject is
  // chosen, because only the mask feature is tested here, so we don't need any
  // attribute.
  typedef itk::LabelObject< PixelType, Dimension >  LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >          LabelMapType;

  // Read the label image and the input image to be masked.
  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  // Convert the label image to a label collection image.
  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );
  //i2l->SetUseBackground( true );

  // Then mask the image. Two inputs are required (the label collection image, and
  // the image to be masked). The label used to mask the image is passed with the
  // SetLabel() method. The background in the output image, where the image is masked,
  // is passed with SetBackground(). The user can choose to mask the image outside the
  // label object (that's the default behavior), or inside the label object with the
  // chosen label, by calling SetNegated(). Finally, the image can be cropped to the
  // masked region, by calling SetCrop( true ), or to a region padded by a border, by
  // calling both SetCrop() and SetCropBorder(). The crop border defaults to 0, and the
  // image is not cropped by default.

  typedef itk::LabelMapMaskImageFilter< LabelMapType, ImageType > MaskFilterType;
  MaskFilterType::Pointer maskFilter = MaskFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( maskFilter, LabelMapMaskImageFilter,
    LabelMapFilter );

  maskFilter->SetInput( i2l->GetOutput() );

  maskFilter->SetFeatureImage( reader2->GetOutput() );

  MaskFilterType::InputImagePixelType label = atoi( argv[4] );
  maskFilter->SetLabel( label );
  TEST_SET_GET_VALUE( label, maskFilter->GetLabel() );

  MaskFilterType::OutputImagePixelType backgroundValue = atoi( argv[5] );
  maskFilter->SetBackgroundValue( backgroundValue );
  TEST_SET_GET_VALUE( backgroundValue, maskFilter->GetBackgroundValue() );

  bool negated = static_cast< bool >(atoi( argv[6] ) );
  maskFilter->SetNegated( negated );
  TEST_SET_GET_VALUE( negated, maskFilter->GetNegated() );
  if( negated )
    {
    maskFilter->NegatedOn();
    TEST_SET_GET_VALUE( true, maskFilter->GetNegated() );
    }
  else
    {
    maskFilter->NegatedOff();
    TEST_SET_GET_VALUE( false, maskFilter->GetNegated() );
    }

  bool crop = static_cast< bool >( atoi( argv[7] ) );
  maskFilter->SetCrop( crop );
  TEST_SET_GET_VALUE( crop, maskFilter->GetCrop() );
  if( crop )
    {
    maskFilter->CropOn();
    TEST_SET_GET_VALUE( true, maskFilter->GetCrop() );
    }
  else
    {
    maskFilter->CropOff();
    TEST_SET_GET_VALUE( false, maskFilter->GetCrop() );
    }

  MaskFilterType::SizeType border;
  border.Fill( atoi(argv[8]) );
  maskFilter->SetCropBorder( border );
  TEST_SET_GET_VALUE( border, maskFilter->GetCropBorder() );

  itk::SimpleFilterWatcher watcher( maskFilter, "LabelMapMaskImageFilter" );

  // Finally, save the output image.
  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( maskFilter->GetOutput() );
  writer->SetFileName( argv[3] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
