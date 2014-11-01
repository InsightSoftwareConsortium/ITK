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
#include "itkImageLinearIteratorWithIndex.h"

#include <iostream>

int itkVectorImageReadWriteTest(int argc, char * argv [])
{

  if ( argc < 2 )
    {
    itkGenericOutputMacro(<<"Need a file to process");
    return EXIT_FAILURE;
    }

  // Test for vector pixel type.

  const unsigned int Dimension = 2;

  // Create image of vector pixels
  typedef itk::Vector< double, 4 >           PixelType;
  typedef itk::Image < PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ImageType::Pointer   inputImage  = ImageType::New();
  ReaderType::Pointer  reader      = ReaderType::New();
  WriterType::Pointer  writer      = WriterType::New();

  // In this test, we will create a 9x9 image of vectors with pixels (4,4)
  // and (1,6) set to 'vector1'. We will filter it using
  // RecursiveGaussianImageFilter and compare a few filtered pixels.
  //
  //Create ON and OFF vectors
  PixelType vector0(0.0);
  PixelType vector1;
  vector1[0] = 1.0;
  vector1[1] =  2.0;
  vector1[2] =  3.0;
  vector1[3] =  4.0;

  typedef itk::ImageLinearConstIteratorWithIndex< ImageType > ConstIteratorType;

  //Create the 9x9 input image
  ImageType::SizeType size;
  size.Fill( 9 );
  ImageType::IndexType index;
  index.Fill( 0 );
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();
  inputImage->FillBuffer( vector0);

  std::cout << "Create image of 9x9 image of vector pixels. IO Read and write it. " << std::endl;

  /* Set pixel (4,4) with the value 1
   * and pixel (1,6) with the value 2
   */
  index[0] = 4;
  index[1] = 4;
  inputImage->SetPixel( index, vector1);
  index[0] = 1;
  index[1] = 6;
  inputImage->SetPixel( index, vector1);

  writer->SetInput(inputImage);
  writer->SetFileName(argv[1]);
  writer->Update();

  reader->SetFileName(argv[1]);
  reader->Update();
  ImageType::Pointer outputImage = reader->GetOutput();

  ConstIteratorType cit( outputImage, outputImage->GetLargestPossibleRegion() );
  index[0] = 4;
  index[1] = 4;
  cit.SetIndex(index);
  if( cit.Get() != vector1 )
    {
    std::cout << "Vector Image Write-Read failed. Tried to write " << vector1 <<
      " But read " << cit.Get() << std::endl;
    return EXIT_FAILURE;
    }
  index[0] = 0;
  index[1] = 0;
  cit.SetIndex(index);
  if( cit.Get() != vector0 )
    {
    std::cout << "Vector Image Write-Read failed. Tried to write " << vector0 <<
      " But read " << cit.Get() << std::endl;
    return EXIT_FAILURE;
    }

  itk::ImageIOBase::Pointer io = reader->GetModifiableImageIO();


  std::cout << "ImageIO Pixel Information: "
            << io->GetPixelTypeAsString( io->GetPixelType() ) << " "
            << io->GetComponentTypeAsString( io->GetComponentType() ) << " "
            << io->GetNumberOfComponents() << std::endl;
  if ( io->GetNumberOfComponents() != 4 ||
       io->GetComponentType() != itk::ImageIOBase::DOUBLE ||
       io->GetPixelType() != itk::ImageIOBase::VECTOR)
    {
    std::cout << "Unexpected pixel information" << std::endl;
    std::cout << "Expected: vector double 4" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Image of vector pixels write-read [PASSED]" << std::endl;

  std::cout << "Test << operator:: Vector1 = " << vector1 << "[PASSED]" << std::endl;
  std::cout << "Test NumericTraits<Vector<double,4>>::ZeroValue() " <<
                        itk::NumericTraits< PixelType >::ZeroValue() << std::endl;
  std::cout << "Test NumericTraits <Vector <double,4 > >::OneValue() " <<
                        itk::NumericTraits< PixelType >::OneValue() << std::endl;

  return EXIT_SUCCESS;
}
