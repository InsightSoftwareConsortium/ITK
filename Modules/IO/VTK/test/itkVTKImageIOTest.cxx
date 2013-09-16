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

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkVTKImageIO.h"
#include "itkRandomImageSource.h"

#include <fstream>
#include <iostream>

#define SPECIFIC_IMAGEIO_MODULE_TEST

template<typename TScalar, unsigned int TDimension>
int ReadWrite(TScalar low, TScalar hi, char *file1, char *file2, bool ascii)
{
  typedef itk::Image<TScalar,TDimension> ImageType;

  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  typename ImageType::SizeValueType size[TDimension];
  for (unsigned int i = 0; i < TDimension; i++)
    {
    size[i]= 2 << (i + 1);
    }
  typename itk::RandomImageSource<ImageType>::Pointer random;
  random = itk::RandomImageSource<ImageType>::New();
  low = hi = static_cast<TScalar>(127);
  random->SetMin(low);
  random->SetMax(hi);
  random->SetSize(size);

  typename ImageType::SpacingValueType spacing[3] = {5.0f, 10.0f, 15.0f};
  typename ImageType::PointValueType origin[3] = {-5.0f, -10.0f, -15.0f};

  random->SetSpacing(spacing);
  random->SetOrigin(origin);

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::VTKImageIO::Pointer vtkIO;
  vtkIO = itk::VTKImageIO::New();
  if (ascii)
    {
    vtkIO->SetFileTypeToASCII();
    }
  else
    {
    vtkIO->SetFileTypeToBinary();
    }

  // Write out the image
  typename itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileName(file1);
  writer->SetImageIO(vtkIO);
  writer->Write();

  // Check that the correct content was written to the header.
  std::ifstream istrm(file1);
  char firstline[25];
  istrm.getline( firstline, 24 );
  istrm.close();
  if( strncmp( firstline, "# vtk DataFile Version ", 24 ) != 0 )
    {
    std::cout << "Header string was not written properly." << std::endl;
    return EXIT_FAILURE;
    }

  if ( !vtkIO->CanReadFile(file1) )
    {
    return EXIT_FAILURE;
    }

  // Create a source object (in this case a reader)
  typename itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(vtkIO);
  reader->SetFileName(file1);
  reader->Update();

  writer->SetInput(reader->GetOutput());
  writer->SetFileName(file2);
  writer->Write();

  return EXIT_SUCCESS;
}


int itkVTKImageIOTest(int argc, char* argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  output1 output2 " << std::endl;
    return EXIT_FAILURE;
    }

  int status = 0;


  status += ReadWrite<float,2>(itk::NumericTraits<float>::NonpositiveMin(),
                             itk::NumericTraits<float>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<float,2>(itk::NumericTraits<float>::NonpositiveMin(),
                             itk::NumericTraits<float>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<float,3>(itk::NumericTraits<float>::NonpositiveMin(),
                             itk::NumericTraits<float>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<float,3>(itk::NumericTraits<float>::NonpositiveMin(),
                             itk::NumericTraits<float>::max(),
                             argv[1], argv[2], true);

  status += ReadWrite<double,2>(itk::NumericTraits<double>::NonpositiveMin(),
                             itk::NumericTraits<double>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<double,2>(itk::NumericTraits<double>::NonpositiveMin(),
                             itk::NumericTraits<double>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<double,3>(itk::NumericTraits<double>::NonpositiveMin(),
                             itk::NumericTraits<double>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<double,3>(itk::NumericTraits<double>::NonpositiveMin(),
                             itk::NumericTraits<double>::max(),
                             argv[1], argv[2], true);

  status += ReadWrite<unsigned char,2>(itk::NumericTraits<unsigned char>::NonpositiveMin(),
                             itk::NumericTraits<unsigned char>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<unsigned char,2>(itk::NumericTraits<unsigned char>::NonpositiveMin(),
                             itk::NumericTraits<unsigned char>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<unsigned char,3>(itk::NumericTraits<unsigned char>::NonpositiveMin(),
                             itk::NumericTraits<unsigned char>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<unsigned char,3>(itk::NumericTraits<unsigned char>::NonpositiveMin(),
                             itk::NumericTraits<unsigned char>::max(),
                             argv[1], argv[2], true);


  status += ReadWrite<char,2>(itk::NumericTraits<char>::NonpositiveMin(),
                             itk::NumericTraits<char>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<char,2>(itk::NumericTraits<char>::NonpositiveMin(),
                             itk::NumericTraits<char>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<char,3>(itk::NumericTraits<char>::NonpositiveMin(),
                             itk::NumericTraits<char>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<char,3>(itk::NumericTraits<char>::NonpositiveMin(),
                             itk::NumericTraits<char>::max(),
                             argv[1], argv[2], true);

  status += ReadWrite<unsigned short,2>(itk::NumericTraits<unsigned short>::NonpositiveMin(),
                             itk::NumericTraits<unsigned short>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<unsigned short,2>(itk::NumericTraits<unsigned short>::NonpositiveMin(),
                             itk::NumericTraits<unsigned short>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<unsigned short,3>(itk::NumericTraits<unsigned short>::NonpositiveMin(),
                             itk::NumericTraits<unsigned short>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<unsigned short,3>(itk::NumericTraits<unsigned short>::NonpositiveMin(),
                             itk::NumericTraits<unsigned short>::max(),
                             argv[1], argv[2], true);

  status += ReadWrite<short,2>(itk::NumericTraits<short>::NonpositiveMin(),
                             itk::NumericTraits<short>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<short,2>(itk::NumericTraits<short>::NonpositiveMin(),
                             itk::NumericTraits<short>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<short,3>(itk::NumericTraits<short>::NonpositiveMin(),
                             itk::NumericTraits<short>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<short,3>(itk::NumericTraits<short>::NonpositiveMin(),
                             itk::NumericTraits<short>::max(),
                             argv[1], argv[2], true);

  status += ReadWrite<int,2>(itk::NumericTraits<int>::NonpositiveMin(),
                             itk::NumericTraits<int>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<int,2>(itk::NumericTraits<int>::NonpositiveMin(),
                             itk::NumericTraits<int>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<int,3>(itk::NumericTraits<int>::NonpositiveMin(),
                             itk::NumericTraits<int>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<int,3>(itk::NumericTraits<int>::NonpositiveMin(),
                             itk::NumericTraits<int>::max(),
                             argv[1], argv[2], true);

  status += ReadWrite<unsigned int,2>(itk::NumericTraits<unsigned int>::NonpositiveMin(),
                             itk::NumericTraits<unsigned int>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<unsigned int,2>(itk::NumericTraits<unsigned int>::NonpositiveMin(),
                             itk::NumericTraits<unsigned int>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<unsigned int,3>(itk::NumericTraits<unsigned int>::NonpositiveMin(),
                             itk::NumericTraits<unsigned int>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<unsigned int,3>(itk::NumericTraits<unsigned int>::NonpositiveMin(),
                             itk::NumericTraits<unsigned int>::max(),
                             argv[1], argv[2], true);

  status += ReadWrite<unsigned long,2>(itk::NumericTraits<unsigned long>::NonpositiveMin(),
                             itk::NumericTraits<unsigned long>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<unsigned long,2>(itk::NumericTraits<unsigned long>::NonpositiveMin(),
                             itk::NumericTraits<unsigned long>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<unsigned long,3>(itk::NumericTraits<unsigned long>::NonpositiveMin(),
                             itk::NumericTraits<unsigned long>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<unsigned long,3>(itk::NumericTraits<unsigned long>::NonpositiveMin(),
                             itk::NumericTraits<unsigned long>::max(),
                             argv[1], argv[2], true);

  status += ReadWrite<long,2>(itk::NumericTraits<long>::NonpositiveMin(),
                             itk::NumericTraits<long>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<long,2>(itk::NumericTraits<long>::NonpositiveMin(),
                             itk::NumericTraits<long>::max(),
                             argv[1], argv[2], true);
  status += ReadWrite<long,3>(itk::NumericTraits<long>::NonpositiveMin(),
                             itk::NumericTraits<long>::max(),
                             argv[1], argv[2], false);
  status += ReadWrite<long,3>(itk::NumericTraits<long>::NonpositiveMin(),
                             itk::NumericTraits<long>::max(),
                             argv[1], argv[2], true);

  return status;
}
