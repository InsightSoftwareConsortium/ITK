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

template<typename TScalar>
int Test1Type(char *file1, char *file2)
{
  int status = 0;

  status += ReadWrite<TScalar,2>(itk::NumericTraits<TScalar>::NonpositiveMin(),
                                 itk::NumericTraits<TScalar>::max(),
                                 file1, file2, false);
  status += ReadWrite<TScalar,2>(itk::NumericTraits<TScalar>::NonpositiveMin(),
                                 itk::NumericTraits<TScalar>::max(),
                                 file1, file2, true);
  status += ReadWrite<TScalar,3>(itk::NumericTraits<TScalar>::NonpositiveMin(),
                                 itk::NumericTraits<TScalar>::max(),
                                 file1, file2, false);
  status += ReadWrite<TScalar,3>(itk::NumericTraits<TScalar>::NonpositiveMin(),
                                 itk::NumericTraits<TScalar>::max(),
                                 file1, file2, true);

  return status;
}

int itkVTKImageIOTest(int argc, char* argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  output1 output2 " << std::endl;
    return EXIT_FAILURE;
    }

  char* file1 = argv[1];
  char* file2 = argv[2];

  int status = 0;

  status += Test1Type<float>(file1, file2);
  status += Test1Type<double>(file1, file2);
  status += Test1Type<unsigned char>(file1, file2);
  status += Test1Type<char>(file1, file2);
  status += Test1Type<unsigned short>(file1, file2);
  status += Test1Type<short>(file1, file2);
  status += Test1Type<unsigned int>(file1, file2);
  status += Test1Type<int>(file1, file2);
  status += Test1Type<unsigned long>(file1, file2);
  status += Test1Type<long>(file1, file2);
  status += Test1Type<unsigned long long>(file1, file2);
  status += Test1Type<long long>(file1, file2);

  return status;
}
