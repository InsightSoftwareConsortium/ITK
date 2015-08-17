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
#include "itkGDCMImageIO.h"
#include "itkMetaDataObject.h"
#include "itkRandomImageSource.h"
#include <string>
#include <sstream>
#include <vector>
#include "itkMath.h"

int itkGDCMImageOrientationPatientTest( int argc, char* argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] <<
      " OutputTestDirectory" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short,3>                 Image3DType;
  typedef itk::Image<short,2>                 Image2DType;
  typedef itk::ImageFileReader< Image3DType > ReaderType;
  typedef itk::RandomImageSource<Image2DType> RandomImageSource2DType;
  typedef itk::ImageFileWriter< Image2DType > Writer2DType;
  typedef itk::GDCMImageIO                    ImageIOType;

  typedef itk::MetaDataDictionary   DictionaryType;
  DictionaryType dict;

  // Create a 2D image
  Image2DType::SpacingType spacing2D;
  spacing2D[0] = 10.0;
  spacing2D[1] = 100.0;

  Image2DType::SizeType size2D;
  size2D.Fill(16);

  RandomImageSource2DType::Pointer src2D =
    RandomImageSource2DType::New();
  src2D->SetMin(0);
  src2D->SetMax(255);
  src2D->SetSpacing(spacing2D);
  src2D->SetSize(size2D);

  ImageIOType::Pointer gdcmIO =
    ImageIOType::New();
  DictionaryType dictionary;

  // Set all required DICOM fields
  std::ostringstream value;

  double origin3D[3];
  origin3D[0] = 1.0;
  origin3D[1] = 2.0;
  origin3D[2] = 3.0;
  value.str("");
  value << origin3D[0] << "\\" << origin3D[1] << "\\" << origin3D[2];
  itk::EncapsulateMetaData<std::string>(dictionary,"0020|0032", value.str());

  Image3DType::DirectionType direction3D;
  direction3D[0][0] = .6;
  direction3D[1][0] = .0;
  direction3D[2][0] =  .8;
  direction3D[0][1] = -.8;
  direction3D[1][1] = .0;
  direction3D[2][1] = .6;
  direction3D[0][2] = 0;
  direction3D[1][2] = 1;
  direction3D[2][2] = 0;
  value.str("");
  value << direction3D[0][0] << "\\" << direction3D[1][0] << "\\" << direction3D[2][0] << "\\" << direction3D[0][1] << "\\" << direction3D[1][1] << "\\" << direction3D[2][1];
  itk::EncapsulateMetaData<std::string>(dictionary,"0020|0037", value.str());

  // GDCM will not write IPP unless the modality is one of CT, MR or RT.
  std::string modality("MR");
  itk::EncapsulateMetaData<std::string>(dictionary, "0008|0060", modality);

  src2D->GetOutput()->SetMetaDataDictionary(dictionary);

  Writer2DType::Pointer writer2D = Writer2DType::New();
  std::ostringstream filename;
  filename.str("");
  filename << argv[1] << "/itkGDCMImageOrientationPatientTest.dcm";
  writer2D->SetInput(src2D->GetOutput());
  writer2D->SetFileName(filename.str().c_str());

  try
    {
    writer2D->SetImageIO(gdcmIO);
    writer2D->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while writing the file: " << filename.str() << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Now read the dicom back and check its origin
  ReaderType::Pointer reader =
    ReaderType::New();
  reader->SetFileName(filename.str().c_str());
  reader->Update();

  Image3DType::DirectionType readerDirection3D;
  readerDirection3D = reader->GetOutput()->GetDirection();
  if ((itk::Math::NotExactlyEquals(readerDirection3D[0][0], direction3D[0][0])) ||
      (itk::Math::NotExactlyEquals(readerDirection3D[1][0], direction3D[1][0])) ||
      (itk::Math::NotExactlyEquals(readerDirection3D[2][0], direction3D[2][0])) ||
      (itk::Math::NotExactlyEquals(readerDirection3D[0][1], direction3D[0][1])) ||
      (itk::Math::NotExactlyEquals(readerDirection3D[1][1], direction3D[1][1])) ||
      (itk::Math::NotExactlyEquals(readerDirection3D[2][1], direction3D[2][1]))
      )
    {
    std::cout << "ERROR: read directions does not equal written directions: "
              << readerDirection3D << " != "
              << direction3D;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
