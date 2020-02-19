/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#if defined(_MSC_VER)
#  pragma warning(disable : 4786)
#endif

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkRegionOfInterestImageFilter.h"

#include "itkAnalyzeObjectLabelMapImageIO.h"
#include "itkAnalyzeObjectMap.h"
#include "itkAnalyzeObjectLabelMapImageIOFactory.h"

int
AnalyzeObjectMapTest(int ac, char * av[])
{
  int error_count = 0;

  if (ac != 8)
  {
    std::cerr << "USAGE: " << av[0]
              << "<inputFileName> <outputFileName> <Nifti file> <NewObjectMapFileName> <oneObjectEntryFileName> "
                 "<blankImageFileName> <OneDimensionFileName>"
              << std::endl;
  }
  const char * InputObjectFileName = av[1];
  const char * OuptputObjectFileName = av[2];
  const char * NiftiFile = av[3];
  const char * CreatingObject = av[4];
  const char * oneObjectEntryFileName = av[5];
  const char * blankImageFileName = av[6];
  const char * OneDimensionFileName = av[7];
  using PixelType = unsigned char;

  using ThreeDimensionImageType = itk::Image<PixelType, 3>;
  using FourDimensionImageType = itk::Image<PixelType, 4>;
  using TwoDimensionImageType = itk::Image<PixelType, 2>;
  using TwoDimensionRGBImageType = itk::Image<itk::RGBPixel<PixelType>, 2>;
  using ThreeDimensionRGBImageType = itk::Image<itk::RGBPixel<PixelType>, 3>;
  using OneDimensionImageType = itk::Image<PixelType, 1>;

  using ThreeDimensionReaderType = itk::ImageFileReader<ThreeDimensionImageType>;
  using ThreeDimensionWriterType = itk::ImageFileWriter<ThreeDimensionImageType>;
  using FourDimensionWriterType = itk::ImageFileWriter<FourDimensionImageType>;
  using FourDimensionReaderType = itk::ImageFileReader<FourDimensionImageType>;
  using TwoDimensionWriterType = itk::ImageFileWriter<TwoDimensionImageType>;
  using TwoDimensionReaderType = itk::ImageFileReader<TwoDimensionImageType>;
  using OneDimensionWriterType = itk::ImageFileWriter<OneDimensionImageType>;
  using OneDimensionReaderType = itk::ImageFileReader<OneDimensionImageType>;

  // This is very important to use if you are not going to install the Analyze Object map code directly into
  // itk.  This means that you can build the Analyze Object map outside of ITK and still use it and treat
  // the code as if it is in ITK.
  itk::ObjectFactoryBase::RegisterFactory(itk::AnalyzeObjectLabelMapImageIOFactory::New());

  ThreeDimensionReaderType::Pointer ThreeDimensionReader = ThreeDimensionReaderType::New();
  ThreeDimensionWriterType::Pointer ThreeDimensionWriter = ThreeDimensionWriterType::New();

  ThreeDimensionReader->SetFileName(InputObjectFileName);
  try
  {
    ThreeDimensionReader->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  // Now that we have an itk image now we need to make the image an object map
  itk::AnalyzeObjectMap<ThreeDimensionImageType, ThreeDimensionRGBImageType>::Pointer ObjectMap =
    itk::AnalyzeObjectMap<ThreeDimensionImageType, ThreeDimensionRGBImageType>::New();

  ObjectMap->ImageToObjectMap(ThreeDimensionReader->GetOutput());

  // Now we can change the object map into an itk RGB image, we then can send this image to the itk-vtk
  // converter and show the image if we wanted to.
  ThreeDimensionRGBImageType::Pointer RGBImage = ObjectMap->ObjectMapToRGBImage();

  ThreeDimensionWriter->SetFileName(OuptputObjectFileName);
  ThreeDimensionWriter->SetInput(ThreeDimensionReader->GetOutput());
  try
  {
    ThreeDimensionWriter->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  // Check the original file against the file that was written out to see if they are the exact same files.
  std::ifstream ReferenceFile;
  ReferenceFile.open(InputObjectFileName, std::ios::binary | std::ios::in);

  std::ifstream WrittenFile;
  WrittenFile.open(OuptputObjectFileName, std::ios::binary | std::ios::in);

  char ReferenceBytes;
  char WrittenBytes;
  int  count = 0;
  while (!ReferenceFile.eof() && !WrittenFile.eof())
  {
    count++;
    ReferenceFile.read(reinterpret_cast<char *>(&ReferenceBytes), sizeof(char));
    WrittenFile.read(reinterpret_cast<char *>(&WrittenBytes), sizeof(char));
    if (ReferenceBytes != WrittenBytes)
    {
      error_count++;
    }
  }

  if (!ReferenceFile.eof())
  {
    error_count++;
    std::cout << "ReferenceFile is not at end of file" << std::endl;
  }
  if (!WrittenFile.eof())
  {
    error_count++;
    std::cout << "WrittenFile is not at end of file" << std::endl;
  }

  ReferenceFile.close();
  WrittenFile.close();

  // End of checking the original versus what was written

  // Now we bring in a nifti file that Hans and Jeffrey created, the image is two squares and a circle of different
  // intensity values.
  // See the paper in the Insight Journal named "AnalyzeObjectLabelMap" for a picutre of the nifti file.
  TwoDimensionReaderType::Pointer TwoDimensionReader = TwoDimensionReaderType::New();

  TwoDimensionReader->SetFileName(NiftiFile);
  try
  {
    TwoDimensionReader->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }
  itk::AnalyzeObjectMap<TwoDimensionImageType>::Pointer CreateObjectMap =
    itk::AnalyzeObjectMap<TwoDimensionImageType>::New();

  CreateObjectMap->AddAnalyzeObjectEntry("You Can Delete Me");
  CreateObjectMap->AddObjectEntryBasedOnImagePixel(TwoDimensionReader->GetOutput(), 200, "Square", 250, 0, 0);
  CreateObjectMap->AddObjectEntryBasedOnImagePixel(TwoDimensionReader->GetOutput(), 128, "Circle", 0, 250, 0);
  CreateObjectMap->AddAnalyzeObjectEntry("Nothing In Here");
  CreateObjectMap->GetObjectEntry(4)->Copy(CreateObjectMap->GetObjectEntry(1));
  CreateObjectMap->DeleteAnalyzeObjectEntry("Nothing In Here");

  TwoDimensionWriterType::Pointer TwoDimensionWriter = TwoDimensionWriterType::New();
  TwoDimensionWriter->SetInput(CreateObjectMap);
  TwoDimensionWriter->SetFileName(CreatingObject);

  try
  {
    TwoDimensionWriter->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  TwoDimensionReader->SetFileName(CreatingObject);
  try
  {
    TwoDimensionReader->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }
  itk::AnalyzeObjectMap<TwoDimensionImageType, TwoDimensionRGBImageType>::Pointer ObjectMapTwo =
    itk::AnalyzeObjectMap<TwoDimensionImageType, TwoDimensionRGBImageType>::New();

  ObjectMapTwo->ImageToObjectMap(TwoDimensionReader->GetOutput());
  TwoDimensionRGBImageType::Pointer RGBImageTwo = ObjectMapTwo->ObjectMapToRGBImage();

  TwoDimensionWriter->SetInput(ObjectMapTwo->PickOneEntry(3));
  TwoDimensionWriter->SetFileName(oneObjectEntryFileName);

  try
  {
    TwoDimensionWriter->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  TwoDimensionReader->SetFileName(oneObjectEntryFileName);
  try
  {
    TwoDimensionReader->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  itk::AnalyzeObjectMap<TwoDimensionImageType, TwoDimensionRGBImageType>::Pointer ObjectMapThree =
    itk::AnalyzeObjectMap<TwoDimensionImageType, TwoDimensionRGBImageType>::New();

  ObjectMapThree->ImageToObjectMap(TwoDimensionReader->GetOutput());
  TwoDimensionRGBImageType::Pointer RGBImageThree = ObjectMapThree->ObjectMapToRGBImage();

  FourDimensionImageType::Pointer         BlankImage = FourDimensionImageType::New();
  const FourDimensionImageType::SizeType  size = { { 50, 20, 20, 3 } };
  const FourDimensionImageType::IndexType orgin = { { 0, 0, 0, 0 } };

  FourDimensionImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(orgin);
  BlankImage->SetRegions(region);
  BlankImage->Allocate();
  BlankImage->FillBuffer(50);
  FourDimensionWriterType::Pointer FourDimensionWriter = FourDimensionWriterType::New();
  FourDimensionWriter->SetInput(BlankImage);
  FourDimensionWriter->SetFileName(blankImageFileName);

  try
  {
    FourDimensionWriter->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  FourDimensionReaderType::Pointer FourDimensionReader = FourDimensionReaderType::New();
  FourDimensionReader->SetFileName(blankImageFileName);
  try
  {
    FourDimensionReader->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  OneDimensionImageType::Pointer         OneDimensionImage = OneDimensionImageType::New();
  const OneDimensionImageType::SizeType  sizeOne = { { 1 } };
  const OneDimensionImageType::IndexType orginOne = { { 0 } };

  OneDimensionImageType::RegionType regionOne;
  regionOne.SetSize(sizeOne);
  regionOne.SetIndex(orginOne);
  OneDimensionImage->SetRegions(regionOne);
  OneDimensionImage->Allocate();
  OneDimensionImage->FillBuffer(230);
  OneDimensionWriterType::Pointer OneDimensionWriter = OneDimensionWriterType::New();
  OneDimensionWriter->SetInput(OneDimensionImage);
  OneDimensionWriter->SetFileName(OneDimensionFileName);

  try
  {
    OneDimensionWriter->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  OneDimensionReaderType::Pointer OneDimensionReader = OneDimensionReaderType::New();
  OneDimensionReader->SetFileName(OneDimensionFileName);
  try
  {
    OneDimensionReader->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl << err << std::endl;
    return EXIT_FAILURE;
  }

  if (error_count)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
