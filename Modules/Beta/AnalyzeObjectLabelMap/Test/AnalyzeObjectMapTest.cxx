/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: ImageReadRegionOfInterestWrite.cxx,v $
  Language:  C++
  Date:      $Date: 2005/08/27 01:46:11 $
  Version:   $Revision: 1.12 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkRegionOfInterestImageFilter.h"
#include "itkImage.h"

#include "itkAnalyzeObjectLabelMapImageIO.h"
#include "itkAnalyzeObjectMap.h"
#include "itkAnalyzeObjectLabelMapImageIOFactory.h"
int main( int argc, char * * argv )
{
  int error_count = 0;

  if( argc != 8 )
    {
    std::cerr << "USAGE: " << argv[0]
              <<
    "<inputFileName> <outputFileName> <Nifti file> <NewObjectMapFileName> <oneObjectEntryFileName> <blankImageFileName> <OneDimensionFileName>"
              << std::endl;
    }
  const char *InputObjectFileName = argv[1];
  const char *OuptputObjectFileName = argv[2];
  const char *NiftiFile = argv[3];
  const char *CreatingObject = argv[4];
  const char *oneObjectEntryFileName = argv[5];
  const char *blankImageFileName = argv[6];
  const char *OneDimensionFileName = argv[7];
  typedef                                                   unsigned char PixelType;

  typedef itk::Image<PixelType,  3>               ThreeDimensionImageType;
  typedef itk::Image<PixelType, 4>                FourDimensionImageType;
  typedef itk::Image<PixelType, 2>                TwoDimensionImageType;
  typedef itk::Image<itk::RGBPixel<PixelType>, 2> TwoDimensionRGBImageType;
  typedef itk::Image<itk::RGBPixel<PixelType>, 3> ThreeDimensionRGBImageType;
  typedef itk::Image<PixelType, 1>                OneDimensionImageType;

  typedef itk::ImageFileReader<ThreeDimensionImageType> ThreeDimensionReaderType;
  typedef itk::ImageFileWriter<ThreeDimensionImageType> ThreeDimensionWriterType;
  typedef itk::ImageFileWriter<FourDimensionImageType>  FourDimensionWriterType;
  typedef itk::ImageFileReader<FourDimensionImageType>  FourDimensionReaderType;
  typedef itk::ImageFileWriter<TwoDimensionImageType>   TwoDimensionWriterType;
  typedef itk::ImageFileReader<TwoDimensionImageType>   TwoDimensionReaderType;
  typedef itk::ImageFileWriter<OneDimensionImageType>   OneDimensionWriterType;
  typedef itk::ImageFileReader<OneDimensionImageType>   OneDimensionReaderType;

  // This is very important to use if you are not going to install the Analyze Object map code directly into
  // itk.  This means that you can build the Analyze Object map outside of ITK and still use it and treat
  // the code as if it is in ITK.
  itk::ObjectFactoryBase::RegisterFactory( itk::AnalyzeObjectLabelMapImageIOFactory::New() );

  ThreeDimensionReaderType::Pointer ThreeDimensionReader = ThreeDimensionReaderType::New();
  ThreeDimensionWriterType::Pointer ThreeDimensionWriter = ThreeDimensionWriterType::New();

  ThreeDimensionReader->SetFileName( InputObjectFileName);
  try
    {
    ThreeDimensionReader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  // Now that we have an itk image now we need to make the image an object map
  itk::AnalyzeObjectMap<ThreeDimensionImageType,
                        ThreeDimensionRGBImageType>::Pointer ObjectMap =
    itk::AnalyzeObjectMap<ThreeDimensionImageType, ThreeDimensionRGBImageType>::New();

  ObjectMap->ImageToObjectMap(ThreeDimensionReader->GetOutput() );

  // Now we can change the object map into an itk RGB image, we then can send this image to the itk-vtk
  // converter and show the image if we wanted to.
  ThreeDimensionRGBImageType::Pointer RGBImage = ObjectMap->ObjectMapToRGBImage();

  ThreeDimensionWriter->SetFileName(OuptputObjectFileName);
  ThreeDimensionWriter->SetInput(ThreeDimensionReader->GetOutput() );
  try
    {
    ThreeDimensionWriter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
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
  while( !ReferenceFile.eof()  && !WrittenFile.eof() )
    {
    count++;
    ReferenceFile.read(reinterpret_cast<char *>(&ReferenceBytes), sizeof(char) );
    WrittenFile.read(reinterpret_cast<char *>(&WrittenBytes), sizeof(char) );
    if( ReferenceBytes != WrittenBytes )
      {
      error_count++;
      }
    }

  if( !ReferenceFile.eof() )
    {
    error_count++;
    std::cout << "ReferenceFile is not at end of file" << std::endl;
    }
  if( !WrittenFile.eof() )
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
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }
  itk::AnalyzeObjectMap<TwoDimensionImageType>::Pointer CreateObjectMap =
    itk::AnalyzeObjectMap<TwoDimensionImageType>::New();

  CreateObjectMap->AddAnalyzeObjectEntry("You Can Delete Me");
  CreateObjectMap->AddObjectEntryBasedOnImagePixel(TwoDimensionReader->GetOutput(), 200, "Square", 250, 0, 0);
  CreateObjectMap->AddObjectEntryBasedOnImagePixel(TwoDimensionReader->GetOutput(), 128, "Circle", 0, 250, 0);
  CreateObjectMap->AddAnalyzeObjectEntry("Nothing In Here");
  CreateObjectMap->GetObjectEntry(4)->Copy(CreateObjectMap->GetObjectEntry(1) );
  CreateObjectMap->DeleteAnalyzeObjectEntry("Nothing In Here");

  TwoDimensionWriterType::Pointer TwoDimensionWriter = TwoDimensionWriterType::New();
  TwoDimensionWriter->SetInput(CreateObjectMap);
  TwoDimensionWriter->SetFileName(CreatingObject);

  try
    {
    TwoDimensionWriter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  TwoDimensionReader->SetFileName(CreatingObject);
  try
    {
    TwoDimensionReader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }
  itk::AnalyzeObjectMap<TwoDimensionImageType,
                        TwoDimensionRGBImageType>::Pointer ObjectMapTwo =
    itk::AnalyzeObjectMap<TwoDimensionImageType, TwoDimensionRGBImageType>::New();

  ObjectMapTwo->ImageToObjectMap(TwoDimensionReader->GetOutput() );
  TwoDimensionRGBImageType::Pointer RGBImageTwo = ObjectMapTwo->ObjectMapToRGBImage();

  TwoDimensionWriter->SetInput(ObjectMapTwo->PickOneEntry(3) );
  TwoDimensionWriter->SetFileName(oneObjectEntryFileName);

  try
    {
    TwoDimensionWriter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  TwoDimensionReader->SetFileName(oneObjectEntryFileName);
  try
    {
    TwoDimensionReader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  itk::AnalyzeObjectMap<TwoDimensionImageType,
                        TwoDimensionRGBImageType>::Pointer ObjectMapThree =
    itk::AnalyzeObjectMap<TwoDimensionImageType, TwoDimensionRGBImageType>::New();

  ObjectMapThree->ImageToObjectMap(TwoDimensionReader->GetOutput() );
  TwoDimensionRGBImageType::Pointer RGBImageThree = ObjectMapThree->ObjectMapToRGBImage();

  FourDimensionImageType::Pointer         BlankImage = FourDimensionImageType::New();
  const FourDimensionImageType::SizeType  size = {{50, 20, 20, 3}};
  const FourDimensionImageType::IndexType orgin = {{0, 0, 0, 0}};

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
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  FourDimensionReaderType::Pointer FourDimensionReader = FourDimensionReaderType::New();
  FourDimensionReader->SetFileName(blankImageFileName);
  try
    {
    FourDimensionReader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  OneDimensionImageType::Pointer         OneDimensionImage = OneDimensionImageType::New();
  const OneDimensionImageType::SizeType  sizeOne = {{1}};
  const OneDimensionImageType::IndexType orginOne = {{0}};

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
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  OneDimensionReaderType::Pointer OneDimensionReader = OneDimensionReaderType::New();
  OneDimensionReader->SetFileName(OneDimensionFileName);
  try
    {
    OneDimensionReader->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl
              << err << std::endl;
    return EXIT_FAILURE;
    }

  if( error_count )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
