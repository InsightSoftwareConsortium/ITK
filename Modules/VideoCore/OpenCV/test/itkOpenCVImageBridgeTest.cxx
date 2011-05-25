#include <iostream>

#include "itkOpenCVImageBridge.h"
#include "itkRGBPixel.h"
#include "itkImageFileReader.h"
#include "itkDifferenceImageFilter.h"

//DEBUG
#include "itkImageFileWriter.h"

//
// Templated test function to do the heavy lifting
//
template<class TPixelType, unsigned int VDimension>
int itkOpenCVImageBridgeTestTemplated(char* filename)
{
  // typedefs
  const unsigned int Dimension =                           VDimension;
  typedef TPixelType                                       PixelType;
  typedef itk::Image< PixelType, Dimension >               ImageType;
  typedef itk::ImageFileReader<ImageType>                  ReaderType;
  typedef itk::DifferenceImageFilter<ImageType, ImageType> DifferenceFilterType;

  //
  // Read the image directly
  //
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);
  reader->Update();
  typename ImageType::Pointer baselineImage = reader->GetOutput();

  //
  // Test IplImage -> itk::Image
  //
  IplImage* inIpl;
  inIpl = cvLoadImage(filename);
  if (!inIpl)
    {
    std::cerr << "Could not load scalar input as IplImage" << std::endl;
    return EXIT_FAILURE;
    }
  typename ImageType::Pointer outIplITK =
    itk::OpenCVImageBridge::IplImageToITKImage< ImageType >(inIpl);

  //
  // Check results
  //
  typename DifferenceFilterType::Pointer differ = DifferenceFilterType::New();
  differ->SetValidInput(baselineImage);
  differ->SetTestInput(outIplITK);
  differ->Update();
  typename DifferenceFilterType::AccumulateType total = differ->GetTotalDifference();

  if (total != 0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name() << std::endl;
    return EXIT_FAILURE;
    }

  //DEBUG
  //std::cout << "Total Difference: " << total << std::endl;

  // Return successfully
  return EXIT_SUCCESS;

}

//
// Main test
//
int itkOpenCVImageBridgeTest ( int argc, char *argv[] )
{
  //
  // Check arguments
  //
  if (argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " scalar_image color_image" << std::endl;
    return EXIT_FAILURE;
    }

#define RUN_TEST(_PixelType, _inFile)\
  if (itkOpenCVImageBridgeTestTemplated< _PixelType, 2 >(_inFile) == EXIT_FAILURE)\
    {\
    return EXIT_FAILURE;\
    }

  //
  // Test for supported types
  //
  RUN_TEST(char, argv[1]);
  RUN_TEST(unsigned char, argv[1]);
  RUN_TEST(short, argv[1]);
  RUN_TEST(unsigned short, argv[1]);
  RUN_TEST(int, argv[1]);
  RUN_TEST(unsigned int, argv[1]);
  RUN_TEST(float, argv[1]);
  RUN_TEST(double, argv[1]);
  //RUN_TEST(itk::RGBPixel<unsigned char>, argv[1]);


  std::cout << "STUB!!" << std::endl;
  return EXIT_SUCCESS;
}
