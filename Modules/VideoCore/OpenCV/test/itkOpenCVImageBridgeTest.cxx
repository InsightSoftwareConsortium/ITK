#include <iostream>

#include "itkOpenCVImageBridge.h"
#include "itkRGBPixel.h"
#include "itkImageFileReader.h"
#include "itkDifferenceImageFilter.h"
#include "itkImageRegionConstIterator.h"

//DEBUG
#include "itkImageFileWriter.h"

//
// Compare RGBPixel Images
//
template<class TPixelValue, unsigned int VDimension>
TPixelValue
RGBImageTotalAbsDifference(
  itk::Image<itk::RGBPixel<TPixelValue>, VDimension>* valid,
  itk::Image<itk::RGBPixel<TPixelValue>, VDimension>* test)
{
  typedef itk::RGBPixel<TPixelValue> PixelType;
  typedef itk::Image<PixelType, VDimension> RGBImageType;
  typedef itk::ImageRegionConstIterator<RGBImageType> IterType;

  IterType validIt(valid, valid->GetLargestPossibleRegion());
  IterType testIt(test, test->GetLargestPossibleRegion());

  TPixelValue totalDiff = 0;

  while(!validIt.IsAtEnd())
    {
    PixelType validPx = validIt.Get();
    PixelType testPx = testIt.Get();

    totalDiff += std::abs(validPx[0] - testPx[0]);
    totalDiff += std::abs(validPx[1] - testPx[1]);
    totalDiff += std::abs(validPx[2] - testPx[2]);

    ++validIt;
    ++testIt;
    }

  return totalDiff;
}

//
// Templated test function to do the heavy lifting for scalar case
//
template<class TPixelType, unsigned int VDimension>
int itkOpenCVImageBridgeTestTemplatedScalar(char* filename)
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
    std::cerr << "Could not load input as IplImage" << std::endl;
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

  // Return successfully
  return EXIT_SUCCESS;
}

//
// Templated test function to do the heavy lifting for RGB case
//
template<class TPixelType, unsigned int VDimension>
int itkOpenCVImageBridgeTestTemplatedRGB(char* filename)
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
    std::cerr << "Could not load input as IplImage" << std::endl;
    return EXIT_FAILURE;
    }
  typename ImageType::Pointer outIplITK =
    itk::OpenCVImageBridge::IplImageToITKImage< ImageType >(inIpl);

  //
  // Check results
  //
  if (RGBImageTotalAbsDifference<typename PixelType::ComponentType, Dimension>(
        baselineImage, outIplITK) != 0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name() << std::endl;
    return EXIT_FAILURE;
    }

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

#define RUN_SCALAR_TEST(_PixelType, _inFile)\
  if (itkOpenCVImageBridgeTestTemplatedScalar< _PixelType, 2 >(_inFile) == EXIT_FAILURE)\
    {\
    return EXIT_FAILURE;\
    }

#define RUN_RGB_TEST(_PixelType, _inFile)\
  if (itkOpenCVImageBridgeTestTemplatedRGB< itk::RGBPixel<_PixelType>, 2 >(_inFile) == EXIT_FAILURE)\
    {\
    return EXIT_FAILURE;\
    }

  //
  // Test for scalar types
  //
  RUN_SCALAR_TEST(char, argv[1]);
  RUN_SCALAR_TEST(unsigned char, argv[1]);
  RUN_SCALAR_TEST(short, argv[1]);
  RUN_SCALAR_TEST(unsigned short, argv[1]);
  RUN_SCALAR_TEST(int, argv[1]);
  RUN_SCALAR_TEST(unsigned int, argv[1]);
  RUN_SCALAR_TEST(float, argv[1]);
  RUN_SCALAR_TEST(double, argv[1]);


  //
  // Test for RGB types
  //
  RUN_RGB_TEST(char, argv[1]);
  RUN_RGB_TEST(unsigned char, argv[1]);
  RUN_RGB_TEST(short, argv[1]);
  RUN_RGB_TEST(unsigned short, argv[1]);
  RUN_RGB_TEST(int, argv[1]);
  RUN_RGB_TEST(unsigned int, argv[1]);
  RUN_RGB_TEST(float, argv[1]);
  RUN_RGB_TEST(double, argv[1]);


  std::cout << "STUB!!" << std::endl;
  return EXIT_SUCCESS;
}
