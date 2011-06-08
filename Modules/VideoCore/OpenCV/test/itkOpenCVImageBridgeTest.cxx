#include <iostream>

#include "itkOpenCVImageBridge.h"
#include "itkRGBPixel.h"
#include "itkImageFileReader.h"
#include "itkDifferenceImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

//-----------------------------------------------------------------------------
// Compare RGBPixel Images
//
template<class TPixelValue, unsigned int VDimension>
TPixelValue
RGBImageTotalAbsDifference(
  const itk::Image<itk::RGBPixel<TPixelValue>, VDimension>* valid,
  const itk::Image<itk::RGBPixel<TPixelValue>, VDimension>* test)
{
  typedef itk::RGBPixel<TPixelValue> PixelType;
  typedef itk::Image<PixelType, VDimension> RGBImageType;
  typedef itk::ImageRegionConstIteratorWithIndex<RGBImageType> IterType;

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


//-----------------------------------------------------------------------------
// Convert the data in the IplImage to the templated type
//
template<class TPixelType>
IplImage* ConvertIplImageDataType(IplImage* in)
{
  // Figure out the right output type
  if (typeid(TPixelType) == typeid(unsigned char))
    {
    IplImage* out = cvCreateImage(cvSize(in->width, in->height), IPL_DEPTH_8U, in->nChannels);
    cvConvertScale(in, out);
    return out;
    }
  else if (typeid(TPixelType) == typeid(char))
    {
    IplImage* out = cvCreateImage(cvSize(in->width, in->height), IPL_DEPTH_8S, in->nChannels);
    cvConvertScale(in, out);
    return out;
    }
  else if (typeid(TPixelType) == typeid(unsigned short))
    {
    IplImage* out = cvCreateImage(cvSize(in->width, in->height), IPL_DEPTH_16U, in->nChannels);
    cvConvertScale(in, out);
    return out;
    }
  else if (typeid(TPixelType) == typeid(short))
    {
    IplImage* out = cvCreateImage(cvSize(in->width, in->height), IPL_DEPTH_16S, in->nChannels);
    cvConvertScale(in, out);
    return out;
    }
  else if (typeid(TPixelType) == typeid(float))
    {
    IplImage* out = cvCreateImage(cvSize(in->width, in->height), IPL_DEPTH_32F, in->nChannels);
    cvConvertScale(in, out);
    return out;
    }
  else if (typeid(TPixelType) == typeid(double))
    {
    IplImage* out = cvCreateImage(cvSize(in->width, in->height), IPL_DEPTH_64F, in->nChannels);
    cvConvertScale(in, out);
    return out;
    }
  else
    {
    itkGenericExceptionMacro("OpenCV doesn't support the requested type");
    }
}


//-----------------------------------------------------------------------------
// Templated test function to do the heavy lifting for scalar case
//
template<class TPixelType, unsigned int VDimension>
int itkOpenCVImageBridgeTestTemplatedScalar(char** argv)
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
  reader->SetFileName(argv[1]);

  reader->Update();
  typename ImageType::Pointer baselineImage = reader->GetOutput();

  //
  // Test IplImage -> itk::Image
  //
  IplImage* inIpl;
  inIpl = cvLoadImage(argv[1], CV_LOAD_IMAGE_GRAYSCALE);
  if (!inIpl)
    {
    std::cerr << "Could not load input as IplImage" << std::endl;
    return EXIT_FAILURE;
    }
  typename ImageType::Pointer outIplITK =
    itk::OpenCVImageBridge::IplImageToITKImage< ImageType >(inIpl);

  // Check results of IplImage -> itk::Image
  typename DifferenceFilterType::Pointer differ = DifferenceFilterType::New();
  differ->SetValidInput(baselineImage);
  differ->SetTestInput(outIplITK);
  differ->Update();
  typename DifferenceFilterType::AccumulateType total = differ->GetTotalDifference();

  if (total != 0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name()
      << " for IplImage -> ITK (scalar)" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test cv::Mat -> itk::Image
  //
  cv::Mat inMat;
  inMat = cv::imread(argv[1]);
  typename ImageType::Pointer outMatITK =
    itk::OpenCVImageBridge::CVMatToITKImage< ImageType >(inMat);

  // Check results of cv::Mat -> itk::Image
  differ->SetTestInput(outMatITK);
  differ->Update();
  total = differ->GetTotalDifference();
  if (total != 0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name()
      << " for cv::Mat -> ITK (scalar)" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test itk::Image -> IplImage
  //
  IplImage* outIpl = itk::OpenCVImageBridge::ITKImageToIplImage< ImageType >(baselineImage);

  // check results of itk::Image -> IplImage
  IplImage* dataConvertedInIpl = ConvertIplImageDataType<PixelType>(inIpl);
  double itkIplDiff = cvNorm(outIpl, dataConvertedInIpl);

  if (itkIplDiff != 0.0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name()
      << " for ITK -> IplImage (scalar)" << std::endl;
    return EXIT_FAILURE;
    }

  // Test number of channels after force3Channels (if type is supported for color images)
  if (typeid(PixelType) == typeid(unsigned short) ||
      typeid(PixelType) == typeid(unsigned char) ||
      typeid(PixelType) == typeid(float))
    {
    cvReleaseImage(&outIpl);
    outIpl = itk::OpenCVImageBridge::ITKImageToIplImage< ImageType >(baselineImage, true);
    if (outIpl->nChannels != 3)
      {
      std::cerr << "force3Channels failed" << std::endl;
      return EXIT_FAILURE;
      }
    }

  //
  // Test itk::Image -> cv::Mat
  //
  cv::Mat outMat = itk::OpenCVImageBridge::ITKImageToCVMat< ImageType >(baselineImage);

  // check results of itk::Image -> IplImage
  IplImage outMatAsIpl = outMat;
  double itkMatDiff = cvNorm(&outMatAsIpl, dataConvertedInIpl);
  if (itkMatDiff != 0.0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name()
      << " for ITK -> cv::Mat (scalar)" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Clean up and return successfully
  //
  cvReleaseImage(&dataConvertedInIpl);
  cvReleaseImage(&inIpl);
  cvReleaseImage(&outIpl);
  return EXIT_SUCCESS;
}


//-----------------------------------------------------------------------------
// Templated test function to do the heavy lifting for RGB case
//
template<class TValueType, unsigned int VDimension>
int itkOpenCVImageBridgeTestTemplatedRGB(char** argv)
{
  // typedefs
  const unsigned int Dimension =                           VDimension;
  typedef TValueType                                       ValueType;
  typedef itk::RGBPixel< ValueType >                       PixelType;
  typedef itk::Image< PixelType, Dimension >               ImageType;
  typedef itk::ImageFileReader<ImageType>                  ReaderType;
  typedef itk::DifferenceImageFilter<ImageType, ImageType> DifferenceFilterType;

  //
  // Read the image directly
  //
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[3]);
  reader->Update();
  typename ImageType::Pointer baselineImage = reader->GetOutput();

  //
  // Test IplImage -> itk::Image
  //
  IplImage* inIpl;
  inIpl = cvLoadImage(argv[2], CV_LOAD_IMAGE_COLOR);
  if (!inIpl)
    {
    std::cerr << "Could not load input as IplImage" << std::endl;
    return EXIT_FAILURE;
    }
  typename ImageType::Pointer outIplITK =
    itk::OpenCVImageBridge::IplImageToITKImage< ImageType >(inIpl);

  // Check results of IplImage -> itk::Image
  if (RGBImageTotalAbsDifference<typename PixelType::ComponentType, Dimension>(
        baselineImage, outIplITK) != 0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name()
      << " for IplImage -> ITK (RGB)" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test cv::Mat -> itk::Image
  //
  cv::Mat inMat;
  inMat = cv::imread(argv[2]);
  typename ImageType::Pointer outMatITK =
    itk::OpenCVImageBridge::CVMatToITKImage< ImageType >(inMat);

  // Check results of cv::Mat -> itk::Image
  if (RGBImageTotalAbsDifference<typename PixelType::ComponentType, Dimension>(
        baselineImage, outIplITK) != 0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name()
      << " for cv::Mat -> ITK (RGB)" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test itk::Image -> IplImage
  //
  IplImage* outIpl = itk::OpenCVImageBridge::ITKImageToIplImage< ImageType >(baselineImage);

  // check results of itk::Image -> IplImage
  IplImage* dataConvertedInIpl = ConvertIplImageDataType<ValueType>(inIpl);
  double itkIplDiff = cvNorm(outIpl, dataConvertedInIpl);

  if (itkIplDiff != 0.0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(ValueType).name()
      << " for ITK -> IplImage (RGB)" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test itk::Image -> cv::Mat
  //
  cv::Mat outMat = itk::OpenCVImageBridge::ITKImageToCVMat< ImageType >(baselineImage);

  // check results of itk::Image -> IplImage
  IplImage outMatAsIpl = outMat;
  double itkMatDiff = cvNorm(&outMatAsIpl, dataConvertedInIpl);
  if (itkMatDiff != 0.0)
    {
    std::cerr << "Images didn't match for pixel type " << typeid(PixelType).name()
      << " for ITK -> cv::Mat (RGB)" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Clean up and return successfully
  //
  cvReleaseImage(&dataConvertedInIpl);
  cvReleaseImage(&inIpl);
  cvReleaseImage(&outIpl);
  return EXIT_SUCCESS;
}


//-----------------------------------------------------------------------------
// Main test
//
int itkOpenCVImageBridgeTest ( int argc, char *argv[] )
{
  //
  // Check arguments
  //
  if (argc < 4)
    {
    std::cerr << "Usage: " << argv[0] << "scalar_image rgb_jpg_image rgb_mha_image" << std::endl;
    return EXIT_FAILURE;
    }

#define RUN_SCALAR_TEST(_PixelType)\
  if (itkOpenCVImageBridgeTestTemplatedScalar< _PixelType, 2 >(argv) == EXIT_FAILURE)\
    {\
    return EXIT_FAILURE;\
    }

#define RUN_RGB_TEST(_ValueType)\
  if (itkOpenCVImageBridgeTestTemplatedRGB< _ValueType, 2 >(argv) == EXIT_FAILURE)\
    {\
    return EXIT_FAILURE;\
    }

  //
  // Test for scalar types
  //
  // Note: We don't test signed char because ITK seems to have trouble reading
  //       images with char pixels.
  //
  RUN_SCALAR_TEST(unsigned char);
  RUN_SCALAR_TEST(short);
  RUN_SCALAR_TEST(unsigned short);
  RUN_SCALAR_TEST(float);
  RUN_SCALAR_TEST(double);


  //
  // Test for RGB types
  //
  // Note: OpenCV only supports unsigned char, unsigned short, and float for
  // color conversion
  //
  RUN_RGB_TEST(unsigned char);
  RUN_RGB_TEST(unsigned short);
  RUN_RGB_TEST(float);

  return EXIT_SUCCESS;
}
