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

#include <iostream>

#include "itkOpenCVImageBridge.h"
#include "itkImageFileReader.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkOpenCVVideoIOFactory.h"

#if defined( CV_VERSION_EPOCH )
#include "highgui.h"
#else
#include "opencv2/imgcodecs.hpp" // cv::imread
#include "opencv2/imgcodecs/imgcodecs_c.h" // CV_LOAD_IMAGE_COLOR
#endif


//-----------------------------------------------------------------------------
// Convert the data in the IplImage to the templated type
//
template<typename TPixelType>
IplImage* ConvertIplImageDataType(IplImage* in)
{
  int depth = 0;

  // Figure out the right output type
  if (typeid(TPixelType) == typeid(unsigned char))
    {
    depth = IPL_DEPTH_8U;
    }
  else if (typeid(TPixelType) == typeid(char))
    {
    depth = IPL_DEPTH_8S;
    }
  else if (typeid(TPixelType) == typeid(unsigned short))
    {
    depth = IPL_DEPTH_16U;
    }
  else if (typeid(TPixelType) == typeid(short))
    {
    depth = IPL_DEPTH_16S;
    }
  else if (typeid(TPixelType) == typeid(float))
    {
    depth = IPL_DEPTH_32F;
    }
  else if (typeid(TPixelType) == typeid(double))
    {
    depth = IPL_DEPTH_64F;
    }
  else
    {
    itkGenericExceptionMacro("OpenCV doesn't support the requested type");
    }

  IplImage* out = cvCreateImage(cvSize(in->width, in->height), depth, in->nChannels);
  cvConvertScale(in, out);
  return out;
}


//-----------------------------------------------------------------------------
// Templated test function to do the heavy lifting for scalar case
//
template<typename TPixelType, unsigned int VDimension>
int itkOpenCVImageBridgeTestTemplatedScalar(char* argv)
{
  // typedefs
  const unsigned int Dimension =                         VDimension;
  typedef TPixelType                                     PixelType;
  typedef itk::Image< PixelType, Dimension >             ImageType;
  typedef itk::ImageFileReader<ImageType>                ReaderType;
  typedef itk::Testing::ComparisonImageFilter<ImageType, ImageType>
                                                         DifferenceFilterType;

  itk::ObjectFactoryBase::RegisterFactory( itk::OpenCVVideoIOFactory::New() );

  //
  // Read the image directly
  //
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv);

  reader->Update();
  typename ImageType::Pointer baselineImage = reader->GetOutput();
  std::cout << "Read image with pixel type "
    << typeid(PixelType).name()
    << " and dimension " << VDimension << std::endl;

  std::cout << "Test IplImage -> itk::Image..." << std::endl;
  IplImage* inIpl;
  inIpl = cvLoadImage(argv, CV_LOAD_IMAGE_ANYDEPTH);
  if (!inIpl)
    {
    std::cerr << "Could not load input as IplImage" << std::endl;
    return EXIT_FAILURE;
    }
  typename ImageType::Pointer outIplITK =
    itk::OpenCVImageBridge::IplImageToITKImage< ImageType >(inIpl);

  if( outIplITK->GetLargestPossibleRegion() != baselineImage->GetLargestPossibleRegion() )
    {
    std::cerr << "Images didn't match: different largest possible region" << std::endl;
    cvReleaseImage(&inIpl);
    return EXIT_FAILURE;
    }

  // Check results of IplImage -> itk::Image
  typename DifferenceFilterType::Pointer differ = DifferenceFilterType::New();
  differ->SetValidInput(baselineImage);
  differ->SetTestInput(outIplITK);
  differ->Update();
  typename DifferenceFilterType::AccumulateType total = differ->GetTotalDifference();

  if (total != 0)
    {
    std::cerr << "Images didn't match for pixel type "
      << typeid(PixelType).name()
      << " for IplImage -> ITK (scalar)" << std::endl;
    cvReleaseImage(&inIpl);
    return EXIT_FAILURE;
    }

  std::cout << "Test cv::Mat -> itk::Image..." << std::endl;
  cv::Mat inMat;
  inMat = cv::imread(argv, CV_LOAD_IMAGE_ANYDEPTH);
  typename ImageType::Pointer outMatITK =
    itk::OpenCVImageBridge::CVMatToITKImage< ImageType >(inMat);

  // Check results of cv::Mat -> itk::Image
  differ->SetTestInput(outMatITK);
  differ->Update();
  total = differ->GetTotalDifference();
  if (total != 0)
    {
    std::cerr << "Images didn't match for pixel type "
      << typeid(PixelType).name()
      << " for cv::Mat -> ITK (scalar)" << std::endl;
    cvReleaseImage(&inIpl);
    return EXIT_FAILURE;
    }

  std::cout << "Test itk::Image -> IplImage..." << std::endl;
  IplImage* outIpl = itk::OpenCVImageBridge::ITKImageToIplImage< ImageType >(baselineImage);

  // check results of itk::Image -> IplImage
  IplImage* dataConvertedInIpl = ConvertIplImageDataType<PixelType>(inIpl);
  double itkIplDiff = cvNorm(outIpl, dataConvertedInIpl);

  if (itkIplDiff != 0.0)
    {
    std::cerr << "Images didn't match for pixel type "
      << typeid(PixelType).name()
      << " for ITK -> IplImage (scalar)" << "; itkIplDiff = "
      << itkIplDiff<< std::endl;
    cvReleaseImage(&dataConvertedInIpl);
    cvReleaseImage(&inIpl);
    cvReleaseImage(&outIpl);
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
      cvReleaseImage(&dataConvertedInIpl);
      cvReleaseImage(&inIpl);
      cvReleaseImage(&outIpl);
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test itk::Image -> cv::Mat..." << std::endl;
  cv::Mat outMat = itk::OpenCVImageBridge::ITKImageToCVMat< ImageType >(baselineImage);

  // check results of itk::Image -> IplImage
  IplImage outMatAsIpl = outMat;
  double itkMatDiff = cvNorm(&outMatAsIpl, dataConvertedInIpl);
  if (itkMatDiff != 0.0)
    {
    std::cerr << "Images didn't match for pixel type "
      << typeid(PixelType).name()
      << " for ITK -> cv::Mat (scalar)" << std::endl;
    cvReleaseImage(&dataConvertedInIpl);
    cvReleaseImage(&inIpl);
    cvReleaseImage(&outIpl);
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

template< typename TPixel >
int itkRunScalarTest( char* argv )
{
  if (itkOpenCVImageBridgeTestTemplatedScalar< TPixel, 2 >(argv) == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }
  if (itkOpenCVImageBridgeTestTemplatedScalar< TPixel, 3 >(argv) == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}


//-----------------------------------------------------------------------------
// Main test
//
int itkOpenCVImageBridgeGrayScaleTest ( int argc, char *argv[] )
{
  //
  // Check arguments
  //
  if (argc != 4)
    {
    std::cerr << "Usage: " << argv[0] << "scalar_image1 scalar_image2 scalar_image3" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test for scalar types
  //
  // Note: We don't test signed char because ITK seems to have trouble reading
  //       images with char pixels.
  //
  std::cout << "\n================================" << std::endl;
  if( itkRunScalarTest< unsigned char >( argv[1] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< short >( argv[1] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< unsigned short >( argv[1] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< float >( argv[1] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< double >( argv[1] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  std::cout << "\n================================" << std::endl;
  std::cout << "scalar 513x512" << std::endl;
  if( itkRunScalarTest< unsigned char >( argv[2] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< short >( argv[2] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< unsigned short >( argv[2] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< float >( argv[2] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< double >( argv[2] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  std::cout << "\n================================" << std::endl;
  std::cout << "two-byte pixel image" << std::endl;
  if( itkRunScalarTest< unsigned short >( argv[3] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< float >( argv[3] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  if( itkRunScalarTest< double >( argv[3] ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
