// this file defines the ImageExamples for the test driver
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(Image1Test);
  REGISTER_TEST(Image2Test);
  REGISTER_TEST(Image3Test);
  REGISTER_TEST(Image4Test);
  REGISTER_TEST(Image5Test);
  REGISTER_TEST(ImageAdaptor1Test);
  REGISTER_TEST(ImageAdaptor2Test);
  REGISTER_TEST(ImageAdaptor3Test);
  REGISTER_TEST(ImageAdaptor4Test);
  REGISTER_TEST(RGBImageTest);
  REGISTER_TEST(VectorImageTest);
}

#undef main
#define main Image1Test
#include "Image1.cxx"

#undef main
#define main Image2Test
#include "Image2.cxx"

#undef main
#define main Image3Test
#include "Image3.cxx"

#undef main
#define main Image4Test
#include "Image4.cxx"

#undef main
#define main Image5Test
#include "Image5.cxx"

#undef main
#define main ImageAdaptor1Test
#include "ImageAdaptor1.cxx"

#undef main
#define main ImageAdaptor2Test
#include "ImageAdaptor2.cxx"

#undef main
#define main ImageAdaptor3Test
#include "ImageAdaptor3.cxx"

#undef main
#define main ImageAdaptor4Test
#include "ImageAdaptor4.cxx"

#undef main
#define main RGBImageTest
#include "RGBImage.cxx"

#undef main
#define main VectorImageTest
#include "VectorImage.cxx"
