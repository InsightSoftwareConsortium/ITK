// this file defines the Iterator Examples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(ImageLinearIteratorWithIndexTest);
  REGISTER_TEST(ImageRandomConstIteratorWithIndexTest);
  REGISTER_TEST(ImageRegionIteratorTest);
  REGISTER_TEST(ImageRegionIteratorWithIndexTest);
  REGISTER_TEST(ImageSliceIteratorWithIndexTest);
}

#undef main
#define main ImageLinearIteratorWithIndexTest
#include "ImageLinearIteratorWithIndex.cxx"

#undef main
#define main ImageRandomConstIteratorWithIndexTest
#include "ImageRandomConstIteratorWithIndex.cxx"

#undef main
#define main ImageRegionIteratorTest
#include "ImageRegionIterator.cxx"

#undef main
#define main ImageRegionIteratorWithIndexTest
#include "ImageRegionIteratorWithIndex.cxx"

#undef main
#define main ImageSliceIteratorWithIndexTest
#include "ImageSliceIteratorWithIndex.cxx"

