/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConvertBufferTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkConvertPixelBuffer.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkImage.h"
#include <iostream>

int itkConvertBufferTest(int, char* [])
{ 
  unsigned int k;
  int piInit[3] = {3,1,4};
  itk::RGBPixel<int> pi = piInit;
  int piaInit[4] = {3,1,4,1};
  itk::RGBAPixel<int> pia = piaInit;
  std::cerr << "RGBPixel<int>: " <<  pi << "\n";
  std::cerr << "RGBAPixel<int>: " << pia << "\n";

  int paInit0[4] = {1,1,1,0};
  int paInit1[4] = {2,2,2,2};
  int paInit2[4] = {3,3,3,4};
  itk::RGBAPixel<int> pa[3];
  pa[0] = paInit0;
  pa[1] = paInit1;
  pa[2] = paInit2;
  int ipa[] = {1, 2, 3};
  itk::RGBPixel<int> p[3];
  // convert from int to RGB<int>
  itk::ConvertPixelBuffer<int, itk::RGBPixel<int>, 
    itk::DefaultConvertPixelTraits<itk::RGBPixel<int> > >::
    Convert(ipa, 1, p, 3);
  std::cerr << "RGB 111 222 333 = ";
  for(int j=0; j < 3; ++j)
    {
    std::cerr << p[j] << ", ";
    }
  std::cerr << "\n";
  int ipa3com[] = {1,1,1, 2,2,2, 3,3,3};
  itk::RGBPixel<float> pf[3];
  // convert from int[3] to RGB<float>
  itk::ConvertPixelBuffer<int, itk::RGBPixel<float>, 
    itk::DefaultConvertPixelTraits<itk::RGBPixel<float> > >::
    Convert(ipa3com, 3, pf, 3);
  std::cerr << "itk::RGBPixel<float> array converted from int\n";
  for( k = 0; k < sizeof(p) / sizeof(itk::RGBPixel<float>); ++k)
    {
    std::cerr << pf[k] << " ";
    }
  std::cerr << "\n";
  std::cerr << "itk::RGBAPixel<int> array \n";
  for( k = 0; k < sizeof(pa) / sizeof(itk::RGBAPixel<int>); ++k)
    {
    std::cerr << pa[k] << " ";
    }
  std::cerr << "\n";
  
  // create an initial array of floats
  float farray[] = {1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.4f, 7.4f, 8.8f, 9.9f  };
  // set the size of the array in number of elements
  const int arraySize = sizeof(farray)/sizeof(farray[0]);
  double darray[arraySize];     // create a double array 
  int iarray[arraySize];        // create an int array 
  // convert the float array to a double array
  itk::ConvertPixelBuffer<float, double,
    itk::DefaultConvertPixelTraits<double> >::
    Convert(farray, 1, darray, arraySize);
  // convert a float array to an int array
  itk::ConvertPixelBuffer<float, int, 
    itk::DefaultConvertPixelTraits<int> >::
    Convert(farray, 1, iarray, arraySize);
  // convert the int array to the float array
  itk::ConvertPixelBuffer<int, float, 
    itk::DefaultConvertPixelTraits<float> >::
    Convert(iarray, 1, farray, arraySize);
  // print out all arrays after conversion
  int i = 0;
  std::cerr << "int array   : ";
  for(i =0; i < arraySize; ++i)
    {
    std::cerr << iarray[i] << " " ;
    }
  std::cerr << "\nfloat array  : ";
  for(i =0; i < arraySize; ++i)
    {
    std::cerr << farray[i] << " " ;
    }
  std::cerr << "\ndouble array : ";
  for(i =0; i < arraySize; ++i)
    {
    std::cerr << darray[i] << " " ;
    }
  std::cerr << "\n";
  
  
  typedef itk::Image<unsigned char, 2> ushort3Image;
  itk::ImageFileReader<ushort3Image>::Pointer reader 
    = itk::ImageFileReader<ushort3Image>::New();
        return 0;
}


