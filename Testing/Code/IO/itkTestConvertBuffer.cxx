/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestConvertBuffer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkConvertPixelBuffer.h"
#include "itkDefaultConvertPixelTraits.h"
#include <iostream>

int main()
{ 
  int k;
  itk::RGBPixel<int> pi = {3, 1, 4};
  itk::RGBAPixel<int> pia = {3, 1, 4, 1};
  std::cerr << "RGBPixel<int>: " <<  pi << "\n";
  std::cerr << "RGBAPixel<int>: " << pia << "\n";

  itk::RGBAPixel<int> pa[] = {{1,1,1,0}, {2,2,2,2}, {3,3,3,4}};
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


