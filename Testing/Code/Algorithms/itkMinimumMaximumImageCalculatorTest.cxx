/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: 
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

#include "itkPhysicalImage.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkSize.h"


typedef itk::Size<3>                                  SizeType;
typedef itk::PhysicalImage<short, 3>                  ImageType;
typedef itk::MinimumMaximumImageCalculator<ImageType>  MinMaxCalculatorType;

/* Define the image size and physical coordinates */
SizeType size = {{20, 20, 20}};
double origin [3] = { 0.0, 0.0, 0.0};
double spacing[3] = { 1, 1 , 1};

int 
main(int argc,char *argv[])
{
    int flag = 0;           /* Did this test program work? */

    std::cout << "Testing Minimum and Maximum Image Calulator:\n";

    /* Allocate a simple test image */
    ImageType::Pointer image = ImageType::New();
    ImageType::RegionType region;
    region.SetSize(size);
    image->SetLargestPossibleRegion(region);
    image->SetRequestedRegion(region);
    image->SetBufferedRegion(region);
    image->Allocate();

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    short minimum = -52;
    short maximum = 103;


    /* Initialize the image contents with the minimum value*/
    itk::Index<3> index;
    for (int slice = 0; slice < 20; slice++) {
        index[2] = slice;
        for (int row = 0; row <20; row++) {
            index[1] = row;
            for (int col = 0; col < 20; col++) {
                index[0] = col;
                image->SetPixel(index, minimum);
            }
        }
    }

    /* Set voxel (10,10,10) to maximum value*/
    index[0] = 10;
    index[1] = 10;
    index[2] = 10;
    image->SetPixel(index, maximum);

    /* Create and initialize the calculator */
    MinMaxCalculatorType::Pointer calculator = MinMaxCalculatorType::New();
    calculator->SetImage(image);

    calculator->Compute();

    /* Return minimum of intensity */
    short minimumResult = calculator->GetMinimum();
    std::cout << "The Minimum intensity value is : " << minimumResult << std::endl;

    if(minimumResult != minimum)
    {
       std::cout << "Minimum Value is wrong : " << minimumResult ;
       std::cout << " != " << minimum << std::endl;
       flag = 1;
    }
  
    /* Return maximum of intensity */
    short maximumResult = calculator->GetMaximum();
    std::cout << "The Maximum intensity value is : " << maximumResult << std::endl;

    if(maximumResult != maximum)
    {
       std::cout << "Maximum Value is wrong : " << maximumResult ;
       std::cout << " != " << maximum << std::endl;
       flag = 2;
    }


    /* Return results of test */
    if (flag != 0) {
        std::cout << "*** Some tests failed" << std::endl;
        return flag; }
    else {
        std::cout << "All tests successfully passed" << std::endl;
        return 0; }

}
