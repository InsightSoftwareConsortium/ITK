/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInterpolateTest.cxx
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

#include <iostream>

#include "itkPhysicalImage.h"
#include "itkIndex.h"
#include "itkSize.h"

#include "itkLinearInterpolateImageFunction.h"


typedef itk::Size<3>                               SizeType;
typedef itk::PhysicalImage<unsigned short, 3>              ImageType;
typedef itk::LinearInterpolateImageFunction<ImageType>  InterpolatorType;
typedef InterpolatorType::IndexType                 IndexType;
typedef InterpolatorType::PointType                 PointType;

int
main(
    int argc,
    char *argv[])
{
    int flag = 0;           /* Did this test program work? */

    std::cout << "Testing image interpolation methods:\n";

    /* Define the image size and physical coordinates */
    SizeType size = {{20, 40, 80}};
    double origin [3] = { 0.5,   0.5,   0.5};
    double spacing[3] = { 0.1,   0.05 , 0.025};

    /* Allocate a simple test image */
    ImageType::Pointer image = ImageType::New();
    ImageType::RegionType region;
    region.SetSize(size);
    image->SetLargestPossibleRegion(region);
    image->SetBufferedRegion(region);
    image->Allocate();

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    /* Initialize the image contents */
    IndexType index;
    for (int slice = 0; slice < 80; slice++) {
        index[2] = slice;
        for (int row = 0; row < 40; row++) {
            index[1] = row;
            for (int col = 0; col < 20; col++) {
                index[0] = col;
                image->SetPixel(index, slice+row+col);
            }
        }
    }

    /* Create and initialize the interpolator */
    InterpolatorType::Pointer interp = InterpolatorType::New();
    interp->SetInputImage(image);

    // FIXME: Add trial evaluations near the border and outside the image

    /* Test evaluation at integer coordinates */
    std::cout << "Evaluate at integer coordinates: " << std::endl;
    IndexType idx = {{10, 20,40}};
    double value1 = interp->Evaluate(idx);
    std::cout << idx << value1 << std::endl;
    if (value1 != 70)  {
        std::cout << "*** Error: correct value is 70" << std::endl;
        flag = 1;
    }

    /* Test evaluation at image border */
    idx[0] = 0;
    value1 = interp->Evaluate(idx);
    std::cout << idx << value1 << std::endl;
    if (value1 != 60)
      {
      std::cout << "*** Error: correct value is 60" << std::endl;
      flag = 1;
      }

    idx[0] = 19;
    value1 = interp->Evaluate(idx);
    std::cout << idx << value1 << std::endl;
    if (value1 != 79)
      {
      std::cout << "*** Error: correct value is 79" << std::endl;
      flag = 1;
      }

    /* Test evaluation outside the image */
    idx[0] = 20;
    value1 = interp->Evaluate(idx);
    std::cout << idx << value1 << std::endl;
    if (value1 != 0)
      {
      std::cout << "*** Error: correct value is 0" << std::endl;
      flag = 1;
      }


    /* Test evaluation at non-integer coordinates */
    std::cout << "Evaluate at non-integer coordinates: ";
    double darray[3] = {5.25, 12.5, 42.0};
    PointType point(darray);
    double value2 = interp->Evaluate(point);
    std::cout << value2 << std::endl;
    if (value2 != 59.75) {
        std::cout << "*** Error: correct value is 59.75" << std::endl;
        flag = 1;
    }

    /* Return results of test */
    if (flag != 0) {
        std::cout << "*** Some test failed" << std::endl;
        return flag; }
    else {
        std::cout << "All tests successfully passed" << std::endl;
        return 0; }
}











