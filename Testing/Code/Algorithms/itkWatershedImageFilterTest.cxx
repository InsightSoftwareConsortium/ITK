/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkWatershedImageFilterTest.cxx
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
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkWatershedImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkRelabelWatershedImageFilter.h"

inline void println(char *s) { std::cout << s << std::endl; }

int main()
{
  typedef itk::Image<float, 2> ImageType2D;
  
  println("Creating some images");
  itk::ImageRegion<2> Region2D;
  
  itk::Size<2>  size2D;
   size2D[0] = 314;
   size2D[1] = 314;
  
  itk::Index<2> orig2D;
   orig2D[0] = 0;
   orig2D[1] = 0;
   
  Region2D.SetSize(size2D);
  Region2D.SetIndex(orig2D);

  ImageType2D::Pointer image2D = ImageType2D::New();
   image2D->SetLargestPossibleRegion(Region2D);
   image2D->SetBufferedRegion(Region2D);
   image2D->SetRequestedRegion(Region2D);
   image2D->Allocate();

 itk::ImageRegionIterator<ImageType2D>
     it2D(image2D, image2D->GetRequestedRegion());  
  println("Initializing an image");
  float q = 0.00f;
  for (it2D = it2D.Begin(); it2D != it2D.End(); ++it2D)
    {
      it2D.Value() = ::sin(q);
      q = q + 0.10f;
    }

  println("Creating the watershed filter");
  itk::WatershedImageFilter<ImageType2D, ImageType2D>::Pointer ws_filter =
                  itk::WatershedImageFilter<ImageType2D, ImageType2D>::New();
  ws_filter->SetInput(image2D);
  ws_filter->SetThreshold(.05f);
  ws_filter->SetLevel(1.0f);

  println("Creating the output relabeler");
  itk::RelabelWatershedImageFilter<ImageType2D, ImageType2D>::Pointer
    ws_merger = itk::RelabelWatershedImageFilter<ImageType2D,
    ImageType2D>::New();
  ws_merger->SetLevel(0.5f);
  ws_merger->SetInput(ws_filter->GetBasicOutput());
  
  println("Executing the filter");
  ws_merger->Update();

  return 0;
}
