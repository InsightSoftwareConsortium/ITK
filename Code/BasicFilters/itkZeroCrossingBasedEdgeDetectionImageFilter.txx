/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroCrossingBasedEdgeDetectionImageFilter.txx
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

#ifndef _itkZeroCrossingBasedEdgeDetectionImageFilter_txx
#define _itkZeroCrossingBasedEdgeDetectionImageFilter_txx


#include "itkZeroCrossingBasedEdgeDetectionImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkZeroCrossingImageFilter.h"

namespace itk
{


  /*
template <class TInputImage, class TOutputImage>
void 
ZeroCrossingBasedEdgeDetectionImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size

    unsigned long radius = 1;

   
  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( radius );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()) )
    {
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    std::ostrstream msg;
    msg << (char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()" << std::ends;
    e.SetLocation(msg.str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

*/

template< class TInputImage, class TOutputImage >
void
ZeroCrossingBasedEdgeDetectionImageFilter< TInputImage, TOutputImage >
::GenerateData( )
{

  //  cout<<"being generating data"<<endl;
  unsigned int i;

  typename  InputImageType::Pointer input  = this->GetInput();

  
  //create the filters that are needed
  DiscreteGaussianImageFilter<TInputImage, TOutputImage>::Pointer gaussianFilter
    = DiscreteGaussianImageFilter<TInputImage, TOutputImage>::New();
  LaplacianImageFilter<TInputImage, TOutputImage>::Pointer  laplacianFilter 
    =  LaplacianImageFilter<TInputImage, TOutputImage>::New();
  ZeroCrossingImageFilter<TInputImage, TOutputImage>:: Pointer zerocrossingFilter = ZeroCrossingImageFilter<TInputImage,TOutputImage>::New();

  //Construct the mini-pipeline
  
  //First apply the Gaussian filter
  gaussianFilter->SetVariance(m_Variance);
  gaussianFilter->SetMaximumError(m_MaximumError);
  gaussianFilter->SetInput(input);

  // calculate the laplacian of the smoothed image
  laplacianFilter->SetInput(gaussianFilter->GetOutput());
  laplacianFilter->Update();

  //Find the zero-crossings of the laplacian
  zerocrossingFilter->SetInput(laplacianFilter->GetOutput());
  zerocrossingFilter->Update();

  // graft the output of the mini-pipeline back onto the filter's output.
  // this copies back the region ivars and meta-data
  
  this->GraftOutput(zerocrossingFilter->GetOutput());

}



  





}//end of itk namespace



#endif
