/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToParametricSpaceFilter.txx
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
#ifndef _itkImageToParametricSpaceFilter_txx
#define _itkImageToParametricSpaceFilter_txx


#include "itkImageToParametricSpaceFilter.h"
#include "itkNumericTraits.h"
#include "itkImageIterator.h"


namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputMesh>
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::ImageToParametricSpaceFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(PointDimension);
}

/**
 *
 */
template <class TInputImage, class TOutputMesh>
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::~ImageToParametricSpaceFilter()
{
}
  


/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::GenerateOutputInformation()
{
  OutputMeshPointer       mesh    = this->GetOutput();
  PointsContainerPointer  points  = mesh->GetPoints();
  InputImagePointer       image   = this->GetInput(0);

  points->Reserve( image->GetRequestedRegion().GetNumberOfPixels() );
}




/**
 *
 */
template <class TInputImage, class TOutputMesh>
void 
ImageToParametricSpaceFilter<TInputImage,TOutputMesh>
::GenerateData(void)
{
  OutputMeshPointer       mesh    = this->GetOutput();
  PointsContainerPointer  points  = mesh->GetPoints();
  InputImagePointer       image   = this->GetInput(0);
  InputImageRegionType    region  = image->GetRequestedRegion();

  unsigned long numberOfPixels    = region.GetNumberOfPixels();

  points->Reserve( numberOfPixels );

  // support progress methods/callbacks
  unsigned long visitPeriod  = 100;
  unsigned long updateVisits = numberOfPixels / visitPeriod;
  unsigned long visitCounter = 0;
 
  for( unsigned int component=0; component<PointDimension; component++)
    {

    image = this->GetInput( component );
    InputImageIterator it( image, image->GetRequestedRegion() );
    
    PointsContainerIterator   point   = points->Begin();

    it.GoToBegin();
    while( !it.IsAtEnd() ) 
      {
        if( visitCounter == updateVisits )
          {
            visitCounter = 0;
            this->UpdateProgress( static_cast<float>( visitCounter ) /
                                  static_cast<float>( updateVisits * visitPeriod ) );
          }
        (point.Value())[ component ] = it.Get();
        ++it;
        ++point;
        ++visitCounter;
      }
    }

}



} // end namespace itk

#endif
