/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHybridFilter.txx
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
#ifndef _itkHybridFilter_txx
#define _itkHybridFilter_txx
#include "itkHybridFilter.h"

namespace itk
{


/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, 
  class TInputMesh, class TOutputMesh>
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::HybridFilter()
{
  m_IterNum = 0;
}



/**
 * Set the balloon force filter
 */
template <class TInputImage, class TOutputImage, 
  class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetBalloonForceFilter( BalloonForceFilterPointer  bffilter )
{
  m_BalloonForceFilter = bffilter;
}

/**
 * Set the gibbs prior filter
 */
template <class TInputImage, class TOutputImage, 
  class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetGibbsPriorFilter( GibbsPriorFilterPointer  gpfilter )
{
  m_GibbsPriorFilter = gpfilter;
}

/**
 * Set the gibbs prior filter input
 */
template <class TInputImage, class TOutputImage, 
  class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetGibbsInput()
{
  const typename TInputImage::Pointer   inputImage(    GetInput()   );
  m_GibbsPriorFilter->SetInput(inputImage);
}

/**
 * Send balloon force filter a new potential from the gibbs prior model
 */
template <class TInputImage, class TOutputImage, 
  class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetPotential( void )
{
  m_BalloonForceFilter->SetPotential(m_GibbsPriorFilter->GetOutput());
}

/**
 * Send balloon force filter a new potential from the gibbs prior model
 */
template <class TInputImage, class TOutputImage, 
  class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::SetObjectRegion( void )
{
  m_GibbsPriorFilter->SetTrainingImage(m_BalloonForceFilter->GetImageOutput());
}

/**
 * One iteration of gibbs prior model and the deformable model
 */
template <class TInputImage, class TOutputImage, 
  class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::Advance( void )
{
  if (m_IterNum != 0) m_GibbsPriorFilter->Modified();
  m_GibbsPriorFilter->Update();
  if (m_IterNum != 0) m_BalloonForceFilter->Modified();
  m_BalloonForceFilter->Update();
}

/**
 * Compute the output image
 */
template <class TInputImage, class TOutputImage, 
  class TInputMesh, class TOutputMesh>
void
HybridFilter<TInputImage,TOutputImage,TInputMesh,TOutputMesh>
::GenerateData()
{
  std::cout << "Hi, HybridFilter generating data ";
  std::cout << std::endl;

  const typename TInputImage::Pointer   inputImage(    GetInput()   );
        typename TOutputImage::Pointer  outputImage(   GetOutput()  );
        typename TOutputImage::Pointer  outputGp = m_GibbsPriorFilter->GetOutput();

  outputImage->SetLargestPossibleRegion( 
      inputImage->GetLargestPossibleRegion() );

  outputImage->SetBufferedRegion( 
      inputImage->GetBufferedRegion() );

  outputImage->SetRequestedRegion( 
      inputImage->GetRequestedRegion() );

  outputImage->Allocate();
  OutputImageIterator outit(outputImage,
    inputImage->GetRequestedRegion());

  while (m_IterNum != 5) 
    {
    Advance();
    m_IterNum++;
    }

  OutputImageIterator gpit(outputGp, outputGp->GetBufferedRegion());

  while( !gpit.IsAtEnd() )
    {
    outit.Set(gpit.Get());
    ++outit;
    ++gpit;
    }
}


} // end namespace itk

#endif
