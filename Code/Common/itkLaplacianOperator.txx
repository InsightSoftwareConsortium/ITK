/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianOperator.txx
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
#ifndef _itkLaplacianOperator_txx
#define _itkLaplacianOperator_txx


namespace itk
{

  //Create the operator
template <class TPixel, unsigned int VDimension, class TAllocator>
void
LaplacianOperator<TPixel, VDimension, TAllocator>
::CreateOperator()
{
  CoefficientVector coefficients;
  
  coefficients = this->GenerateCoefficients();

  this->Fill(coefficients);
  
}


  //This function fills the coefficients into the corresponding neighborhodd.
template <class TPixel, unsigned int VDimension, class TAllocator>
void  
LaplacianOperator <TPixel, VDimension, TAllocator>
::Fill(const CoefficientVector &coeff)
{

  Superclass::CoefficientVector::const_iterator it;

  std::slice* temp_slice;
  temp_slice = new std::slice(0, coeff.size(),1);
  
  Self::SliceIteratorType data(this, *temp_slice);
  delete temp_slice;
 
  it = coeff.begin();

  // Copy the coefficients into the neighborhood
  for (data = data.Begin(); data < data.End(); ++data, ++it)
    {
      *data = *it;
    }

}



template <class TPixel, unsigned int VDimension, class TAllocator>
typename LaplacianOperator<TPixel, VDimension, TAllocator>
::CoefficientVector
LaplacianOperator<TPixel, VDimension, TAllocator>
::GenerateCoefficients()
{
  unsigned int i;
  unsigned int w = 1;

  for(i = 0; i < VDimension; i ++)
    {
      w = w*3;
    }

  std::vector<PixelType> coeff(w);
  CoefficientVector coeffP(w);
  int offset[2 * VDimension];

  // Here we set the radius to 1's, here the
  // operator is 3x3 for 2D, 3x3x3 for 3D.

  unsigned long k[VDimension];

  for (unsigned int i = 0; i<VDimension; ++i)
    {
      k[i] = 1;
    }

  this->SetRadius(k);
  
  for ( i = 0 ; i < 2 * VDimension; i+= 2)
    {
      offset[i] = - this->GetStride(i/2);
      offset[i+1] = this->GetStride(i/2);
    }

  //Set the coefficients 
  coeff[w/2] = -2.0 * VDimension;
  
  for (i = 0 ; i < 2 * VDimension; i++)
    {
      coeff[w/2 + offset[i]] = 1.0;
    } 

  for ( i = 0; i < w; i ++)
    {
      coeffP[i] = coeff[i]/(2.0 *VDimension) ;
    }

    return coeffP;
    
}

} // namespace itk

#endif
