/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSCSLRealToComplexConjugateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSCSLRealToComplexConjugateImageFilter_txx
#define _itkSCSLRealToComplexConjugateImageFilter_txx

#ifdef USE_SCSL
#include "itkSCSLRealToComplexConjugateImageFilter.h"

#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"

#include <complex.h>
#include <scsl_fft.h>

namespace itk
{
#ifndef SCSL_INT_T
#define SCSL_INT_T int
#endif
template <class TPixel, unsigned int Dimension>
void
SCSLRealToComplexConjugateImageFilter<TPixel,Dimension>::
GenerateData()
{
  // get pointers to the input and output
  typename TInputImageType::ConstPointer  inputPtr  = this->GetInput();
  typename TOutputImageType::Pointer      outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  const typename TInputImageType::SizeType&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  unsigned int num_dims = inputPtr->GetImageDimension();

  if(num_dims != outputPtr->GetImageDimension())
    return;

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();
#define MAX2(a,b) ((a) > (b) ? (a) : (b))
#define MAX3(a,b,c)  MAX2(MAX2(a,b),c)
  unsigned dims[32];
      {
      int i;
      for(i = 0; i < Dimension; i++)
          {
          dims[i] = inputSize[i];
          }
      for(; i < 32; i++)
          {
          dims[i] = 1;
          }
      }

  std::complex<TPixel> *out = outputPtr->GetBufferPointer();
  /* return type valus are undocumented SCSL_INT_T rval;*/
  SCSL_INT_T isys[2]={1,0};
#define NF 256
#define NFR 256
  if(typeid(TPixel) == typeid(double))
    {
    switch(num_dims)
      {
      case 1:
      case 2:
        {
        double *table  = new double[(dims[0]+NFR) + (2 * dims[1] + NF)];
        double *work = new double[dims[0] + 4 * dims[1]];
        /*return type values are undocumented rval = */
          dzfft2d(0,            // sign
                  dims[0], // size of input x
                  dims[1], // size of input in y
                  1.0,     // scale
                  const_cast<double *>
                  (reinterpret_cast<const double *>
                   (inputPtr->GetBufferPointer())),
                  dims[0], // ldx -- number of rows in x array
                  reinterpret_cast<std::complex<double> *>(out),
                  dims[0]/2 + 1,
                  table,
                  work,
                  isys);
        /*return type values are undocumented rval = */
          dzfft2d(1,            // sign
                  dims[0], // size of input x
                  dims[1], //
                  1.0,     // scale
                  const_cast<double *>
                  (reinterpret_cast<const double *>
                   (inputPtr->GetBufferPointer())),
                  dims[0], //
                  reinterpret_cast<std::complex<double> *>(out),
                  dims[0]/2 + 1,
                  table,
                  work,
                  isys);
        delete [] table;
        delete [] work;
        }
        break;
      case 3:
        {
        double *table  = new double[(dims[0] + NFR) + (2 * dims[1] + NF) +
                                    (2 * dims[2] + NF)];
        double *work = new double[dims[0] + 3 * dims[2]];
        /*return type values are undocumented rval = */
          dzfft3d(0,       // sign
                  dims[0], // size of input
                  dims[1], // ""
                  dims[2], // ""
                  1.0,     // scale
                  const_cast<double *>
                  (reinterpret_cast<const double *>
                   (inputPtr->GetBufferPointer())), // input
                  dims[0], // ldx
                  dims[1], // ldx2
                  reinterpret_cast<std::complex<double> *>(out), // output
                  (dims[0]/2 + 1), // ldy
                  dims[2],      // ldy2
                  table,work,isys);
        /*return type values are undocumented rval = */
          dzfft3d(1,       // sign
                  dims[0], // size of input
                  dims[1], // ""
                  dims[2], // ""
                  1.0,     // scale
                  const_cast<double *>
                  (reinterpret_cast<const double *>
                   (inputPtr->GetBufferPointer())), // input
                  dims[0], // ldx
                  dims[1], // ldx2
                  reinterpret_cast<std::complex<double> *>(out), // output
                  (dims[0]/2 + 1), // ldy
                  dims[1],      // ldy2
                  table,work,isys);
        delete [] table;
        delete [] work;
        }
        break;
      default:
        break;
      }
    }
  else if(typeid(TPixel) == typeid(float))
    {
    switch(num_dims)
      {
      case 1:
      case 2:
        {
        float *table  = new float[(dims[0]+NFR) + (2 * dims[1] + NF)];
        float *work = new float[dims[0] + 4 * dims[1]];
        /*return type values are undocumented rval = */
          scfft2d(0,            // sign
                  dims[0], // size of input x
                  dims[1], // size of input in y
                  1.0,     // scale
                  const_cast<float *>
                  (reinterpret_cast<const float *>
                   (inputPtr->GetBufferPointer())),
                  dims[0], // ldx -- number of rows in x array
                  reinterpret_cast<std::complex<float> *>(out),
                  dims[0]/2 + 1,
                  table,
                  work,
                  isys);
        /*return type values are undocumented rval = */
          scfft2d(1,            // sign
                  dims[0], // size of input x
                  dims[1], //
                  1.0,     // scale
                  const_cast<float *>
                  (reinterpret_cast<const float *>
                   (inputPtr->GetBufferPointer())),
                  dims[0], //
                  reinterpret_cast<std::complex<float> *>(out),
                  dims[0]/2 + 1,
                  table,
                  work,
                  isys);
        delete [] table;
        delete [] work;
        }
        break;
      case 3:
        {
        float *table  = new float[(dims[0] + NFR) + (2 * dims[1] + NF) +
                                    (2 * dims[2] + NF)];
        float *work = new float[dims[0] + 3 * dims[2]];
        /*return type values are undocumented rval = */
          scfft3d(0,       // sign
                  dims[0], // size of input
                  dims[1], // ""
                  dims[2], // ""
                  1.0,     // scale
                  const_cast<float *>
                  (reinterpret_cast<const float *>
                   (inputPtr->GetBufferPointer())), // input
                  dims[0], // ldx
                  dims[1], // ldx2
                  reinterpret_cast<std::complex<float> *>(out), // output
                  (dims[0]/2 + 1), // ldy
                  dims[1],      // ldy2
                  table,work,isys);
        /*return type values are undocumented rval = */
          scfft3d(1,       // sign
                  dims[0], // size of input
                  dims[1], // ""
                  dims[2], // ""
                  1.0,     // scale
                  const_cast<float *>
                  (reinterpret_cast<const float *>
                   (inputPtr->GetBufferPointer())), // input
                  dims[0], // ldx
                  dims[1], // ldx2
                  reinterpret_cast<std::complex<float> *>(out), // output
                  (dims[0]/2 + 1), // ldy
                  dims[1],      // ldy2
                  table,work,isys);
        delete [] table;
        delete [] work;
        }
        break;
      default:
        break;
      }
    }
}
  template <class TPixel, unsigned int Dimension>
  bool
  SCSLRealToComplexConjugateImageFilter<TPixel,Dimension>::
  FullMatrix()
  {
    return false;
  }

template <class TPixel, unsigned int Dimension>
void
SCSLRealToComplexConjugateImageFilter<TPixel,Dimension>::
PrintSelf(std::ostream& os,Indent indent) const
{
}
}

#endif
#endif
