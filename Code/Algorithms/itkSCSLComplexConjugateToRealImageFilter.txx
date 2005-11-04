/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSCSLComplexConjugateToRealImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSCSLComplexConjugateToRealImageFilter_txx
#define _itkSCSLComplexConjugateToRealImageFilter_txx

#ifdef USE_SCSL
#include "itkSCSLComplexConjugateToRealImageFilter.h"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
namespace itk
{
#ifndef SCSL_INT_T
#define SCSL_INT_T int
#endif

template <typename TPixel, unsigned int Dimension>
void
SCSLComplexConjugateToRealImageFilter<TPixel,Dimension>::
GenerateData()
{
  // get pointers to the input and output
  typename TInputImageType::ConstPointer  inputPtr  = this->GetInput();
  typename TOutputImageType::Pointer      outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  const typename TInputImageType::SizeType&   outputSize
    = outputPtr->GetLargestPossibleRegion().GetSize();
  unsigned int num_dims = outputPtr->GetImageDimension();

  if(num_dims != outputPtr->GetImageDimension())
    return;

  unsigned int total_size  = 1;
  unsigned int tmp_size = 1;
  unsigned dims[32];
      {
      int i;
      for(i = 0; i < Dimension; i++)
          {
          dims[i] = outputSize[i];
          total_size *= dims[i];
          tmp_size *= (i == 0 ? (dims[i]+2) : dims[i]);
          }
      for(; i < 32; i++)
          {
          dims[i] = 1;
          }
      }

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  std::complex<TPixel> *in = const_cast<std::complex<TPixel> *>
    (inputPtr->GetBufferPointer());
#define MAX2(a,b) ((a) > (b) ? (a) : (b))
#define MAX3(a,b,c)  MAX2(MAX2(a,b),c)
  /* return type values are undocumented SCSL_INT_T rval; */
  SCSL_INT_T isys[2] = {1,0};
#define NF 256
#define NFR 256
  if(typeid(TPixel) == typeid(double))
    {
    double *out = reinterpret_cast<double *>(outputPtr->GetBufferPointer());
    double *tmp_buf = reinterpret_cast<double *>(new TPixel[tmp_size]);
    switch(num_dims)
      {
      case 1:
      case 2:
        {
        double *table  = new double[(dims[0]+NFR) + (2 * dims[1] + NF)];
        double *work = new double[dims[0] + 4 * dims[1]];

        /*return type values are undocumented rval = */zdfft2d(0,       // sign
                       dims[0], // size of input x
                       dims[1], // "" y
                       1.0,     // scale
                       const_cast<std::complex<double> *>
                       (reinterpret_cast  // input
                       <const std::complex<double> *>
                        (inputPtr->GetBufferPointer())),
                       dims[0]/2 + 1, // ldx
                       tmp_buf, // output
                       dims[0] + 2,
                       table, work,isys);
        /*return type values are undocumented rval = */zdfft2d(-1,       // sign
                       dims[0], // size of input x
                       dims[1], // "" y
                       1.0,     // scale
                       const_cast<std::complex<double> *>
                       (reinterpret_cast  // input
                       <const std::complex<double> *>
                        (inputPtr->GetBufferPointer())),
                       dims[0]/2 + 1, // ldx
                       tmp_buf, // output
                       dims[0] + 2,
                       table, work,isys);
        delete [] table;
        delete [] work;
        }
        break;
      case 3:
        {
        double *work = new double[dims[1] + 4 * dims[2]];
        double *table  = new double[(dims[0] + 256) +
                                    (2 * dims[1] + 256) +
                                    (2 * dims[2] + 256)];
        /*return type values are undocumented rval = */zdfft3d(0,       // sign
                       dims[0], // size of input
                       dims[1], // ""
                       dims[2], // ""
                       1.0,     // scale
                       const_cast<std::complex<double> *>
                       (reinterpret_cast  // input
                       <const std::complex<double> *>
                        (inputPtr->GetBufferPointer())),
                       (dims[0]/2+1),
                       dims[1],
                       tmp_buf,
                       dims[0]+2,
                       dims[1],
                       table,work,isys);
        /*return type values are undocumented rval = */zdfft3d(-1,       // sign
                       dims[0], // size of input
                       dims[1], // ""
                       dims[2], // ""
                       1.0,     // scale
                       const_cast<std::complex<double> *>
                       (reinterpret_cast  // input
                       <const std::complex<double> *>
                        (inputPtr->GetBufferPointer())),
                       (dims[0]/2+1),
                       dims[1],
                       tmp_buf,
                       dims[0]+2,
                       dims[1],
                       table,work,isys);
        delete [] table;
        delete [] work;
        }
        break;
      default:
        break;
      }
    for(int k = 0; k < dims[2]; k++)
      {
      for(int j = 0; j < dims[1]; j++)
        {
        for(int i = 0; i < dims[0]; i++)
          out[i + (dims[0] * j) + ((dims[0] * dims[1]) * k)] =
#if 1
            tmp_buf[i + ((dims[0]+2)*j) + (((dims[0]+2) * dims[1]) * k)];
#else
            tmp_buf[i + (dims[0]*j) + ((dims[0] * dims[1]) * k)];

#endif
        }
      }
    delete [] tmp_buf;
    }
  else if(typeid(TPixel) == typeid(float))
    {
    float *out = reinterpret_cast<float *>(outputPtr->GetBufferPointer());
    float *tmp_buf = reinterpret_cast<float *>(new TPixel[tmp_size]);
    switch(num_dims)
      {
      case 1:
      case 2:
        {
        float *table  = new float[(dims[0]+NFR) + (2 * dims[1] + NF)];
        float *work = new float[dims[0] + 4 * dims[1]];

        /*return type values are undocumented rval = */csfft2d(0,       // sign
                       dims[0], // size of input x
                       dims[1], // "" y
                       1.0,     // scale
                       const_cast<std::complex<float> *>
                       (reinterpret_cast  // input
                       <const std::complex<float> *>
                        (inputPtr->GetBufferPointer())),
                       dims[0]/2 + 1, // ldx
                       tmp_buf, // output
                       dims[0] + 2,
                       table, work,isys);
        /*return type values are undocumented rval = */csfft2d(-1,       // sign
                       dims[0], // size of input x
                       dims[1], // "" y
                       1.0,     // scale
                       const_cast<std::complex<float> *>
                       (reinterpret_cast  // input
                       <const std::complex<float> *>
                        (inputPtr->GetBufferPointer())),
                       dims[0]/2 + 1, // ldx
                       tmp_buf, // output
                       dims[0] + 2,
                       table, work,isys);
        delete [] table;
        delete [] work;
        }
        break;
      case 3:
        {
        float *work = new float[dims[1] + 4 * dims[2]];
        float *table  = new float[(dims[0] + 256) +
                                    (2 * dims[1] + 256) +
                                    (2 * dims[2] + 256)];
        /*return type values are undocumented rval = */csfft3d(0,       // sign
                       dims[0], // size of input
                       dims[1], // ""
                       dims[2], // ""
                       1.0,     // scale
                       const_cast<std::complex<float> *>
                       (reinterpret_cast  // input
                       <const std::complex<float> *>
                        (inputPtr->GetBufferPointer())),
                       (dims[0]/2+1),
                       dims[1],
                       tmp_buf,
                       dims[0]+2,
                       dims[1],
                       table,work,isys);
        /*return type values are undocumented rval = */csfft3d(-1,       // sign
                       dims[0], // size of input
                       dims[1], // ""
                       dims[2], // ""
                       1.0,     // scale
                       const_cast<std::complex<float> *>
                       (reinterpret_cast  // input
                       <const std::complex<float> *>
                        (inputPtr->GetBufferPointer())),
                       (dims[0]/2+1),
                       dims[1],
                       tmp_buf,
                       dims[0]+2,
                       dims[1],
                       table,work,isys);
        delete [] table;
        delete [] work;
        }
        break;
      default:
        break;
      }
    for(int k = 0; k < dims[2]; k++)
      {
      for(int j = 0; j < dims[1]; j++)
        {
        for(int i = 0; i < dims[0]; i++)
          out[i + (dims[0] * j) + ((dims[0] * dims[1]) * k)] =
#if 1
            tmp_buf[i + ((dims[0]+2)*j) + (((dims[0]+2) * dims[1]) * k)];
#else
            tmp_buf[i + (dims[0]*j) + ((dims[0] * dims[1]) * k)];

#endif
        }
      }
    delete [] tmp_buf;
    }

  ImageRegionIterator<TOutputImageType> it(outputPtr,
                                           outputPtr->GetLargestPossibleRegion());
  while(!it.IsAtEnd())
    {
    it.Set(it.Value() / total_size);
    ++it;
    }
}
  template <class TPixel, unsigned int Dimension>
  bool
  SCSLComplexConjugateToRealImageFilter<TPixel,Dimension>::
  FullMatrix()
  {
    return false;
  }


template <typename TPixel, unsigned int Dimension>
void
SCSLComplexConjugateToRealImageFilter<TPixel,Dimension>::
PrintSelf(std::ostream& os,Indent indent) const
{
}
}
#endif
#endif
