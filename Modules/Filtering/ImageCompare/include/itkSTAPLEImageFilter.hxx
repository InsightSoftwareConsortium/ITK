/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSTAPLEImageFilter_hxx
#define itkSTAPLEImageFilter_hxx
#include "itkSTAPLEImageFilter.h"

#include "itkImageScanlineIterator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
STAPLEImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_MaximumIterations = " << m_MaximumIterations << std::endl;
  os << indent << "m_ForegroundValue = " << m_ForegroundValue << std::endl;
  os << indent << "m_ConfidenceWeight = " << m_ConfidenceWeight << std::endl;
  os << indent << "m_ElapsedIterations = " << m_ElapsedIterations << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
STAPLEImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  const double epsilon = 1.0e-10;

  typedef ImageScanlineConstIterator< TInputImage > IteratorType;
  typedef ImageScanlineIterator< TOutputImage >     FuzzyIteratorType;

  const double min_rms_error = 1.0e-14; // 7 digits of precision

  unsigned int i, iter;
  ProcessObject::DataObjectPointerArraySizeType number_of_input_files;

  // Allocate the output "fuzzy" image.
  this->GetOutput()->SetBufferedRegion( this->GetOutput()->GetRequestedRegion() );
  this->GetOutput()->Allocate();
  typename TOutputImage::Pointer W = this->GetOutput();

  // Initialize the output to all 0's
  W->FillBuffer( 0.0 );

  // Record the number of input files.
  number_of_input_files = this->GetNumberOfIndexedInputs();

  IteratorType *D_it = new IteratorType[number_of_input_files];

  double *p = new double[number_of_input_files];  // sensitivity
  double *q = new double[number_of_input_files];  // specificity

  double *last_q = new double[number_of_input_files];
  double *last_p = new double[number_of_input_files];

  for ( i = 0; i < number_of_input_files; ++i )
    {
    last_p[i] = -10.0;
    last_q[i] = -10.0;
    }

  // Come up with an initial Wi which is simply the average of
  // all the segmentations.
  IteratorType in;
  FuzzyIteratorType out;
  for ( i = 0; i < number_of_input_files; ++i )
    {
    if ( this->GetInput(i)->GetRequestedRegion() != W->GetRequestedRegion() )
      {
      itkExceptionMacro(<< "One or more input images do not contain matching RequestedRegions");
      }

    in  = IteratorType( this->GetInput(i), W->GetRequestedRegion() );
    out = FuzzyIteratorType( W, W->GetRequestedRegion() );

    while ( !in.IsAtEnd() )
      {
      while ( !in.IsAtEndOfLine() )
        {
        if ( in.Get() > m_ForegroundValue - epsilon && in.Get()
             < m_ForegroundValue + epsilon )
          {
          out.Set(out.Get() + 1.0);
          }
        ++in;
        ++out;
        } // end scanline
      in.NextLine();
      out.NextLine();
      }  // end while
    }

  // Divide sum by num of files, calculate the estimate of g_t
  double N = 0.0;
  double g_t = 0.0;
  out.GoToBegin();
  while ( !out.IsAtEnd() )
    {
    while ( !out.IsAtEndOfLine() )
      {
      out.Set( out.Get() / static_cast< double >( number_of_input_files ) );
      g_t += out.Get();
      N = N + 1.0;
      ++out;
      } // end of scanline
    out.NextLine();
    }
  g_t = ( g_t / N ) * m_ConfidenceWeight;

  double p_num, p_denom, q_num, q_denom;

  for ( iter = 0; iter < m_MaximumIterations; ++iter )
    {
    // Now iterate on estimating specificity and sensitivity
    for ( i = 0; i < number_of_input_files; ++i )
      {
      in = IteratorType( this->GetInput(i), W->GetRequestedRegion() );
      out = FuzzyIteratorType( W, W->GetRequestedRegion() );

      p_num = p_denom = q_num = q_denom = 0.0;

      // Sensitivity and specificity of this user
      while ( !in.IsAtEnd() )
        {
        while ( !in.IsAtEndOfLine() )
          {
          if ( in.Get() > m_ForegroundValue - epsilon
               && in.Get() < m_ForegroundValue + epsilon ) // Dij == 1
            {
            p_num += out.Get(); // out.Get() := Wi
            }
          else //        if (in.Get() != m_ForegroundValue) // Dij == 0
            {
            q_num += ( 1.0 - out.Get() ); // out.Get() := Wi
            }
          p_denom += out.Get();
          q_denom += ( 1.0 - out.Get() );
          ++in;
          ++out;
          } // end of scanline
        in.NextLine();
        out.NextLine();
        }

      p[i] = p_num / p_denom;
      q[i] = q_num / q_denom;
      }

    // Now recreate W using the new p's and q's
    // Need an iterator on each D
    // const double g_t = 0.1;  // prior likelihood that a pixel is incl.in
    // segmentation
    double alpha1, beta1;

    for ( i = 0; i < number_of_input_files; ++i )
      {
      D_it[i] = IteratorType( this->GetInput(i), W->GetRequestedRegion() );
      }

    out = FuzzyIteratorType( W, W->GetRequestedRegion() );

    out.GoToBegin();
    while ( !out.IsAtEnd())
      {
      while ( !out.IsAtEndOfLine() )
        {
        alpha1 = beta1 = 1.0;
        for ( i = 0; i < number_of_input_files; ++i )
          {
          if ( D_it[i].Get() > m_ForegroundValue - epsilon && D_it[i].Get() < m_ForegroundValue + epsilon )
            // Dij == 1
            {
            alpha1 = alpha1 * p[i];
            beta1  = beta1 * ( 1.0 - q[i] );
            }
          else //Dij == 0
            {
            alpha1 = alpha1 * ( 1.0 - p[i] );
            beta1  = beta1  * q[i];
            }
          ++D_it[i];
          }
        out.Set( g_t * alpha1
                 / ( g_t * alpha1  + ( 1.0 - g_t ) * beta1 ) );
        ++out;
        } // end scanline
      for ( i = 0; i < number_of_input_files; ++i )
        {
        D_it[i].NextLine();
        }
      out.NextLine();
      }

    this->InvokeEvent( IterationEvent() );

    // Check for convergence
    bool flag = false;
    if ( iter != 0 )  // not on the first iteration
      {
      flag = true;
      for ( i = 0; i < number_of_input_files; ++i )
        {
        if ( ( ( p[i] - last_p[i] ) * ( p[i] - last_p[i] ) ) > min_rms_error )
          {
          flag = false;
          break;
          }
        if ( ( ( q[i] - last_q[i] ) * ( q[i] - last_q[i] ) ) > min_rms_error )
          {
          flag = false;
          break;
          }
        }
      }
    for ( i = 0; i < number_of_input_files; ++i )
      {
      last_p[i] = p[i];
      last_q[i] = q[i];
      }

    if ( this->GetAbortGenerateData() )
      {
      this->ResetPipeline();
      flag = true;
      }

    if ( flag == true )
      {
      break;
      }
    }

  // Copy p's, q's, etc. to member variables

  m_Sensitivity.clear();
  m_Specificity.clear();
  for ( i = 0; i < number_of_input_files; i++ )
    {
    m_Sensitivity.push_back(p[i]);
    m_Specificity.push_back(q[i]);
    }
  m_ElapsedIterations = iter;

  delete[] q;
  delete[] p;
  delete[] last_q;
  delete[] last_p;
  delete[] D_it;
}
} // end namespace itk

#endif
