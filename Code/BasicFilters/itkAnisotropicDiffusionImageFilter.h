/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkAnisotropicDiffusionImageFilter_h_
#define __itkAnisotropicDiffusionImageFilter_h_


#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkAnisotropicDiffusionEquation.h"
namespace itk {

template <class TInputImage, class TOutputImage>
class AnisotropicDiffusionImageFilter
  : public DenseFiniteDifferenceImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard itk Self & Superclass typedefs
   */
  typedef AnisotropicDiffusionImageFilter Self;
  typedef DenseFiniteDifferenceImageFilter<TInputImage, TOutputImage>
   Superclass;

  typedef typename Superclass::InputImageType   InputImageType;
  typedef typename Superclass::OutputImageType  OutputImageType;
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /**
   * Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass.
   */
  enum { ImageDimension = Superclass::ImageDimension };

  /**
   * The pixel type of the output image will be used in computations.
   * Inherited from the superclass.
   */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::TimeStepType TimeStepType;

  /**
   * Define what a scalar value is.
   */
  typedef typename OutputImageType::ScalarValueType ScalarValueType;

 /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);

  itkTypeMacro(AnisotropicDiffusionImageFilter,
               DenseFiniteDifferenceImageFilter);
  itkSetMacro(Iterations, unsigned int);
  itkGetMacro(Iterations, unsigned int);
  itkSetMacro(TimeStep, TimeStepType);
  itkGetMacro(TimeStep, TimeStepType);
  itkSetMacro(ConductanceParameter, ScalarValueType);
  itkGetMacro(ConductanceParameter, ScalarValueType);
  
protected:
  AnisotropicDiffusionImageFilter()
    {
      m_Iterations = 0;
      m_ConductanceParameter = NumericTraits<ScalarValueType>::One;
      m_TimeStep = 0.125f;
    }
  ~AnisotropicDiffusionImageFilter() {}
  AnisotropicDiffusionImageFilter(const Self&) {}
  
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent)
    {
      os << indent << "AnisotropicDiffusionImageFilter";
      Superclass::PrintSelf(os, indent.GetNextIndent());
    }
  
  /**
   * Supplies the halting criteria for this class of filters.  The
   * algorithm will stop after a user-specified number of iterations.
   */
  virtual bool Halt()
    {
      if (this->GetElapsedIterations() == m_Iterations) return true;
      else return false;
    }

  /**
   *
   */
  virtual void InitializeIteration()
    {
      AnisotropicDiffusionEquation<UpdateBufferType> *f = 
        dynamic_cast<AnisotropicDiffusionEquation<UpdateBufferType> *>
        (this->GetDifferenceEquation().GetPointer());
      f->SetConductanceParameter(m_ConductanceParameter);
      f->CalculateAverageGradientMagnitudeSquared(this->GetOutput());
      f->InitializeIteration();
    }
  
private:
  ScalarValueType  m_ConductanceParameter;
  unsigned int     m_Iterations;
  TimeStepType     m_TimeStep;
};

} // end namspace itk

#endif
