/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkCurvatureFlowImageFilter.h
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
#ifndef __itkCurvatureFlowImageFilter_h_
#define __itkCurvatureFlowImageFilter_h_

#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkCurvatureFlowFunction.h"

namespace itk {


/** \class CurvatureFlowImageFilter
  * \brief Denoise an image using curvature driven flow.
  *
  * CurvatureFlowImageFilter implements a curvature driven image denoising 
  * algorithm. Iso-brightness contours in the input image are viewed as a 
  * level set. The level set is then evolved using a curvature-based speed 
  * function.
  *
  * The advantage of this approach is that sharp boundaries are preserved
  * with smoothing occuring only within a region.
  *
  * Note that unlike level set segmentation algorithms,
  * the image to be denoised is already the level set and can be set
  * directly as the input using the SetInput() method.
  *
  * This filter has two parameters: the number of update iterations to 
  * be performed and the timestep between each update.
  *
  * This filter make use of the finite difference solver hierarchy
  * and uses a zero flux Neumann boundary condition when computing derivatives
  * near the data boundary.
  *
  * Caveats: 
  * This filter assumes that the input and output types have the same dimensions.
  *
  * Reference:
  * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  * Cambridge Press, Chapter 16, Second edition, 1999.
  *
  * \sa DenseFiniteDifferenceImageFilter
  * \sa CurvatureFlow2DEquation
  * \sa CurvatureFlow3DEquation
  *
  * \ingroup ImageEnhancement 
  *
  */
template <class TInputImage, class TOutputImage>
class CurvatureFlowImageFilter
  : public DenseFiniteDifferenceImageFilter<TInputImage, TOutputImage>
{
public:

  /**
   * Standard "Self" typedef
   */
  typedef CurvatureFlowImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef DenseFiniteDifferenceImageFilter<TInputImage, TOutputImage>
   Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(CurvatureFlowImageFilter,
               DenseFiniteDifferenceImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * InputImage type
   */
  typedef typename Superclass::InputImageType  InputImageType;

  /**
   * OutputImage type
   */
  typedef typename Superclass::OutputImageType OutputImageType;

  /**
   * FiniteDifferenceEquation type
   */
  typedef typename Superclass::FiniteDifferenceEquationType
    FiniteDifferenceEquationType;

  /**
   * CurvatureFlowFunction type
   */
  typedef CurvatureFlowFunction<OutputImageType>
    CurvatureFlowFunctionType;

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

  /**
   * The time step type. Inherited from the superclass.
   */
  typedef typename Superclass::TimeStepType TimeStepType;

  /**
   * Set the number of iterations to be performed.
   */
  itkSetMacro(NumberOfIterations, unsigned int);

  /**
   * Get the number of iterations to be performed.
   */
  itkGetMacro(NumberOfIterations, unsigned int);

  /**
   * Set the timestep parameter.
   */
  itkSetMacro(TimeStep, TimeStepType);

  /**
   * Get the timestep parameter.
   */
  itkGetMacro(TimeStep, TimeStepType);
  
protected:

  CurvatureFlowImageFilter();
  ~CurvatureFlowImageFilter() {}
  CurvatureFlowImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /**
   * Supplies the halting criteria for this class of filters.  The
   * algorithm will stop after a user-specified number of iterations.
   */
  virtual bool Halt()
    {
      if (this->GetElapsedIterations() == m_NumberOfIterations) return true;
      else return false;
    }

  /**
   * Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method.
   */
  virtual void InitializeIteration();
  
private:

  unsigned int    m_NumberOfIterations;
  TimeStepType    m_TimeStep;

};

} // end namspace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureFlowImageFilter.txx"
#endif

#endif
