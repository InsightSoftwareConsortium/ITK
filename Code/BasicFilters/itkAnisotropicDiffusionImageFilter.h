/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAnisotropicDiffusionImageFilter_h_
#define __itkAnisotropicDiffusionImageFilter_h_

#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkAnisotropicDiffusionFunction.h"
#include "itkNumericTraits.h"

namespace itk {

/** \class AnisotropicDiffusionImageFilter
 *  This filter is the base class for a set of filters that iteratively diffuse
 *  images by solving non-linear partial differential equations based on the
 *  classical heat equation.  For an overview of the diffusion algorithm, see
 *  AnisotropicImageFunction.  AnisotropicDiffusionImageFilter is a subclass
 *  of itkFiniteDifferenceImageFilter and is part of the finite difference
 *  solver hierarchy.
 *
 *  \par Inputs and Outputs
 *  This is an image-to-image filter.  The requirements for data types and
 *  dimensionality of the input and output are defined by subclasses.
 *
 *  \par Parameters
 *  Set/GetIterations specifies the number of iterations (time-step updates)
 *  that the solver will perform to produce a solution image.  The appropriate
 *  number of iterations is dependent on the application and the image being
 *  processed.  As a general rule, the more iterations performed, the more
 *  diffused the image will become.
 *
 *  \par
 *  Set/GetTimeStep sets the time step to be used for each iteration (update).
 *  This parameter is described in detail in itkAnisotropicImageFunction.
 *
 *  \par
 *  Set/GetConductanceParameter set a common parameter used by subclasses of
 *  itkAnisotropicDiffusionFunction.   See itkAnisotropicDiffusionFunction for
 *  detailed information.
 *
 *  \par How to use this filter
 *  AnisotropicDiffusionImageFilter must be subclassed to be used.  This class
 *  implements a generic framework for other diffusion filters.
 *
 *  \sa GradientAnisotropicDiffusionImageFilter
 *  \sa VectorGradientAnisotropicDiffusionImageFilter
 *  \sa CurvatureAnisotropicDiffusionImageFilter
 *  \sa VectorCurvatureAnisotropicDiffusionImageFilter
 * \ingroup ImageEnhancement */
template <class TInputImage, class TOutputImage>
class AnisotropicDiffusionImageFilter
  : public DenseFiniteDifferenceImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef AnisotropicDiffusionImageFilter Self;
  typedef DenseFiniteDifferenceImageFilter<TInputImage, TOutputImage>
   Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information. */
  itkTypeMacro(AnisotropicDiffusionImageFilter,
               DenseFiniteDifferenceImageFilter);

  /** Capture information from the superclass. */
  typedef typename Superclass::InputImageType   InputImageType;
  typedef typename Superclass::OutputImageType  OutputImageType;
  typedef typename Superclass::UpdateBufferType UpdateBufferType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Set/Get the number of iterations that the filter will run. */
  itkSetMacro(Iterations, unsigned int);
  itkGetMacro(Iterations, unsigned int);

  /** Set/Get the time step for each iteration */
  itkSetMacro(TimeStep, TimeStepType);
  itkGetMacro(TimeStep, TimeStepType);

  /** Set/Get the conductance parameter governing sensitivity of the
      conductance equation. */
  itkSetMacro(ConductanceParameter, double);
  itkGetMacro(ConductanceParameter, double);

  /** The following parameters are not used at this time.  Setting them will
      have no effect on the output */
  itkSetMacro(ConductanceScalingUpdateInterval, unsigned int);
  itkGetMacro(ConductanceScalingUpdateInterval, unsigned int);
  itkSetMacro(ConductanceScalingParameter, double);
  itkGetMacro(ConductanceScalingParameter, double);

  /** Supplies a fixed value for the average gradient magnitude of the image to 
      the AnisotropicDiffusionFunction at each iteration.  The average gradient 
      magnitude is normally calculated over the entire image before each
      iteration and is used as a scaling factor in the calculations of change
      at a pixel.  This method is  useful in streaming applications to avoid
      block artifacts by overriding the normal gradient magnitude calculation
      (i.e. all image chunks are scaled uniformly). */
  void SetFixedAverageGradientMagnitude(double a)
    {
      m_FixedAverageGradientMagnitude= a;
      this->Modified();
      m_GradientMagnitudeIsFixed = true;
    }
  itkGetMacro(FixedAverageGradientMagnitude, double);
  
protected:
  AnisotropicDiffusionImageFilter()
    {
      m_Iterations = 0;
      m_ConductanceParameter = 1.0;
      m_TimeStep = 0.125f;
      m_FixedAverageGradientMagnitude = 0.0;
      m_GradientMagnitudeIsFixed = false;
    }
  ~AnisotropicDiffusionImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    {
      Superclass::PrintSelf(os, indent.GetNextIndent());
      os << indent << "TimeStep: " << m_TimeStep << std::endl;
      os << indent << "ConductanceParameter: "
         << m_ConductanceParameter << std::endl;
      os << indent << "ConductanceScalingParameter: "
         << m_ConductanceScalingParameter << std::endl;
      os << indent << "Iterations: " << m_Iterations
         << std::endl;
      os << indent << "ConductanceScalingUpdateInterval: "
         << m_ConductanceScalingUpdateInterval << std::endl;
      os << indent << "FixedAverageGradientMagnitude: "
         << m_FixedAverageGradientMagnitude << std::endl;
    }
  
  /** Supplies the halting criteria for this class of filters.  The
   * algorithm will stop after a user-specified number of iterations. */
  virtual bool Halt()
    {
      if (this->GetElapsedIterations() == m_Iterations) return true;
      else return false;
    }

  /** Prepare for the iteration process. */
  virtual void InitializeIteration()
    {
      AnisotropicDiffusionFunction<UpdateBufferType> *f = 
        dynamic_cast<AnisotropicDiffusionFunction<UpdateBufferType> *>
        (this->GetDifferenceFunction().GetPointer());
      if (! f)
        {  throw ExceptionObject(__FILE__, __LINE__);    }
      
      f->SetConductanceParameter(m_ConductanceParameter);
      
      if (m_GradientMagnitudeIsFixed == false)
        {
          f->CalculateAverageGradientMagnitudeSquared(this->GetOutput());
        }
      else
        {
          f->SetAverageGradientMagnitudeSquared(m_FixedAverageGradientMagnitude 
                                                *
                                                m_FixedAverageGradientMagnitude);
        }
      f->InitializeIteration();

      if (m_Iterations != 0)
          this->UpdateProgress(((float)(this->GetElapsedIterations()))
                               /((float)(m_Iterations)));
      else this->UpdateProgress(0);
    }

  bool m_GradientMagnitudeIsFixed;
  
private:
  AnisotropicDiffusionImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  double           m_ConductanceParameter;
  double           m_ConductanceScalingParameter;
  unsigned int     m_Iterations;
  unsigned int     m_ConductanceScalingUpdateInterval;
  double           m_FixedAverageGradientMagnitude;

  TimeStepType     m_TimeStep;
  
};

} // end namspace itk

#endif
