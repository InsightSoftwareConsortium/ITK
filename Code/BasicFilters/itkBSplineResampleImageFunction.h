/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineResampleImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBSplineResampleImageFunction_h
#define __itkBSplineResampleImageFunction_h

#include "itkBSplineInterpolateImageFunction.h"

namespace itk
{
/** \class BSplineResampleImageFunction
 * \brief Resample image intensity from a BSpline coefficient image.
 *
 * This class resample the image intensity at a non-integer position
 * from the input BSpline coefficient image.
 * 
 * Spline order may be from 0 to 5.
 *
 * In ITK, BSpline coefficient can be generated using a
 * BSplineDecompositionImageFilter. Using this image function in
 * conjunction with ResampleImageFunction allows the reconstruction
 * of the original image at different resolution and size.
 *
 * \sa BSplineInterpolateImageFunction
 * \sa BSplineDecompositionImageFilter
 * \sa ResampleImageFilter
 *
 * \ingroup ImageFunctions
 */
template <class TImageType, class TCoordRep = float>
class ITK_EXPORT BSplineResampleImageFunction : 
    public BSplineInterpolateImageFunction<
  TImageType,TCoordRep,ITK_TYPENAME TImageType::PixelType > 
{
public:
  /** Standard class typedefs. */
  typedef BSplineResampleImageFunction       Self;
  typedef BSplineInterpolateImageFunction<
    TImageType,TCoordRep, ITK_TYPENAME TImageType::PixelType >  Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineReconstructionImageFunction, 
               BSplineInterpolateImageFunction);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Set the input image representing the BSplineCoefficients */
  virtual void SetInputImage(const TImageType * inputData)
  {
    // bypass my superclass
    this->InterpolateImageFunction<TImageType,TCoordRep>::SetInputImage(inputData);
    m_Coefficients = inputData;
    m_DataLength = m_Coefficients->GetBufferedRegion().GetSize();
  }

protected:
  BSplineResampleImageFunction() {};
  virtual ~BSplineResampleImageFunction() {};
  void PrintSelf(std::ostream& os, Indent indent) const
  {
    this->Superclass::PrintSelf( os, indent );
  }

private:
  
};

} // namespace itk


#endif

