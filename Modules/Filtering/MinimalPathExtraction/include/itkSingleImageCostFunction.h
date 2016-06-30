/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkSingleImageCostFunction.h,v $
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef itkSingleImageCostFunction_h
#define itkSingleImageCostFunction_h

#include "itkNumericTraits.h"
#include "itkExceptionObject.h"
#include "itkContinuousIndex.h"
#include "itkSingleValuedCostFunction.h"
#include "itkInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkPhysicalCentralDifferenceImageFunction.h"

namespace itk
{

/** \class SingleImageCostFunction
 * \brief This class is a cost function which queries
 *        an underlying image for the single value.
 *
 * The user is expected to provide an image representing the
 * underlying cost function. The user may also provide an image
 * interpolator (if not provided the LinearInterpolateImageFunction
 * is used by default). The gradient is computed using central
 * differences in physical space.
 *
 * The parameters are the physical location (itkPoint) of the current
 * position. Initialize() must be called before using this cost function.
 *
 * \author Dan Mueller, Queensland University of Technology, dan.muel[at]gmail.com
 *
 * \ingroup Numerics Optimizers
 *
 * \ingroup MinimalPathExtraction
 */
template <class TImage>
class ITK_EXPORT SingleImageCostFunction : public SingleValuedCostFunction
{
public:
  /** Standard class typedefs. */
  typedef SingleImageCostFunction  Self;
  typedef SingleValuedCostFunction Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SingleImageCostFunction, SingleValuedCostFunction);

  /** MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef typename Superclass::MeasureType MeasureType;

  /** DerivativeType typedef.
   *  It defines a type used to return the cost function derivative. */
  typedef typename Superclass::DerivativeType DerivativeType;

  /** ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef typename Superclass::ParametersType ParametersType;

  /**  Type of the Image. */
  typedef TImage                           ImageType;
  typedef typename TImage::PixelType       ImagePixelType;
  typedef typename ImageType::ConstPointer ImageConstPointer;

  /** Constant for the image dimension */
  itkStaticConstMacro(ImageDimension, unsigned int, ImageType::ImageDimension);

  /** Type used for representing point components */
  typedef Superclass::ParametersValueType CoordRepType;

  /** Type for locations */
  typedef Index<ImageDimension>                         IndexType;
  typedef Point<CoordRepType, ImageDimension>           PointType;
  typedef ContinuousIndex<CoordRepType, ImageDimension> ContinuousIndexType;

  /** Type of the Interpolator class */
  typedef InterpolateImageFunction<ImageType, CoordRepType>       InterpolatorType;
  typedef LinearInterpolateImageFunction<ImageType, CoordRepType> DefaultInterpolatorType;

  /** Type of the GradientImageFunction class */
  typedef PhysicalCentralDifferenceImageFunction<ImageType, CoordRepType> GradientImageFunctionType;

  /** Get/set the Interpolator. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetConstObjectMacro(Interpolator, InterpolatorType);

  /** Get/set the Image.  */
  itkSetConstObjectMacro(Image, ImageType);
  itkGetConstObjectMacro(Image, ImageType);

  /** Initialize the cost function */
  virtual void
  Initialize(void) throw(ExceptionObject);

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters(void) const ITK_OVERRIDE
  {
    return ImageDimension;
  }

  /** This method returns the value of the cost function corresponding
   * to the specified parameters. */
  virtual MeasureType
  GetValue(const ParametersType & parameters) const ITK_OVERRIDE;

  /** This method returns the derivative of the cost function corresponding
   * to the specified parameters. */
  virtual void
  GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const ITK_OVERRIDE;

protected:
  SingleImageCostFunction();
  virtual ~SingleImageCostFunction() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SingleImageCostFunction);

  ImageConstPointer                           m_Image;
  typename InterpolatorType::Pointer          m_Interpolator;
  typename GradientImageFunctionType::Pointer m_GradientImageFunction;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSingleImageCostFunction.hxx"
#endif

#endif
