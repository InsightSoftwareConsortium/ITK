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
#include "itkMacro.h"
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
  ITK_DISALLOW_COPY_AND_ASSIGN(SingleImageCostFunction);

  /** Standard class type alias. */
  using Self = SingleImageCostFunction;
  using Superclass = SingleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SingleImageCostFunction, SingleValuedCostFunction);

  /** MeasureType type alias.
   *  It defines a type used to return the cost function value. */
  using MeasureType = typename Superclass::MeasureType;

  /** DerivativeType type alias.
   *  It defines a type used to return the cost function derivative. */
  using DerivativeType = typename Superclass::DerivativeType;

  /** ParametersType type alias.
   *  It defines a position in the optimization search space. */
  using ParametersType = typename Superclass::ParametersType;

  /**  Type of the Image. */
  using ImageType = TImage;
  using ImagePixelType = typename TImage::PixelType;
  using ImageConstPointer = typename ImageType::ConstPointer;

  /** Constant for the image dimension */
  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  /** Type used for representing point components */
  using CoordRepType = Superclass::ParametersValueType;

  /** Type for locations */
  using IndexType = Index<ImageDimension>;
  using PointType = Point<CoordRepType, ImageDimension>;
  using ContinuousIndexType = ContinuousIndex<CoordRepType, ImageDimension>;

  /** Type of the Interpolator class */
  using InterpolatorType = InterpolateImageFunction<ImageType, CoordRepType>;
  using DefaultInterpolatorType = LinearInterpolateImageFunction<ImageType, CoordRepType>;

  /** Type of the GradientImageFunction class */
  using GradientImageFunctionType = PhysicalCentralDifferenceImageFunction<ImageType, CoordRepType>;

  /** Get/set the Interpolator. */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetConstObjectMacro(Interpolator, InterpolatorType);

  /** Get/set the Image.  */
  itkSetConstObjectMacro(Image, ImageType);
  itkGetConstObjectMacro(Image, ImageType);

  /** Get/set the DerivativeThreshold.  */
  itkSetMacro(DerivativeThreshold, DerivativeType::ValueType);
  itkGetConstReferenceMacro(DerivativeThreshold, DerivativeType::ValueType);

  /** Initialize the cost function */
  virtual void
  Initialize(void) throw(ExceptionObject);

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters(void) const override
  {
    return ImageDimension;
  }

  /** This method returns the value of the cost function corresponding
   * to the specified parameters. */
  MeasureType
  GetValue(const ParametersType & parameters) const override;

  /** This method returns the derivative of the cost function corresponding
   * to the specified parameters. */
  void
  GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const override;

  // Set these depending on whether you will be minimizing or maximizing.
  // They control the value returned when the point isn't inside the buffer.
  void
  SetMinimize()
  {
    m_OutsideValue = itk::NumericTraits<ImagePixelType>::max();
  }
  void
  SetMaximize()
  {
    m_OutsideValue = itk::NumericTraits<ImagePixelType>::NonpositiveMin();
  }


protected:
  SingleImageCostFunction();
  ~SingleImageCostFunction() override {}
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ImageConstPointer                           m_Image;
  typename InterpolatorType::Pointer          m_Interpolator;
  typename GradientImageFunctionType::Pointer m_GradientImageFunction;
  /** Used to define the value outside the image buffer. Important when
   *  path points are on the edge of an image */
  ImagePixelType                     m_OutsideValue;
  typename DerivativeType::ValueType m_DerivativeThreshold;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSingleImageCostFunction.hxx"
#endif

#endif
