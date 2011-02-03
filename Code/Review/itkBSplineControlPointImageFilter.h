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
#ifndef __itkBSplineControlPointImageFilter_h
#define __itkBSplineControlPointImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkBSplineKernelFunction.h"
#include "itkCoxDeBoorBSplineKernelFunction.h"
#include "itkFixedArray.h"
#include "itkPointSet.h"
#include "itkSingleValuedCostFunction.h"
#include "itkVariableSizeMatrix.h"
#include "itkVector.h"
#include "itkVectorContainer.h"

#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

namespace itk
{

/**
 * \class ParameterCostFunction
 *
 * \brief Used to estimate the parameters of a B-spline object given an initial
 * guess
 *
 * This constitutes a helper class specifically for the function
 *
 * void CalculateParametersClosestToDataPoint( PointDataType, PointType & );
 *
 * in the main class.  This function allows one to find the closest parameter
 * values of the B-spline object corresponding to a single point.  For example,
 * suppose one has a series of points derived from white matter tractography
 * which represent a noisy single tract.  One can then use the companion
 * class itkBSplineScatteredDataPointSetToImageFilter to create a smooth
 * representation of this tract, i.e. a 3-D univariate B-spline curve.  Now
 * suppose the user has a point in space near this tract and s/he wants to
 * determine which point on the B-spline curve is closest to this query point.
 * The user can take the control points from the fitting operation (i.e. the
 * results of itkBSplineScatteredDataPointSetToImageFilter) using the function
 *
 * GetControlPointPhiLattice()
 *
 * and input that control point lattice into this class.  S/he can then
 * call CalculateParametersClosestToDataPoint( PointDataType, PointType & )
 * to get this parametric point on the curve.
 */
template<class TControlPointLattice>
class ITK_EXPORT ParameterCostFunction
  : public SingleValuedCostFunction
{
public:
  typedef ParameterCostFunction    Self;
  typedef SingleValuedCostFunction Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Extract parametric dimension from input image. */
  itkStaticConstMacro( ParametricDimension, unsigned int,
                       TControlPointLattice::ImageDimension );

  /** Run-time type information (and related methods). */
  itkTypeMacro( ParameterCostFunction, SingleValuedCostFunction );

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  typedef Superclass::MeasureType    MeasureType;
  typedef Superclass::DerivativeType DerivativeType;
  typedef Superclass::ParametersType ParametersType;

  typedef FixedArray<unsigned,
    itkGetStaticConstMacro( ParametricDimension )>    ArrayType;

  /*
   * Define the parameters of the B-spline object.
   */

  /**
   * Set the control point lattice defining the B-spline object.
   */
  itkSetObjectMacro( ControlPointLattice, TControlPointLattice );

  /**
   * Set the parametric origin of the B-spline object domain.
   */
  itkSetMacro( Origin, typename TControlPointLattice::PointType );

  /**
   * Set the parametric spacing of the B-spline object domain.
   */
  itkSetMacro( Spacing, typename TControlPointLattice::SpacingType );

  /**
   * Set the parametric size of the B-spline object domain.
   */
  itkSetMacro( Size, typename TControlPointLattice::SizeType );

  /**
   * Set the parametric direction of the B-spline object domain.
   */
  itkSetMacro( Direction, typename TControlPointLattice::DirectionType );

  /**
   * Set the spline order of the B-spline object.
   */
  itkSetMacro( SplineOrder, ArrayType );

  /**
   * Set the boolean array indicating the periodicity of the B-spline object.
   * This array of 0/1 values defines whether a particular dimension of the
   * parametric space is to be considered periodic or not. For example, if you
   * are using interpolating along a 1D closed curve, the array type will have
   * size 1, and you should set the first element of this array to the value
   * "1". In the case that you were interpolating in a planar surface with
   * cylindrical topology, the array type will have two components, and you
   * should set to "1" the component that goes around the cylinder, and set to
   * "0" the component that goes from the top of the cylinder to the bottom.
   * This will indicate the periodity of that parameter to the filter.
   * Internally, in order to make periodic the domain of the parameter, the
   * filter will reuse some of the points at the beginning of the domain as if
   * they were also located at the end of the domain. The number of points to
   * be reused will depend on the spline order. As a user, you don't need to
   * replicate the points, the filter will do this for you. */
  itkSetMacro( CloseDimension, ArrayType );

  /**
   * Set the query data point which will be used to find which parametric value
   * is closest to the B-spline object.
   */
  itkSetMacro( DataPoint, typename TControlPointLattice::PixelType );

  /**
   * Get the value of the specified cost function.
   */
  virtual MeasureType GetValue( const ParametersType & parameters ) const;

  /**
   * Get the derivative of the specified cost function.
   */
  virtual void GetDerivative( const ParametersType & parameters,
                              DerivativeType & derivative ) const;

  /**
   * Get the number of parameters the specified cost function.
   */
  virtual unsigned int GetNumberOfParameters() const;

protected:
  ParameterCostFunction();
  virtual ~ParameterCostFunction();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  ParameterCostFunction(const Self&); //purposely not implemented
  void operator=(const Self&);        //purposely not implemented

  typename TControlPointLattice::Pointer         m_ControlPointLattice;
  typename TControlPointLattice::PointType       m_Origin;
  typename TControlPointLattice::SpacingType     m_Spacing;
  typename TControlPointLattice::SizeType        m_Size;
  typename TControlPointLattice::DirectionType   m_Direction;

  ArrayType m_SplineOrder;
  ArrayType m_CloseDimension;

  typename TControlPointLattice::PixelType       m_DataPoint;
};

/**
 * \class BSplineControlPointImageFilter
 *
 * \brief Process a given a B-spline grid of control points.
 *
 * \par  The output of the class itkBSplineScatteredDataPointSetToImageFilter
 * is a control point grid defining a B-spline object.  This class is used to
 * hold various routines meant to operate on that control point grid.  In
 * addition to specifying the control point grid as the input, the user
 * must also supply the spline order and the parametric domain (i.e. size,
 * domain, origin, direction, spacing).  The output of the filter is the sampled
 * B-spline object.
 *
 * Other operations include
 *   1. Evaluation of the B-spline object at any point in the domain.
 *   2. Evaluation of the gradient of the B-spline object at any point in the
 *      domain.
 *   3. Evaluation of the Jacobian of the B-spline object at any point in the
 *      domain.
 *   4. Evaluation of the Hessian of the B-spline object at any point in the
 *      domain.
 *   5. Refinement of the input control point lattice such that the
 *      corresponding B-spline mesh resolution is twice as great as
 *      the original while maintaining the same valued B-spline object.
 *   6. Inverse estimation.  Given a user-specified data point, one can
 *      find the parameters which minimize the "distance" between the evaluated
 *      data point and the B-spline object evaluated at those parameters.
 *      This is useful, for example, in determining the parametric values of
 *      a point on the curve closest to a user-specified point.
 *
 * \author Nicholas J. Tustison
 *
 * \par REFERENCE
 * S. Lee, G. Wolberg, and S. Y. Shin, "Scattered Data Interpolation
 * with Multilevel B-Splines", IEEE Transactions on Visualization and
 * Computer Graphics, 3(3):228-244, 1997.
 *
 * \par REFERENCE
 * N.J. Tustison and J.C. Gee, "Generalized n-D C^k Scattered Data Approximation
 * with COnfidence Values", Proceedings of the MIAR conference, August 2006.
 */

template <class TInputImage, class TOutputImage = TInputImage>
class BSplineControlPointImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  typedef BSplineControlPointImageFilter                Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Extract dimension from input image. */
  itkStaticConstMacro( ImageDimension, unsigned int,
                       TInputImage::ImageDimension );

  typedef TInputImage  ControlPointLatticeType;
  typedef TOutputImage OutputImageType;

  /** Image typedef support. */
  typedef typename OutputImageType::PixelType  PixelType;
  typedef typename OutputImageType::RegionType RegionType;
  typedef typename OutputImageType::IndexType  IndexType;
  typedef typename OutputImageType::PointType  PointType;
  typedef typename OutputImageType::PointType  ContinuousIndexType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  typedef typename OutputImageType::SpacingType   SpacingType;
  typedef typename OutputImageType::PointType     OriginType;
  typedef typename OutputImageType::SizeType      SizeType;
  typedef typename OutputImageType::DirectionType DirectionType;

  /** Other typedef */
  typedef float RealType;
  typedef Image<RealType,
    itkGetStaticConstMacro( ImageDimension )>       RealImageType;
  typedef typename RealImageType::Pointer           RealImagePointer;

  typedef FixedArray<unsigned,
    itkGetStaticConstMacro( ImageDimension )>       ArrayType;
  typedef VariableSizeMatrix<RealType>              GradientType;

  /** PointSet typedef support. */
  typedef PointSet<PixelType,
    itkGetStaticConstMacro( ImageDimension )>       PointSetType;
  typedef typename PointSetType::PixelType          PointDataType;
  typedef typename PointSetType::PointDataContainer PointDataContainerType;
  typedef Image<PointDataType,
    itkGetStaticConstMacro( ImageDimension )>       PointDataImageType;
  typedef typename PointDataImageType::Pointer      PointDataImagePointer;

  /** Interpolation kernel type (default spline order = 3) */
  typedef CoxDeBoorBSplineKernelFunction<3> KernelType;
  typedef BSplineKernelFunction<0>          KernelOrder0Type;
  typedef BSplineKernelFunction<1>          KernelOrder1Type;
  typedef BSplineKernelFunction<2>          KernelOrder2Type;
  typedef BSplineKernelFunction<3>          KernelOrder3Type;

  /**
   * Set the spline order of the B-spline object for all parametric dimensions.
   * Default = 3.
   */
  void SetSplineOrder( unsigned int );

  /**
   * Set the spline order array where each element of the array corresponds to
   * a single parametric dimension of the B-spline object.  Default = 3.
   */
  void SetSplineOrder( ArrayType );

  /**
   * Get the spline order array of the B-spline object.  Default = 3.
   */
  itkGetConstReferenceMacro( SplineOrder, ArrayType );

  /**
   * Set the boolean array indicating the periodicity of the B-spline object.
   * This array of 0/1 values defines whether a particular dimension of the
   * parametric space is to be considered periodic or not. For example, if you
   * are using interpolating along a 1D closed curve, the array type will have
   * size 1, and you should set the first element of this array to the value
   * "1". In the case that you were interpolating in a planar surface with
   * cylindrical topology, the array type will have two components, and you
   * should set to "1" the component that goes around the cylinder, and set to
   * "0" the component that goes from the top of the cylinder to the bottom.
   * This will indicate the periodity of that parameter to the filter.
   * Internally, in order to make periodic the domain of the parameter, the
   * filter will reuse some of the points at the beginning of the domain as if
   * they were also located at the end of the domain. The number of points to
   * be reused will depend on the spline order. As a user, you don't need to
   * replicate the points, the filter will do this for you. */
  itkSetMacro( CloseDimension, ArrayType );

  /**
   * Get the boolean array indicating which dimensions are closed.
   */
  itkGetConstReferenceMacro( CloseDimension, ArrayType );

  /**
   * Set the parametric spacing of the B-spline object domain.
   */
  itkSetMacro( Spacing, SpacingType );

  /**
   * Get the parametric spacing of the B-spline object domain.
   */
  itkGetConstMacro( Spacing, SpacingType );

  /**
   * Set the parametric origin of the B-spline object domain.
   */
  itkSetMacro( Origin, OriginType );

  /**
   * Get the parametric origin of the B-spline object domain.
   */
  itkGetConstMacro( Origin, OriginType );

  /**
   * Set the parametric size of the B-spline object domain.
   */
  itkSetMacro( Size, SizeType );

  /**
   * Get the parametric size of the B-spline object domain.
   */
  itkGetConstMacro( Size, SizeType );

  /**
   * Set the sampled object direction.  Note that this is not used in any of the
   * calculations in this class.  The only reason why it is included is due
   * to the fact that we use the image class to represent the B-spline
   * parametric domain (due to the maturity of the image class and the
   * wealth of helper functions associated with it).  This is where the
   * incongruence between the B-spline parametric domain and the concept of an
   * image is most obvious.  An ITK image is a representation of a physical
   * object thus it also exists in physical space which includes orientation.
   * However, it can be argued that the parametric domain, per se, does not
   * exist in physical space and it is only a parametric mapping which allows
   * us to go from the parametric space to the physical space.  Therefore,
   * the direction is not used.
   */
  itkSetMacro( Direction, DirectionType );

  /**
   * Get the sampled B-spline object direction.
   */
  itkGetConstMacro( Direction, DirectionType );

  /**
   * Evaluate the resulting B-spline object at a specified point in the
   * parametric domain.
   */
  void EvaluateAtPoint( PointType, PointDataType & );

  /**
   * Evaluate the resulting B-spline object at a specified index in the
   * parametric domain.
   */
  void EvaluateAtIndex( IndexType, PointDataType & );

  /**
   * Evaluate the resulting B-spline object at a specified continuous index in
   * the parametric domain.
   */
  void EvaluateAtContinuousIndex( ContinuousIndexType, PointDataType & );

  /**
   * Evaluate the resulting B-spline object at a specified internal parameteric
   * point.  Note that the internal parameterization over each dimension of the
   * B-spline object is [0, 1).
   */
  void Evaluate( PointType, PointDataType & );

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified point
   * in the parametric domain.
   */
  void EvaluateGradientAtPoint( PointType, GradientType & );

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified index
   * in the parametric domain.
   */
  void EvaluateGradientAtIndex( IndexType, GradientType & );

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified
   * continuous index in the parametric domain.
   */
  void EvaluateGradientAtContinuousIndex( ContinuousIndexType, GradientType & );

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified
   * internal parameteric point.  Note that the internal parameterization over
   * each dimension of the B-spline object is [0, 1).
   */
  void EvaluateGradient( PointType, GradientType & );

  /**
   * Evaluate the Jacobian of the resulting B-spline object at a specified
   * point in the parametric domain.
   */
  void EvaluateJacobianAtPoint( PointType pt, GradientType &jac )
    {
    this->EvaluateGradientAtPoint( pt, jac );
    GradientType I( jac.Rows(), jac.Cols() );
    I.SetIdentity();
    jac += I;
    }

  /**
   * Evaluate the Jacobian of the resulting B-spline object at a specified
   * index in the parametric domain.
   */
  void EvaluateJacobianAtIndex( IndexType idx, GradientType &jac )
    {
    this->EvaluateGradientAtIndex( idx, jac );
    GradientType I( jac.Rows(), jac.Cols() );
    I.SetIdentity();
    jac += I;
    }

  /**
   * Evaluate the Jacobian of the resulting B-spline object at a specified
   * continuous index in the parametric domain.
   */
  void EvaluateJacobianAtContinuousIndex( ContinuousIndexType cidx,
                                          GradientType &jac )
    {
    this->EvaluateGradientAtContinuousIndex( cidx, jac );
    GradientType I( jac.Rows(), jac.Cols() );
    I.SetIdentity();
    jac += I;
    }

  /**
   * Evaluate the jacobian of the resulting B-spline object at a specified
   * internal parameteric point.  Note that the internal parameterization over
   * each dimension of the B-spline object is [0, 1).
   */
  void EvaluateJacobian( PointType pt, GradientType &jac )
    {
    this->EvaluateGradient( pt, jac );
    GradientType I( jac.Rows(), jac.Cols() );
    I.SetIdentity();
    jac += I;
    }

  /**
   * Evaluate the Hessian of the resulting B-spline object at a specified
   * point within the parametric domain.  Since the Hessian for a vector
   * function is a 3-tensor, one must specify the component.
   */
  void EvaluateHessianAtPoint( PointType, GradientType &, unsigned int );

  /**
   * Evaluate the Hessian of the resulting B-spline object at a specified
   * index within the parametric domain.  Since the Hessian for a vector
   * function is a 3-tensor, one must specify the component.
   */
  void EvaluateHessianAtIndex( IndexType, GradientType &, unsigned int );

  /**
   * Evaluate the Hessian of the resulting B-spline object at a specified con-
   * tinuous index within the parametric domain.  Since the Hessian for a vector
   * function is a 3-tensor, one must specify the component.
   */
  void EvaluateHessianAtContinuousIndex( ContinuousIndexType,
                                         GradientType &, unsigned int );

  /**
   * Evaluate the hessian of the resulting B-spline object at a specified
   * internal parameteric point.  Note that the internal parameterization over
   * each dimension of the B-spline object is [0, 1).
   */
  void EvaluateHessian( PointType, GradientType &, unsigned int );

  /**
   * Given a B-spline object value and an initial parametric guess, use
   * bounded optimization (LBFGSB) to find the parameters corresponding to the
   * B-spline object value.
   */
  void CalculateParametersClosestToDataPoint( PointDataType, PointType & );

  /**
   * Generate a refined control point lattice from the input control point
   * lattice such that the resolution is doubled for each level.  This is
   * further described in the references.  Note that if one sets the number
   * of refinement levels to all 1's, the control point lattice is not increased
   * in resolution.  Doubling the resolution starts at 2 refinement levels.
   */
  typename ControlPointLatticeType::Pointer
    RefineControlPointLattice( ArrayType );

protected:
  BSplineControlPointImageFilter();
  virtual ~BSplineControlPointImageFilter();
  void PrintSelf( std::ostream& os, Indent indent ) const;

  /**
   * Multithreaded function which generates the output sampled B-spline object.
   */
  void ThreadedGenerateData( const OutputImageRegionType &, int );

private:
  BSplineControlPointImageFilter( const Self& ); //purposely not implemented
  void operator=( const Self& );                 //purposely not implemented


  /**
   * Before splitting, we need to allocate memory for the output sampled
   * B-spline object based on the multi-threading functionality
   */
  void BeforeThreadedGenerateData();

  /**
   * Based on the way CollapsePhiLattice() is written, we want to split on the
   * the last dimension.
   */
  int SplitRequestedRegion( int, int, OutputImageRegionType & );

  /**
   * Sub-function used by GenerateOutputImageFast() to generate the sampled
   * B-spline object quickly.
   */
  void CollapsePhiLattice( PointDataImageType *, PointDataImageType *,
    RealType, unsigned int );

  /**
   * Private function to handle the internal ivars.
   */
  void SetNumberOfLevels( ArrayType );

  /** Parameters for the output image. */
  SizeType                                     m_Size;
  SpacingType                                  m_Spacing;
  OriginType                                   m_Origin;
  DirectionType                                m_Direction;

  bool                                         m_DoMultilevel;
  bool                                         m_GenerateOutputImage;
  unsigned int                                 m_MaximumNumberOfLevels;
  unsigned int                                 m_CurrentLevel;
  ArrayType                                    m_NumberOfControlPoints;
  ArrayType                                    m_CloseDimension;
  ArrayType                                    m_SplineOrder;
  ArrayType                                    m_NumberOfLevels;

  vnl_matrix<RealType>       m_RefinedLatticeCoefficients[ImageDimension];

  vnl_vector<RealType>                         m_BSplineWeights[ImageDimension];
  RealImagePointer                             m_NeighborhoodWeightImage;

  typename KernelType::Pointer                 m_Kernel[ImageDimension];
  typename KernelOrder0Type::Pointer           m_KernelOrder0;
  typename KernelOrder1Type::Pointer           m_KernelOrder1;
  typename KernelOrder2Type::Pointer           m_KernelOrder2;
  typename KernelOrder3Type::Pointer           m_KernelOrder3;

  RealType m_BSplineEpsilon;

  inline typename RealImageType::IndexType
  NumberToIndex( unsigned int number, typename RealImageType::SizeType size )
    {
    typename RealImageType::IndexType k;
    k[0] = 1;

    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      k[i] = size[ImageDimension-i-1]*k[i-1];
      }
    typename RealImageType::IndexType index;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      index[ImageDimension-i-1]
        = static_cast<unsigned int>( number/k[ImageDimension-i-1] );
      number %= k[ImageDimension-i-1];
      }
    return index;
    }

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineControlPointImageFilter.txx"
#endif

#endif
