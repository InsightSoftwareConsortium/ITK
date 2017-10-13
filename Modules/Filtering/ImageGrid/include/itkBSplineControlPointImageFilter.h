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
#ifndef itkBSplineControlPointImageFilter_h
#define itkBSplineControlPointImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkBSplineKernelFunction.h"
#include "itkCoxDeBoorBSplineKernelFunction.h"
#include "itkFixedArray.h"
#include "itkPointSet.h"
#include "itkVariableSizeMatrix.h"
#include "itkVector.h"
#include "itkVectorContainer.h"

#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"

namespace itk
{

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
 * This code was contributed in the Insight Journal paper:
 * "N-D C^k B-Spline Scattered Data Approximation"
 * by Nicholas J. Tustison, James C. Gee
 * https://hdl.handle.net/1926/140
 * http://www.insight-journal.org/browse/publication/57
 *
 * \author Nicholas J. Tustison
 * \ingroup ITKImageGrid
 */

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT BSplineControlPointImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  typedef BSplineControlPointImageFilter                Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineControlPointImageFilter, ImageToImageFilter);

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
  typedef FixedArray<RealType,
    itkGetStaticConstMacro( ImageDimension )>       RealArrayType;

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
   * Set/Get the boolean array indicating the periodicity of the B-spline object.
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
  itkGetConstReferenceMacro( CloseDimension, ArrayType );

  /**
   * Set/Get the parametric spacing of the B-spline object domain.
   */
  itkSetMacro( Spacing, SpacingType );
  itkGetConstMacro( Spacing, SpacingType );

  /**
   * Set/Get the parametric origin of the B-spline object domain.
   */
  itkSetMacro( Origin, OriginType );
  itkGetConstMacro( Origin, OriginType );

  /**
   * Set/Get the parametric size of the B-spline object domain.
   */
  itkSetMacro( Size, SizeType );
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
  virtual ~BSplineControlPointImageFilter() ITK_OVERRIDE;
  void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /**
   * Multi-threaded function which generates the output sampled B-spline object.
   */
  void ThreadedGenerateData( const OutputImageRegionType &, ThreadIdType ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineControlPointImageFilter);


  /**
   * Before splitting, we need to allocate memory for the output sampled
   * B-spline object based on the multi-threading functionality
   */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /**
   * Based on the way CollapsePhiLattice() is written, we want to split on the
   * the last dimension.
   */
  virtual unsigned int SplitRequestedRegion( unsigned int, unsigned int, OutputImageRegionType & ) ITK_OVERRIDE;

  /**
   * Sub-function used by GenerateOutputImageFast() to generate the sampled
   * B-spline object quickly.
   */
  void CollapsePhiLattice( PointDataImageType *, PointDataImageType *,
    const RealType, const unsigned int );

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
  unsigned int                                 m_MaximumNumberOfLevels;
  ArrayType                                    m_NumberOfControlPoints;
  ArrayType                                    m_CloseDimension;
  ArrayType                                    m_SplineOrder;
  ArrayType                                    m_NumberOfLevels;

  vnl_matrix<RealType>       m_RefinedLatticeCoefficients[ImageDimension];

  typename KernelType::Pointer                 m_Kernel[ImageDimension];
  typename KernelOrder0Type::Pointer           m_KernelOrder0;
  typename KernelOrder1Type::Pointer           m_KernelOrder1;
  typename KernelOrder2Type::Pointer           m_KernelOrder2;
  typename KernelOrder3Type::Pointer           m_KernelOrder3;

  RealType                                     m_BSplineEpsilon;

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
#include "itkBSplineControlPointImageFilter.hxx"
#endif

#endif
