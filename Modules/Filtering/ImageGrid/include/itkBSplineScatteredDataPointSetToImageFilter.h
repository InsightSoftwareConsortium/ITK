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
#ifndef itkBSplineScatteredDataPointSetToImageFilter_h
#define itkBSplineScatteredDataPointSetToImageFilter_h

#include "itkPointSetToImageFilter.h"

#include "itkBSplineKernelFunction.h"
#include "itkCoxDeBoorBSplineKernelFunction.h"
#include "itkVectorContainer.h"

#include "vnl/vnl_matrix.h"

namespace itk
{
/** \class BSplineScatteredDataPointSetToImageFilter
 * \brief Image filter which provides a B-spline output approximation.
 *
 * Given an n-D image with scattered data, this filter finds
 * a fast approximation to that irregularly spaced data using uniform
 * B-splines. The traditional method of inverting the observation
 * matrix to find a least-squares fit is made obsolete.  Therefore,
 * memory issues are not a concern and inverting large matrices is
 * not applicable. In addition, this allows fitting to be multi-threaded.
 * This class generalizes from Lee's original paper to encompass
 * n-D data in m-D parametric space and any *feasible* B-spline order as well
 * as the option of specifying a confidence value for each point.
 *
 * In addition to specifying the input point set, one must specify the number
 * of control points.  The specified number of control points must be
 * greater than m_SplineOrder.  If one wishes to use the multilevel component of
 * this algorithm, one must also specify the number of levels in the
 * hierarchy.  If this is desired, the number of control points becomes
 * the number of control points for the coarsest level.  The algorithm
 * then increases the number of control points at each level so that
 * the B-spline n-D grid is refined to twice the previous level.
 *
 * There are two parts to fitting scattered data: the parameterization
 * assignment problem and the fitting problem given a parameterization.
 * This filter only addresses the second problem in that the user must
 * provide a parametric value for each scattered datum. Different parametric
 * assignment schemes result in different B-spline object outputs.
 *
 * This filter is general in that it accepts n-D scattered data in m-D parametric
 * dimensions. Input to this filter is an m-D point set with a Vector data type
 * of n dimensions. This means that the parametric values are stored in the
 * points container of the point set whereas the scattered data are stored in
 * the points data container of the point set.
 *
 * Typical B-spline objects include curves, which have a parametric dimension of
 * 1 and a data dimension of 2 or 3 (depending on the space in which the curve
 * resides) and deformation fields which commonly have parametric and data
 * dimensions of 2 or 3 (again depending on the space of the field).
 * As an example, a curve through a set of 2D points has data dimension 2 and
 * parametric dimension 1. The univariate curve could be represented as: <x(u),y(u)>
 * Another example is a 3D deformation of 3D points, which has parametric
 * dimension 3 and data dimension 3 and can be represented as:
 * <dx(u,v,w), dy(u,v,w), dz(u,v,w)>. However, as mentioned before, the code is
 * general such that, if the user wanted, she could model a time varying 3-D
 * displacement field which resides in 4-D space as
 * <dx(u, v, w, t), dy(u, v, w, t), dz(u, v, w, t)>.
 *
 * The output is an image defining the sampled B-spline parametric domain where
 * each pixel houses the sampled B-spline object value. For a curve fit to 3-D
 * points, the output is a 1-D image where each voxel contains a vector with
 * the approximated (x,y,z) location. The continuous, finite, rectilinear domain
 * (as well as the sampling rate) is specified via the combination of the SetSpacing()
 * and SetSize() functions.  For a 2-D deformation on 2-D points, the output is a 2-D image
 * where each voxel contains the approximated (dx, dy) vector.
 *
 * The parameterization must be specified using SetPoint, where the actual
 * coordinates of the point are set via SetPointData. For example, to compute a
 * spline through the (ordered) 2D points (5,6) and (7,8), you should use:
 *
 * \code
 * typedef itk::Vector< float, 2 > DataType;
 * PointSetType::PointType param0;
 * param0[0] = 0.0;
 * DataType p0;
 * p0[0] =  10.0; p0[1]= 10.0;
 * pointSet->SetPoint(0, param0);
 * pointSet->SetPointData( 0, p0 );
 *
 * PointSetType::PointType param1;
 * param1[0] = 1.0;
 * DataType p1;
 * p1[0] =  80.0; p1[1]= 50.0;
 * pointSet->SetPoint(1, param1);
 * pointSet->SetPointData( 1, p1 );
 * \endcode
 *
 * \author Nicholas J. Tustison
 *
 * This code was contributed in the Insight Journal paper:
 * "N-D C^k B-Spline Scattered Data Approximation"
 * by Nicholas J. Tustison, James C. Gee
 * https://hdl.handle.net/1926/140
 * http://www.insight-journal.org/browse/publication/57
 *
 *
 * \par REFERENCE
 * S. Lee, G. Wolberg, and S. Y. Shin, "Scattered Data Interpolation
 * with Multilevel B-Splines", IEEE Transactions on Visualization and
 * Computer Graphics, 3(3):228-244, 1997.
 *
 * \par REFERENCE
 * N.J. Tustison and J.C. Gee, "Generalized n-D C^k Scattered Data Approximation
 * with Confidence Values", Proceedings of the MIAR conference, August 2006.
 *
 * \ingroup ITKImageGrid
 */

template< typename TInputPointSet, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BSplineScatteredDataPointSetToImageFilter:
  public PointSetToImageFilter< TInputPointSet, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BSplineScatteredDataPointSetToImageFilter             Self;
  typedef PointSetToImageFilter<TInputPointSet, TOutputImage>   Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineScatteredDataPointSetToImageFilter,
    PointSetToImageFilter );

  /** Extract dimension from the output image. */
  itkStaticConstMacro( ImageDimension, unsigned int,
    TOutputImage::ImageDimension );

  typedef TOutputImage                              ImageType;
  typedef TInputPointSet                            PointSetType;

  /** Image typedef support. */
  typedef typename ImageType::PixelType             PixelType;
  typedef typename ImageType::RegionType            RegionType;
  typedef typename ImageType::SizeType              SizeType;
  typedef typename ImageType::IndexType             IndexType;

  /** PointSet typedef support. */
  typedef typename PointSetType::PointType          PointType;
  typedef typename PointSetType::Pointer            PointSetPointer;
  typedef typename PointSetType::PixelType          PointDataType;
  typedef typename PointSetType::PointDataContainer PointDataContainerType;

  /** Other typedefs. */
  typedef float                                     RealType;
  typedef VectorContainer<unsigned, RealType>       WeightsContainerType;

  /** Image types. */
  typedef Image<PointDataType,
    itkGetStaticConstMacro( ImageDimension )>       PointDataImageType;
  typedef Image<RealType,
    itkGetStaticConstMacro( ImageDimension )>       RealImageType;
  typedef typename RealImageType::Pointer           RealImagePointer;
  typedef typename PointDataImageType::Pointer      PointDataImagePointer;
  typedef FixedArray<unsigned,
    itkGetStaticConstMacro( ImageDimension )>       ArrayType;
  typedef FixedArray<RealType,
    itkGetStaticConstMacro( ImageDimension )>       RealArrayType;

  /** Interpolation kernel type (default spline order = 3). */
  typedef CoxDeBoorBSplineKernelFunction<3>         KernelType;
  typedef BSplineKernelFunction<0>                  KernelOrder0Type;
  typedef BSplineKernelFunction<1>                  KernelOrder1Type;
  typedef BSplineKernelFunction<2>                  KernelOrder2Type;
  typedef BSplineKernelFunction<3>                  KernelOrder3Type;


  /** Set the spline order assuming it is the same in all parametric dimensions.
   * The spline order determines the continuity between B-spline elements and
   * the degree of polynomial used to construct the B-spline elements. Default
   * = 3. */
  void SetSplineOrder( unsigned int );

  /** Set the spline order for each parametric dimension separately. The spline
   * order determines the continuity between B-spline elements and the degree of
   * polynomial used to construct the B-spline elements. Default = 3. */
  void SetSplineOrder( const ArrayType & );

  /** Get the spline order for all parametric dimensions. The spline order
   * determines the continuity between B-spline elements and the degree of
   * polynomial used to construct the B-spline elements. Default = 3. */
  itkGetConstReferenceMacro( SplineOrder, ArrayType );

  /** Set/Get the number of control points for each parametric dimension at the
   * initial fitting level. The B-spline mesh size is equal to the number
   * of control points minus the spline order. Default = 4 in each dimension.
   */
  itkSetMacro( NumberOfControlPoints, ArrayType );
  itkGetConstReferenceMacro( NumberOfControlPoints, ArrayType );

  /** Get the number of current control points for each parametric dimension at
   * the current fitting level. The B-spline mesh size is equal to the number
   * of control points minus the spline order. Default = 4 in each dimension.
   */
  itkGetConstReferenceMacro( CurrentNumberOfControlPoints, ArrayType );

  /** Set the number of fitting levels assuming the number of fitting levels is
   * the same for each parametric dimension. Starting with the mesh size
   * implied by setting the number of control points, the mesh size is doubled
   * at each fitting level. Default = 1 in all parametric dimensions. */
  void SetNumberOfLevels( unsigned int );

  /** Set the number of fitting levels in each parametric dimension separately.
   * Starting with the mesh size implied by setting the number of control
   * points, the mesh size is doubled at each fitting level. Default = 1 in all
   * parametric dimensions. */
  void SetNumberOfLevels( const ArrayType & );

  /** Get the number of fitting levels for all parametric dimensions. Starting
   * with the mesh size implied by setting the number of control points, the
   * mesh size is doubled at each fitting level. Default = 1 in all parametric
   * dimensions. */
  itkGetConstReferenceMacro( NumberOfLevels, ArrayType );

  /** Set/Get the epsilon used for B-splines. The B-spline parametric domain in
   * 1-D is defined on the half-closed interval [a,b). Extension to n-D is
   * defined similarly. This presents some difficulty for defining the
   * the image domain to be co-extensive with the parametric domain. We use
   * the B-spline epsilon to push the edge of the image boundary inside the
   * B-spline parametric domain. */
  itkSetMacro( BSplineEpsilon, RealType );
  itkGetConstMacro( BSplineEpsilon, RealType );

  /** Set/Get the array to define the periodicity of the dimensions in the
   * parametric space is to be.
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

  /** A weighted fitting is possible where each input point is assigned a
   * relative weighting. */
  void SetPointWeights( WeightsContainerType *weights );

  /** Set/Get whether or not the sampled output B-spline object is constructed.
   * The result of the fitting process is an n-D grid of control points which
   * describe the continuous B-spline object. */
  itkSetMacro( GenerateOutputImage, bool );
  itkGetConstReferenceMacro( GenerateOutputImage, bool );
  itkBooleanMacro( GenerateOutputImage );

  /** Get the control point lattice produced by the fitting process. */
  PointDataImagePointer GetPhiLattice()
    {
    return static_cast<PointDataImageType *>( this->ProcessObject::GetOutput( 1 ) );
    }

protected:
  BSplineScatteredDataPointSetToImageFilter();
  virtual ~BSplineScatteredDataPointSetToImageFilter() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void ThreadedGenerateData( const RegionType &, ThreadIdType ) ITK_OVERRIDE;

  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void AfterThreadedGenerateData() ITK_OVERRIDE;

  unsigned int SplitRequestedRegion( unsigned int, unsigned int, RegionType & ) ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineScatteredDataPointSetToImageFilter);

  /** Function used to propagate the fitting solution at one fitting level
   * to the next level with the mesh resolution doubled. */
  void RefineControlPointLattice();

  /** Determine the residuals after fitting to one level. */
  void UpdatePointSet();

  /** This function is not used as it requires an evaluation of all
   * (SplineOrder+1)^ImageDimensions B-spline weights for each evaluation. */
  void GenerateOutputImage();

  /** Function used to generate the sampled B-spline object quickly. */
  void ThreadedGenerateDataForFitting( const RegionType &, ThreadIdType );

  /** Function used to generate the sampled B-spline object quickly. */
  void ThreadedGenerateDataForReconstruction( const RegionType &, ThreadIdType );

  /** Sub-function used by GenerateOutputImageFast() to generate the sampled
   * B-spline object quickly. */
  void CollapsePhiLattice( PointDataImageType *, PointDataImageType *,
    const RealType, const unsigned int );

  /** Set the grid parametric domain parameters such as the origin, size,
   * spacing, and direction. */
  void SetPhiLatticeParametricDomainParameters();

  /** Convert number to index given a size of image. Used to index
   * the local control point neighborhoods. */
  IndexType NumberToIndex( const unsigned int, const SizeType );

  bool                                         m_DoMultilevel;
  bool                                         m_GenerateOutputImage;
  bool                                         m_UsePointWeights;
  unsigned int                                 m_MaximumNumberOfLevels;
  unsigned int                                 m_CurrentLevel;
  ArrayType                                    m_NumberOfControlPoints;
  ArrayType                                    m_CurrentNumberOfControlPoints;
  ArrayType                                    m_CloseDimension;
  ArrayType                                    m_SplineOrder;
  ArrayType                                    m_NumberOfLevels;

  typename WeightsContainerType::Pointer       m_PointWeights;

  typename PointDataImageType::Pointer         m_PhiLattice;
  typename PointDataImageType::Pointer         m_PsiLattice;

  vnl_matrix<RealType>     m_RefinedLatticeCoefficients[ImageDimension];

  typename PointDataContainerType::Pointer     m_InputPointData;
  typename PointDataContainerType::Pointer     m_OutputPointData;

  typename KernelType::Pointer                 m_Kernel[ImageDimension];

  typename KernelOrder0Type::Pointer           m_KernelOrder0;
  typename KernelOrder1Type::Pointer           m_KernelOrder1;
  typename KernelOrder2Type::Pointer           m_KernelOrder2;
  typename KernelOrder3Type::Pointer           m_KernelOrder3;

  std::vector<RealImagePointer>                m_OmegaLatticePerThread;
  std::vector<PointDataImagePointer>           m_DeltaLatticePerThread;

  RealType                                     m_BSplineEpsilon;
  bool                                         m_IsFittingComplete;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineScatteredDataPointSetToImageFilter.hxx"
#endif

#endif
