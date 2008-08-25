/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineScatteredDataPointSetToImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBSplineScatteredDataPointSetToImageFilter_h
#define __itkBSplineScatteredDataPointSetToImageFilter_h

#include "itkPointSetToImageFilter.h"
#include "itkCoxDeBoorBSplineKernelFunction.h"
#include "itkFixedArray.h"
#include "itkVariableSizeMatrix.h"
#include "itkVector.h"
#include "itkVectorContainer.h"

#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class BSplineScatteredDataPointSetToImageFilter.h
 * \brief Image filter which provides a B-spline output approximation.
 *
 * Given an n-D image with scattered data, this filter finds
 * a fast approximation to that irregulary spaced data using uniform
 * B-splines.  The traditional method of inverting the observation
 * matrix to find a least-squares fit is made obsolete.  Therefore,
 * memory issues are not a concern and inverting large matrices are
 * unnecessary.  The reference below gives the algorithm for 2-D images
 * and cubic splines.  This class generalizes that work to encompass n-D
 * images and any *feasible* B-spline order.
 *
 * In addition to specifying the input point set, one must specify the number
 * of control points.  If one wishes to use the multilevel component of
 * this algorithm, one must also specify the number of levels in the
 * hieararchy.  If this is desired, the number of control points becomes
 * the number of control points for the coarsest level.  The algorithm
 * then increases the number of control points at each level so that
 * the B-spline n-D grid is refined to twice the previous level.  The
 * scattered data is specified by the pixel values.  Pixels which
 * are not to be included in the calculation of the B-spline grid must
 * have a value equal to that specified by the variable m_BackgroundValue.
 *
 * Note that the specified number of control points must be > m_SplineOrder.
 *
 *
 * \author Nicholas J. Tustison
 *
 * Contributed by Nicholas J. Tustison, James C. Gee
 * in the Insight Journal paper:
 * http://hdl.handle.net/1926/140
 *
 * \par REFERENCE
 * S. Lee, G. Wolberg, and S. Y. Shin, "Scattered Data Interpolation
 * with Multilevel B-Splines", IEEE Transactions on Visualization and
 * Computer Graphics, 3(3):228-244, 1997.
 *
 * N.J. Tustison and J.C. Gee, "Generalized n-D C^k Scattered Data Approximation
 * with COnfidence Values", Proceedings of the MIAR conference, August 2006.
 */

template <class TInputPointSet, class TOutputImage>
class BSplineScatteredDataPointSetToImageFilter
: public PointSetToImageFilter<TInputPointSet, TOutputImage>
{
public:
  typedef BSplineScatteredDataPointSetToImageFilter  Self;
  typedef PointSetToImageFilter
    <TInputPointSet, TOutputImage>                   Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Extract dimension from input and output image. */
  itkStaticConstMacro( ImageDimension, unsigned int,
                       TOutputImage::ImageDimension );

  typedef TOutputImage                               ImageType;
  typedef TInputPointSet                             PointSetType;

  /** Image typedef support. */
  typedef typename ImageType::PixelType              PixelType;
  typedef typename ImageType::RegionType             RegionType;
  typedef typename ImageType::SizeType               SizeType;
  typedef typename ImageType::IndexType              IndexType;
  typedef typename ImageType::PointType              ContinuousIndexType;

  /** PointSet typedef support. */
  typedef typename PointSetType::PointType           PointType;
  typedef typename PointSetType::PixelType           PointDataType;
  typedef typename PointSetType::PointDataContainer  PointDataContainerType;

  /** Other typedef */
  typedef float                                      RealType;
  typedef VectorContainer<unsigned, RealType>        WeightsContainerType;
  typedef Image<PointDataType,
    itkGetStaticConstMacro( ImageDimension )>        PointDataImageType;
  typedef Image<RealType,
    itkGetStaticConstMacro( ImageDimension )>        RealImageType;
  typedef FixedArray<unsigned,
    itkGetStaticConstMacro( ImageDimension )>        ArrayType;
  typedef VariableSizeMatrix<RealType>               GradientType;

  /**
   * Interpolation kernel type (default spline order = 3)
   */
  typedef CoxDeBoorBSplineKernelFunction<3>          KernelType;

  /** Helper functions */

  void SetNumberOfLevels( unsigned int );
  void SetNumberOfLevels( ArrayType );
  itkGetConstReferenceMacro( NumberOfLevels, ArrayType );

  void SetSplineOrder( unsigned int );
  void SetSplineOrder( ArrayType );
  itkGetConstReferenceMacro( SplineOrder, ArrayType );

  itkSetMacro( NumberOfControlPoints, ArrayType );
  itkGetConstReferenceMacro( NumberOfControlPoints, ArrayType );
  itkGetConstReferenceMacro( CurrentNumberOfControlPoints, ArrayType );

  itkSetMacro( CloseDimension, ArrayType );
  itkGetConstReferenceMacro( CloseDimension, ArrayType );

  itkSetMacro( GenerateOutputImage, bool );
  itkGetConstReferenceMacro( GenerateOutputImage, bool );
  itkBooleanMacro( GenerateOutputImage );

  void SetPointWeights( WeightsContainerType * weights );

  /**
   * Get the control point lattice.
   */
  itkSetMacro( PhiLattice, typename PointDataImageType::Pointer );
  itkGetConstMacro( PhiLattice, typename PointDataImageType::Pointer );

  /**
   * Evaluate the resulting B-spline object at a specified
   * point or index within the image domain.
   */
  void EvaluateAtPoint( PointType, PointDataType & );
  void EvaluateAtIndex( IndexType, PointDataType & );
  void EvaluateAtContinuousIndex( ContinuousIndexType, PointDataType & );

  /**
   * Evaluate the resulting B-spline object at a specified
   * parameteric point.  Note that the parameterization over
   * each dimension of the B-spline object is [0, 1].
   */
  void Evaluate( PointType, PointDataType & );

  /**
   * Evaluate the gradient of the resulting B-spline object at a specified
   * point or index within the image domain.
   */
  void EvaluateGradientAtPoint( PointType, GradientType & );
  void EvaluateGradientAtIndex( IndexType, GradientType & );
  void EvaluateGradientAtContinuousIndex( ContinuousIndexType, GradientType & );

  /**
   * Evaluate the gradient of the resulting B-spline object
   * at a specified parameteric point.  Note that the
   * parameterization over each dimension of the B-spline
   * object is [0, 1].
   */
  void EvaluateGradient( PointType, GradientType & );

protected:
  BSplineScatteredDataPointSetToImageFilter();
  virtual ~BSplineScatteredDataPointSetToImageFilter();
  void PrintSelf( std::ostream& os, Indent indent ) const;

  void GenerateData();

private:

  //purposely not implemented
  BSplineScatteredDataPointSetToImageFilter(const Self&);
  void operator=(const Self&);

  void GenerateControlLattice();
  void RefineControlLattice();
  void UpdatePointSet();
  void GenerateOutputImage();
  void GenerateOutputImageFast();
  void CollapsePhiLattice( PointDataImageType *,
                           PointDataImageType *,
                           RealType, unsigned int );


  bool                                           m_DoMultilevel;
  bool                                           m_GenerateOutputImage;
  bool                                           m_UsePointWeights;
  unsigned int                                   m_MaximumNumberOfLevels;
  unsigned int                                   m_CurrentLevel;
  ArrayType                                      m_NumberOfControlPoints;
  ArrayType                                      m_CurrentNumberOfControlPoints;
  ArrayType                                      m_CloseDimension;
  ArrayType                                      m_SplineOrder;
  ArrayType                                      m_NumberOfLevels;
  typename WeightsContainerType::Pointer         m_PointWeights;
  typename KernelType::Pointer                   m_Kernel[ImageDimension];
  typename PointDataImageType::Pointer           m_PhiLattice;
  typename PointDataImageType::Pointer           m_PsiLattice;
  typename PointDataContainerType::Pointer       m_InputPointData;
  typename PointDataContainerType::Pointer       m_OutputPointData;

  RealType                                       m_BSplineEpsilon;

  vnl_matrix<RealType>
    m_RefinedLatticeCoefficients[ImageDimension];

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
#include "itkBSplineScatteredDataPointSetToImageFilter.txx"
#endif

#endif
