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
#ifndef itkTriangleMeshToBinaryImageFilter_h
#define itkTriangleMeshToBinaryImageFilter_h

#include "itkImageSource.h"

#include "itkPolygonCell.h"
#include "itkMapContainer.h"
#include "itkVectorContainer.h"
#include "itkAutomaticTopologyMeshSource.h"
#include "itkPointSet.h"

#include <vector>

namespace itk
{
class Point1D
{
public:
  double m_X;
  int    m_Sign;

  Point1D(){}
  Point1D(const double p, const int s)
  {
    m_X = p;
    m_Sign = s;
  }

  Point1D(const Point1D & point)
  {
    m_X = point.m_X;
    m_Sign = point.m_Sign;
  }

  double getX() const
  {
    return m_X;
  }

  int  getSign() const
  {
    return m_Sign;
  }
};

/** \class TriangleMeshToBinaryImageFilter
 *
 * \brief 3D Rasterization algorithm Courtesy of Dr David Gobbi of Atamai Inc.

 * \author Leila Baghdadi, MICe, Hospital for Sick Childern, Toronto, Canada,
 * \ingroup ITKMesh
 */
template< typename TInputMesh, typename TOutputImage >
class ITK_TEMPLATE_EXPORT TriangleMeshToBinaryImageFilter:public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef TriangleMeshToBinaryImageFilter Self;
  typedef ImageSource< TOutputImage >     Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  typedef typename TOutputImage::IndexType        IndexType;
  typedef typename TOutputImage::SizeType         SizeType;
  typedef TOutputImage                            OutputImageType;
  typedef typename OutputImageType::Pointer       OutputImagePointer;
  typedef typename OutputImageType::ValueType     ValueType;
  typedef typename OutputImageType::SpacingType   SpacingType;
  typedef typename OutputImageType::DirectionType DirectionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TriangleMeshToBinaryImageFilter, ImageSource);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef TInputMesh                                     InputMeshType;
  typedef typename InputMeshType::Pointer                InputMeshPointer;
  typedef typename InputMeshType::PointType              InputPointType;
  typedef typename InputMeshType::PixelType              InputPixelType;
  typedef typename InputMeshType::MeshTraits::CellTraits InputCellTraitsType;
  typedef typename InputMeshType::CellType               CellType;
  typedef typename InputMeshType::CellsContainerPointer  CellsContainerPointer;
  typedef typename InputMeshType::CellsContainerIterator CellsContainerIterator;

  typedef typename InputMeshType::PointsContainer InputPointsContainer;
  typedef typename InputPointsContainer::Pointer  InputPointsContainerPointer;
  typedef typename InputPointsContainer::Iterator InputPointsContainerIterator;

  typedef itk::PointSet< double, 3 >             PointSetType;
  typedef typename PointSetType::PointsContainer PointsContainer;

  typedef itk::Point< double, 3 > PointType;
  typedef itk::Point< double, 2 > Point2DType;

  typedef itk::Array< double > DoubleArrayType;

  typedef std::vector< Point1D >                Point1DVector;
  typedef std::vector< std::vector< Point1D > > Point1DArray;

  typedef std::vector< Point2DType >                Point2DVector;
  typedef std::vector< std::vector< Point2DType > > Point2DArray;

  typedef std::vector< PointType >                PointVector;
  typedef std::vector< std::vector< PointType > > PointArray;

  typedef std::vector< int > StencilIndexVector;
  /** Spacing (size of a pixel) of the output image. The
   * spacing is the geometric distance between image samples.
   * It is stored internally as double, but may be set from
   * float. \sa GetSpacing() */
  itkSetMacro(Spacing, SpacingType);
  virtual void SetSpacing(const double spacing[3]);

  virtual void SetSpacing(const float spacing[3]);

  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** The Direction is a matix of direction cosines
   *  that specify the direction between samples.
   * */
  itkSetMacro(Direction, DirectionType);
  itkGetConstMacro(Direction, DirectionType);

  /** Set/Get the value for pixels inside the spatial object.
  * By default, this filter will return an image
  * If this "inside" value is changed to a non-null value,
  * the output produced by this filter will be a mask with inside/outside values
  * specified by the user. */
  itkSetMacro(InsideValue, ValueType);
  itkGetConstMacro(InsideValue, ValueType);

  /** Set/Get the value for pixels outside the spatial object.
  * By default, this filter will return an image
  * If this "outside" value is changed to a non-null value,
  * the output produced by this filter will be a mask with inside/outside values
  * specified by the user. */
  itkSetMacro(OutsideValue, ValueType);
  itkGetConstMacro(OutsideValue, ValueType);

  /** The origin of the output image. The origin is the geometric
   * coordinates of the index (0,0,...,0).  It is stored internally
   * as double but may be set from float.
   * \sa GetOrigin() */
  itkSetMacro(Origin, PointType);
  virtual void SetOrigin(const double origin[3]);

  virtual void SetOrigin(const float origin[3]);

  itkGetConstReferenceMacro(Origin, PointType);

  /** Set/Get Index */
  itkSetMacro(Index, IndexType);
  itkGetConstMacro(Index, IndexType);

  /** Set/Get Size */
  itkSetMacro(Size, SizeType);
  itkGetConstMacro(Size, SizeType);

  /** Set the mesh input of this process object.  */
  using Superclass::SetInput;
  void SetInput(InputMeshType *input);

  void SetInfoImage(OutputImageType *InfoImage)
  {
    if ( InfoImage != m_InfoImage )
      {
      this->Modified();
      m_InfoImage = InfoImage;
      }
  }

  /** Get the mesh input of this process object.  */
  InputMeshType * GetInput();

  InputMeshType * GetInput(unsigned int idx);

  /* Set the tolerance for doing spatial searches of the polydata. */
  itkSetMacro(Tolerance, double);
  itkGetConstMacro(Tolerance, double);

protected:
  TriangleMeshToBinaryImageFilter();
  ~TriangleMeshToBinaryImageFilter() ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE {}  // do nothing
  virtual void GenerateData() ITK_OVERRIDE;

  virtual void RasterizeTriangles();

  static int PolygonToImageRaster(PointVector coords, Point1DArray & zymatrix, int extent[6]);

  OutputImageType *m_InfoImage;

  IndexType m_Index;

  SizeType m_Size;

  SpacingType m_Spacing;

  PointType m_Origin;        //start value

  double m_Tolerance;

  ValueType m_InsideValue;
  ValueType m_OutsideValue;

  DirectionType m_Direction;

  StencilIndexVector m_StencilIndex;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TriangleMeshToBinaryImageFilter);

  static bool ComparePoints2D(Point2DType a, Point2DType b);

  static bool ComparePoints1D(Point1D a, Point1D b);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTriangleMeshToBinaryImageFilter.hxx"
#endif

#endif
