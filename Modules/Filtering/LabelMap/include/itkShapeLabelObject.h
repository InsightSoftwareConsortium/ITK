/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkShapeLabelObject_h
#define itkShapeLabelObject_h

#include "itkLabelObject.h"
#include "itkLabelMap.h"
#include "itkMath.h"
#include "itkAffineTransform.h"

namespace itk
{
/**
 *\class ShapeLabelObject
 *  \brief A Label object to store the common attributes related to the shape of the object
 *
 * ShapeLabelObject stores  the common attributes related to the shape of the object
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \ingroup DataRepresentation
 * \ingroup ITKLabelMap
 */
template <typename TLabel, unsigned int VImageDimension>
class ShapeLabelObject : public LabelObject<TLabel, VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShapeLabelObject);

  /** Standard class type aliases */
  using Self = ShapeLabelObject;
  using Superclass = LabelObject<TLabel, VImageDimension>;
  using LabelObjectType = typename Superclass::LabelObjectType;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapeLabelObject, LabelObject);

  using LabelMapType = LabelMap<Self>;

  static constexpr unsigned int ImageDimension = VImageDimension;

  using IndexType = typename Superclass::IndexType;

  using LabelType = TLabel;

  using LineType = typename Superclass::LineType;

  using LengthType = typename Superclass::LengthType;

  using AttributeType = typename Superclass::AttributeType;

  /** The number of pixels. */
  static constexpr AttributeType NUMBER_OF_PIXELS = 100;

  /** PhysicalSize is the size of the object in physical units.
   * It is equal to the NumberOfPixels multiplied by the
   * physical pixel size. Its type is double. */
  static constexpr AttributeType PHYSICAL_SIZE = 101;

  /** Centroid is the position of the center of the shape in
   * physical coordinates. It is not constrained to be in the
   * object, and thus can be outside if the object is not convex.*/
  static constexpr AttributeType CENTROID = 104;

  static constexpr AttributeType BOUNDING_BOX = 105;

  /** NumberOfPixelsOnBorder is the number of pixels in the objects
   * which are on the border of the image. A pixel on several borders
   * (a pixel in a corner) is counted only one time, so the size on
   * border can't be greater than the size of the object. This attribute
   * is particularly useful to remove the objects which are touching
   * too much the border. Its type is unsigned long.*/
  static constexpr AttributeType NUMBER_OF_PIXELS_ON_BORDER = 106;

  /** PerimeterOnBorder is the physical size of the objects which are on
   * the border of the image. In 2D, it is a distance, in 3D, a surface,
   * etc. Contrary to the PhysicalSize attribute which is directly linked to
   * the NumberOfPixels, this attribute is not directly linked to the
   * NumberOfPixelsOnBorder attribute. This attribute is particularly useful
   * to remove the objects which are touching too much the border.
   * Its type is double.*/
  static constexpr AttributeType PERIMETER_ON_BORDER = 107;

  /** FeretDiameter is the diameter in physical units of the sphere which
   * include all the object. The feret diameter is not computed by default,
   * because of its high computation. Its type is double.*/
  static constexpr AttributeType FERET_DIAMETER = 108;

  /** PrincipalMoments contains the principal moments.*/
  static constexpr AttributeType PRINCIPAL_MOMENTS = 109;

  /** BinaryPrincipalAxes contains the principal axes of the object.*/
  static constexpr AttributeType PRINCIPAL_AXES = 110;

  /** Elongation is the  ratio of the largest principal moment to the
   * second largest principal moment. Its value is greater or equal to 1.
   * Its type is double.*/
  static constexpr AttributeType ELONGATION = 111;

  /** The perimeter of the object.*/
  static constexpr AttributeType PERIMETER = 112;

  static constexpr AttributeType ROUNDNESS = 113;

  /** EquivalentRadius is the equivalent radius of the hypersphere of the
   * same size than the label object. The value depends on the image spacing.
   * Its type is double.*/
  static constexpr AttributeType EQUIVALENT_SPHERICAL_RADIUS = 114;

  /** EquivalentPerimeter is the equivalent perimeter of the hypersphere of
   * the same size than the label object. The value depends on the image spacing.
   * Its type is double.*/
  static constexpr AttributeType EQUIVALENT_SPHERICAL_PERIMETER = 115;

  /** EquivalentEllipsoidPerimeter is the size of the ellipsoid of the same size
   * and the same ratio on all the axes than the label object. The value depends
   * on the image spacing.*/
  static constexpr AttributeType EQUIVALENT_ELLIPSOID_DIAMETER = 116;

  static constexpr AttributeType FLATNESS = 117;

  static constexpr AttributeType PERIMETER_ON_BORDER_RATIO = 118;


  /** Origin of the oriented bounding box defined by the principle
   * axes, and the oriented bounding box size */
  static constexpr AttributeType ORIENTED_BOUNDING_BOX_ORIGIN = 119;


  /** Size of the oriented bounding box defined by the principle axes,
   * and the bounding box origin.
   *
   * The combination of the OBB origin, OBB size and OBB direction (
   * principal axes ) defines a coordinate system suitable to use
   * resample the OBB onto it's own image.
   */
  static constexpr AttributeType ORIENTED_BOUNDING_BOX_SIZE = 120;

  static AttributeType
  GetAttributeFromName(const std::string & s)
  {
    if (s == "NumberOfPixels")
    {
      return NUMBER_OF_PIXELS;
    }
    else if (s == "PhysicalSize")
    {
      return PHYSICAL_SIZE;
    }
    else if (s == "Centroid")
    {
      return CENTROID;
    }
    else if (s == "BoundingBox")
    {
      return BOUNDING_BOX;
    }
    else if (s == "NumberOfPixelsOnBorder")
    {
      return NUMBER_OF_PIXELS_ON_BORDER;
    }
    else if (s == "PerimeterOnBorder")
    {
      return PERIMETER_ON_BORDER;
    }
    else if (s == "FeretDiameter")
    {
      return FERET_DIAMETER;
    }
    else if (s == "PrincipalMoments")
    {
      return PRINCIPAL_MOMENTS;
    }
    else if (s == "PrincipalAxes")
    {
      return PRINCIPAL_AXES;
    }
    else if (s == "Elongation")
    {
      return ELONGATION;
    }
    else if (s == "Perimeter")
    {
      return PERIMETER;
    }
    else if (s == "Roundness")
    {
      return ROUNDNESS;
    }
    else if (s == "EquivalentSphericalRadius")
    {
      return EQUIVALENT_SPHERICAL_RADIUS;
    }
    else if (s == "EquivalentSphericalPerimeter")
    {
      return EQUIVALENT_SPHERICAL_PERIMETER;
    }
    else if (s == "EquivalentEllipsoidDiameter")
    {
      return EQUIVALENT_ELLIPSOID_DIAMETER;
    }
    else if (s == "Flatness")
    {
      return FLATNESS;
    }
    else if (s == "PerimeterOnBorderRatio")
    {
      return PERIMETER_ON_BORDER_RATIO;
    }
    else if (s == "OrientedBoundingBoxOrigin")
    {
      return ORIENTED_BOUNDING_BOX_ORIGIN;
    }
    else if (s == "OrientedBoundingBoxSize")
    {
      return ORIENTED_BOUNDING_BOX_SIZE;
    }
    // can't recognize the name
    return Superclass::GetAttributeFromName(s);
  }

  static std::string
  GetNameFromAttribute(const AttributeType & a)
  {
    std::string name;
    switch (a)
    {
      case NUMBER_OF_PIXELS:
        name = "NumberOfPixels";
        break;
      case PHYSICAL_SIZE:
        name = "PhysicalSize";
        break;
      case CENTROID:
        name = "Centroid";
        break;
      case BOUNDING_BOX:
        name = "BoundingBox";
        break;
      case NUMBER_OF_PIXELS_ON_BORDER:
        name = "NumberOfPixelsOnBorder";
        break;
      case PERIMETER_ON_BORDER:
        name = "PerimeterOnBorder";
        break;
      case FERET_DIAMETER:
        name = "FeretDiameter";
        break;
      case PRINCIPAL_MOMENTS:
        name = "PrincipalMoments";
        break;
      case PRINCIPAL_AXES:
        name = "PrincipalAxes";
        break;
      case ELONGATION:
        name = "Elongation";
        break;
      case PERIMETER:
        name = "Perimeter";
        break;
      case ROUNDNESS:
        name = "Roundness";
        break;
      case EQUIVALENT_SPHERICAL_RADIUS:
        name = "EquivalentSphericalRadius";
        break;
      case EQUIVALENT_SPHERICAL_PERIMETER:
        name = "EquivalentSphericalPerimeter";
        break;
      case EQUIVALENT_ELLIPSOID_DIAMETER:
        name = "EquivalentEllipsoidDiameter";
        break;
      case FLATNESS:
        name = "Flatness";
        break;
      case PERIMETER_ON_BORDER_RATIO:
        name = "PerimeterOnBorderRatio";
        break;
      case ORIENTED_BOUNDING_BOX_ORIGIN:
        name = "OrientedBoundingBoxOrigin";
        break;
      case ORIENTED_BOUNDING_BOX_SIZE:
        name = "OrientedBoundingBoxSize";
        break;
      default:
        // can't recognize the name
        name = Superclass::GetNameFromAttribute(a);
        break;
    }
    return name;
  }

  using RegionType = ImageRegion<VImageDimension>;

  using CentroidType = Point<double, VImageDimension>;

  using MatrixType = Matrix<double, VImageDimension, VImageDimension>;

  using VectorType = Vector<double, VImageDimension>;

public:
  using OrientedBoundingBoxDirectionType = MatrixType;

  using OrientedBoundingBoxPointType = Point<double, VImageDimension>;

  using OrientedBoundingBoxSizeType = Vector<double, VImageDimension>;

  using OrientedBoundingBoxVerticesType =
    FixedArray<OrientedBoundingBoxPointType, Math::UnsignedPower<unsigned int>(2, ImageDimension)>;


  const RegionType &
  GetBoundingBox() const
  {
    return m_BoundingBox;
  }

  void
  SetBoundingBox(const RegionType & v)
  {
    m_BoundingBox = v;
  }

  const double &
  GetPhysicalSize() const
  {
    return m_PhysicalSize;
  }

  void
  SetPhysicalSize(const double & v)
  {
    m_PhysicalSize = v;
  }

  const SizeValueType &
  GetNumberOfPixels() const
  {
    return m_NumberOfPixels;
  }

  void
  SetNumberOfPixels(const SizeValueType & v)
  {
    m_NumberOfPixels = v;
  }

  const CentroidType &
  GetCentroid() const
  {
    return m_Centroid;
  }

  void
  SetCentroid(const CentroidType & centroid)
  {
    m_Centroid = centroid;
  }

  const SizeValueType &
  GetNumberOfPixelsOnBorder() const
  {
    return m_NumberOfPixelsOnBorder;
  }

  void
  SetNumberOfPixelsOnBorder(const SizeValueType & v)
  {
    m_NumberOfPixelsOnBorder = v;
  }

  const double &
  GetPerimeterOnBorder() const
  {
    return m_PerimeterOnBorder;
  }

  void
  SetPerimeterOnBorder(const double & v)
  {
    m_PerimeterOnBorder = v;
  }

  const double &
  GetFeretDiameter() const
  {
    return m_FeretDiameter;
  }

  void
  SetFeretDiameter(const double & v)
  {
    m_FeretDiameter = v;
  }

  const VectorType &
  GetPrincipalMoments() const
  {
    return m_PrincipalMoments;
  }

  void
  SetPrincipalMoments(const VectorType & v)
  {
    m_PrincipalMoments = v;
  }

  const MatrixType &
  GetPrincipalAxes() const
  {
    return m_PrincipalAxes;
  }

  void
  SetPrincipalAxes(const MatrixType & v)
  {
    m_PrincipalAxes = v;
  }

  const double &
  GetElongation() const
  {
    return m_Elongation;
  }

  void
  SetElongation(const double & v)
  {
    m_Elongation = v;
  }

  const double &
  GetPerimeter() const
  {
    return m_Perimeter;
  }

  void
  SetPerimeter(const double & v)
  {
    m_Perimeter = v;
  }

  const double &
  GetRoundness() const
  {
    return m_Roundness;
  }

  void
  SetRoundness(const double & v)
  {
    m_Roundness = v;
  }

  const double &
  GetEquivalentSphericalRadius() const
  {
    return m_EquivalentSphericalRadius;
  }

  void
  SetEquivalentSphericalRadius(const double & v)
  {
    m_EquivalentSphericalRadius = v;
  }

  const double &
  GetEquivalentSphericalPerimeter() const
  {
    return m_EquivalentSphericalPerimeter;
  }

  void
  SetEquivalentSphericalPerimeter(const double & v)
  {
    m_EquivalentSphericalPerimeter = v;
  }

  const VectorType &
  GetEquivalentEllipsoidDiameter() const
  {
    return m_EquivalentEllipsoidDiameter;
  }

  void
  SetEquivalentEllipsoidDiameter(const VectorType & v)
  {
    m_EquivalentEllipsoidDiameter = v;
  }

  const double &
  GetFlatness() const
  {
    return m_Flatness;
  }

  void
  SetFlatness(const double & v)
  {
    m_Flatness = v;
  }

  const double &
  GetPerimeterOnBorderRatio() const
  {
    return m_PerimeterOnBorderRatio;
  }

  void
  SetPerimeterOnBorderRatio(const double & v)
  {
    m_PerimeterOnBorderRatio = v;
  }

  const OrientedBoundingBoxPointType &
  GetOrientedBoundingBoxOrigin() const
  {
    return m_OrientedBoundingBoxOrigin;
  }

  void
  SetOrientedBoundingBoxOrigin(const OrientedBoundingBoxPointType & v)
  {
    m_OrientedBoundingBoxOrigin = v;
  }

  const OrientedBoundingBoxSizeType &
  GetOrientedBoundingBoxSize() const
  {
    return m_OrientedBoundingBoxSize;
  }

  void
  SetOrientedBoundingBoxSize(const OrientedBoundingBoxSizeType & v)
  {
    m_OrientedBoundingBoxSize = v;
  }


  // some helper methods - not really required, but really useful!

  /** Get the BoundingBox as an ImageRegion. */
  const RegionType &
  GetRegion() const
  {
    return m_BoundingBox;
  }


  /** Get the direction matrix for the oriented bounding box
   * coordinates. This is an alias for the principal axes. */
  const OrientedBoundingBoxDirectionType &
  GetOrientedBoundingBoxDirection() const
  {
    return this->GetPrincipalAxes();
  }

  /** Get an array of point vertices which define the corners of the
   * oriented bounding box in physical space.
   *
   * The first element in the array contains minimum coordinate values
   * which correspond to the origin while the last contains the maximum.
   * Use the index of the array in binary to determine min/max for the
   * indexed vertex. For example, in 2D, binary  counting will give[0,0], [0,1],
   * [1,0], [1,1], which corresponds to [minX,minY], [minX,maxY],
   * [maxX,minY], [maxX,maxY].
   */
  OrientedBoundingBoxVerticesType
  GetOrientedBoundingBoxVertices() const
  {
    const MatrixType obbToPhysical(this->GetOrientedBoundingBoxDirection().GetTranspose());


    OrientedBoundingBoxVerticesType vertices;

    // Use binary index to map the vertices of the OBB to an array. For
    // example, in 2D, binary  counting will give[0,0], [0,1], [1,0],
    // [1,1], which corresponds to [minX,minY], [minX,maxY],
    // [maxX,minY], [maxX,maxY].
    for (unsigned int i = 0; i < OrientedBoundingBoxVerticesType::Length; ++i)
    {
      constexpr unsigned int         msb = 1 << (ImageDimension - 1);
      Vector<double, ImageDimension> offset;
      for (unsigned int j = 0; j < ImageDimension; ++j)
      {
        if (i & msb >> j)
        {
          offset[j] = m_OrientedBoundingBoxSize[j];
        }
        else
        {
          offset[j] = 0;
        }
      }
      vertices[i] = m_OrientedBoundingBoxOrigin + obbToPhysical * offset;
    }
    return vertices;
  }

  /** Affine transform for mapping to and from principal axis */
  using AffineTransformType = AffineTransform<double, VImageDimension>;
  using AffineTransformPointer = typename AffineTransformType::Pointer;

  /** Get the affine transform from principal axes to physical axes
   * This method returns an affine transform which transforms from
   * the principal axes coordinate system to physical coordinates. */
  AffineTransformPointer
  GetPrincipalAxesToPhysicalAxesTransform() const
  {
    typename AffineTransformType::MatrixType matrix;
    typename AffineTransformType::OffsetType offset;
    for (unsigned int i = 0; i < VImageDimension; i++)
    {
      offset[i] = m_Centroid[i];
      for (unsigned int j = 0; j < VImageDimension; j++)
      {
        matrix[j][i] = m_PrincipalAxes[i][j]; // Note the transposition
      }
    }

    AffineTransformPointer result = AffineTransformType::New();

    result->SetMatrix(matrix);
    result->SetOffset(offset);

    return result;
  }

  /** Get the affine transform from physical axes to principal axes
   * This method returns an affine transform which transforms from
   * the physical coordinate system to the principal axes coordinate
   * system. */
  AffineTransformPointer
  GetPhysicalAxesToPrincipalAxesTransform() const
  {
    typename AffineTransformType::MatrixType matrix;
    typename AffineTransformType::OffsetType offset;
    for (unsigned int i = 0; i < VImageDimension; i++)
    {
      offset[i] = m_Centroid[i];
      for (unsigned int j = 0; j < VImageDimension; j++)
      {
        matrix[j][i] = m_PrincipalAxes[i][j]; // Note the transposition
      }
    }

    AffineTransformPointer result = AffineTransformType::New();
    result->SetMatrix(matrix);
    result->SetOffset(offset);

    AffineTransformPointer inverse = AffineTransformType::New();
    result->GetInverse(inverse);

    return inverse;
  }

  template <typename TSourceLabelObject>
  void
  CopyAttributesFrom(const TSourceLabelObject * src)
  {
    Superclass::template CopyAttributesFrom<TSourceLabelObject>(src);

    m_BoundingBox = src->GetBoundingBox();
    m_NumberOfPixels = src->GetNumberOfPixels();
    m_PhysicalSize = src->GetPhysicalSize();
    m_Centroid = src->GetCentroid();
    m_NumberOfPixelsOnBorder = src->GetNumberOfPixelsOnBorder();
    m_PerimeterOnBorder = src->GetPerimeterOnBorder();
    m_FeretDiameter = src->GetFeretDiameter();
    m_PrincipalMoments = src->GetPrincipalMoments();
    m_PrincipalAxes = src->GetPrincipalAxes();
    m_Elongation = src->GetElongation();
    m_Perimeter = src->GetPerimeter();
    m_Roundness = src->GetRoundness();
    m_EquivalentSphericalRadius = src->GetEquivalentSphericalRadius();
    m_EquivalentSphericalPerimeter = src->GetEquivalentSphericalPerimeter();
    m_EquivalentEllipsoidDiameter = src->GetEquivalentEllipsoidDiameter();
    m_Flatness = src->GetFlatness();
    m_PerimeterOnBorderRatio = src->GetPerimeterOnBorderRatio();
    m_OrientedBoundingBoxOrigin = src->GetOrientedBoundingBoxOrigin();
    m_OrientedBoundingBoxSize = src->GetOrientedBoundingBoxSize();
  }

  template <typename TSourceLabelObject>
  void
  CopyAllFrom(const TSourceLabelObject * src)
  {
    itkAssertOrThrowMacro((src != nullptr), "Null Pointer");
    this->template CopyLinesFrom<TSourceLabelObject>(src);
    this->template CopyAttributesFrom<TSourceLabelObject>(src);
  }

protected:
  ShapeLabelObject()
  {
    m_NumberOfPixels = 0;
    m_PhysicalSize = 0;
    m_Centroid.Fill(0);
    m_NumberOfPixelsOnBorder = 0;
    m_PerimeterOnBorder = 0;
    m_FeretDiameter = 0;
    m_PrincipalMoments.Fill(0);
    m_PrincipalAxes.Fill(0);
    m_Elongation = 0;
    m_Perimeter = 0;
    m_Roundness = 0;
    m_EquivalentSphericalRadius = 0;
    m_EquivalentSphericalPerimeter = 0;
    m_EquivalentEllipsoidDiameter.Fill(0);
    m_Flatness = 0;
    m_PerimeterOnBorderRatio = 0;
    m_OrientedBoundingBoxSize.Fill(0);
    m_OrientedBoundingBoxOrigin.Fill(0);
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);

    os << indent << "NumberOfPixels: " << m_NumberOfPixels << std::endl;
    os << indent << "PhysicalSize: " << m_PhysicalSize << std::endl;
    os << indent << "Perimeter: " << m_Perimeter << std::endl;
    os << indent << "NumberOfPixelsOnBorder: " << m_NumberOfPixelsOnBorder << std::endl;
    os << indent << "PerimeterOnBorder: " << m_PerimeterOnBorder << std::endl;
    os << indent << "PerimeterOnBorderRatio: " << m_PerimeterOnBorderRatio << std::endl;
    os << indent << "Elongation: " << m_Elongation << std::endl;
    os << indent << "Flatness: " << m_Flatness << std::endl;
    os << indent << "Roundness: " << m_Roundness << std::endl;
    os << indent << "Centroid: " << m_Centroid << std::endl;
    os << indent << "BoundingBox: ";
    m_BoundingBox.Print(os, indent);
    os << indent << "EquivalentSphericalRadius: " << m_EquivalentSphericalRadius << std::endl;
    os << indent << "EquivalentSphericalPerimeter: " << m_EquivalentSphericalPerimeter << std::endl;
    os << indent << "EquivalentEllipsoidDiameter: " << m_EquivalentEllipsoidDiameter << std::endl;
    os << indent << "PrincipalMoments: " << m_PrincipalMoments << std::endl;
    os << indent << "PrincipalAxes: " << std::endl << m_PrincipalAxes;
    os << indent << "FeretDiameter: " << m_FeretDiameter << std::endl;
    os << indent << "m_OrientedBoundingBoxSize: " << m_OrientedBoundingBoxSize << std::endl;
    os << indent << "m_OrientedBoundingBoxOrigin: " << m_OrientedBoundingBoxOrigin << std::endl;
  }

private:
  RegionType    m_BoundingBox;
  SizeValueType m_NumberOfPixels;
  double        m_PhysicalSize;
  CentroidType  m_Centroid;
  SizeValueType m_NumberOfPixelsOnBorder;
  double        m_PerimeterOnBorder;
  double        m_FeretDiameter;
  VectorType    m_PrincipalMoments;
  MatrixType    m_PrincipalAxes;
  double        m_Elongation;
  double        m_Perimeter;
  double        m_Roundness;
  double        m_EquivalentSphericalRadius;
  double        m_EquivalentSphericalPerimeter;
  VectorType    m_EquivalentEllipsoidDiameter;
  double        m_Flatness;
  double        m_PerimeterOnBorderRatio;

  OrientedBoundingBoxSizeType  m_OrientedBoundingBoxSize;
  OrientedBoundingBoxPointType m_OrientedBoundingBoxOrigin;
};
} // end namespace itk

#endif
