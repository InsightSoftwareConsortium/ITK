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
#ifndef itkLabelGeometryImageFilter_hxx
#define itkLabelGeometryImageFilter_hxx

#include "itkLabelGeometryImageFilter.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkCastImageFilter.h"

#include "itkAffineTransform.h"
#include "itkResampleImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"

namespace itk
{

namespace
{
//
// Private Helper functions
//
template< unsigned int NDimension >
vnl_matrix< double >
CalculateRotationMatrix(const vnl_symmetric_eigensystem< double > &eig)
{
  vnl_matrix<double> rotationMatrix(NDimension, NDimension, 0);
  for ( unsigned int i = 0; i < NDimension; i++ )
    {
    rotationMatrix.set_column( i, eig.get_eigenvector(i) );
    }
  // After vnl_symmetric_eigensystem, the columns of V are the eigenvectors,
  // sorted by increasing eigenvalue, from most negative to most positive.
  // First reorder the eigenvectors by decreasing eigenvalue.
  rotationMatrix.fliplr();

  // Next, check whether the determinant of the matrix is negative.
  // If it is, then the vectors do not follow the right-hand rule.  We
  // can fix this by making one of them negative.  Make the last
  // eigenvector (with smallest eigenvalue) negative.
  float matrixDet;
  if ( NDimension == 2 )
    {
    matrixDet = vnl_det(rotationMatrix[0], rotationMatrix[1]);
    }
  else if ( NDimension == 3 )
    {
    matrixDet = vnl_det(rotationMatrix[0], rotationMatrix[1], rotationMatrix[2]);
    }
  else
    {
    matrixDet = 0.0f;
    std::cerr << "ERROR: Determinant cannot be calculated for this dimension!" << std::endl;
    }

  if ( matrixDet < 0 )
    {
    rotationMatrix.set_column( NDimension - 1,
                               -rotationMatrix.get_column(NDimension - 1) );
    }

  // Transpose the matrix to yield the rotation matrix.
  rotationMatrix.inplace_transpose();

  return rotationMatrix;
}

template<typename TLabelImage, typename TIntensityImage, typename TInputImage >
bool
CalculateOrientedImage(
  const vnl_symmetric_eigensystem< double > &eig,
  typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::LabelGeometry & labelGeometry,
  bool useLabelImage,
  const TInputImage *inputImage)
{
  static ITK_CONSTEXPR_VAR unsigned int Dimension = TInputImage::ImageDimension;

  // CalculateOrientedBoundingBoxVertices needs to have already been
  // called.  This is taken care of by the flags.

  // Rotate the original object to align with the coordinate
  // system defined by the eigenvectors.
  // Since the resampler moves from the output image to the input
  // image, we take the transpose of the rotation matrix.
  vnl_matrix<double> vnl_RotationMatrix = CalculateRotationMatrix< Dimension >(eig);
  vnl_RotationMatrix.inplace_transpose();

  // Set up the transform.  Here the center of rotation is the
  // centroid of the object, the rotation matrix is specified by the
  // eigenvectors, and there is no translation.
  typedef itk::AffineTransform< double, Dimension > TransformType;
  typename TransformType::Pointer transform = TransformType::New();
  typename TransformType::MatrixType rotationMatrix(vnl_RotationMatrix);
  typename TransformType::CenterType center;
  typename TInputImage::PointType origin;
  for( unsigned int i = 0; i < Dimension; i++ )
  {
    center[i] = labelGeometry.m_Centroid[i] * inputImage->GetSpacing()[i];
    origin[i] = labelGeometry.m_OrientedBoundingBoxOrigin[i] * inputImage->GetSpacing()[i];
  }
  typename TransformType::OutputVectorType translation;
  translation.Fill(0);
  transform->SetCenter(center);
  transform->SetTranslation(translation);
  transform->SetMatrix(rotationMatrix);

  typedef itk::ResampleImageFilter< TInputImage, TIntensityImage > ResampleFilterType;
  typename ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  // The m_OrientedBoundingBoxSize is specified to float precision.
  // Here we need an integer size large enough to contain all of the points, so
  // we take the ceil of it.
  // We also need to ensure that that bounding box is not outside of
  // the image bounds.
  typename ResampleFilterType::SizeType boundingBoxSize;
  for ( unsigned int i = 0; i < Dimension; i++ )
    {
    boundingBoxSize[i] = ( typename ResampleFilterType::SizeType::SizeValueType )std::ceil(
      labelGeometry.m_OrientedBoundingBoxSize[i]);
    }

  resampler->SetTransform(transform);
  resampler->SetSize(boundingBoxSize);
  resampler->SetOutputSpacing( inputImage->GetSpacing() );
  resampler->SetOutputOrigin( origin );
  resampler->SetInput(inputImage);

  if ( useLabelImage )
    {
    // Set up the interpolator.
    // Use a nearest neighbor interpolator since these are labeled images.
    typedef itk::NearestNeighborInterpolateImageFunction< TInputImage, double > InterpolatorType;
    typename InterpolatorType::Pointer interpolator  = InterpolatorType::New();
    resampler->SetInterpolator(interpolator);
    }
  else
    {
    // Set up the interpolator.
    // Use a linear interpolator since these are intensity images.
    typedef itk::LinearInterpolateImageFunction< TInputImage, double > InterpolatorType;
    typename InterpolatorType::Pointer interpolator  = InterpolatorType::New();
    resampler->SetInterpolator(interpolator);
    }
  resampler->Update();
  labelGeometry.m_OrientedIntensityImage->Graft( resampler->GetOutput() );

  return true;
}

}

template< typename TLabelImage, typename TIntensityImage >
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::LabelGeometryImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_CalculatePixelIndices = false;
  m_CalculateOrientedBoundingBox = false;
  m_CalculateOrientedLabelRegions = false;
  m_CalculateOrientedIntensityRegions = false;
}

template< typename TLabelImage, typename TIntensityImage >
void
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GenerateData()
{
  LabelPixelType label;

  // Iterator over the label image.
  ImageRegionConstIterator< TLabelImage > labelIt ( this->GetInput(),
                                                    this->GetInput()->GetBufferedRegion() );

  typedef ImageRegionConstIteratorWithIndex< TLabelImage > ImageIteratorWithIndexType;

  // Iterator over the mapping from labels to geometry values.
  MapIterator mapIt;

  // begin with empty m_LabelGeometryMapper and m_AllLabels
  m_LabelGeometryMapper.clear();
  m_AllLabels.clear();

  // Do the work
  while ( !labelIt.IsAtEnd() )
    {
    label = labelIt.Get();

    mapIt = m_LabelGeometryMapper.find(label);

    // Is the label already in the mapper?
    // If not, create a new geometry object.
    if ( mapIt == m_LabelGeometryMapper.end() )
      {
      typedef typename MapType::value_type MapValueType;

      // map insert is defined as: pair<iterator, bool> insert(const value_type&
      // x)
      mapIt = m_LabelGeometryMapper.insert( MapValueType( label, LabelGeometry() ) ).first;
      }

    // Update the geometry values.

    // LABEL
    ( *mapIt ).second.m_Label = label;

    // BOUNDING BOX
    // The bounding box is defined in (min, max) pairs, such as
    // (xmin,xmax,ymin,ymax,zmin,zmax).
    typename ImageIteratorWithIndexType::IndexType index = labelIt.GetIndex();
    for ( unsigned int i = 0; i < ( 2 * ImageDimension ); i += 2 )
      {
      // Update min
      if ( ( *mapIt ).second.m_BoundingBox[i] > index[i / 2] )
        {
        ( *mapIt ).second.m_BoundingBox[i] = index[i / 2];
        }
      // Update max
      if ( ( *mapIt ).second.m_BoundingBox[i + 1] < index[i / 2] )
        {
        ( *mapIt ).second.m_BoundingBox[i + 1] = index[i / 2];
        }
      }

    // VOLUME
    ( *mapIt ).second.m_ZeroOrderMoment++;

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      // FIRST ORDER RAW MOMENTS
      ( *mapIt ).second.m_FirstOrderRawMoments[i] += index[i];
      }

    // SECOND ORDER RAW MOMENTS
    // Even for ND, the second order moments can be found from just
    // two nested loops since second order moments consider only
    // interactions between pairs of indices.
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      // It is only necessary to fill in half of the matrix since it is
      // symmetric.
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        ( *mapIt ).second.m_SecondOrderRawMoments(i, j) += index[i] * index[j];
        }
      }

    if ( m_CalculatePixelIndices == true )
      {
      // Pixel location list
      ( *mapIt ).second.m_PixelIndices.push_back(index);
      }

    ++labelIt;
    }

  const TIntensityImage *intensityImage = this->GetIntensityInput();

  // If an intensity image is defined, we can also calculate further
  // features.
  if ( intensityImage )
    {
    RealType value;

    // Iterator over the intensity image.
    typedef ImageRegionConstIteratorWithIndex< TIntensityImage > IntensityImageIteratorType;

    IntensityImageIteratorType it( intensityImage, intensityImage->GetBufferedRegion() );

    typename IntensityImageIteratorType::IndexType index;

    labelIt.GoToBegin();

    while ( !it.IsAtEnd() )
      {
      label = labelIt.Get();
      mapIt = m_LabelGeometryMapper.find(label);

      value = static_cast< RealType >( it.Get() );
      index = it.GetIndex();

      // INTEGRATED PIXEL VALUE
      ( *mapIt ).second.m_Sum += value;

      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        // FIRST ORDER WEIGHTED RAW MOMENTS
        ( *mapIt ).second.m_FirstOrderWeightedRawMoments[i] += index[i]
                                                               * ( typename LabelIndexType::IndexValueType )value;
        }

      ++it;
      ++labelIt;
      }
    }

  // If there is no intensity input defined, the oriented
  // intensity regions cannot be calculated.
  if ( !intensityImage )
    {
    if ( m_CalculateOrientedIntensityRegions )
      {
      std::cerr
      << "ERROR: An input intensity image must be used in order to calculate the oriented intensity image."
      << std::endl;
      }
    m_CalculateOrientedIntensityRegions = false;
    }

  // We need to add to the second order moment the second order
  // moment of a pixel.  This can be derived analytically.  The first
  // order moment of a pixel can be shown to be 0, and the first order
  // cross moment can also be shown to be 0.  The second order moment
  // can be shown to be 1/12.
  float pixelSecondOrderCentralMoment = 1.0f / 12.0f;

  // Now that the m_LabelGeometryMapper has been updated for all
  // pixels in the image, we can calculate other geometrical values.
  // Loop through all labels of the image.
  for ( mapIt = m_LabelGeometryMapper.begin(); mapIt != m_LabelGeometryMapper.end(); mapIt++ )
    {
    // Update the bounding box measurements.
    ( *mapIt ).second.m_BoundingBoxVolume = 1;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      ( *mapIt ).second.m_BoundingBoxSize[i] =
        ( *mapIt ).second.m_BoundingBox[2 * i + 1] - ( *mapIt ).second.m_BoundingBox[2 * i] + 1;
      ( *mapIt ).second.m_BoundingBoxVolume = ( *mapIt ).second.m_BoundingBoxVolume
                                              * ( *mapIt ).second.m_BoundingBoxSize[i];
      }

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      // Normalize the centroid sum by the count to get the centroid.
      ( *mapIt ).second.m_Centroid[i] =
        static_cast< typename LabelPointType::ValueType >( ( *mapIt ).second.m_FirstOrderRawMoments[i] )
        / ( *mapIt ).second.m_ZeroOrderMoment;

      // This is the weighted sum.  It only calculates correctly if
      // the intensity image is defined.
      if ( !intensityImage )
        {
        ( *mapIt ).second.m_WeightedCentroid[i] = 0.0;
        }
      else
        {
        ( *mapIt ).second.m_WeightedCentroid[i] =
          static_cast< typename LabelPointType::ValueType >( ( *mapIt ).second.m_FirstOrderWeightedRawMoments[i] )
          / ( *mapIt ).second.m_Sum;
        }
      }

    // Using the raw moments, we can calculate the central moments.
    MatrixType normalizedSecondOrderCentralMoments(ImageDimension, ImageDimension, 0);
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        normalizedSecondOrderCentralMoments(i,
                                            j) =
          ( ( *mapIt ).second.m_SecondOrderRawMoments(i,
                                                      j) ) / ( ( *mapIt ).second.m_ZeroOrderMoment )
          - ( *mapIt ).second.m_Centroid[i]
          * ( *mapIt ).second.m_Centroid[j];
        // We need to add to the second order moment the second order
        // moment of a pixel.  This can be derived analytically.
        if ( i == j )
          {
          normalizedSecondOrderCentralMoments(i, j) += pixelSecondOrderCentralMoment;
          }
        }
      }

    // Compute the eigenvalues/eigenvectors of the covariance matrix.
    // The result is stored in increasing eigenvalues with
    // corresponding eigenvectors.
    vnl_symmetric_eigensystem< double > eig(normalizedSecondOrderCentralMoments);

    // Calculate the eigenvalues/eigenvectors
    VectorType eigenvalues(ImageDimension, 0);
    MatrixType eigenvectors(ImageDimension, ImageDimension, 0);
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      eigenvectors.set_column( i, eig.get_eigenvector(i) );
      eigenvalues[i] = eig.get_eigenvalue(i);
      }
    ( *mapIt ).second.m_Eigenvalues = eigenvalues;
    ( *mapIt ).second.m_Eigenvectors = eigenvectors;

    itk::FixedArray< float, ImageDimension > axesLength;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      axesLength[i] = 4 * std::sqrt(eigenvalues[i]);
      }
    ( *mapIt ).second.m_AxesLength = axesLength;

    // The following three features are currently only meaningful in 2D.
    ( *mapIt ).second.m_Eccentricity = std::sqrt( ( eigenvalues[ImageDimension-1] - eigenvalues[0] ) / eigenvalues[ImageDimension-1] );
    ( *mapIt ).second.m_Elongation = axesLength[ImageDimension-1] / axesLength[0];
    RealType orientation = std::atan2(eig.get_eigenvector(ImageDimension-1)[1], eig.get_eigenvector(ImageDimension-1)[0]);
    // Change the orientation from being between -pi to pi to being from 0 to pi.
    // We can add pi because the orientation of the major axis is symmetric about the origin.
    ( *mapIt ).second.m_Orientation = orientation < 0.0 ? orientation + itk::Math::pi : orientation;

    if ( m_CalculateOrientedBoundingBox == true )
      {
      // Calculate the oriented bounding box using the eigenvectors.
      CalculateOrientedBoundingBoxVertices(eig, ( *mapIt ).second);
      }
    if ( m_CalculateOrientedLabelRegions == true )
      {
      CalculateOrientedImage<TLabelImage, TIntensityImage>(
        eig, ( *mapIt ).second, true, this->GetInput() );
      }
    if ( m_CalculateOrientedIntensityRegions == true )
      {
      // If there is no intensity input defined, the oriented
      // intensity regions cannot be calculated.
      if ( this->GetIntensityInput() )
        {
        CalculateOrientedImage<TLabelImage, TIntensityImage>(
          eig, ( *mapIt ).second, false, this->GetIntensityInput() );
        }
      }

    m_AllLabels.push_back( ( *mapIt ).first );
    }
}

template< typename TLabelImage, typename TIntensityImage >
bool
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::CalculateOrientedBoundingBoxVertices(vnl_symmetric_eigensystem< double > eig, LabelGeometry & labelGeometry)
{
  // Calculate the oriented bounding box using the eigenvectors.
  // For each label, the pixels are rotated to the new coordinate
  // system defined by the eigenvectors.  The bounding boxes are
  // calculated in this space, and then they are rotated back to the
  // original coordinate system to define the oriented bounding boxes.
  // The reverse rotation is the transpose of the rotation matrix.

  // m_PixelIndices needs to have already been calculated.  This is
  // handled by the flags.

  MatrixType rotationMatrix = CalculateRotationMatrix< TLabelImage::ImageDimension >(eig);
  MatrixType inverseRotationMatrix = rotationMatrix.transpose();

  labelGeometry.m_RotationMatrix = rotationMatrix;

  // Convert the pixel locations to a vnl_matrix.
  // Subtract the centroid of the region so that the rotation will
  // be about the center of the region.
  MatrixType pixelLocations(ImageDimension, labelGeometry.m_PixelIndices.size(), 0);
  for ( unsigned int i = 0; i < labelGeometry.m_PixelIndices.size(); i++ )
    {
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      pixelLocations(j, i) = labelGeometry.m_PixelIndices[i][j] - labelGeometry.m_Centroid[j];
      }
    }

  // Rotate the points by the rotation matrix from the eigenvectors.
  MatrixType transformedPixelLocations = rotationMatrix * pixelLocations;

  // Find the min and max of the point locations in this new
  // coordinate system.  These values will be float, so we use a
  // BoundingBoxFloatType rather than BoundingBoxType.
  // The bounding box order is [minX,maxX,minY,maxY,...]
  BoundingBoxFloatType transformedBoundingBox;
  // Initialize the bounding box values.
  for ( unsigned int i = 0; i < ImageDimension * 2; i += 2 )
    {
    transformedBoundingBox[i] = NumericTraits< float >::max();
    transformedBoundingBox[i + 1] = NumericTraits< float >::NonpositiveMin();
    }

  for ( unsigned int column = 0; column < transformedPixelLocations.columns(); column++ )
    {
    for ( unsigned int i = 0; i < ( 2 * ImageDimension ); i += 2 )
      {
      // Update min
      if ( transformedBoundingBox[i] > transformedPixelLocations(i / 2, column) )
        {
        transformedBoundingBox[i] = transformedPixelLocations(i / 2, column);
        }
      // Update max
      if ( transformedBoundingBox[i + 1] < transformedPixelLocations(i / 2, column) )
        {
        transformedBoundingBox[i + 1] = transformedPixelLocations(i / 2, column);
        }
      }
    }

  // Add 0.5 pixel buffers on each side of the bounding box to be sure to
  // encompass the pixels and not cut through them.
  for ( unsigned int i = 0; i < ( 2 * ImageDimension ); i += 2 )
    {
    // If the index corresponds with a min, subtract 0.5.
    transformedBoundingBox[i] -= 0.5;
    // Otherwise, add 0.5.
    transformedBoundingBox[i + 1] += 0.5;
    }

  // The bounding box volume can be calculated directly from the
  // transformed bounding box.
  // Since 0.5 was already added to the border of the bounding box,
  // it is not necessary to add 1 to the range to get the correct range.
  labelGeometry.m_OrientedBoundingBoxVolume = 1;
  for ( unsigned int i = 0; i < ( 2 * ImageDimension ); i += 2 )
    {
    labelGeometry.m_OrientedBoundingBoxSize[i / 2] = transformedBoundingBox[i + 1] - transformedBoundingBox[i];
    labelGeometry.m_OrientedBoundingBoxVolume *= transformedBoundingBox[i + 1] - transformedBoundingBox[i];
    }

  // The bounding box cannot be transformed directly because an
  // oriented bounding box cannot be specified simply by the min and
  // max in each dimension.  Instead, the oriented bounding box is
  // specified by the points corresponding to the vertices of the
  // oriented bounding box.  We find these from the oriented
  // bounding box and transform them back to the original coordinate frame.
  // The order of the vertices corresponds with binary counting,
  // where min is zero and max is one.  For example, in 2D, binary
  // counting will give [0,0],[0,1],[1,0],[1,1], which corresponds
  // to [minX,minY],[minX,maxY],[maxX,minY],[maxX,maxY].
  // Loop through each dimension of the bounding box and find all of the
  // vertices.
  unsigned int numberOfVertices = 1 << ImageDimension;
  MatrixType     transformedBoundingBoxVertices(ImageDimension, numberOfVertices, 0);
  int            val;
  LabelIndexType binaryIndex;
  int            arrayIndex;
  for ( unsigned int i = 0; i < numberOfVertices; i++ )
    {
    val = i;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      // This is the binary index as described above.
      binaryIndex[j] = val % 2;
      val = val / 2;
      // This is the index into the transformedBoundingBox array
      // corresponding to the binaryIndex.
      arrayIndex = binaryIndex[j] + 2 * j;
      transformedBoundingBoxVertices(j, i) = transformedBoundingBox[arrayIndex];
      }
    }

  // Transform the transformed bounding box vertices back to the
  // original coordinate system.
  MatrixType orientedBoundingBoxVertices = inverseRotationMatrix * transformedBoundingBoxVertices;

  // Add the centroid back to each of the vertices since it was
  // subtracted when the points were rotated.
  for ( unsigned int i = 0; i < orientedBoundingBoxVertices.columns(); i++ )
    {
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      orientedBoundingBoxVertices(j, i) += labelGeometry.m_Centroid[j];
      // Copy the oriented bounding box vertices back to a vector of
      // points for the mapper.
      labelGeometry.m_OrientedBoundingBoxVertices[i][j] = orientedBoundingBoxVertices(j, i);
      }
    }

  // Find the origin of the oriented bounding box.
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    labelGeometry.m_OrientedBoundingBoxOrigin[i] = transformedBoundingBox[2 * i] + labelGeometry.m_Centroid[i];
    }

  return true;
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::LabelIndicesType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetPixelIndices(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    LabelIndicesType emptyVector;
    emptyVector.clear();
    return emptyVector;
    }
  else
    {
    return ( *mapIt ).second.m_PixelIndices;
    }
}

template< typename TLabelImage, typename TIntensityImage >
SizeValueType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetVolume(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return 0;
    }
  else
    {
    return ( *mapIt ).second.m_ZeroOrderMoment;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RealType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetIntegratedIntensity(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< RealType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_Sum;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::LabelPointType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetCentroid(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    LabelPointType emptyCentroid;
    emptyCentroid.Fill(NumericTraits< typename LabelPointType::ValueType >::ZeroValue());
    return emptyCentroid;
    }
  else
    {
    return ( *mapIt ).second.m_Centroid;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::LabelPointType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetWeightedCentroid(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    LabelPointType emptyCentroid;
    emptyCentroid.Fill(NumericTraits< typename LabelPointType::ValueType >::ZeroValue());
    return emptyCentroid;
    }
  else
    {
    return ( *mapIt ).second.m_WeightedCentroid;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::VectorType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetEigenvalues(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    VectorType emptyVector(ImageDimension, 0);
    return emptyVector;
    }
  else
    {
    return ( *mapIt ).second.m_Eigenvalues;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::MatrixType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetEigenvectors(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    MatrixType emptyMatrix(ImageDimension, ImageDimension, 0);
    return emptyMatrix;
    }
  else
    {
    return ( *mapIt ).second.m_Eigenvectors;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::AxesLengthType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetAxesLength(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    LabelPointType emptyAxesLength;
    emptyAxesLength.Fill(NumericTraits< typename AxesLengthType::ValueType >::ZeroValue());
    return emptyAxesLength;
    }
  else
    {
    return ( *mapIt ).second.m_AxesLength;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RealType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetMinorAxisLength(LabelPixelType label) const
{
  AxesLengthType axisLength = GetAxesLength(label);

  return axisLength[0];
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RealType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetMajorAxisLength(LabelPixelType label) const
{
  AxesLengthType axisLength = GetAxesLength(label);

  return axisLength[ImageDimension - 1];
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RealType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetEccentricity(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< RealType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_Eccentricity;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RealType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetElongation(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< RealType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_Elongation;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RealType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetOrientation(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< RealType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_Orientation;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::BoundingBoxType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetBoundingBox(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    BoundingBoxType emptyBox;
    emptyBox.Fill(NumericTraits< typename BoundingBoxType::ValueType >::ZeroValue());
    // label does not exist, return a default value
    return emptyBox;
    }
  else
    {
    return ( *mapIt ).second.m_BoundingBox;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RealType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetBoundingBoxVolume(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< RealType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_BoundingBoxVolume;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::LabelSizeType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetBoundingBoxSize(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    LabelSizeType emptySize;
    emptySize.Fill(NumericTraits< typename LabelSizeType::SizeValueType >::ZeroValue());
    return emptySize;
    }
  else
    {
    return ( *mapIt ).second.m_BoundingBoxSize;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::BoundingBoxVerticesType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetOrientedBoundingBoxVertices(LabelPixelType label) const
{
  unsigned int numberOfVertices = 1 << ImageDimension;
  MapConstIterator mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    LabelPointType emptyPoint;
    emptyPoint.Fill(0);
    BoundingBoxVerticesType emptyVertices;
    emptyVertices.resize(numberOfVertices, emptyPoint);
    return emptyVertices;
    }
  else
    {
    return ( *mapIt ).second.m_OrientedBoundingBoxVertices;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RealType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetOrientedBoundingBoxVolume(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< RealType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_OrientedBoundingBoxVolume;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::LabelPointType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetOrientedBoundingBoxSize(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
//     LabelSizeType emptySize;
//     emptySize.Fill( NumericTraits<LabelSizeType::SizeValueType>::ZeroValue());
//     return emptySize;
    LabelPointType emptySize;
    emptySize.Fill(NumericTraits< typename LabelPointType::ValueType >::ZeroValue());
    return emptySize;
    }
  else
    {
    return ( *mapIt ).second.m_OrientedBoundingBoxSize;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::LabelPointType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetOrientedBoundingBoxOrigin(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    LabelPointType emptySize;
    emptySize.Fill(NumericTraits< typename LabelPointType::ValueType >::ZeroValue());
    return emptySize;
    }
  else
    {
    return ( *mapIt ).second.m_OrientedBoundingBoxOrigin;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::MatrixType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetRotationMatrix(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    MatrixType emptyMatrix(ImageDimension, ImageDimension, 0);
    return emptyMatrix;
    }
  else
    {
    return ( *mapIt ).second.m_RotationMatrix;
    }
}

template< typename TLabelImage, typename TIntensityImage >
typename LabelGeometryImageFilter< TLabelImage, TIntensityImage >::RegionType
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetRegion(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);

  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    RegionType emptyRegion;
    // label does not exist, return a default value
    return emptyRegion;
    }
  else
    {
    BoundingBoxType bbox = this->GetBoundingBox(label);
    IndexType       index;
    SizeType        size;

    unsigned int dimension = bbox.Size() / 2;

    for ( unsigned int i = 0; i < dimension; i++ )
      {
      index[i] = bbox[2 * i];
      size[i] = bbox[2 * i + 1] - bbox[2 * i] + 1;
      }
    RegionType region;
    region.SetSize(size);
    region.SetIndex(index);

    return region;
    }
}

template< typename TLabelImage, typename TIntensityImage >
TLabelImage *
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetOrientedLabelImage(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return ITK_NULLPTR;
    }
  else
    {
    return ( *mapIt ).second.m_OrientedLabelImage;
    }
}

template< typename TLabelImage, typename TIntensityImage >
TIntensityImage *
LabelGeometryImageFilter< TLabelImage, TIntensityImage >
::GetOrientedIntensityImage(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelGeometryMapper.find(label);
  if ( mapIt == m_LabelGeometryMapper.end() )
    {
    // label does not exist, return a default value
    return ITK_NULLPTR;
    }
  else
    {
    return ( *mapIt ).second.m_OrientedIntensityImage;
    }
}

template< typename TImage, typename TLabelImage >
void
LabelGeometryImageFilter< TImage, TLabelImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of labels: " << m_LabelGeometryMapper.size()
     << std::endl;

  MapConstIterator mapIt;
  for ( mapIt = m_LabelGeometryMapper.begin(); mapIt != m_LabelGeometryMapper.end(); mapIt++ )
    {
    typedef typename NumericTraits< LabelPixelType >::PrintType  LabelPrintType;
    os << indent << "Label[" << (LabelPrintType)( ( *mapIt ).second.m_Label ) << "]: ";
    os << "\t Volume: " << ( *mapIt ).second.m_ZeroOrderMoment;
    os << "\t Integrated Intensity: " << ( *mapIt ).second.m_Sum;
    os << "\t Centroid: " << ( *mapIt ).second.m_Centroid;
    os << "\t Weighted Centroid: " << ( *mapIt ).second.m_WeightedCentroid;
    os << "\t Axes Length: " << ( *mapIt ).second.m_AxesLength;
    os << "\t Eccentricity: " << ( *mapIt ).second.m_Eccentricity;
    os << "\t Elongation: " << ( *mapIt ).second.m_Elongation;
    os << "\t Orientation: " << ( *mapIt ).second.m_Orientation;
    os << "\t Bounding box: " << ( *mapIt ).second.m_BoundingBox;
    os << "\t Bounding box volume: " << ( *mapIt ).second.m_BoundingBoxVolume;
    os << "\t Bounding box size: " << ( *mapIt ).second.m_BoundingBoxSize;
    // Oriented bounding box verticies
    os << "\t Oriented bounding box volume: " << ( *mapIt ).second.m_OrientedBoundingBoxVolume;
    os << "\t Oriented bounding box size: " << ( *mapIt ).second.m_OrientedBoundingBoxSize;
    // Rotation matrix
    os << std::endl;
    os << "\t Calculate oriented intensity regions: " << m_CalculateOrientedIntensityRegions;
    os << "\t Calculate pixel indices: " << m_CalculatePixelIndices;
    os << "\t Calculate oriented bounding box: " << m_CalculateOrientedBoundingBox;
    os << "\t Calculate oriented label regions: " << m_CalculateOrientedLabelRegions;
    os << "\n\n";
    }
}
} // end namespace itk
#endif
