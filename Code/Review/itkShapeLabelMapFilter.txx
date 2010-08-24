/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeLabelMapFilter_txx
#define __itkShapeLabelMapFilter_txx

#include "itkShapeLabelMapFilter.h"
#include "itkProgressReporter.h"
#include "itkNeighborhoodIterator.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "vnl/algo/vnl_real_eigensystem.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "vnl/vnl_math.h"

namespace itk
{
template< class TImage, class TLabelImage >
ShapeLabelMapFilter< TImage, TLabelImage >
::ShapeLabelMapFilter()
{
  m_ComputeFeretDiameter = false;
  m_ComputePerimeter = false;
}

template< class TImage, class TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  // Generate the label image, if needed
  if ( m_ComputeFeretDiameter || m_ComputePerimeter )
    {
    if ( !m_LabelImage )
      {
      // generate an image of the labelized image
      typedef LabelMapToLabelImageFilter< TImage, LabelImageType > LCI2IType;
      typename LCI2IType::Pointer lci2i = LCI2IType::New();
      lci2i->SetInput( this->GetOutput() );
      // Respect the number of threads of the filter
      lci2i->SetNumberOfThreads( this->GetNumberOfThreads() );
      lci2i->Update();
      m_LabelImage = lci2i->GetOutput();
      }
    }

  // Delegate the computation of the perimeter to a dedicated calculator
  if ( m_ComputePerimeter )
    {
    m_PerimeterCalculator = PerimeterCalculatorType::New();
    m_PerimeterCalculator->SetImage(m_LabelImage);
    m_PerimeterCalculator->Compute();
    }
}

template< class TImage, class TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::ThreadedProcessLabelObject(LabelObjectType *labelObject)
{
  ImageType *            output = this->GetOutput();
  const LabelPixelType & label = labelObject->GetLabel();

  // Compute the size per pixel, to be used later
  double sizePerPixel = 1;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    sizePerPixel *= output->GetSpacing()[i];
    }

  typename std::vector< double > sizePerPixelPerDimension;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    sizePerPixelPerDimension.push_back(sizePerPixel / output->GetSpacing()[i]);
    }

  // Compute the max the index on the border of the image
  IndexType borderMin = output->GetLargestPossibleRegion().GetIndex();
  IndexType borderMax = borderMin;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    borderMax[i] += output->GetLargestPossibleRegion().GetSize()[i] - 1;
    }

  // Init the vars
  unsigned long                             size = 0;
  ContinuousIndex< double, ImageDimension > centroid;
  centroid.Fill(0);
  IndexType mins;
  mins.Fill( NumericTraits< long >::max() );
  IndexType maxs;
  maxs.Fill( NumericTraits< long >::NonpositiveMin() );
  unsigned long sizeOnBorder = 0;
  double        physicalSizeOnBorder = 0;
  MatrixType    centralMoments;
  centralMoments.Fill(0);

  typename LabelObjectType::LineContainerType::const_iterator lit;
  typename LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();

  // Iterate over all the lines
  for ( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
    {
    const IndexType & idx = lit->GetIndex();
    unsigned long     length = lit->GetLength();

    // Update the size
    size += length;

    // Update the centroid - and report the progress
    // First, update the axes that are not 0
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      centroid[i] += (long)length * idx[i];
      }
    // Then, update the axis 0
    centroid[0] += idx[0] * (long)length + ( length * ( length - 1 ) ) / 2.0;

    // Update the mins and maxs
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( idx[i] < mins[i] )
        {
        mins[i] = idx[i];
        }
      if ( idx[i] > maxs[i] )
        {
        maxs[i] = idx[i];
        }
      }
    // Must fix the max for the axis 0
    if ( idx[0] + (long)length > maxs[0] )
      {
      maxs[0] = idx[0] + length - 1;
      }

    // Object is on a border ?
    bool isOnBorder = false;
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      if ( idx[i] == borderMin[i] || idx[i] == borderMax[i] )
        {
        isOnBorder = true;
        break;
        }
      }
    if ( isOnBorder )
      {
      // The line touch a border on a dimension other than 0, so
      // all the line touch a border
      sizeOnBorder += length;
      }
    else
      {
      // We must check for the dimension 0
      bool isOnBorder0 = false;
      if ( idx[0] == borderMin[0] )
        {
        // One more pixel on the border
        sizeOnBorder++;
        isOnBorder0 = true;
        }
      if ( !isOnBorder0 || length > 1 )
        {
        // We can check for the end of the line
        if ( idx[0] + (long)length - 1 == borderMax[0] )
          {
          // One more pixel on the border
          sizeOnBorder++;
          }
        }
      }

    // Physical size on border
    // First, the dimension 0
    if ( idx[0] == borderMin[0] )
      {
      // Fhe beginning of the line
      physicalSizeOnBorder += sizePerPixelPerDimension[0];
      }
    if ( idx[0] + (long)length - 1 == borderMax[0] )
      {
      // And the end of the line
      physicalSizeOnBorder += sizePerPixelPerDimension[0];
      }
    // Then the other dimensions
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      if ( idx[i] == borderMin[i] )
        {
        // one border
        physicalSizeOnBorder += sizePerPixelPerDimension[i] * length;
        }
      if ( idx[i] == borderMax[i] )
        {
        // and the other
        physicalSizeOnBorder += sizePerPixelPerDimension[i] * length;
        }
      }

    // moments computation
// ****************************************************************
// that commented code is the basic implementation. The next peace of code
// give the same result in a much efficient way, by using expended formulae
// allowed by the binary case instead of loops.
// ****************************************************************
//     long endIdx0 = idx[0] + length;
//     for( IndexType iidx = idx; iidx[0]<endIdx0; iidx[0]++)
//       {
//       typename LabelObjectType::CentroidType pP;
//       output->TransformIndexToPhysicalPoint(iidx, pP);
//
//       for(unsigned int i=0; i<ImageDimension; i++)
//         {
//         for(unsigned int j=0; j<ImageDimension; j++)
//           {
//           centralMoments[i][j] += pP[i] * pP[j];
//           }
//         }
//       }
    // get the physical position and the spacing - they are used several times
    // later
    typename LabelObjectType::CentroidType physicalPosition;
    output->TransformIndexToPhysicalPoint(idx, physicalPosition);
    const typename ImageType::SpacingType & spacing = output->GetSpacing();
    // the sum of x positions, also reused several times
    double sumX = length * ( physicalPosition[0] + ( spacing[0] * ( length - 1 ) ) / 2.0 );
    // the real job - the sum of square of x positions
    // that's the central moments for dims 0, 0
    centralMoments[0][0] += length * ( physicalPosition[0] * physicalPosition[0]
                                       + spacing[0]
                                       * ( length
                                           - 1 ) * ( ( spacing[0] * ( 2 * length - 1 ) ) / 6.0 + physicalPosition[0] ) );
    // the other ones
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      // do this one here to avoid the double assigment in the following loop
      // when i == j
      centralMoments[i][i] += length * physicalPosition[i] * physicalPosition[i];
      // central moments are symetrics, so avoid to compute them 2 times
      for ( unsigned int j = i + 1; j < ImageDimension; j++ )
        {
        // note that we won't use that code if the image dimension is less than
        // 3
        // --> the tests should be in 3D at least
        double cm = length * physicalPosition[i] * physicalPosition[j];
        centralMoments[i][j] += cm;
        centralMoments[j][i] += cm;
        }
      // the last moments: the ones for the dimension 0
      double cm = sumX * physicalPosition[i];
      centralMoments[i][0] += cm;
      centralMoments[0][i] += cm;
      }
    }

  // final computation
  typename LabelObjectType::RegionType::SizeType regionSize;
  double minSize = NumericTraits< double >::max();
  double maxSize = NumericTraits< double >::NonpositiveMin();
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    centroid[i] /= size;
    regionSize[i] = maxs[i] - mins[i] + 1;
    double s = regionSize[i] * output->GetSpacing()[i];
    minSize = vnl_math_min(s, minSize);
    maxSize = vnl_math_max(s, maxSize);
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      centralMoments[i][j] /= size;
      }
    }
  typename LabelObjectType::RegionType region(mins, regionSize);
  typename LabelObjectType::CentroidType physicalCentroid;
  output->TransformContinuousIndexToPhysicalPoint(centroid, physicalCentroid);

  // Center the second order moments
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      centralMoments[i][j] -= physicalCentroid[i] * physicalCentroid[j];
      }
    }

  // Compute principal moments and axes
  VectorType                          principalMoments;
  vnl_symmetric_eigensystem< double > eigen( centralMoments.GetVnlMatrix() );
  vnl_diag_matrix< double >           pm = eigen.D;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    principalMoments[i] = pm(i, i);
    }
  MatrixType principalAxes = eigen.V.transpose();

  // Add a final reflection if needed for a proper rotation,
  // by multiplying the last row by the determinant
  vnl_real_eigensystem                     eigenrot( principalAxes.GetVnlMatrix() );
  vnl_diag_matrix< vcl_complex< double > > eigenval = eigenrot.D;
  vcl_complex< double >                    det(1.0, 0.0);

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    det *= eigenval(i, i);
    }

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    principalAxes[ImageDimension - 1][i] *= std::real(det);
    }

  double elongation = 0;
  double flatness = 0;
  if ( ImageDimension < 2 )
    {
    elongation = 1;
    flatness = 1;
    }
  else if ( principalMoments[0] != 0 )
    {
    elongation = vcl_sqrt(principalMoments[ImageDimension - 1] / principalMoments[ImageDimension - 2]);
    flatness = vcl_sqrt(principalMoments[1] / principalMoments[0]);
    }

  double physicalSize = size * sizePerPixel;
  double equivalentRadius = HyperSphereRadiusFromVolume(physicalSize);
  double equivalentPerimeter = HyperSpherePerimeter(equivalentRadius);

  // Compute equivalent ellipsoid radius
  VectorType ellipsoidSize;
  double     edet = 1.0;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    edet *= principalMoments[i];
    }
  edet = vcl_pow(edet, 1.0 / ImageDimension);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( edet != 0.0 )
      {
      ellipsoidSize[i] = 2.0 *equivalentRadius *vcl_sqrt(principalMoments[i] / edet);
      }
    else
      {
      ellipsoidSize[i] = 0.0;
      }
    }

  // Set the values in the object
  labelObject->SetSize(size);
  labelObject->SetPhysicalSize(physicalSize);
  labelObject->SetRegion(region);
  labelObject->SetCentroid(physicalCentroid);
  if ( minSize != 0 )
    {
    labelObject->SetRegionElongation(maxSize / minSize);
    }
  if ( region.GetNumberOfPixels() != 0 )
    {
    labelObject->SetSizeRegionRatio( size / (double)region.GetNumberOfPixels() );
    }
  labelObject->SetSizeOnBorder(sizeOnBorder);
  labelObject->SetPhysicalSizeOnBorder(physicalSizeOnBorder);
  labelObject->SetBinaryPrincipalMoments(principalMoments);
  labelObject->SetBinaryPrincipalAxes(principalAxes);
  labelObject->SetBinaryElongation(elongation);
  labelObject->SetEquivalentRadius(equivalentRadius);
  labelObject->SetEquivalentPerimeter(equivalentPerimeter);
  labelObject->SetEquivalentEllipsoidSize(ellipsoidSize);
  labelObject->SetBinaryFlatness(flatness);

  // Don't compute the Feret Diameter on the 0 label!
  if ( m_ComputeFeretDiameter && labelObject->GetLabel() != 0 )
    {
    this->ComputeFeretDiameter(labelObject);
    }

  // Be sure that the calculator has the perimeter estimation for that label.
  // The calculator may not have the label if the object is only on a border.
  // It will occurre for sure when processing a 2D image with a 3D filter.
  if ( m_ComputePerimeter && m_PerimeterCalculator->HasLabel(label) )
    {
    double perimeter = m_PerimeterCalculator->GetPerimeter(label);
    labelObject->SetPerimeter(perimeter);
    labelObject->SetRoundness(equivalentPerimeter / perimeter);
    }
}

template< class TImage, class TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::ComputeFeretDiameter(LabelObjectType *labelObject)
{
  const LabelPixelType & label = labelObject->GetLabel();

  typedef typename std::deque< IndexType > IndexListType;
  IndexListType idxList;

  // the iterators
  typename LabelObjectType::LineContainerType::const_iterator lit;
  typename LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();

  typedef typename itk::ConstNeighborhoodIterator< LabelImageType > NeighborIteratorType;
  SizeType neighborHoodRadius;
  neighborHoodRadius.Fill(1);
  NeighborIteratorType                        it( neighborHoodRadius, m_LabelImage, m_LabelImage->GetBufferedRegion() );
  ConstantBoundaryCondition< LabelImageType > lcbc;
  // Use label + 1 to have a label different of the current label on the border
  lcbc.SetConstant(label + 1);
  it.OverrideBoundaryCondition(&lcbc);
  it.GoToBegin();

  // Iterate over all the lines
  for ( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
    {
    const IndexType & firstIdx = lit->GetIndex();
    unsigned long     length = lit->GetLength();

    long endIdx0 = firstIdx[0] + length;
    for ( IndexType idx = firstIdx; idx[0] < endIdx0; idx[0]++ )
      {
      // Move the iterator to the new location
      it += idx - it.GetIndex();

      // Push the pixel in the list if it is on the border of the object
      for ( unsigned i = 0; i < it.Size(); i++ )
        {
        if ( it.GetPixel(i) != label )
          {
          idxList.push_back(idx);
          break;
          }
        }
      }
    }

  ImageType *output = this->GetOutput();

  const typename ImageType::SpacingType & spacing = output->GetSpacing();

  typedef typename ImageType::OffsetValueType OffsetValueType;

  // We can now search the feret diameter
  double feretDiameter = 0;
  for ( typename IndexListType::const_iterator iIt1 = idxList.begin();
        iIt1 != idxList.end();
        iIt1++ )
    {
    typename IndexListType::const_iterator iIt2 = iIt1;
    for ( iIt2++; iIt2 != idxList.end(); iIt2++ )
      {
      // Compute the length between the 2 indexes
      double length = 0;
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        const OffsetValueType indexDifference = ( iIt1->operator[](i) - iIt2->operator[](i) );
        length += vcl_pow(indexDifference * spacing[i], 2);
        }
      if ( feretDiameter < length )
        {
        feretDiameter = length;
        }
      }
    }
  // Final computation
  feretDiameter = vcl_sqrt(feretDiameter);

  // Finally put the values in the label object
  labelObject->SetFeretDiameter(feretDiameter);
}

template< class TImage, class TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::AfterThreadedGenerateData()
{
  Superclass::AfterThreadedGenerateData();

  // Release the label image
  m_LabelImage = NULL;
  // and the perimeter calculator
  m_PerimeterCalculator = NULL;
}

template< class TImage, class TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ComputeFeretDiameter: " << m_ComputeFeretDiameter << std::endl;
  os << indent << "ComputePerimeter: " << m_ComputePerimeter << std::endl;
}

template< class TImage, class TLabelImage >
long
ShapeLabelMapFilter< TImage, TLabelImage >
::Factorial(const long n)
{
  if ( n < 1 )
    {
    return 1;
    }
  return n * Factorial(n - 1);
}

template< class TImage, class TLabelImage >
long
ShapeLabelMapFilter< TImage, TLabelImage >
::DoubleFactorial(const long n)
{
  if ( n < 2 )
    {
    return 1;
    }
  return n * DoubleFactorial(n - 2);
}

template< class TImage, class TLabelImage >
double
ShapeLabelMapFilter< TImage, TLabelImage >
::GammaN2p1(const long n)
{
  const bool even = n % 2 == 0;

  if ( even )
    {
    return Factorial(n / 2);
    }
  else
    {
    return vcl_sqrt(vnl_math::pi) * DoubleFactorial(n) / vcl_pow(2, ( n + 1 ) / 2.0);
    }
}

template< class TImage, class TLabelImage >
double
ShapeLabelMapFilter< TImage, TLabelImage >
::HyperSphereVolume(const double radius)
{
  const double dblImageDimension = static_cast< double >( ImageDimension );

  return vcl_pow(vnl_math::pi, dblImageDimension * 0.5) * vcl_pow(radius, dblImageDimension) / GammaN2p1(ImageDimension);
}

template< class TImage, class TLabelImage >
double
ShapeLabelMapFilter< TImage, TLabelImage >
::HyperSpherePerimeter(const double radius)
{
  return ImageDimension * HyperSphereVolume(radius) / radius;
}

template< class TImage, class TLabelImage >
double
ShapeLabelMapFilter< TImage, TLabelImage >
::HyperSphereRadiusFromVolume(const double volume)
{
  return vcl_pow(volume * GammaN2p1(ImageDimension) / vcl_pow(vnl_math::pi, ImageDimension * 0.5), 1.0 / ImageDimension);
}
} // end namespace itk
#endif
