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
#ifndef itkShapeLabelMapFilter_hxx
#define itkShapeLabelMapFilter_hxx

#include "itkShapeLabelMapFilter.h"
#include "itkProgressReporter.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkConstantBoundaryCondition.h"
#include "itkGeometryUtilities.h"
#include "itkConnectedComponentAlgorithm.h"
#include "vnl/algo/vnl_real_eigensystem.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "itkMath.h"
#include <deque>
#include <map>

namespace itk
{
template< typename TImage, typename TLabelImage >
ShapeLabelMapFilter< TImage, TLabelImage >
::ShapeLabelMapFilter()
{
  m_ComputeFeretDiameter = false;
  m_ComputePerimeter = true;
  m_ComputeOrientedBoundingBox = false;
}

template< typename TImage, typename TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  // Generate the label image, if needed
  if ( m_ComputeFeretDiameter )
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
}

template< typename TImage, typename TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::ThreadedProcessLabelObject(LabelObjectType *labelObject)
{
  ImageType *            output = this->GetOutput();

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
  SizeValueType                             nbOfPixels = 0;
  ContinuousIndex< double, ImageDimension > centroid;
  centroid.Fill(0);
  IndexType mins;
  mins.Fill( NumericTraits< IndexValueType >::max() );
  IndexType maxs;
  maxs.Fill( NumericTraits< IndexValueType >::NonpositiveMin() );
  SizeValueType nbOfPixelsOnBorder = 0;
  double        perimeterOnBorder = 0;
  MatrixType    centralMoments;
  centralMoments.Fill(0);

  typedef typename LabelObjectType::LengthType  LengthType;

  // Iterate over all the lines
  typename LabelObjectType::ConstLineIterator lit( labelObject );
  while( ! lit.IsAtEnd() )
    {
    const IndexType & idx = lit.GetLine().GetIndex();
    LengthType     length = lit.GetLine().GetLength();

    // Update the nbOfPixels
    nbOfPixels += length;

    // Update the centroid - and report the progress
    // First, update the axes that are not 0
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      centroid[i] += (OffsetValueType)length * idx[i];
      }
    // Then, update the axis 0
    centroid[0] += idx[0] * (OffsetValueType)length + ( length * ( length - 1 ) ) / 2.0;

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
    if ( idx[0] + (OffsetValueType)length > maxs[0] )
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
      nbOfPixelsOnBorder += length;
      }
    else
      {
      // We must check for the dimension 0
      bool isOnBorder0 = false;
      if ( idx[0] == borderMin[0] )
        {
        // One more pixel on the border
        nbOfPixelsOnBorder++;
        isOnBorder0 = true;
        }
      if ( !isOnBorder0 || length > 1 )
        {
        // We can check for the end of the line
        if ( idx[0] + (OffsetValueType)length - 1 == borderMax[0] )
          {
          // One more pixel on the border
          nbOfPixelsOnBorder++;
          }
        }
      }

    // Physical size on border
    // First, the dimension 0
    if ( idx[0] == borderMin[0] )
      {
      // Fhe beginning of the line
      perimeterOnBorder += sizePerPixelPerDimension[0];
      }
    if ( idx[0] + (OffsetValueType)length - 1 == borderMax[0] )
      {
      // And the end of the line
      perimeterOnBorder += sizePerPixelPerDimension[0];
      }
    // Then the other dimensions
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      if ( idx[i] == borderMin[i] )
        {
        // one border
        perimeterOnBorder += sizePerPixelPerDimension[i] * length;
        }
      if ( idx[i] == borderMax[i] )
        {
        // and the other
        perimeterOnBorder += sizePerPixelPerDimension[i] * length;
        }
      }

    // moments computation
    //
    //  This computation has changed from what is documented in the
    //  original publication ( see class level documentation for
    //  reference).  It has been re derived to properly support the
    //  direction cosine matrix in the image.
    //
    // Using the same optimization of the computation and substitutions,
    // the new computation is derived with the following:
    //
    //   p_i = o_i + s_0*d_i_0 * x, where s_0 is spacing the line run, and
    //   d_i_0, it the components of the first column of the direction cosine
    //        matrix, and x is the index offset from o_i.
    //
    // Then the elements of the central moments _all_ are:
    //   S_i_j = sum_L_in_O( sum_p_in_L( p_i dot p_j ) )
    //
    // Then we follow the paper, in substituting p, expanding and
    // substituting for known summations over x. This is very similar to
    // equation 9 in the paper but with p_i dot p_j and NOT p_i dot p_i.

    if (length <= 2)
      {

      // The following code is the basic implementation. The next
      // piece of code gives the same result in an efficient way, by
      // using expended formulae allowed by the binary case instead of
      // loops.
     IndexValueType endIdx0 = idx[0] + length;
     for( IndexType iidx = idx; iidx[0]<endIdx0; iidx[0]++)
       {
       typename LabelObjectType::CentroidType pP;
       output->TransformIndexToPhysicalPoint(iidx, pP);

       for(unsigned int i=0; i<ImageDimension; i++)
         {
         centralMoments[i][i] += pP[i] * pP[i];
         for(unsigned int j=i+1; j<ImageDimension; j++)
           {
           const double cm =  pP[i] * pP[j];
           centralMoments[i][j] += cm;
           centralMoments[j][i] += cm;
           }
         }
       }
      }
    else
      {
      // get the physical position and the spacing - they are used several times
      // later
      typename LabelObjectType::CentroidType physicalPosition;
      output->TransformIndexToPhysicalPoint(idx, physicalPosition);

      const typename ImageType::DirectionType &direction = output->GetDirection();
      VectorType scale(output->GetSpacing()[0]);
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        scale[i] *= direction(i,0);
        }

      const double lcoff_1 = ( length - 1.0 )/2.0;
      const double lcoff_2 = (2.0*length - 1.0)/3.0;

      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        centralMoments[i][i] += length * ( physicalPosition[i] * physicalPosition[i]
                                           + lcoff_1*(2.0 * physicalPosition[i] * scale[i]
                                                      + lcoff_2 * scale[i] *  scale[i]));

        for ( unsigned int j = i+1; j < ImageDimension; j++ )
          {
          const double cm =  length * ( physicalPosition[i] * physicalPosition[j]
                                        + lcoff_1*( physicalPosition[i] * scale[j] + scale[i] * physicalPosition[j]
                                                    + lcoff_2 * scale[i] *  scale[j]));
          centralMoments[j][i] += cm;
          centralMoments[i][j] += cm;

          }

        }

      }

    ++lit;
    }


  // final computation
  typename LabelObjectType::RegionType::SizeType boundingBoxSize;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    centroid[i] /= nbOfPixels;
    boundingBoxSize[i] = maxs[i] - mins[i] + 1;
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      centralMoments[i][j] /= nbOfPixels;
      }
    }
  typename LabelObjectType::RegionType boundingBox(mins, boundingBoxSize);
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
    principalMoments[i] = pm(i);
    }
  MatrixType principalAxes = eigen.V.transpose();

  // Add a final reflection if needed for a proper rotation,
  // by multiplying the last row by the determinant
  vnl_real_eigensystem                     eigenrot( principalAxes.GetVnlMatrix() );
  vnl_diag_matrix< std::complex< double > > eigenval = eigenrot.D;
  std::complex< double >                    det(1.0, 0.0);

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    det *= eigenval(i);
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
  else
    {
    if ( Math::NotAlmostEquals( principalMoments[0], itk::NumericTraits< typename VectorType::ValueType >::ZeroValue() ) )
      {
      flatness = std::sqrt(principalMoments[1] / principalMoments[0]);
      }
    if ( Math::NotAlmostEquals( principalMoments[ImageDimension - 2], itk::NumericTraits< typename VectorType::ValueType >::ZeroValue() ) )
      {
      elongation = std::sqrt(principalMoments[ImageDimension - 1] / principalMoments[ImageDimension - 2]);
      }
    }

  double physicalSize = nbOfPixels * sizePerPixel;
  double equivalentRadius = GeometryUtilities::HyperSphereRadiusFromVolume(ImageDimension, physicalSize);
  double equivalentPerimeter = GeometryUtilities::HyperSpherePerimeter(ImageDimension, equivalentRadius);

  // Compute equivalent ellipsoid radius
  VectorType ellipsoidDiameter;
  double     edet = 1.0;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    edet *= principalMoments[i];
    }
  edet = std::pow(edet, 1.0 / ImageDimension);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( edet != 0.0 )
      {
      ellipsoidDiameter[i] = 2.0 *equivalentRadius *std::sqrt(principalMoments[i] / edet);
      }
    else
      {
      ellipsoidDiameter[i] = 0.0;
      }
    }

  // Set the values in the object
  labelObject->SetNumberOfPixels(nbOfPixels);
  labelObject->SetPhysicalSize(physicalSize);
  labelObject->SetBoundingBox(boundingBox);
  labelObject->SetCentroid(physicalCentroid);
  labelObject->SetNumberOfPixelsOnBorder(nbOfPixelsOnBorder);
  labelObject->SetPerimeterOnBorder(perimeterOnBorder);
  labelObject->SetPrincipalMoments(principalMoments);
  labelObject->SetPrincipalAxes(principalAxes);
  labelObject->SetElongation(elongation);
  labelObject->SetEquivalentSphericalRadius(equivalentRadius);
  labelObject->SetEquivalentSphericalPerimeter(equivalentPerimeter);
  labelObject->SetEquivalentEllipsoidDiameter(ellipsoidDiameter);
  labelObject->SetFlatness(flatness);

  if ( m_ComputeFeretDiameter )
    {
    this->ComputeFeretDiameter(labelObject);
    }

  if ( m_ComputePerimeter )
    {
    this->ComputePerimeter(labelObject);
    }

   if ( m_ComputeOrientedBoundingBox )
    {
    this->ComputeOrientedBoundingBox(labelObject);
    }
}

template< typename TImage, typename TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::ComputeFeretDiameter(LabelObjectType *labelObject)
{
  const LabelPixelType & label = labelObject->GetLabel();

  typedef typename std::deque< IndexType > IndexListType;
  IndexListType idxList;

  typedef typename itk::ConstNeighborhoodIterator< LabelImageType > NeighborIteratorType;
  SizeType neighborHoodRadius;
  neighborHoodRadius.Fill(1);
  NeighborIteratorType                        it( neighborHoodRadius, m_LabelImage, m_LabelImage->GetBufferedRegion() );
  ConstantBoundaryCondition< LabelImageType > lcbc;
  // Use label + 1 to have a label different of the current label on the border
  lcbc.SetConstant(label + 1);
  it.OverrideBoundaryCondition(&lcbc);
  it.GoToBegin();

  typedef typename NeighborIteratorType::NeighborIndexType   NeighborIndexType;

  // Iterate over all the indexes
  typename LabelObjectType::ConstIndexIterator iit( labelObject );
  while( ! iit.IsAtEnd() )
    {
    // Move the iterator to the new location
    it += iit.GetIndex() - it.GetIndex();

    // Push the pixel in the list if it is on the border of the object
    for ( NeighborIndexType i = 0; i < it.Size(); i++ )
      {
      if ( it.GetPixel(i) != label )
        {
        idxList.push_back( iit.GetIndex() );
        break;
        }
      }
    ++iit;
    }

  ImageType *output = this->GetOutput();

  const typename ImageType::SpacingType & spacing = output->GetSpacing();

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
        length += std::pow(indexDifference * spacing[i], 2);
        }
      if ( feretDiameter < length )
        {
        feretDiameter = length;
        }
      }
    }
  // Final computation
  feretDiameter = std::sqrt(feretDiameter);

  // Finally put the values in the label object
  labelObject->SetFeretDiameter(feretDiameter);
}

template< typename TImage, typename TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::ComputePerimeter(LabelObjectType *labelObject)
{
  // store the lines in a N-1D image of vectors
  typedef std::deque< typename LabelObjectType::LineType > VectorLineType;
  typedef itk::Image< VectorLineType, ImageDimension - 1 > LineImageType;
  typename LineImageType::Pointer lineImage = LineImageType::New();
  typename LineImageType::IndexType lIdx;
  typename LineImageType::SizeType lSize;
  RegionType boundingBox = labelObject->GetBoundingBox();
  for( unsigned int i=0; i<ImageDimension-1; i++ )
    {
    lIdx[i] = boundingBox.GetIndex()[i+1];
    lSize[i] = boundingBox.GetSize()[i+1];
    }
  typename LineImageType::RegionType lRegion;
  lRegion.SetIndex( lIdx );
  lRegion.SetSize( lSize );
  // enlarge the region a bit to avoid boundary problems
  typename LineImageType::RegionType elRegion(lRegion);
  lSize.Fill(1);
  elRegion.PadByRadius(lSize);
  // std::cout << boundingBox << "  " << lRegion << "  " << elRegion << std::endl;
  // now initialize the image
  lineImage->SetRegions( elRegion );
  lineImage->Allocate();
  lineImage->FillBuffer( VectorLineType() );

  // std::cout << "lineContainer.size(): " << lineContainer.size() << std::endl;

  // Iterate over all the lines and fill the image of lines
  typename LabelObjectType::ConstLineIterator lit( labelObject );
  while( ! lit.IsAtEnd() )
    {
    const IndexType & idx = lit.GetLine().GetIndex();
    for( unsigned int i=0; i<ImageDimension-1; i++ )
      {
      lIdx[i] = idx[i+1];
      }
    lineImage->GetPixel( lIdx ).push_back( lit.GetLine() );
    ++lit;
    }

  // a data structure to store the number of intercepts on each direction
  typedef typename std::map<OffsetType, SizeValueType, typename OffsetType::LexicographicCompare> MapInterceptType;
  MapInterceptType intercepts;
  // int nbOfDirections = (int)std::pow( 2.0, (int)ImageDimension ) - 1;
  // intecepts.resize(nbOfDirections + 1);  // code begins at position 1

  // now iterate over the vectors of lines
  typedef ConstShapedNeighborhoodIterator< LineImageType > LineImageIteratorType;
  LineImageIteratorType lIt( lSize, lineImage, lRegion ); // the original, non padded region
  setConnectivity( &lIt, true );
  for( lIt.GoToBegin(); !lIt.IsAtEnd(); ++lIt )
    {
    const VectorLineType & ls = lIt.GetCenterPixel();

    // there are two intercepts on the 0 axis for each line
    OffsetType no;
    no.Fill(0);
    no[0] = 1;
    // std::cout << no << "-> " << 2 * ls.size() << std::endl;
    intercepts[no] += 2 * static_cast<SizeValueType>( ls.size() );

    // and look at the neighbors
    typename LineImageIteratorType::ConstIterator ci;
    for (ci = lIt.Begin(); ci != lIt.End(); ci++)
      {
          // std::cout << "-------------" << std::endl;
      // the vector of lines in the neighbor
      const VectorLineType & ns = ci.Get();
      // prepare the offset to be stored in the intercepts map
      typename LineImageType::OffsetType lno = ci.GetNeighborhoodOffset();
      no[0] = 0;
      for( unsigned int i=0; i<ImageDimension-1; i++ )
        {
        no[i+1] = itk::Math::abs(lno[i]);
        }
      OffsetType dno = no; // offset for the diagonal
      dno[0] = 1;

      // now process the two lines to search the pixels on the contour of the object
      if( ls.empty() )
        {
        // std::cout << "ls.empty()" << std::endl;
        // nothing to do
        }
      if( ns.empty() )
        {
        // no line in the neighbors - all the lines in ls are on the contour
        for( typename VectorLineType::const_iterator li = ls.begin(); li != ls.end(); ++li )
          {
          // std::cout << "ns.empty()" << std::endl;
          const typename LabelObjectType::LineType & l = *li;
          // add as much intercepts as the line size
          intercepts[no] += l.GetLength();
          // and 2 times as much diagonal intercepts as the line size
          intercepts[dno] += l.GetLength() * 2;
          }
        }
      else
        {
        // std::cout << "else" << std::endl;
        // TODO - fix the code when the line starts at  NumericTraits<IndexValueType>::NonpositiveMin()
        // or end at  NumericTraits<IndexValueType>::max()
        typename VectorLineType::const_iterator li = ls.begin();
        typename VectorLineType::const_iterator ni = ns.begin();

        IndexValueType lZero = 0;
        IndexValueType lMin = 0;
        IndexValueType lMax = 0;

        IndexValueType nMin = NumericTraits<IndexValueType>::NonpositiveMin() + 1;
        IndexValueType nMax = ni->GetIndex()[0] - 1;

        while( li!=ls.end() )
          {
          // update the current line min and max. Neighbor line data is already up to date.
          lMin = li->GetIndex()[0];
          lMax = lMin + li->GetLength() - 1;

          // add as much intercepts as intersections of the 2 lines
          intercepts[no] += std::max( lZero, std::min(lMax, nMax) - std::max(lMin, nMin) + 1 );
          // std::cout << "============" << std::endl;
          // std::cout << "  lMin:" << lMin << " lMax:" << lMax << " nMin:" << nMin << " nMax:" << nMax;
          // std::cout << " count: " << std::max( 0l, std::min(lMax, nMax) - std::max(lMin, nMin) + 1 ) << std::endl;
          // std::cout << "  " << no << ": " << intercepts[no] << std::endl;
          // std::cout << std::max( lZero, std::min(lMax, nMax+1) - std::max(lMin, nMin+1) + 1 ) << std::endl;
          // std::cout << std::max( lZero, std::min(lMax, nMax-1) - std::max(lMin, nMin-1) + 1 ) << std::endl;
          // left diagonal intercepts
          intercepts[dno] += std::max( lZero, std::min(lMax, nMax+1) - std::max(lMin, nMin+1) + 1 );
          // right diagonal intercepts
          intercepts[dno] += std::max( lZero, std::min(lMax, nMax-1) - std::max(lMin, nMin-1) + 1 );

          // go to the next line or the next neighbor depending on where we are
          if(nMax <= lMax )
            {
            // go to next neighbor
            nMin = ni->GetIndex()[0] + ni->GetLength();
            ni++;

            if( ni != ns.end() )
              {
              nMax = ni->GetIndex()[0] - 1;
              }
            else
              {
              nMax = NumericTraits<IndexValueType>::max() - 1;
              }
            }
          else
            {
            // go to next line
            li++;
            }
          }

        }
      }
    }

  // compute the perimeter based on the intercept counts
  double perimeter = PerimeterFromInterceptCount( intercepts, this->GetOutput()->GetSpacing() );
  labelObject->SetPerimeter( perimeter );
  labelObject->SetRoundness( labelObject->GetEquivalentSphericalPerimeter() / perimeter );
  labelObject->SetPerimeterOnBorderRatio( labelObject->GetPerimeterOnBorder() / perimeter );
}

template< typename TImage, typename TLabelImage >
template<typename TMapIntercept, typename TSpacing>
double
ShapeLabelMapFilter< TImage, TLabelImage >
::PerimeterFromInterceptCount( TMapIntercept & intercepts, const TSpacing & spacing )
{
  // std::cout << "PerimeterFromInterceptCount<>" << std::endl;
  double perimeter = 0.0;
  double pixelSize = 1.0;
  int dim = TSpacing::GetVectorDimension();
  for ( int i = 0; i < dim; i++ )
    {
    pixelSize *= spacing[i];
    }

  for( int i=0; i<dim; i++ )
    {
    OffsetType no;
    no.Fill(0);
    no[i] = 1;
    // std::cout << no << ": " << intercepts[no] << std::endl;
    perimeter += pixelSize / spacing[i] * intercepts[no]/2.0;
    }

  // Crofton's constant
  perimeter *= GeometryUtilities::HyperSphereVolume( dim, 1.0 )
                 / GeometryUtilities::HyperSphereVolume( dim - 1, 1.0 );
  return perimeter;
}

#if ! defined(ITK_DO_NOT_USE_PERIMETER_SPECIALIZATION)
template< typename TImage, typename TLabelImage >
double
ShapeLabelMapFilter< TImage, TLabelImage >
::PerimeterFromInterceptCount( MapIntercept2Type & intercepts, const Spacing2Type spacing )
{
  // std::cout << "PerimeterFromInterceptCount2" << std::endl;
  double dx = spacing[0];
  double dy = spacing[1];

  Offset2Type nx =  {{1, 0}};
  Offset2Type ny =  {{0, 1}};
  Offset2Type nxy = {{1, 1}};

  // std::cout << "nx: " << intercepts[nx] << std::endl;
  // std::cout << "ny: " << intercepts[ny] << std::endl;
  // std::cout << "nxy: " << intercepts[nxy] << std::endl;

  double perimeter = 0.0;
  perimeter += dy * intercepts[nx]/2.0;
  perimeter += dx * intercepts[ny]/2.0;
  perimeter += dx*dy / spacing.GetNorm() * intercepts[nxy]/2.0;
  perimeter *= itk::Math::pi / 4.0;
  return perimeter;
}

template< typename TImage, typename TLabelImage >
double
ShapeLabelMapFilter< TImage, TLabelImage >
::PerimeterFromInterceptCount( MapIntercept3Type & intercepts, const Spacing3Type spacing )
{
  // std::cout << "PerimeterFromInterceptCount3" << std::endl;
  double dx = spacing[0];
  double dy = spacing[1];
  double dz = spacing[2];
  double dxy = std::sqrt( spacing[0]*spacing[0] + spacing[1]*spacing[1] );
  double dxz = std::sqrt( spacing[0]*spacing[0] + spacing[2]*spacing[2] );
  double dyz = std::sqrt( spacing[1]*spacing[1] + spacing[2]*spacing[2] );
  double dxyz = std::sqrt( spacing[0]*spacing[0] + spacing[1]*spacing[1] + spacing[2]*spacing[2] );
  double vol = spacing[0]*spacing[1]*spacing[2];

  // 'magical numbers', corresponding to area of voronoi partition on the
  // unit sphere, when germs are the 26 directions on the unit cube
  // Sum of (c1+c2+c3 + c4*2+c5*2+c6*2 + c7*4) equals 1.
  double c1 = 0.04577789120476 * 2;  // Ox
  double c2 = 0.04577789120476 * 2;  // Oy
  double c3 = 0.04577789120476 * 2;  // Oz
  double c4 = 0.03698062787608 * 2;  // Oxy
  double c5 = 0.03698062787608 * 2;  // Oxz
  double c6 = 0.03698062787608 * 2;  // Oyz
  double c7 = 0.03519563978232 * 2;  // Oxyz
  // TODO - recompute those values if the spacing is non isotrope

  Offset3Type nx =   {{1, 0, 0}};
  Offset3Type ny =   {{0, 1, 0}};
  Offset3Type nz =   {{0, 0, 1}};
  Offset3Type nxy =  {{1, 1, 0}};
  Offset3Type nxz =  {{1, 0, 1}};
  Offset3Type nyz =  {{0, 1, 1}};
  Offset3Type nxyz = {{1, 1, 1}};

  // std::cout << "nx: " << intercepts[nx] << std::endl;
  // std::cout << "ny: " << intercepts[ny] << std::endl;
  // std::cout << "nz: " << intercepts[nz] << std::endl;
  // std::cout << "nxy: " << intercepts[nxy] << std::endl;
  // std::cout << "nxz: " << intercepts[nxz] << std::endl;
  // std::cout << "nyz: " << intercepts[nyz] << std::endl;
  // std::cout << "nxyz: " << intercepts[nxyz] << std::endl;

  double perimeter = 0.0;
  perimeter += vol/dx * intercepts[nx]/2.0 * c1;
  perimeter += vol/dy * intercepts[ny]/2.0 * c2;
  perimeter += vol/dz * intercepts[nz]/2.0 * c3;
  perimeter += vol/dxy * intercepts[nxy]/2.0 * c4;
  perimeter += vol/dxz * intercepts[nxz]/2.0 * c5;
  perimeter += vol/dyz * intercepts[nyz]/2.0 * c6;
  perimeter += vol/dxyz * intercepts[nxyz]/2.0 * c7;
  perimeter *= 4;
  return perimeter;
}
#endif


template< typename TImage, typename TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::ComputeOrientedBoundingBox(LabelObjectType *labelObject)
{

  typedef vnl_matrix<double> VNLMatrixType;
  typedef vnl_vector<double> VNLVectorType;

  const ImageType *            output = this->GetOutput();

  VNLMatrixType principalAxesBasisMatrix = labelObject->GetPrincipalAxes().GetVnlMatrix();

  const typename LabelObjectType::CentroidType centroid = labelObject->GetCentroid();
  const unsigned int numLines = labelObject->GetNumberOfLines();

  // Create a matrix where the columns are the physical points of the
  // start and end of each RLE line from the label map, relative to
  // the centroid
  VNLMatrixType pixelLocations( ImageDimension, labelObject->GetNumberOfLines()*2 );
  for( unsigned int l = 0; l < numLines; ++l )
    {
    typename LabelObjectType::LineType line = labelObject->GetLine(l);

    // add start index of line as physical point relative to centroid
    IndexType idx = line.GetIndex();
    typename ImageType::PointType pt;
    output->TransformIndexToPhysicalPoint(idx, pt);
    for(unsigned int j = 0; j < ImageDimension; ++j)
      {
      pixelLocations(j,l*2) = pt[j] - centroid[j];
      }

    // add end index of line as physical point relative to centroid
    idx[0] += line.GetLength() - 1;
    output->TransformIndexToPhysicalPoint(idx, pt);
    for(unsigned int j = 0; j < ImageDimension; ++j)
      {
      pixelLocations(j,l*2+1) = pt[j] - centroid[j];
      }
    }


  // Project the physical points onto principal axes
  VNLMatrixType transformedPixelLocations = principalAxesBasisMatrix * pixelLocations;

  // find the bounds in the projected domain
  assert( transformedPixelLocations.columns() != 0 );
  VNLVectorType minimumPrincipalAxis = transformedPixelLocations.get_column(0);
  VNLVectorType maximumPrincipalAxis = transformedPixelLocations.get_column(0);

  for ( unsigned int column = 1; column < transformedPixelLocations.columns(); column++ )
    {
    for ( unsigned int i = 0; i < ImageDimension; ++i )
      {
      const double value = transformedPixelLocations(i, column);
      minimumPrincipalAxis[i] = std::min(minimumPrincipalAxis[i], value);
      maximumPrincipalAxis[i] = std::max(maximumPrincipalAxis[i], value);
      }
    }

  // The minimumPrincipalAxis/maximumPrincipalAxis is from center of pixel to center of pixel
  // in the principal axis basis. The full extent of the pixels needs
  // to include the offset bits from the center of the pixel to the
  // corners. The extrema of the OBB is increased by checking all
  // corners of the pixels, via computing the offset vector from the
  // center to the corner in the principal axis basis.

  VNLVectorType adjusted_minimumPrincipalAxis = minimumPrincipalAxis;
  VNLVectorType adjusted_maximumPrincipalAxis = maximumPrincipalAxis;

  const typename ImageType::SpacingType & spacing = output->GetSpacing();

  // iterate over all corners (2^D) of the pixel
  for ( unsigned int p = 0; p < 1u<<ImageDimension; ++p )
    {
    Vector<double, ImageDimension> spacingAxis(0.5*spacing);

    // permute signs of spacing vector components, based on a bit of p
    // to component of spacingAxis mapping
    for (unsigned int i = 0; i < ImageDimension; ++i)
      {
      if (p & 1u<<i)
        {
        spacingAxis[i] *= -1;
        }
      }

    Vector<double, ImageDimension> physicalOffset;
    output->TransformLocalVectorToPhysicalVector(spacingAxis, physicalOffset);
    VNLVectorType  paOffset = principalAxesBasisMatrix*physicalOffset.GetVnlVector();

    for ( unsigned int i = 0; i < ImageDimension; ++i )
      {
      adjusted_minimumPrincipalAxis[i] = std::min(adjusted_minimumPrincipalAxis[i], minimumPrincipalAxis[i]+paOffset[i]);
      adjusted_maximumPrincipalAxis[i] = std::max(adjusted_maximumPrincipalAxis[i], maximumPrincipalAxis[i]+paOffset[i]);
      }
    }

  minimumPrincipalAxis = adjusted_minimumPrincipalAxis;
  maximumPrincipalAxis = adjusted_maximumPrincipalAxis;

  // real physical size, in basis space
  Vector<double, ImageDimension> rsize;
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    rsize[i] = std::abs(maximumPrincipalAxis[i]-minimumPrincipalAxis[i]);
    }


  //
  // Invert rotation matrix, we will now convert points from the
  // projected space back to the physical one, for the origin
  //
  principalAxesBasisMatrix.inplace_transpose();

  typename LabelObjectType::OrientedBoundingBoxPointType origin;
  VNLVectorType min = principalAxesBasisMatrix*minimumPrincipalAxis;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
     origin[i] = min[i] + centroid[i];
    }

  labelObject->SetOrientedBoundingBoxSize(rsize);
  labelObject->SetOrientedBoundingBoxOrigin(origin);

}

template< typename TImage, typename TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::AfterThreadedGenerateData()
{
  Superclass::AfterThreadedGenerateData();

  // Release the label image
  m_LabelImage = ITK_NULLPTR;
}

template< typename TImage, typename TLabelImage >
void
ShapeLabelMapFilter< TImage, TLabelImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ComputeFeretDiameter: " << m_ComputeFeretDiameter << std::endl;
  os << indent << "ComputePerimeter: " << m_ComputePerimeter << std::endl;
  os << indent << "ComputeOrientedBoundingBox: " << m_ComputeOrientedBoundingBox << std::endl;
}

} // end namespace itk
#endif
