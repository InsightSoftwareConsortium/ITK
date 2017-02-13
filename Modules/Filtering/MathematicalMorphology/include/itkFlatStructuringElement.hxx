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
#ifndef itkFlatStructuringElement_hxx
#define itkFlatStructuringElement_hxx
#include "itkMath.h"
#include "itkFlatStructuringElement.h"
#include <cmath>
#include <vector>

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h"

namespace itk
{
template< typename TImage, typename TKernel >
class VanHerkGilWermanDilateImageFilter;
}

#include "itkVanHerkGilWermanDilateImageFilter.h"

namespace itk
{

template< unsigned int NDimension >
FlatStructuringElement< NDimension >
FlatStructuringElement< NDimension >
::Polygon(RadiusType radius, unsigned lines)
{
  Self res = Self();
  GeneratePolygon(res, radius, lines);
  return res;
}

template< unsigned int VDimension >
template< typename TStructuringElement, typename TRadius >
void
FlatStructuringElement< VDimension >
::GeneratePolygon(TStructuringElement & , TRadius , unsigned )
{
  itkGenericExceptionMacro("Only dimension 2 and 3 are suported.");
}

template< unsigned int VDimension >
void
FlatStructuringElement< VDimension >
::GeneratePolygon(itk::FlatStructuringElement< 2 > & res, itk::Size<2> radius, unsigned lines)
{
  // radial decomposition method from "Radial Decomposition of Discs
  // and Spheres" - CVGIP: Graphical Models and Image Processing
  //std::cout << "2 dimensions" << std::endl;
  res.SetRadius(radius);
  res.SetDecomposable(true);

  unsigned int rr = 0;
  for ( unsigned i = 0; i < 2; i++ )
    {
    if ( radius[i] > rr ) { rr = radius[i]; }
    }
  if ( lines == 0 )
    {
    // select some default line values
    if ( rr <= 3 ) { lines = 2; }
    else if ( rr <= 8 )
      {
      lines = 4;
      }
    else { lines = 6; }
    }
  // start with a circle - figure out the length of the structuring
  // element we need -- This method results in a polygon with 2*lines
  // sides, each side with length k, where k is the structuring
  // element length. Therefore the value of k we need to produce the
  // radius we want is: (M_PI * rr * 2)/(2*lines)
  const double k1(( Math::pi * (double)radius[0] ) / ( (double)lines ));
  const double k2(( Math::pi * (double)radius[1] ) / ( (double)lines ));
  const double step(Math::pi / lines);
  double theta(0.0);

  // just to ensure that we get the last one
  while ( theta <= Math::pi / 2.0 + 0.0001 )
    {
    LType2 O;
    O[0] = (float)(k1 * std::cos(theta));
    O[1] = (float)(k2 * std::sin(theta));
    if ( !res.CheckParallel(O) )
      {
      res.AddLine(O);
      }
    O[0] = (float)(k1 * std::cos(-theta));
    O[1] = (float)(k2 * std::sin(-theta));
    if ( !res.CheckParallel(O) )
      {
      res.AddLine(O);
      }
    theta += step;
    }

  res.ComputeBufferFromLines();
}

//    O[0] = k1 * std::cos(phi) * std::cos(theta);
//    O[1] = k2 * std::cos(phi) * std::sin(theta);
//    O[2] = k3 * std::sin(theta);

template< unsigned int VDimension >
void
FlatStructuringElement< VDimension >
::GeneratePolygon(itk::FlatStructuringElement< 3 > & res, itk::Size<3> radius, unsigned lines)
{
  res.SetRadius(radius);
  res.SetDecomposable(true);

  // std::cout << "3 dimensions" << std::endl;
  unsigned int rr = 0;
  int          iterations = 1;
  int          faces = lines * 2;
  for ( unsigned i = 0; i < 3; i++ )
    {
    if ( radius[i] > rr ) { rr = radius[i]; }
    }
  switch ( faces )
    {
    case 12:
      {
      // dodecahedron
      float    phi = ( 1.0 + std::sqrt(5.0) ) / 2.0;
      float    b = 1.0 / phi;
      float    c = 2.0 - phi;
      unsigned facets = 12;
      typedef std::vector< FacetType3 > FacetArrayType;
      FacetArrayType FacetArray;
      FacetArray.resize(facets);
      // set up vectors normal to the faces - only put in 3 points for
      // each face:
      // face 1
      LType3     PP(0.0);
      FacetType3 Fc;
      b /= 2.0;
      c /= 2.0;

      PP[0] = c; PP[1] = 0; PP[2] = 0.5;
      Fc.P1 = PP;
      PP[0] = -c; PP[1] = 0; PP[2] = 0.5;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = b; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[0] = Fc;

      PP[0] = -c; PP[1] = 0; PP[2] = 0.5;
      Fc.P1 = PP;
      PP[0] = c; PP[1] = 0; PP[2] = 0.5;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = -b; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[1] = Fc;

      PP[0] = c; PP[1] = 0; PP[2] = -0.5;
      Fc.P1 = PP;
      PP[0] = -c; PP[1] = 0; PP[2] = -0.5;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = -b; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[2] = Fc;

      PP[0] = -c; PP[1] = 0; PP[2] = -0.5;
      Fc.P1 = PP;
      PP[0] = c; PP[1] = 0; PP[2] = -0.5;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = b; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[3] = Fc;

      PP[0] = 0; PP[1] = 0.5; PP[2] = -c;
      Fc.P1 = PP;
      PP[0] = 0; PP[1] = 0.5; PP[2] = c;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = b; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[4] = Fc;

      PP[0] = 0; PP[1] = 0.5; PP[2] = c;
      Fc.P1 = PP;
      PP[0] = 0; PP[1] = 0.5; PP[2] = -c;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = b; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[5] = Fc;

      PP[0] = 0; PP[1] = -0.5; PP[2] = -c;
      Fc.P1 = PP;
      PP[0] = 0; PP[1] = -0.5; PP[2] = c;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = -b; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[6] = Fc;

      PP[0] = 0; PP[1] = -0.5; PP[2] = c;
      Fc.P1 = PP;
      PP[0] = 0; PP[1] = -0.5; PP[2] = -c;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = -b; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[7] = Fc;

      PP[0] = 0.5; PP[1] = c; PP[2] = 0;
      Fc.P1 = PP;
      PP[0] = 0.5; PP[1] = -c; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = -b; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[8] = Fc;

      PP[0] = 0.5; PP[1] = -c; PP[2] = 0;
      Fc.P1 = PP;
      PP[0] = 0.5; PP[1] = c; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = b; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[9] = Fc;

      PP[0] = -0.5; PP[1] = c; PP[2] = 0;
      Fc.P1 = PP;
      PP[0] = -0.5; PP[1] = -c; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = -b; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[10] = Fc;

      PP[0] = -0.5; PP[1] = -c; PP[2] = 0;
      Fc.P1 = PP;
      PP[0] = -0.5; PP[1] = c; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = b; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[11] = Fc;
      for ( unsigned j = 0; j < facets; j++ )
        {
        // Find a line perpendicular to each face
        LType3 L, A, B;
        A = FacetArray[j].P2 - FacetArray[j].P1;
        B = FacetArray[j].P3 - FacetArray[j].P1;
        L[0] = A[1] * B[2] - B[1] * A[2];
        L[1] = B[0] * A[2] - A[0] * B[2];
        L[2] = A[0] * B[1] - B[0] * A[1];

        L.Normalize();
        // Scale to required length
        L *= rr;
        if ( !res.CheckParallel(L) )
          {
          res.AddLine(L);
          }
        }
      }
      break;
    case 14:
      {
      // cube with the corners cut off
      LType3 A;
      // The axes
      A[0] = 1; A[1] = 0; A[2] = 0;
      A *= rr;
      res.AddLine(A);
      A[0] = 0; A[1] = 1; A[2] = 0;
      A *= rr;
      res.AddLine(A);
      A[0] = 0; A[1] = 0; A[2] = 1;
      A *= rr;
      res.AddLine(A);
      // Diagonals
      A[0] = 1; A[1] = 1; A[2] = 1;
      A.Normalize();
      A *= rr;
      res.AddLine(A);

      A[0] = -1; A[1] = 1; A[2] = 1;
      A.Normalize();
      A *= rr;
      res.AddLine(A);

      A[0] = 1; A[1] = -1; A[2] = 1;
      A.Normalize();
      A *= rr;
      res.AddLine(A);

      A[0] = -1; A[1] = -1; A[2] = 1;
      A.Normalize();
      A *= rr;
      res.AddLine(A);
      }
      break;
    case 20:
      {
      // Icosahedron
      float    phi = ( 1.0 + std::sqrt(5.0) ) / 2.0;
      float    a = 0.5;
      float    b = 1.0 / ( 2.0 * phi );
      unsigned facets = 20;
      typedef std::vector< FacetType3 > FacetArrayType;
      FacetArrayType FacetArray;
      FacetArray.resize(facets);
      // set up vectors normal to the faces - only put in 3 points for
      // each face:
      // face 1
      LType3     PP(0.0);
      FacetType3 Fc;

      PP[0] = 0; PP[1] = b; PP[2] = -a;
      Fc.P1 = PP;
      PP[0] = b; PP[1] = a; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = a; PP[2] = 0;
      Fc.P3 = PP;
      FacetArray[0] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = a;
      Fc.P1 = PP;
      PP[0] = -b; PP[1] = a; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = a; PP[2] = 0;
      Fc.P3 = PP;
      FacetArray[1] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = a;
      Fc.P1 = PP;
      PP[0] = 0; PP[1] = -b; PP[2] = a;
      Fc.P2 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[2] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = a;
      Fc.P1 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = b;
      Fc.P2 = PP;
      PP[0] = 0; PP[1] = -b; PP[2] = a;
      Fc.P3 = PP;
      FacetArray[3] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = -a;
      Fc.P1 = PP;
      PP[0] = 0; PP[1] = -b; PP[2] = -a;
      Fc.P2 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[4] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = -a;
      Fc.P1 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = -b;
      Fc.P2 = PP;
      PP[0] = 0; PP[1] = -b; PP[2] = -a;
      Fc.P3 = PP;
      FacetArray[5] = Fc;

      PP[0] = 0; PP[1] = -b; PP[2] = a;
      Fc.P1 = PP;
      PP[0] = b; PP[1] = -a; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = -a; PP[2] = 0;
      Fc.P3 = PP;
      FacetArray[6] = Fc;

      PP[0] = 0; PP[1] = -b; PP[2] = -a;
      Fc.P1 = PP;
      PP[0] = -b; PP[1] = -a; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = -a; PP[2] = 0;
      Fc.P3 = PP;
      FacetArray[7] = Fc;

      PP[0] = -b; PP[1] = a; PP[2] = 0;
      Fc.P1 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = b;
      Fc.P2 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[8] = Fc;

      PP[0] = -b; PP[1] = -a; PP[2] = 0;
      Fc.P1 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = -b;
      Fc.P2 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[9] = Fc;

      PP[0] = b; PP[1] = a; PP[2] = 0;
      Fc.P1 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = -b;
      Fc.P2 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[10] = Fc;

      PP[0] = b; PP[1] = -a; PP[2] = 0;
      Fc.P1 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = b;
      Fc.P2 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[11] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = a;
      Fc.P1 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = b;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = a; PP[2] = 0;
      Fc.P3 = PP;
      FacetArray[12] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = a;
      Fc.P1 = PP;
      PP[0] = b; PP[1] = a; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[13] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = -a;
      Fc.P1 = PP;
      PP[0] = -b; PP[1] = a; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[14] = Fc;

      PP[0] = 0; PP[1] = b; PP[2] = -a;
      Fc.P1 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = -b;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = a; PP[2] = 0;
      Fc.P3 = PP;
      FacetArray[15] = Fc;

      PP[0] = 0; PP[1] = -b; PP[2] = -a;
      Fc.P1 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = -b;
      Fc.P2 = PP;
      PP[0] = -b; PP[1] = -a; PP[2] = 0;
      Fc.P3 = PP;
      FacetArray[16] = Fc;

      PP[0] = 0; PP[1] = -b; PP[2] = -a;
      Fc.P1 = PP;
      PP[0] = b; PP[1] = -a; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = -b;
      Fc.P3 = PP;
      FacetArray[17] = Fc;

      PP[0] = 0; PP[1] = -b; PP[2] = a;
      Fc.P1 = PP;
      PP[0] = -b; PP[1] = -a; PP[2] = 0;
      Fc.P2 = PP;
      PP[0] = -a; PP[1] = 0; PP[2] = b;
      Fc.P3 = PP;
      FacetArray[18] = Fc;

      PP[0] = 0; PP[1] = -b; PP[2] = a;
      Fc.P1 = PP;
      PP[0] = a; PP[1] = 0; PP[2] = b;
      Fc.P2 = PP;
      PP[0] = b; PP[1] = -a; PP[2] = 0;
      Fc.P3 = PP;
      FacetArray[19] = Fc;

      for ( unsigned j = 0; j < facets; j++ )
        {
        // Find a line perpendicular to each face
        LType3 L, A, B;
        A = FacetArray[j].P2 - FacetArray[j].P1;
        B = FacetArray[j].P3 - FacetArray[j].P1;
        L[0] = A[1] * B[2] - B[1] * A[2];
        L[1] = B[0] * A[2] - A[0] * B[2];
        L[2] = A[0] * B[1] - B[0] * A[1];

        L.Normalize();
        // Scale to required length
        L *= rr;
        if ( !res.CheckParallel(L) )
          {
          res.AddLine(L);
          }
        }
      }
      break;
    case 32:
      {
      iterations = 1;
      // 2 iterations leads to 128 faces, which is too many
      // subdivision of octahedron
      // create triangular facet approximation to a sphere - begin with
      // unit sphere
      // total number of facets is 8 * (4^iterations)
      unsigned int facets = 8 * (int)std::pow( (double)4, iterations );
      double        sqrt2 = std::sqrt(2.0);

      typedef std::vector< FacetType3 > FacetArrayType;
      FacetArrayType FacetArray;
      FacetArray.resize(facets);

      // original corners of octahedron
      LType3 P0(0.0), P1(0.0), P2(0.0), P3(0.0), P4(0.0), P5(0.0);
      P0[0] = 0;         P0[1] = 0;       P0[2] = 1;
      P1[0] = 0;         P1[1] = 0;       P1[2] = -1;
      P2[0] = -1.0 / sqrt2; P2[1] = -1 / sqrt2; P2[2] = 0;
      P3[0] = 1 / sqrt2;   P3[1] = -1 / sqrt2; P3[2] = 0;
      P4[0] = 1 / sqrt2;   P4[1] = 1 / sqrt2; P4[2] = 0;
      P5[0] = -1 / sqrt2;  P5[1] = 1 / sqrt2; P5[2] = 0;

      FacetType3 F0, F1, F2, F3, F4, F5, F6, F7;
      F0.P1 = P0; F0.P2 = P3; F0.P3 = P4;
      F1.P1 = P0; F1.P2 = P4; F1.P3 = P5;
      F2.P1 = P0; F2.P2 = P5; F2.P3 = P2;
      F3.P1 = P0; F3.P2 = P2; F3.P3 = P3;
      F4.P1 = P1; F4.P2 = P4; F4.P3 = P3;
      F5.P1 = P1; F5.P2 = P5; F5.P3 = P4;
      F6.P1 = P1; F6.P2 = P2; F6.P3 = P5;
      F7.P1 = P1; F7.P2 = P3; F7.P3 = P2;

      FacetArray[0] = F0;
      FacetArray[1] = F1;
      FacetArray[2] = F2;
      FacetArray[3] = F3;
      FacetArray[4] = F4;
      FacetArray[5] = F5;
      FacetArray[6] = F6;
      FacetArray[7] = F7;
      int pos = 8;
      // now subdivide the octahedron
      for ( unsigned it = 0; it < (unsigned)iterations; it++ )
        {
        // Bisect edges and move to sphere
        unsigned ntold = pos;
        for ( unsigned i = 0; i < ntold; i++ )
          {
          LType3 Pa, Pb, Pc;
          for ( unsigned d = 0; d <  3; d++ )
            {
            Pa[d] = ( FacetArray[i].P1[d] + FacetArray[i].P2[d] ) / 2;
            Pb[d] = ( FacetArray[i].P2[d] + FacetArray[i].P3[d] ) / 2;
            Pc[d] = ( FacetArray[i].P3[d] + FacetArray[i].P1[d] ) / 2;
            }
          Pa.Normalize();
          Pb.Normalize();
          Pc.Normalize();
          FacetArray[pos].P1 = FacetArray[i].P1;
          FacetArray[pos].P2 = Pa;
          FacetArray[pos].P3 = Pc;
          pos++;
          FacetArray[pos].P1 = Pa;
          FacetArray[pos].P2 = FacetArray[i].P2;
          FacetArray[pos].P3 = Pb;
          pos++;
          FacetArray[pos].P1 = Pb;
          FacetArray[pos].P2 = FacetArray[i].P3;
          FacetArray[pos].P3 = Pc;
          pos++;
          FacetArray[i].P1 = Pa;
          FacetArray[i].P2 = Pb;
          FacetArray[i].P3 = Pc;
          }
        }

      for ( unsigned j = 0; j < facets; j++ )
        {
        // Find a line perpendicular to each face
        LType3 L, A, B;
        A = FacetArray[j].P2 - FacetArray[j].P1;
        B = FacetArray[j].P3 - FacetArray[j].P1;
        L[0] = A[1] * B[2] - B[1] * A[2];
        L[1] = B[0] * A[2] - A[0] * B[2];
        L[2] = A[0] * B[1] - B[0] * A[1];

        L.Normalize();
        // Scale to required length
        L *= rr;
        if ( !res.CheckParallel(L) )
          {
          res.AddLine(L);
          }
        }
      }
      break;
    default:
      itkGenericExceptionMacro("Unsupported number of lines: " << lines << ". Supported values are 6, 7, 10 and 16.");
    }
  res.ComputeBufferFromLines();
}

template< unsigned int VDimension >
FlatStructuringElement< VDimension > FlatStructuringElement< VDimension >
::Box(RadiusType radius)
{
  // this should work for any number of dimensions
  Self res = Self();

  res.SetDecomposable(true);
  res.SetRadius(radius);
  for ( unsigned i = 0; i < VDimension; i++ )
    {
    if ( radius[i] != 0 )
      {
      LType L;
      L.Fill(0);
      L[i] = radius[i] * 2 + 1;
      res.AddLine(L);
      }
    }
  // this doesn't work if one of the dimensions is zero. Suspect an
  //"inconsistency" in the way
//  res.ComputeBufferFromLines();
  Iterator kernel_it;
  for ( kernel_it = res.Begin(); kernel_it != res.End(); ++kernel_it )
    {
    *kernel_it = true;
    }

  return res;
}

template< unsigned int VDimension >
FlatStructuringElement< VDimension > FlatStructuringElement< VDimension >
::Cross(RadiusType radius)
{
  // this should work for any number of dimensions
  Self res = Self();

  res.m_Decomposable = false;
  res.SetRadius(radius);
  Iterator kernel_it;
  for ( kernel_it = res.Begin(); kernel_it != res.End(); ++kernel_it )
    {
    *kernel_it = false;
    }
  for ( int d = 0; d < (int)VDimension; d++ )
    {
    OffsetType o;
    o.Fill(0);
    for ( int i = -(int)radius[d]; i <= (int)radius[d]; i++ )
      {
      o[d] = i;
      res[o] = true;
      }
    }

  return res;
}

template< unsigned int VDimension >
FlatStructuringElement< VDimension > FlatStructuringElement< VDimension >
::Ball(RadiusType radius, bool radiusIsParametric)
{
  Self res = Self();

  res.SetRadius(radius);
  res.m_Decomposable = false;
  res.SetRadiusIsParametric(radiusIsParametric);

  unsigned int i;

  // Create an image to hold the ellipsoid
  //
  typename ImageType::Pointer sourceImage = ImageType::New();
  typename ImageType::RegionType region;
  RadiusType size = radius;
  for ( i = 0; i < (int)VDimension; i++ )
    {
    size[i] = 2 * size[i] + 1;
    }
  region.SetSize(size);

  sourceImage->SetRegions(region);
  sourceImage->Allocate();
  // sourceImage->Print( std::cout );

  // Set the background to be zero
  //
  ImageRegionIterator< ImageType > it(sourceImage, region);

  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    it.Set(false);
    }

  // Create the ellipsoid
  //

  // Ellipsoid spatial function typedef
  typedef EllipsoidInteriorExteriorSpatialFunction< VDimension > EllipsoidType;

  // Create an ellipsoid spatial function for the source image
  typename EllipsoidType::Pointer spatialFunction = EllipsoidType::New();

  // Define and set the axes lengths for the ellipsoid
  typename EllipsoidType::InputType axes;
  for ( i = 0; i < VDimension; i++ )
    {
    if( res.GetRadiusIsParametric() )
      {
      axes[i] = 2 * res.GetRadius(i);
      }
    else
      {
      axes[i] = res.GetSize(i);
      }
    }
  spatialFunction->SetAxes(axes);

  // Define and set the center of the ellipsoid in physical space
  typename EllipsoidType::InputType center;
  for ( i = 0; i < VDimension; i++ )
    {
    // put the center of ellipse in the middle of the center pixel
    center[i] = res.GetRadius(i) + 0.5;
    }
  spatialFunction->SetCenter(center);

  // Define the orientations of the ellipsoid axes, for now, we'll use
  // the identify matrix
  typename EllipsoidType::OrientationType orientations;
  orientations.fill(0.0);
  orientations.fill_diagonal(1.0);
  spatialFunction->SetOrientations(orientations);

  typename ImageType::IndexType seed;
  for ( i = 0; i < VDimension; i++ )
    {
    seed[i] = res.GetRadius(i);
    }
  FloodFilledSpatialFunctionConditionalIterator< ImageType, EllipsoidType >
  sfi = FloodFilledSpatialFunctionConditionalIterator< ImageType,
                                                       EllipsoidType >(sourceImage, spatialFunction, seed);
  sfi.SetCenterInclusionStrategy();

  // Iterate through the entire image and set interior pixels to 1
  for (; !sfi.IsAtEnd(); ++sfi )
    {
    sfi.Set(true);
    }

  // Copy the ellipsoid into the kernel
  //
  Iterator kernel_it;
  for ( it.GoToBegin(), kernel_it = res.Begin(); !it.IsAtEnd(); ++it, ++kernel_it )
    {
    *kernel_it = it.Get();
    }

  // Clean up
  //   ...temporary image should be cleaned up by SmartPointers automatically

  return res;
}

template< unsigned int NDimension >
FlatStructuringElement< NDimension >
FlatStructuringElement< NDimension >
::Annulus(RadiusType radius,
          unsigned int thickness,
          bool includeCenter,
          bool radiusIsParametric)
{
  Self result = Self();

  result.SetRadius(radius);
  result.m_Decomposable = false;
  result.SetRadiusIsParametric( radiusIsParametric );

  // Create an image to hold the ellipsoid
  //
  typename ImageType::Pointer kernelImage = ImageType::New();
  typename ImageType::RegionType region;
  RadiusType size = radius;
  for ( unsigned int i = 0; i < NDimension; i++ )
    {
    size[i] = 2 * size[i] + 1;
    }
  region.SetSize(size);

  kernelImage->SetRegions(region);
  kernelImage->Allocate();

  // Set the background to be zero
  //
  ImageRegionIterator< ImageType > it(kernelImage, region);

  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    it.Set(false);
    }

  // Create two ellipsoids
  //

  // Ellipsoid spatial function typedef
  typedef EllipsoidInteriorExteriorSpatialFunction< NDimension > EllipsoidType;

  // Create an ellipsoid spatial function for the source image
  typename EllipsoidType::Pointer ellipsoidOuter = EllipsoidType::New();
  typename EllipsoidType::Pointer ellipsoidInner = EllipsoidType::New();

  // Define and set the axes lengths for the ellipsoid
  typename EllipsoidType::InputType axesOuter;
  typename EllipsoidType::InputType axesInner;
  for ( unsigned int i = 0; i < NDimension; i++ )
    {
    if( result.GetRadiusIsParametric() )
      {
      axesOuter[i] = 2 * result.GetRadius(i);
      axesInner[i] = std::max(2 * (OffsetValueType)radius[i] - 2 * (OffsetValueType)thickness, (OffsetValueType)1);
      }
    else
      {
      axesOuter[i] = result.GetSize(i);
      axesInner[i] = std::max(2 * (OffsetValueType)radius[i] + 1 - 2 * (OffsetValueType)thickness, (OffsetValueType)1);
      }
    }
  ellipsoidOuter->SetAxes(axesOuter);
  ellipsoidInner->SetAxes(axesInner);

  // Define and set the center of the ellipsoid in physical space
  typename EllipsoidType::InputType center;
  for ( unsigned int i = 0; i < NDimension; i++ )
    {
    // put the center of ellipse in the middle of the center pixel
    center[i] = result.GetRadius(i) + 0.5;
    }
  ellipsoidOuter->SetCenter(center);
  ellipsoidInner->SetCenter(center);

  // Define the orientations of the ellipsoid axes, for now, we'll use
  // the identity matrix
  typename EllipsoidType::OrientationType orientations;
  orientations.fill(0.0);
  orientations.fill_diagonal(1.0);
  ellipsoidOuter->SetOrientations(orientations);
  ellipsoidInner->SetOrientations(orientations);

  // Create the starting seed
  typename ImageType::IndexType seed;
  for ( unsigned int i = 0; i < NDimension; i++ )
    {
    seed[i] = result.GetRadius(i);
    }

  // Define the iterators for each ellipsoid
  typedef FloodFilledSpatialFunctionConditionalIterator< ImageType, EllipsoidType >
  FloodIteratorType;
  FloodIteratorType itEllipsoidOuter =
    FloodIteratorType(kernelImage, ellipsoidOuter, seed);
  FloodIteratorType itEllipsoidInner =
    FloodIteratorType(kernelImage, ellipsoidInner, seed);
  itEllipsoidOuter.SetCenterInclusionStrategy();
  itEllipsoidInner.SetCenterInclusionStrategy();

  // Iterate through the image and set all outer pixels to 'ON'
  for (; !itEllipsoidOuter.IsAtEnd(); ++itEllipsoidOuter )
    {
    itEllipsoidOuter.Set(true);
    }
  // Iterate through the image and set all inner pixels to 'OFF'
  for (; !itEllipsoidInner.IsAtEnd(); ++itEllipsoidInner )
    {
    itEllipsoidInner.Set(false);
    }

  // Set center pixel if included
  kernelImage->SetPixel(seed, includeCenter);

  // Copy the annulus into the kernel
  //
  Iterator kernel_it;
  for ( it.GoToBegin(), kernel_it = result.Begin(); !it.IsAtEnd(); ++it, ++kernel_it )
    {
    *kernel_it = it.Get();
    }

  // Clean up
  //   ...temporary image should be cleaned up by SmartPointers automatically

  return result;
}

template< unsigned int VDimension >
bool
FlatStructuringElement< VDimension >::CheckParallel(LType NewVec) const
{
  LType NN = NewVec;

  NN.Normalize();
  for ( unsigned i = 0; i < m_Lines.size(); i++ )
    {
    LType LL = m_Lines[i];
    LL.Normalize();
    float L = NN * LL;
    if ( ( 1.0 - std::fabs(L) ) < 0.000001 ) { return ( true ); }
    }
  return ( false );
}

template< unsigned int VDimension >
void FlatStructuringElement< VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if ( m_Decomposable )
    {
    os << indent << "SE decomposition:" << std::endl;
    for ( unsigned i = 0; i < m_Lines.size(); i++ )
      {
      os << indent << m_Lines[i] << std::endl;
      }
    }
}

template< unsigned int VDimension >
void
FlatStructuringElement< VDimension >::ComputeBufferFromLines()
{
  if ( !m_Decomposable )
    {
    itkGenericExceptionMacro("Element must be decomposable.");
    }

  // create an image with a single pixel in the center which will be dilated
  // by the structuring lines (with AnchorDilateImageFilter) so the content
  // of the buffer will reflect the shape of the structuring element

  // Create an image to hold the ellipsoid
  //
  typename ImageType::Pointer sourceImage = ImageType::New();
  typename ImageType::RegionType region;
  RadiusType size = this->GetRadius();
  for ( int i = 0; i < (int)VDimension; i++ )
    {
    size[i] = 2 * size[i] + 1;
    }
  region.SetSize(size);
  sourceImage->SetRegions(region);
  sourceImage->Allocate();

  // sourceImage->Print(std::cout);

  // Set the background to be zero
  //
  ImageRegionIterator< ImageType > it(sourceImage, region);

  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    it.Set(false);
    }

  // set the center pixel to 1
  typename ImageType::IndexType center;
  for ( int i = 0; i < (int)VDimension; i++ )
    {
    center[i] = this->GetRadius()[i];
    }
  sourceImage->SetPixel(center, true);

  // initialize the kernel with everything to false, to avoid warnings in
  // valgrind in the SetKernel() method
  Iterator kernel_it;
  for ( kernel_it = this->Begin(); kernel_it != this->End(); ++kernel_it )
    {
    *kernel_it = false;
    }

  // dilate the pixel
  typedef VanHerkGilWermanDilateImageFilter< ImageType, Self > DilateType;
  typename DilateType::Pointer dilate = DilateType::New();
  // suspect that multithreading does odd things when images are
  // really tiny
  dilate->SetNumberOfThreads(1);
  dilate->SetInput(sourceImage);
  dilate->SetKernel(*this);
  dilate->Update();

  // copy back the image to the kernel
  ImageRegionIterator< ImageType > oit(dilate->GetOutput(), region);
  for ( oit.GoToBegin(), kernel_it = this->Begin(); !oit.IsAtEnd(); ++oit, ++kernel_it )
    {
    *kernel_it = oit.Get();
    }
}

/** Check if size of input Image is odd in all dimensions, throwing exception if even */
template< unsigned int VDimension >
typename FlatStructuringElement< VDimension >::RadiusType
FlatStructuringElement< VDimension >::CheckImageSize(
  const typename FlatStructuringElement< VDimension >::ImageType * image)
{
  const RadiusType &size = image->GetLargestPossibleRegion().GetSize();

  for( unsigned int i = 0; i < VDimension; ++i )
    {
    if( ( size[i] % 2 ) == 0 )
      {
      itkGenericExceptionMacro("FlatStructuringElement constructor from image: size of input Image must be odd in all dimensions");
      }
    }
  return size;
}

template< unsigned int VDimension >
FlatStructuringElement< VDimension >
FlatStructuringElement< VDimension >::FromImage(
        const typename FlatStructuringElement< VDimension >::ImageType* image)
{
  Self res = Self();
  RadiusType size = res.CheckImageSize(image);
  Index< VDimension > centerIdx;

  for( unsigned int i = 0; i < VDimension; ++i )
    {
    size[i] = size[i] / 2;
    centerIdx[i] = size[i];
    }
  res.SetRadius( size );

  for( unsigned int j = 0; j < res.Size(); ++j )
    {
    const PixelType& val = image->GetPixel( centerIdx + res.GetOffset( j ) );
    // Neighborhood (therefore PixelType) in FlatStructringElement is bool
    if (val)
      {
      res[j] = true;
      }
    else
      {
      res[j] = false;
      }
    }

  return res;
}

}

#endif
