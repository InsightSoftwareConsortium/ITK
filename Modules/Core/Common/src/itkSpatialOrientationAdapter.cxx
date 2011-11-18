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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkSpatialOrientationAdapter.h"

namespace itk
{
SpatialOrientationAdapter::OrientationType
SpatialOrientationAdapter
::FromDirectionCosines(const DirectionType & Dir)
{
  int      axes[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  unsigned dominant_axis;

  dominant_axis = Function::Max3(Dir[0][0], Dir[1][0], Dir[2][0]);
  axes[dominant_axis] = Function::Sign(Dir[dominant_axis][0]);
  dominant_axis = Function::Max3(Dir[0][1], Dir[1][1], Dir[2][1]);
  axes[dominant_axis + 3] = Function::Sign(Dir[dominant_axis][1]);
  dominant_axis = Function::Max3(Dir[0][2], Dir[1][2], Dir[2][2]);
  axes[dominant_axis + 6] = Function::Sign(Dir[dominant_axis][2]);

  SpatialOrientation::CoordinateTerms terms[3] = {
    SpatialOrientation::ITK_COORDINATE_UNKNOWN,
    SpatialOrientation::ITK_COORDINATE_UNKNOWN,
    SpatialOrientation::ITK_COORDINATE_UNKNOWN
    };

  for ( unsigned i = 0; i < 3; i++ )
    {
    if ( int(axes[( i * 3 )]) == 1 )
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Right;
      }
    else if ( axes[( i * 3 )] == -1 )
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Left;
      }
    else if ( axes[( i * 3 ) + 1] == 1 )
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Anterior;
      }
    else if ( axes[( i * 3 ) + 1] == -1 )
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Posterior;
      }
    else if ( axes[( i * 3 ) + 2] == 1 )
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Inferior;
      }
    else if ( axes[( i * 3 ) + 2] == -1 )
      {
      terms[i] = SpatialOrientation::ITK_COORDINATE_Superior;
      }
    }
  //
  // all terms must be defined, otherwise just punt
  if ( terms[0] == SpatialOrientation::ITK_COORDINATE_UNKNOWN
       || terms[1] == SpatialOrientation::ITK_COORDINATE_UNKNOWN
       || terms[2] == SpatialOrientation::ITK_COORDINATE_UNKNOWN )
    {
    return SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
    }

  return static_cast< SpatialOrientation::ValidCoordinateOrientationFlags >
         ( ( terms[0] << SpatialOrientation::ITK_COORDINATE_PrimaryMinor )
           + ( terms[1] << SpatialOrientation::ITK_COORDINATE_SecondaryMinor )
           + ( terms[2] << SpatialOrientation::ITK_COORDINATE_TertiaryMinor ) );
}

SpatialOrientationAdapter::DirectionType
SpatialOrientationAdapter
::ToDirectionCosines(const OrientationType & Or)
{
  typedef SpatialOrientation::CoordinateTerms CoordinateTerms;

  CoordinateTerms terms[3];
  terms[0] =
    static_cast< CoordinateTerms >
    ( ( Or >> SpatialOrientation::ITK_COORDINATE_PrimaryMinor ) & 0xff );
  terms[1] =
    static_cast< CoordinateTerms >
    ( ( Or >> SpatialOrientation::ITK_COORDINATE_SecondaryMinor ) & 0xff );
  terms[2] =
    static_cast< CoordinateTerms >
    ( ( Or >> SpatialOrientation::ITK_COORDINATE_TertiaryMinor ) & 0xff );
  DirectionType direction;
  direction.Fill(0.0);
  for ( unsigned int i = 0; i < DirectionType::ColumnDimensions; i++ )
    {
    switch ( terms[i] )
      {
      case SpatialOrientation::ITK_COORDINATE_Right:
        direction[0][i] = 1;
        break;
      case SpatialOrientation::ITK_COORDINATE_Left:
        direction[0][i] = -1;
        break;
      case SpatialOrientation::ITK_COORDINATE_Anterior:
        if ( DirectionType::RowDimensions > 1 ) { direction[1][i] = 1; }
        break;
      case SpatialOrientation::ITK_COORDINATE_Posterior:
        if ( DirectionType::RowDimensions > 1 ) { direction[1][i] = -1; }
        break;
      case SpatialOrientation::ITK_COORDINATE_Inferior:
        if ( DirectionType::RowDimensions > 2 ) { direction[2][i] = 1; }
        break;
      case SpatialOrientation::ITK_COORDINATE_Superior:
        if ( DirectionType::RowDimensions > 2 ) { direction[2][i] = -1; }
        break;
      case SpatialOrientation::ITK_COORDINATE_UNKNOWN:
      default:
        //TODO:  Should there be a default?  Throw an exception?
        break;
      }
    }
  //TODO:  Should check that directions are orthoganal.
  return direction;
}
} // end namespace itk
