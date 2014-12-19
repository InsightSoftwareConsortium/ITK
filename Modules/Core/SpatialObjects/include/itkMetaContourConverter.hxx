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
#ifndef itkMetaContourConverter_hxx
#define itkMetaContourConverter_hxx

#include "itkMetaContourConverter.h"

namespace itk
{

/** Constructor */
template< unsigned int NDimensions >
MetaContourConverter< NDimensions >
::MetaContourConverter()
{}

template< unsigned int NDimensions >
typename MetaContourConverter< NDimensions >::MetaObjectType *
MetaContourConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new ContourMetaObjectType);
}

/** Convert a metaContour into an Contour SpatialObject  */
template< unsigned int NDimensions >
typename MetaContourConverter< NDimensions >::SpatialObjectPointer
MetaContourConverter< NDimensions >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const ContourMetaObjectType *contourMO = dynamic_cast<const MetaContour *>(mo);
  if(contourMO == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't downcast MetaObject to MetaContour");
    }

  ContourSpatialObjectPointer contourSO =
    ContourSpatialObjectType::New();

  double spacing[NDimensions];

  unsigned int ndims = contourMO->NDims();
  for ( unsigned int i = 0; i < ndims; i++ )
    {
    spacing[i] = contourMO->ElementSpacing()[i];
    }
  contourSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  contourSO->GetProperty()->SetName( contourMO->Name() );
  contourSO->SetId( contourMO->ID() );
  contourSO->SetParentId( contourMO->ParentID() );
  contourSO->GetProperty()->SetRed(contourMO->Color()[0]);
  contourSO->GetProperty()->SetGreen(contourMO->Color()[1]);
  contourSO->GetProperty()->SetBlue(contourMO->Color()[2]);
  contourSO->GetProperty()->SetAlpha(contourMO->Color()[3]);
  contourSO->SetClosed( const_cast<ContourMetaObjectType *>(contourMO)->Closed() );
  contourSO->SetAttachedToSlice( const_cast<ContourMetaObjectType *>(contourMO)->AttachedToSlice() );
  contourSO->SetDisplayOrientation( const_cast<ContourMetaObjectType *>(contourMO)->DisplayOrientation() );

  // First the control points
  typedef typename ContourSpatialObjectType::ControlPointType ControlPointType;

  typename ContourMetaObjectType::ControlPointListType::const_iterator itCP =
    contourMO->GetControlPoints().begin();

  for ( unsigned int identifier = 0; identifier < contourMO->GetControlPoints().size(); identifier++ )
    {
    ControlPointType pnt;

    typedef typename ControlPointType::PointType PointType;
    PointType point;
    PointType pickedPoint;

    typedef typename ControlPointType::VectorType VectorType;
    VectorType normal;

    for ( unsigned int i = 0; i < ndims; i++ )
      {
      point[i] = ( *itCP )->m_X[i];
      }

    for ( unsigned int i = 0; i < ndims; i++ )
      {
      pickedPoint[i] = ( *itCP )->m_XPicked[i];
      }

    for ( unsigned int i = 0; i < ndims; i++ )
      {
      normal[i] = ( *itCP )->m_V[i];
      }

    pnt.SetID( ( *itCP )->m_Id );
    pnt.SetRed( ( *itCP )->m_Color[0] );
    pnt.SetGreen( ( *itCP )->m_Color[1] );
    pnt.SetBlue( ( *itCP )->m_Color[2] );
    pnt.SetAlpha( ( *itCP )->m_Color[3] );

    pnt.SetPosition(point);
    pnt.SetPickedPoint(pickedPoint);
    pnt.SetNormal(normal);

    contourSO->GetControlPoints().push_back(pnt);
    itCP++;
    }

  // Then the interpolated points
  typedef typename ContourSpatialObjectType::InterpolatedPointType  InterpolatedPointType;
  typename ContourMetaObjectType::InterpolatedPointListType::const_iterator
    itI = contourMO->GetInterpolatedPoints().begin();

  for ( unsigned int identifier = 0; identifier < contourMO->GetInterpolatedPoints().size(); identifier++ )
    {
    InterpolatedPointType pnt;

    typedef typename ControlPointType::PointType PointType;
    PointType point;

    typedef typename ControlPointType::VectorType VectorType;
    VectorType normal;

    for ( unsigned int i = 0; i < ndims; i++ )
      {
      point[i] = ( *itI )->m_X[i];
      }

    pnt.SetID( ( *itI )->m_Id );
    pnt.SetRed( ( *itI )->m_Color[0] );
    pnt.SetGreen( ( *itI )->m_Color[1] );
    pnt.SetBlue( ( *itI )->m_Color[2] );
    pnt.SetAlpha( ( *itI )->m_Color[3] );

    pnt.SetPosition(point);
    contourSO->GetInterpolatedPoints().push_back(pnt);
    itI++;
    }

  return contourSO.GetPointer();
}

/** Convert a Contour SpatialObject into a metaContour */
template< unsigned int NDimensions >
typename MetaContourConverter< NDimensions>::MetaObjectType *
MetaContourConverter< NDimensions >
::SpatialObjectToMetaObject(const SpatialObjectType *so)
{
  ContourSpatialObjectConstPointer contourSO =
    dynamic_cast<const ContourSpatialObjectType *>(so);
  if(contourSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to ContourSpatialObject");
    }
  MetaContour *contourMO = new MetaContour(NDimensions);


  // fill in the control points information
  typename ContourSpatialObjectType::ControlPointListType::const_iterator itCP;

  for ( itCP = contourSO->GetControlPoints().begin();
        itCP != contourSO->GetControlPoints().end();
        itCP++ )
    {
    ContourControlPnt *pnt = new ContourControlPnt(NDimensions);

    pnt->m_Id = ( *itCP ).GetID();

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *itCP ).GetPosition()[d];
      }

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_XPicked[d] = ( *itCP ).GetPickedPoint()[d];
      }

    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_V[d] = ( *itCP ).GetNormal()[d];
      }

    pnt->m_Color[0] = ( *itCP ).GetRed();
    pnt->m_Color[1] = ( *itCP ).GetGreen();
    pnt->m_Color[2] = ( *itCP ).GetBlue();
    pnt->m_Color[3] = ( *itCP ).GetAlpha();

    contourMO->GetControlPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    contourMO->ControlPointDim("id x y xp yp v1 v2 r g b a");
    }
  else if ( NDimensions == 3 )
    {
    contourMO->ControlPointDim("id x y z xp yp zp v1 v2 v3 r gn be a");
    }

  // fill in the interpolated points information
  typename ContourSpatialObjectType::InterpolatedPointListType::const_iterator itI;
  for ( itI = contourSO->GetInterpolatedPoints().begin();
        itI != contourSO->GetInterpolatedPoints().end();
        itI++ )
    {
    ContourInterpolatedPnt *pnt = new ContourInterpolatedPnt(NDimensions);

    pnt->m_Id = ( *itI ).GetID();
    for ( unsigned int d = 0; d < NDimensions; d++ )
      {
      pnt->m_X[d] = ( *itI ).GetPosition()[d];
      }

    pnt->m_Color[0] = ( *itI ).GetRed();
    pnt->m_Color[1] = ( *itI ).GetGreen();
    pnt->m_Color[2] = ( *itI ).GetBlue();
    pnt->m_Color[3] = ( *itI ).GetAlpha();

    contourMO->GetInterpolatedPoints().push_back(pnt);
    }

  if ( NDimensions == 2 )
    {
    contourMO->InterpolatedPointDim("id x y r g b a");
    }
  else if ( NDimensions == 3 )
    {
    contourMO->InterpolatedPointDim("id x y z r g b a");
    }

  // Set the interpolation type
  switch ( contourSO->GetInterpolationType() )
    {
    case ContourSpatialObjectType::EXPLICIT_INTERPOLATION:
      contourMO->Interpolation(MET_EXPLICIT_INTERPOLATION);
      break;
    case ContourSpatialObjectType::LINEAR_INTERPOLATION:
      contourMO->Interpolation(MET_LINEAR_INTERPOLATION);
      break;
    case ContourSpatialObjectType::BEZIER_INTERPOLATION:
      contourMO->Interpolation(MET_BEZIER_INTERPOLATION);
      break;
    default:
      contourMO->Interpolation(MET_NO_INTERPOLATION);
    }

  float color[4];
  for ( unsigned int i = 0; i < 4; i++ )
    {
    color[i] = contourSO->GetProperty()->GetColor()[i];
    }
  contourMO->Color(color);
  contourMO->ID( contourSO->GetId() );
  contourMO->Closed( contourSO->GetClosed() );
  contourMO->AttachedToSlice( contourSO->GetAttachedToSlice() );
  contourMO->DisplayOrientation( contourSO->GetDisplayOrientation() );

  if ( contourSO->GetParent() )
    {
    contourMO->ParentID( contourSO->GetParent()->GetId() );
    }

  for ( unsigned int i = 0; i < NDimensions; i++ )
    {
    contourMO->ElementSpacing(i, contourSO->GetIndexToObjectTransform()
                            ->GetScaleComponent()[i]);
    }
  contourMO->BinaryData(true);
  return contourMO;
}

} // end namespace itk

#endif
