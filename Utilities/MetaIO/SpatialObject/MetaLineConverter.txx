/*=========================================================================

  Program:   itkUNC
  Module:    MetaLineConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  Author:    Julien Jomier (julien@jomier.com)

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaLineConverter__txx
#define __MetaLineConverter__txx

#include "MetaLineConverter.h"

/** Constructor */ 
template <unsigned int NDimensions>                                          
MetaLineConverter<NDimensions>
::MetaLineConverter()
{
  
}


/** Convert a metaLine into an Line SpatialObject  */
template <unsigned int NDimensions>       
typename MetaLineConverter<NDimensions>::SpatialObjectPointer
MetaLineConverter<NDimensions>
::MetaLineToLineSpatialObject(MetaLine * Line)
{ 

  typedef itk::LineSpatialObject<NDimensions> LineSpatialObjectType;
  typename LineSpatialObjectType::Pointer line = LineSpatialObjectType::New();
  
  double spacing[NDimensions];
  line->SetReferenceCount(2);
 
  unsigned int ndims = Line->NDims();
  for(unsigned int i=0;i<ndims;i++)
  {
    spacing[i]=Line->ElementSpacing()[i];
  }
  line->SetSpacing(spacing);
  line->GetProperty()->SetName((char*)Line->Name());
  line->SetParentId(Line->ParentID());
  line->SetId(Line->ID());
  line->GetProperty()->SetRed(Line->Color()[0]);
  line->GetProperty()->SetGreen(Line->Color()[1]);
  line->GetProperty()->SetBlue(Line->Color()[2]);
  line->GetProperty()->SetAlpha(Line->Color()[3]);

  typedef itk::LineSpatialObjectPoint<NDimensions> LinePointType;
  typedef LinePointType* LinePointPointer;

  
  typedef MetaLine::PointListType ListType;
  ListType::iterator it2 = Line->GetPoints().begin();
    
  vnl_vector<double> v(ndims);
  
  for(unsigned int id=0;id< Line->GetPoints().size();id++)
  {
    LinePointType pnt;
    
    typedef typename LinePointType::PointType PointType;
    PointType point;
    typedef typename LinePointType::VectorType NormalType;

    for(unsigned int i=0;i<ndims;i++)
    {
      point[i]=(*it2)->m_X[i];
    }

    pnt.SetPosition(point);

    for(unsigned int i=0;i<ndims-1;i++)
    {
      NormalType normal;
      for(unsigned int j=0;j<ndims;j++)
      {
        normal[j]=(*it2)->m_V[i][j];
      }
      pnt.SetNormal(normal,i);
        
    }

    
    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    line->GetPoints().push_back(pnt);
    it2++;
  }
 
  return line;
}

/** Convert an Line SpatialObject into a metaLine */
template <unsigned int NDimensions>       
MetaLine*
MetaLineConverter<NDimensions>
::LineSpatialObjectToMetaLine(SpatialObjectType * spatialObject)
{ 
  MetaLine* Line = new MetaLine(NDimensions);

  // fill in the Line information
   
  typename SpatialObjectType::PointListType::const_iterator i;
  for(i = dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().begin(); i != dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().end(); i++)
  {
    LinePnt* pnt = new LinePnt(NDimensions);
    
    for(unsigned int d=0;d<NDimensions;d++)
    {
      pnt->m_X[d]=(*i).GetPosition()[d];
    }

    for(unsigned int n=0;n<NDimensions-1;n++)
    {
      for(unsigned int d=0;d<NDimensions;d++)
      {
        pnt->m_V[n][d]=((*i).GetNormal(n))[d];
      }
    }

    pnt->m_Color[0] = (*i).GetRed();
    pnt->m_Color[1] = (*i).GetGreen();
    pnt->m_Color[2] = (*i).GetBlue();
    pnt->m_Color[3] = (*i).GetAlpha();

    Line->GetPoints().push_back(pnt); 
  }
    
  if(NDimensions == 2)
  {
    Line->PointDim("x y v1x v1y v2x v2y red green blue alpha");
  }
  else if(NDimensions == 3)
  {
    Line->PointDim("x y z v1x v1y v1z v2x v2y v2z red green blue alpha");
  }

  float color[4];
  for(unsigned int i=0;i<4;i++)
  {
    color[i]=spatialObject->GetProperty()->GetColor()[i];
  }

  Line->Color(color);
  Line->ID( spatialObject->GetId());
  Line->ParentID(spatialObject->GetParentId());
  Line->NPoints(Line->GetPoints().size());

  return Line;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>       
typename MetaLineConverter<NDimensions>::SpatialObjectPointer
MetaLineConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaLine* Line = new MetaLine();
  Line->Read(name);
  spatialObject = MetaLineToLineSpatialObject(Line);

  return spatialObject;
}


/** Write a meta Line file */
template <unsigned int NDimensions>
bool
MetaLineConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaLine* Line = LineSpatialObjectToMetaLine(spatialObject);
  Line->Write(name);
  return true;
}

#endif
