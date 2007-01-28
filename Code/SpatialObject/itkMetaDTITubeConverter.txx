/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDTITubeConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaDTITubeConverter_txx
#define __itkMetaDTITubeConverter_txx

#include "itkMetaDTITubeConverter.h"

namespace itk  
{


/** Constructor */ 
template <unsigned int NDimensions>
MetaDTITubeConverter<NDimensions>
::MetaDTITubeConverter()
{
  
}


/** Convert a MetaDTITube into an Tube SpatialObject  */
template <unsigned int NDimensions>
typename MetaDTITubeConverter<NDimensions>::SpatialObjectPointer
MetaDTITubeConverter<NDimensions>
::MetaDTITubeToDTITubeSpatialObject(MetaDTITube * tube)
{ 
  typedef itk::DTITubeSpatialObject<NDimensions> DTITubeSpatialObjectType;
  typename DTITubeSpatialObjectType::Pointer tub = 
                                          DTITubeSpatialObjectType::New();
  double spacing[NDimensions];

  unsigned int ndims = tube->NDims();
  for(unsigned int i=0;i<ndims;i++)
    {
    spacing[i]=tube->ElementSpacing()[i];
    }

  tub->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  tub->GetProperty()->SetName(tube->Name());
  tub->SetParentPoint(tube->ParentPoint());
  tub->SetId(tube->ID());
  tub->SetParentId(tube->ParentID());
  tub->GetProperty()->SetRed(tube->Color()[0]);
  tub->GetProperty()->SetGreen(tube->Color()[1]);
  tub->GetProperty()->SetBlue(tube->Color()[2]);
  tub->GetProperty()->SetAlpha(tube->Color()[3]);

  typedef itk::DTITubeSpatialObjectPoint<NDimensions> TubePointType;
  typedef TubePointType*                              TubePointPointer;

  typedef MetaDTITube::PointListType ListType;
  ListType::iterator it2 = tube->GetPoints().begin();
    
  itk::CovariantVector<double,NDimensions> v; 
  itk::Vector<double,NDimensions> t;
  
  for(unsigned int id=0;id< tube->GetPoints().size();id++)
    {
    TubePointType pnt;
    
    typedef typename DTITubeSpatialObjectType::PointType PointType;
    PointType point;

    for(unsigned int i=0;i<ndims;i++)
      {
      point[i]=(*it2)->m_X[i];
      }

    // Get the fields from the metaIO
    const DTITubePnt::FieldListType & metaFields = (*it2)->GetExtraFields();
    DTITubePnt::FieldListType::const_iterator extraIt = metaFields.begin();
    while(extraIt != metaFields.end())
      {
      // Do not add the optional fields
      if( ((*extraIt).first != "r")
        && ((*extraIt).first != "v1x")
        && ((*extraIt).first != "v1y")
        && ((*extraIt).first != "v1z") 
        && ((*extraIt).first != "v2x")
        && ((*extraIt).first != "v2y")
        && ((*extraIt).first != "v2z")
        && ((*extraIt).first != "tx")
        && ((*extraIt).first != "ty")
        && ((*extraIt).first != "tz")
        && ((*extraIt).first != "red")
        && ((*extraIt).first != "green")
        && ((*extraIt).first != "blue")
        && ((*extraIt).first != "alpha")
        && ((*extraIt).first != "id")
        )
        {
        pnt.AddField((*extraIt).first.c_str(),(*extraIt).second);
        }
      extraIt++;
      }

    pnt.SetPosition(point);

    float* tensor = new float[6];

    for(unsigned int i=0;i<6;i++)
      {
      tensor[i]=(*it2)->m_TensorMatrix[i];
      }
    pnt.SetTensorMatrix(tensor);

    delete []tensor;


    // This attribute are optional
    if((*it2)->GetField("r") != -1)
      {
      pnt.SetRadius((*it2)->GetField("r"));
      }

    if((*it2)->GetField("v1x") != -1)
      {
      v[0]= (*it2)->GetField("v1x");
      v[1]= (*it2)->GetField("v1y");
      if(ndims == 3)
        {
        v[2]= (*it2)->GetField("v1z");
        } 
      pnt.SetNormal1(v);
      }
   
    
    if((*it2)->GetField("v2x") != -1)
      {
      v[0]= (*it2)->GetField("v2x");
      v[1]= (*it2)->GetField("v2y");
      if(ndims == 3)
        {
        v[2]= (*it2)->GetField("v2z");
        } 
      pnt.SetNormal1(v);
      }
  
    if((*it2)->GetField("tx") != -1)
      {
      t[0]= (*it2)->GetField("tx");
      t[1]= (*it2)->GetField("ty");
      if(ndims == 3)
        {
        t[2]= (*it2)->GetField("tz");
        }   
      pnt.SetTangent(t);
      }


    if((*it2)->GetField("red") != -1)
      {
      pnt.SetRed((*it2)->GetField("red"));
      }
    
    if((*it2)->GetField("green") != -1)
      {
      pnt.SetGreen((*it2)->GetField("green"));
      }
   
    if((*it2)->GetField("blue") != -1)
      {
      pnt.SetBlue((*it2)->GetField("blue"));
      }

    if((*it2)->GetField("alpha") != -1)
      {
      pnt.SetAlpha((*it2)->GetField("alpha"));
      }

    if((*it2)->GetField("id") != -1)
      {
      pnt.SetID((int)((*it2)->GetField("id")));
      }

    tub->GetPoints().push_back(pnt);

    it2++;
    }
  return tub;
}

/** Convert an Tube SpatialObject into a MetaDTITube */
template <unsigned int NDimensions>
MetaDTITube*
MetaDTITubeConverter<NDimensions>
::DTITubeSpatialObjectToMetaDTITube(SpatialObjectType * spatialObject)
{ 
  MetaDTITube* tube = new MetaDTITube(NDimensions);

  // Check what are the fields to be written
  bool writeNormal1 = false;
  bool writeNormal2 = false;
  bool writeTangent = false;
  bool writeRadius = false;
  bool writeColor = false;
  bool writeAlpha = false;
  bool writeID = false;

  typename SpatialObjectType::PointListType::const_iterator i;
  for(i = dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().begin(); 
      i != dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().end();
      i++)
    {
    // Optional fields (written only if not default values)
    if((*i).GetID() != -1)
      {
      writeID = true;
      }

    if((*i).GetRadius() != 0)
      {
      writeRadius = true;
      }

    unsigned int d;
    for(d=0;d<NDimensions;d++)
     {
     if((*i).GetNormal1()[d] != 0)
       {
       writeNormal1 = true;
       }
     if((*i).GetNormal2()[d] != 0)
       {
       writeNormal2 = true;
       }
     if((*i).GetTangent()[d] != 0)
       {
       writeTangent = true;
       }
     }

    // write the color if changed
    if( ((*i).GetRed() != 1.0)
      || ((*i).GetGreen() != 0.0)
      || ((*i).GetBlue() != 0.0)
      )
      {
      writeColor = true;
      }
      
    if((*i).GetAlpha() != 1.0)
      {
      writeAlpha = true;
      }
    }

  // fill in the tube information
  for(i = dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().begin(); 
      i != dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().end();
      i++)
    {
    DTITubePnt* pnt = new DTITubePnt(NDimensions);

    for(unsigned int d=0;d<NDimensions;d++)
      {
      pnt->m_X[d] = (*i).GetPosition()[d];
      }
      
    const DTITubePnt::FieldListType & metaFields = (*i).GetFields();
    DTITubePnt::FieldListType::const_iterator extraIt = metaFields.begin();
    while(extraIt != metaFields.end())
      {
      pnt->AddField((*extraIt).first.c_str(),(*extraIt).second);
      extraIt++;
      }

    for(unsigned int d=0;d<6;d++)
      {
      pnt->m_TensorMatrix[d]=(*i).GetTensorMatrix()[d];
      }

    // Optional fields (written only if not default values)
    if(writeID)
      {
      pnt->AddField("id",(*i).GetID());
      }

    if(writeRadius)
      {
      pnt->AddField("r",(*i).GetRadius());
      }

    if(writeNormal1)
      {  
      pnt->AddField("v1x",(*i).GetNormal1()[0]);
      pnt->AddField("v1y",(*i).GetNormal1()[1]);   
      if(NDimensions == 3)
        {
        pnt->AddField("v1z",(*i).GetNormal1()[2]);   
        }
      }

    if(writeNormal2)
      {
      pnt->AddField("v2x",(*i).GetNormal2()[0]);
      pnt->AddField("v2y",(*i).GetNormal2()[1]);
      if(NDimensions == 3)
        {
        pnt->AddField("v2z",(*i).GetNormal2()[2]);
        }
      }

    if(writeTangent)
      {
      pnt->AddField("tx",(*i).GetTangent()[0]);
      pnt->AddField("ty",(*i).GetTangent()[1]);
      if(NDimensions == 3)
        {
        pnt->AddField("tz",(*i).GetTangent()[2]);
        }
      }  

    // write the color if changed
    if(writeColor)
      {
      pnt->AddField("red",(*i).GetRed());
      pnt->AddField("green",(*i).GetGreen());
      pnt->AddField("blue",(*i).GetBlue());
      }
      
    if(writeAlpha)
      { 
      pnt->AddField("alpha",(*i).GetAlpha());
      }

    tube->GetPoints().push_back(pnt); 
    }
   
  tube->PointDim("x y z tensor1 tensor2 tensor3 tensor4 tensor5 tensor6");

  float color[4];
  for(unsigned int i=0;i<4;i++)
    {
    color[i]=spatialObject->GetProperty()->GetColor()[i];
    }

  tube->Color(color);
  tube->ID( spatialObject->GetId());

  if(spatialObject->GetParent())
    {
    tube->ParentID(spatialObject->GetParent()->GetId());
    }
  tube->ParentPoint(spatialObject->GetParentPoint());
  tube->NPoints(tube->GetPoints().size());

  for(unsigned int i=0;i<NDimensions;i++)
    {
    tube->ElementSpacing(i, spatialObject->GetIndexToObjectTransform()
                                         ->GetScaleComponent()[i]);
    }
  return tube;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>
typename MetaDTITubeConverter<NDimensions>::SpatialObjectPointer
MetaDTITubeConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaDTITube* Tube = new MetaDTITube();
  Tube->Read(name);
  spatialObject = MetaDTITubeToDTITubeSpatialObject(Tube);
  delete Tube;
  return spatialObject;
}


/** Write a meta Tube file */
template <unsigned int NDimensions>
bool
MetaDTITubeConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaDTITube* Tube = DTITubeSpatialObjectToMetaDTITube(spatialObject);
  Tube->Write(name);
  delete Tube;
  return true;
}

} // end namespace itk 

#endif
