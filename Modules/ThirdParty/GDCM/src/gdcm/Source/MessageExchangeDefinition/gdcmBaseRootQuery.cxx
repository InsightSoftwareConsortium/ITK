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
/*
contains: a baseclass which will produce a dataset for c-find and c-move with
patient root

This class contains the functionality used in patient c-find and c-move
queries.  StudyRootQuery derives from this class.

Namely:
1) list all tags associated with a particular query type
2) produce a query dataset via tag association

Eventually, it can be used to validate a particular dataset type.

The dataset held by this object (or, really, one of its derivates) should be
passed to a c-find or c-move query.

*/

#include "gdcmBaseRootQuery.h"
#include "gdcmDataElement.h"
#include "gdcmTag.h"
#include "gdcmDict.h"
#include "gdcmDictEntry.h"
#include "gdcmDicts.h"
#include "gdcmGlobal.h"
#include "gdcmAttribute.h"
#include "gdcmWriter.h"
#include "gdcmPrinter.h"
#include <limits>

namespace gdcm
{

  BaseRootQuery::BaseRootQuery()
    : BaseQuery()
  {
    //nothing to do, really
  }
  BaseRootQuery::~BaseRootQuery()
  {
    //nothing to do, really
  }

  static const char *QueryLevelStrings[] = {
    "PATIENT ",
    "STUDY ",
    "SERIES",
    "IMAGE ",
  };

  const char *BaseRootQuery::GetQueryLevelString( EQueryLevel ql )
  {
    return QueryLevelStrings[ ql ];
  }

  int BaseRootQuery::GetQueryLevelFromString( const char * str )
  {
    if( str )
    {
      const std::string s = str;
      static const int n =
        sizeof( QueryLevelStrings ) / sizeof( *QueryLevelStrings );
      for( int i = 0; i < n; ++i )
      {
        if( s == QueryLevelStrings[ i ] )
        {
          return i;
        }
      }
    }
    return -1; // FIXME never use it as valid enum
  }

  QueryBase * BaseRootQuery::Construct( ERootType inRootType, EQueryLevel qlevel )
  {
    QueryBase* qb = NULL;
    // Check no new extension:
    assert( inRootType == ePatientRootType || inRootType == eStudyRootType );

    if( qlevel == ePatient )
    {
      if( inRootType == ePatientRootType )
      {
        qb = new QueryPatient;
      }
    }
    else if( qlevel == eStudy )
    {
      qb = new QueryStudy;
    }
    else if( qlevel == eSeries )
    {
      qb = new QuerySeries;
    }
    else if( qlevel == eImage )
    {
      qb = new QueryImage;
    }
    return qb;
  }

  EQueryLevel BaseRootQuery::GetQueryLevelFromQueryRoot( ERootType roottype )
  {
    EQueryLevel level = ePatient;
    switch( roottype )
    {
    case ePatientRootType:
      level = ePatient;
      break;
    case eStudyRootType:
      level = eStudy;
      break;
    }
    assert( level == eStudy || level == ePatient );
    return level;
  }

} // end namespace gdcm
