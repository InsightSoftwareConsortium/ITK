/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMWLMFindQuery_H
#define GDCMWLMFindQuery_H

#include "gdcmBaseRootQuery.h"

namespace gdcm
{
/**
 * \brief PatientRootQuery
 * \details contains: the class which will produce a dataset for c-find with patient root
 */
class GDCM_EXPORT WLMFindQuery : public BaseRootQuery
{
  friend class QueryFactory;
public:
  WLMFindQuery();

  // no sense here
  void InitializeDataSet(const EQueryLevel& inQueryLevel);
  std::vector<Tag> GetTagListByLevel(const EQueryLevel& inQueryLevel);
  // validate query has required tag
  bool ValidateQuery(bool inStrict = true) const;

  UIDs::TSName GetAbstractSyntaxUID() const;
protected :
  DataSet GetValidDataSet() const;
};

} // end namespace gdcm

#endif // GDCMWLMFindQuery_H
