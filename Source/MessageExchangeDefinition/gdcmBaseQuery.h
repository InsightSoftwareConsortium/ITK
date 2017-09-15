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
#ifndef GDCMBASEQUERY_H
#define GDCMBASEQUERY_H

#include "gdcmDataSet.h"
#include "gdcmUIDs.h"
#include "gdcmObject.h"

namespace gdcm
{
  class QueryFactory;
  class DictEntry;
    
  enum ENQueryType
    {
    eCreateMMPS = 0,
    eSetMMPS
    };
/**
 * \brief BaseQuery
 * \details contains: a baseclass which will produce a dataset for all dimse messages
 *
 *
 */
class GDCM_EXPORT BaseQuery : public Object
{
  //these four classes contain the required, unique, and optional tags from the standard.
  //used both to list the tags as well as to validate a dataset, if ever we were to do so.
protected:

  DataSet mDataSet;
  friend class QueryFactory;
  BaseQuery();

  std::string mHelpDescription; //used when generating the help output
  std::string mSopInstanceUID;

  void SetSearchParameter(const Tag& inTag, const DictEntry& inDictEntry, const std::string& inValue);
  
  bool ValidDataSet( const DataSet & dataSetToValid, const DataSet & dataSetReference  ) const ;
public:
  virtual ~BaseQuery();

  void SetSearchParameter(const Tag& inTag, const std::string& inValue);
  void SetSearchParameter(const std::string& inKeyword, const std::string& inValue);

  const std::ostream &WriteHelpFile(std::ostream &os);

  //this function allows writing of the query to disk for storing for future use
  //virtual in case it needs to be overiden
  //returns false if the operation failed
  bool WriteQuery(const std::string& inFileName);

  /// Set/Get the internal representation of the query as a DataSet
  DataSet const & GetQueryDataSet() const;
  DataSet & GetQueryDataSet();
  void AddQueryDataSet(const DataSet & ds);

  virtual bool ValidateQuery( bool inStrict = true ) const = 0;

  virtual UIDs::TSName GetAbstractSyntaxUID() const = 0;
  std::string GetSOPInstanceUID() const { return mSopInstanceUID ; }
  void SetSOPInstanceUID( const std::string & iSopInstanceUID ) { mSopInstanceUID = iSopInstanceUID ; }



  void Print(std::ostream &os) const;
};

} // end namespace gdcm

#endif //GDCMBASEROOTQUERY_H
