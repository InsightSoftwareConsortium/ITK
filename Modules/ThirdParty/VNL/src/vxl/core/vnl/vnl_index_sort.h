#ifndef vnl_index_sort_h_
#define vnl_index_sort_h_
//:
// \file
// \author Michael R. Bowers
//

#include <algorithm>
#include <utility>
#include <vector>
#include <vnl/vnl_vector.h>
#include <vcl_compiler.h>
#include "vnl/vnl_export.h"

template <class TValue, class TIndex>
class VNL_TEMPLATE_EXPORT vnl_index_sort
{
 public:

   //: typedefs for vector sorting
  typedef vnl_vector<TValue> SortVectorType;
  typedef vnl_vector<TIndex> SortVectorIndexType;

  //: typedefs for matrix sorting
  typedef vnl_matrix<TValue> SortMatrixType;
  typedef vnl_matrix<TIndex> SortMatrixIndexType;

  //: matrix sort along rows or columns?
  enum DirectionType {ByRow, ByColumn} Direction;

  //: just sort indices
  void vector_sort(
    const SortVectorType& values,
    SortVectorIndexType& indices)
  {
    sortIndices(values, indices);
  }

  //: sort indices and values
  void vector_sort(
    const SortVectorType& values,
    SortVectorType& sorted_values,
    SortVectorIndexType& indices)
  {
    vector_sort(values, indices);

    // gets values from sorted indices
    reindexValues(values, indices, sorted_values);
  }

  //: sort indices, return sorted values in place
  void vector_sort_in_place(
    SortVectorType& values,
    SortVectorIndexType& indices)
  {
    vector_sort(values, indices);
    SortVectorType tmpValues(values);

    // gets values and indices from sorted indices
    reindexValues(tmpValues, indices, values);
  }

  //: matrix sort
  // specify along rows or columns
  void matrix_sort(
    DirectionType direction,
    const SortMatrixType& values,
    SortMatrixType& sorted_values,
    SortMatrixIndexType& indices)
  {
    sorted_values.set_size(values.rows(), values.cols());
    indices.set_size(values.rows(), values.cols());

    SortVectorType valVect;
    SortVectorType sortedValVect;
    SortVectorIndexType indVect;
    for (unsigned int vIx = 0;
         vIx < (direction == ByRow ? values.rows() : values.cols()); vIx++)
    {
      getVector(values, direction, vIx, valVect);
      vector_sort(valVect, sortedValVect, indVect);
      putVector(sortedValVect, direction, vIx, sorted_values);
      putVector(indVect, direction, vIx, indices);
    }
  }

 private:
  //: Implementation class - Do Not Use.
  // Author - Ian Scott
  template <class T, class I>
  struct sort_index_compare_functor
  {
    const T *data;
    bool operator () (const I &a, const I &b)
    {
      return data[a] < data[b];
    }
  };

  //: sort the indices of a vector
  // Author - Ian Scott
  void sortIndices(const SortVectorType& v, SortVectorIndexType& s)
  {
    sort_index_compare_functor<TValue, TIndex> c;
    c.data = v.data_block();
    s.set_size(v.size());

    for (TIndex ix = 0; ix < (TIndex) v.size(); ix++) s[ix] = ix;

    std::sort(s.begin(), s.end(), c);
  }

  //: reorder values from sorted indices
  void reindexValues(
    const SortVectorType& values,
    const SortVectorIndexType& indices,
    SortVectorType& sorted_values)
  {
    sorted_values.set_size(values.size());
    for (TIndex ix = 0; ix < (TIndex) values.size(); ix++)
      sorted_values[ix] = values[indices[ix]];
  }

  //: get specified vector from matrix depending on direction
  template<class T>
  void getVector(
    const vnl_matrix<T>& fromMat,
    DirectionType direction,
    int whichVect,
    vnl_vector<T>& toVect)
  {
    switch (direction)
    {
      case ByRow:
        toVect = fromMat.get_row(whichVect);
        break;
      case ByColumn:
        toVect = fromMat.get_column(whichVect);
        break;
      default:
        toVect.clear();
        break;
    }
  }

  //: put specified vector to matrix depending on direction
  template<class T>
  void putVector(
    const vnl_vector<T>& fromVect,
    DirectionType direction,
    int whichVect,
    vnl_matrix<T>& toMat)
  {
    switch (direction)
    {
      case ByRow:
        toMat.set_row(whichVect, fromVect);
        break;
      case ByColumn:
        toMat.set_column(whichVect, fromVect);
        break;
      default:
        break;
    }
  }
};

#endif
