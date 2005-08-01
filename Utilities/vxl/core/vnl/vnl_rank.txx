// This is core/vnl/vnl_rank.txx
#ifndef vnl_rank_txx_
#define vnl_rank_txx_

#include "vnl_rank.h"

template <class T>
vnl_matrix<T> vnl_rank_row_reduce(vnl_matrix<T> const& mat, vnl_rank_pivot_type t)
{
  vnl_matrix<T> a = mat;
  bool changed = true;
  unsigned int m = a.rows(), n=a.columns();
  while (changed)
  {
    changed = false;
    for (unsigned int r=0; r<m; ++r)
    {
      unsigned int c=0; while (c<n && a[r][c] != 1 && a[r][c] != -1) ++c;
      if (c==n) continue;
      for (unsigned int s=0; s<m; ++s)
      {
        if (s==r || a[s][c] == 0) continue;
        for (unsigned int d=0; d<n; ++d)
          if (d!=c) a[s][d] -= a[r][d] * a[r][c] * a[s][c];
        a[s][c] = T(0);
        changed = true;
      }
    }
  }
  if (t == vnl_rank_pivot_one) return a;
  changed = true;
  while (changed)
  {
    changed = false;
    for (unsigned int r=0; r<m; ++r)
    {
      unsigned int c=0; while (c<n && a[r][c] == 0) ++c;
      if (c==n) continue; // zero row
      for (unsigned int s=0; s<m; ++s)
      {
        if (s==r) continue;
        T scale = a[s][c] / a[r][c];
        // Note that this can possibly be an integer division, so
        // it is *not* guaranteed that a[r][c] * scale == a[s][c] .
        if (scale == 0) continue;
        for (unsigned int d=0; d<n; ++d)
          if (d!=c) a[s][d] -= a[r][d] * scale;
        a[s][c] -= a[r][c] * scale;
        changed = true;
      }
    }
  }
  return a;
}

template <class T>
vnl_matrix<T> vnl_rank_column_reduce(vnl_matrix<T> const& mat, vnl_rank_pivot_type t)
{
  vnl_matrix<T> a = mat;
  bool changed = true;
  unsigned int m = a.rows(), n=a.columns();
  while (changed)
  {
    changed = false;
    for (unsigned int c=0; c<n; ++c)
    {
      unsigned int r=0; while (r<m && a[r][c] != 1 && a[r][c] != -1) ++r;
      if (r==m) continue;
      for (unsigned int d=0; d<n; ++d)
      {
        if (d==c || a[r][d] == 0) continue;
        for (unsigned int s=0; s<m; ++s)
          if (s!=r) a[s][d] -= a[s][c] * a[r][c] * a[r][d];
        a[r][d] = T(0);
        changed = true;
      }
    }
  }
  if (t == vnl_rank_pivot_one) return a;
  changed = true;
  while (changed)
  {
    changed = false;
    for (unsigned int c=0; c<n; ++c)
    {
      unsigned int r=0; while (r<m && a[r][c] == 0) ++r;
      if (r==m) continue; // zero row
      for (unsigned int d=0; d<n; ++d)
      {
        if (d==c) continue;
        T scale = a[r][d] / a[r][c];
        // Note that this can possibly be an integer division, so
        // it is *not* guaranteed that a[r][c] * scale == a[r][d] .
        if (scale == 0) continue;
        for (unsigned int s=0; s<m; ++s)
          if (s!=r) a[s][d] -= a[s][c] * scale;
        a[r][d] -= a[r][c] * scale;
        changed = true;
      }
    }
  }
  return a;
}

template <class T>
vnl_matrix<T> vnl_rank_row_column_reduce(vnl_matrix<T> const& mat, vnl_rank_pivot_type t)
{
  vnl_matrix<T> a = mat;
  bool changed = true;
  unsigned int m = a.rows(), n=a.columns();
  while (changed)
  {
    changed = false;
    for (unsigned int r=0; r<m; ++r)
    {
      unsigned int c=0; while (c<n && a[r][c] != 1 && a[r][c] != -1) ++c;
      if (c==n) continue;
      for (unsigned int s=0; s<m; ++s)
      {
        if (s==r || a[s][c] == 0) continue;
        for (unsigned int d=0; d<n; ++d)
          if (d!=c) a[s][d] -= a[r][d] * a[r][c] * a[s][c];
        a[s][c] = T(0);
        changed = true;
      }
    }
    for (unsigned int c=0; c<n; ++c)
    {
      unsigned int r=0; while (r<m && a[r][c] != 1 && a[r][c] != -1) ++r;
      if (r==m) continue;
      for (unsigned int d=0; d<n; ++d)
      {
        if (d==c || a[r][d] == 0) continue;
        for (unsigned int s=0; s<m; ++s)
          if (s!=r) a[s][d] -= a[s][c] * a[r][c] * a[r][d];
        a[r][d] = T(0);
        changed = true;
      }
    }
  }
  if (t == vnl_rank_pivot_one) return a;
  changed = true;
  while (changed)
  {
    changed = false;
    for (unsigned int r=0; r<m; ++r)
    {
      unsigned int c=0; while (c<n && a[r][c] == 0) ++c;
      if (c==n) continue; // zero row
      for (unsigned int s=0; s<m; ++s)
      {
        if (s==r) continue;
        T scale = a[s][c] / a[r][c];
        // Note that this can possibly be an integer division, so
        // it is *not* guaranteed that a[r][c] * scale == a[s][c] .
        if (scale == 0) continue;
        for (unsigned int d=0; d<n; ++d)
          if (d!=c) a[s][d] -= a[r][d] * scale;
        a[s][c] -= a[r][c] * scale;
        changed = true;
      }
    }
    for (unsigned int c=0; c<n; ++c)
    {
      unsigned int r=0; while (r<m && a[r][c] == 0) ++r;
      if (r==m) continue; // zero row
      for (unsigned int d=0; d<n; ++d)
      {
        if (d==c) continue;
        T scale = a[r][d] / a[r][c];
        // Note that this can possibly be an integer division, so
        // it is *not* guaranteed that a[r][c] * scale == a[r][d] .
        if (scale == 0) continue;
        for (unsigned int s=0; s<m; ++s)
          if (s!=r) a[s][d] -= a[s][c] * scale;
        a[r][d] -= a[r][c] * scale;
        changed = true;
      }
    }
  }
  return a;
}

template <class T>
unsigned int vnl_rank(vnl_matrix<T> const& mat, vnl_rank_type t)
{
  unsigned int rank = 0;
  if (t == vnl_rank_row)
  {
    vnl_matrix<T> a = vnl_rank_row_reduce(mat, vnl_rank_pivot_all);
    for (unsigned int r=0; r<a.rows(); ++r)
    {
      unsigned int c=0;
      while (c<a.columns() && a[r][c] == 0) ++c;
      if (c!=a.columns()) ++rank; // not all elements in row r are 0
    }
  }
  else
  {
    vnl_matrix<T> a = (t == vnl_rank_column) ? vnl_rank_column_reduce(mat,vnl_rank_pivot_all) :
                                               vnl_rank_row_column_reduce(mat,vnl_rank_pivot_all);
    for (unsigned int c=0; c<a.columns(); ++c)
    {
      unsigned int r=0;
      while (r<a.rows() && a[r][c] == 0) ++r;
      if (r!=a.rows()) ++rank; // not all elements in column c are 0
    }
  }
  return rank;
}

#undef VNL_RANK_INSTANTIATE
#define VNL_RANK_INSTANTIATE(T) \
template vnl_matrix<T > vnl_rank_row_reduce(vnl_matrix<T > const&, vnl_rank_pivot_type);\
template vnl_matrix<T > vnl_rank_column_reduce(vnl_matrix<T > const&, vnl_rank_pivot_type);\
template vnl_matrix<T > vnl_rank_row_column_reduce(vnl_matrix<T > const&, vnl_rank_pivot_type);\
template unsigned int vnl_rank(vnl_matrix<T > const&, vnl_rank_type)

#endif // vnl_rank_txx_
