//:
// \file
// Tests for vnl_index_sort.h, written by Michael R. Bowers, September 2011.

#ifdef STANDALONE // if running as a stand-alone (test) program,
                  // options -n and -m (matrix sizes) are available
#include <algorithm>
#include <iostream>
#include <getopt.h>
#else             // else, this program follows the testlib style mechanism
#include <testlib/testlib_test.h>
#endif

#include <vnl/vnl_index_sort.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_random.h>
#include <vcl_compiler.h>

// The following defaults can be overridden with options -n and -m of the standalone version
const int cNumElRows = 10;
const int cNumElCols = 6;
// And these are the effectivly used values:
static int numelrows;
static int numelcols;

#ifdef STANDALONE
static void showUsage(const char* programName)
{
  std::cerr << "USAGE: " << programName << " [OPTIONS]\n"
           << "\t\t-h --help : print this message\n"
           << "\t\t-n --numelrows <numelrows>: number of elements to sort (def = " << cNumElRows << ").\n"
           << "\t\t-m --numelcols <numelcols>: number of elements to sort (def = " << cNumElCols << ").\n";
}

static struct option long_options[] =
{
    {"help",             no_argument,       NULL, 'h'},
    {"numelrows",        required_argument, NULL, 'n'},
    {"numelcols",        required_argument, NULL, 'm'},
    {0,                  0,    0,   0}
};

int test_vnl_index_sort();

int main( int argc, char *argv[] )
{
  numelrows = cNumElRows;
  numelcols = cNumElCols;

  int c = 0;
  while (c >= 0)
  {
      int option_index = 0;

      switch ((c = getopt_long(argc, argv, "hn:m:",
                               long_options, &option_index)))
      {
          case 'h':
              showUsage(argv[0]);
              return 0;
          case 'n':
              numelrows = atoi(optarg);
              break;
          case 'm':
              numelcols = atoi(optarg);
              break;
          default:
              if (c > 0) { showUsage(argv[0]); return 0; }
      }
  }

  int r = test_vnl_index_sort();
  if (r)
    std::cerr << "******* " << r << " SORT TESTS FAILED******\n";
  return r;
}

#define TEST(t,a,b) \
  if (a != b) std::cerr << t << " FAILED.\n"; \
  else        std::cout << t << " check PASSED." << std::endl;

#define TESTMAIN(x) // no-op

#endif // STANDALONE

int test_vnl_index_sort()
{
  typedef double             MeasurementValueType;
  typedef vnl_vector<MeasurementValueType> MeasurementVectorType;
  typedef vnl_matrix<MeasurementValueType> MeasurementMatrixType;

  typedef int                RankValType;
  typedef vnl_vector<RankValType> IndexVectorType;
  typedef vnl_matrix<RankValType> IndexMatrixType;

  typedef vnl_index_sort<MeasurementValueType, RankValType> IndexSortType;

  int caughtError = 0;

  MeasurementVectorType randomVals(numelrows);

  vnl_random genRand(9667566);
  for (int cx = 0; cx < numelrows; ++cx)
    randomVals(cx) = genRand.lrand32(numelrows * 2);

  IndexSortType indexSort;

  MeasurementVectorType sortedVals;
  IndexVectorType sortIndices;
  indexSort.vector_sort(randomVals, sortedVals, sortIndices);

  MeasurementVectorType vclSortedVals(randomVals);
  std::sort(vclSortedVals.begin(), vclSortedVals.end());

  // check against std::sort
  TEST("Random Vector Sort", vclSortedVals, sortedVals);
  if (vclSortedVals!=sortedVals) ++caughtError;

  // check indices
  bool sortCheckFail = false;
  for (unsigned int ix = 0; ix < randomVals.size(); ++ix)
    if (vclSortedVals(ix) != randomVals(sortIndices(ix))) sortCheckFail = true;

  TEST("Random Vector Index Sort", sortCheckFail, false);
  if (sortCheckFail) ++caughtError;

  // check in-place sort
  sortedVals = randomVals;
  indexSort.vector_sort_in_place(sortedVals, sortIndices);

  TEST("In Place Vector Sort", vclSortedVals, sortedVals);
  if (sortCheckFail) ++caughtError;

  // check indices
  sortCheckFail = false;
  for (unsigned int ix = 0; ix < randomVals.size(); ++ix)
    if (vclSortedVals(ix) != randomVals(sortIndices(ix)))
      sortCheckFail = true;

  TEST("In Place Vector Index Sort", sortCheckFail, false);
  if (sortCheckFail) ++caughtError;

  // check matrix sorting now...
  MeasurementMatrixType randomValM(numelrows, numelcols);
  for (unsigned int rx = 0; rx < randomValM.rows(); ++rx)
    for (unsigned int cx = 0; cx < randomValM.cols(); ++cx)
      randomValM(rx, cx) = genRand.lrand32(numelrows * 2);

  MeasurementMatrixType sortedValM;
  IndexMatrixType sortedIndicesM;

  // do the matrix sort row-wise
  indexSort.matrix_sort(
    IndexSortType::ByRow, randomValM, sortedValM, sortedIndicesM);

  bool sortIndexCheckFail;
  sortCheckFail = sortIndexCheckFail = false;

  for (unsigned int rx = 0; rx < randomValM.rows() && !sortIndexCheckFail && !sortCheckFail; ++rx)
  {
    MeasurementVectorType unsortedVectCheck = randomValM.get_row(rx);
    MeasurementVectorType vclSortedVectCheck(unsortedVectCheck);
    std::sort(vclSortedVectCheck.begin(), vclSortedVectCheck.end());
    MeasurementVectorType sortedVectCheck = sortedValM.get_row(rx);
    IndexVectorType indexVectCheck = sortedIndicesM.get_row(rx);

    // check against std::sort
    sortCheckFail = (vclSortedVectCheck != sortedVectCheck);

    // check indices
    for (unsigned int ix = 0; ix < sortedVectCheck.size(); ++ix)
      if (sortedVectCheck(ix) != unsortedVectCheck(indexVectCheck(ix)))
        sortIndexCheckFail = true;
  }
  TEST("Row-Wise Matrix Sort", sortCheckFail, false);
  if (sortCheckFail) ++caughtError;
  TEST("Row-Wise Matrix Index Sort", sortIndexCheckFail, false);
  if (sortIndexCheckFail) ++caughtError;

  // do the matrix sort column-wise
  sortCheckFail = sortIndexCheckFail = false;
  indexSort.matrix_sort(
    IndexSortType::ByColumn, randomValM, sortedValM, sortedIndicesM);

  for (unsigned int rx = 0; rx < randomValM.cols() && !sortIndexCheckFail && !sortCheckFail; ++rx)
  {
    MeasurementVectorType unsortedVectCheck = randomValM.get_column(rx);
    MeasurementVectorType vclSortedVectCheck(unsortedVectCheck);
    std::sort(vclSortedVectCheck.begin(), vclSortedVectCheck.end());
    MeasurementVectorType sortedVectCheck = sortedValM.get_column(rx);
    IndexVectorType indexVectCheck = sortedIndicesM.get_column(rx);

    // check against std::sort
    sortCheckFail = (vclSortedVectCheck != sortedVectCheck);

    // check indices
    for (unsigned int ix = 0; ix < sortedVectCheck.size(); ++ix)
      if (sortedVectCheck(ix) != unsortedVectCheck(indexVectCheck(ix)))
        sortIndexCheckFail = true;
  }
  TEST("Column-Wise Matrix Sort", sortCheckFail, false);
  if (sortCheckFail) ++caughtError;
  TEST("Column-Wise Matrix Index Sort", sortIndexCheckFail, false);
  if (sortIndexCheckFail) ++caughtError;

  // check In-place matrix sorting now...
  for (unsigned int rx = 0; rx < randomValM.rows(); ++rx)
    for (unsigned int cx = 0; cx < randomValM.cols(); ++cx)
      randomValM(rx, cx) = genRand.lrand32(numelrows * 2);

  // initialize to random for in-place sort check...
  sortedValM = randomValM;

  // do the matrix sort row-wise
  indexSort.matrix_sort(
    IndexSortType::ByRow, sortedValM, sortedValM, sortedIndicesM);

  sortCheckFail = sortIndexCheckFail = false;
  for (unsigned int rx = 0; rx < randomValM.rows() && !sortIndexCheckFail && !sortCheckFail; ++rx)
  {
    MeasurementVectorType unsortedVectCheck = randomValM.get_row(rx);
    MeasurementVectorType vclSortedVectCheck(unsortedVectCheck);
    std::sort(vclSortedVectCheck.begin(), vclSortedVectCheck.end());
    MeasurementVectorType sortedVectCheck = sortedValM.get_row(rx);
    IndexVectorType indexVectCheck = sortedIndicesM.get_row(rx);

    // check against std::sort
    sortCheckFail = (vclSortedVectCheck != sortedVectCheck);

    // check indices
    for (unsigned int ix = 0; ix < sortedVectCheck.size(); ++ix)
      if (sortedVectCheck(ix) != unsortedVectCheck(indexVectCheck(ix)))
        sortIndexCheckFail = true;
  }
  TEST("In-place Row-Wise Matrix Sort", sortCheckFail, false);
  if (sortCheckFail) ++caughtError;
  TEST("In-place Row-Wise Matrix Index Sort", sortIndexCheckFail, false);
  if (sortIndexCheckFail) ++caughtError;

  sortedValM = randomValM;
  // do the in-place matrix sort column-wise
  indexSort.matrix_sort(
    IndexSortType::ByColumn, sortedValM, sortedValM, sortedIndicesM);

  sortCheckFail = sortIndexCheckFail = false;
  for (unsigned int rx = 0; rx < randomValM.cols() && !sortIndexCheckFail && !sortCheckFail; ++rx)
  {
    MeasurementVectorType unsortedVectCheck = randomValM.get_column(rx);
    MeasurementVectorType vclSortedVectCheck(unsortedVectCheck);
    std::sort(vclSortedVectCheck.begin(), vclSortedVectCheck.end());
    MeasurementVectorType sortedVectCheck = sortedValM.get_column(rx);
    IndexVectorType indexVectCheck = sortedIndicesM.get_column(rx);

    // check against std::sort
    sortCheckFail = (vclSortedVectCheck != sortedVectCheck);

    // check indices
    for (unsigned int ix = 0; ix < sortedVectCheck.size(); ++ix)
      if (sortedVectCheck(ix) != unsortedVectCheck(indexVectCheck(ix)))
        sortIndexCheckFail = true;
  }
  TEST("In-place Column-Wise Matrix Sort", sortCheckFail, false);
  if (sortCheckFail) ++caughtError;
  TEST("In-place Column-Wise Matrix Index Sort", sortIndexCheckFail, false);
  if (sortIndexCheckFail) ++caughtError;

  return caughtError;
}

TESTMAIN(test_vnl_index_sort);
