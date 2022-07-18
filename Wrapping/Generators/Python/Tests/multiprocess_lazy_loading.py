#!/usr/bin/env python
# LazyLoading must be threadsafe
#
# The loading of modules in python *must* occur as a
# single atomic transaction in multiprocessing environments (i.e.
# the module should only be loaded by one thread).
#
# The LazyLoading of ITK did not treat the loading of
# modules as an atomic transaction, and multiple threads
# would attempt to load the cascading dependencies out of
# order.
#
# The `getattr` override that allows LazyLoading to work
# in the case where the module is *not* loaded now blocks
# while the first thread completes the delayed module loading.
# After the first thread completes module load as an atomic
# transaction, the other threads fall through (skip loading)
# and return the value requested.
#
# Need to use a recursive lock for thread ownership so that the
# first thread can can acquire a RLock as often as needed while
# recursively processing dependent modules lazy loads.  Other threads need
# to wait until this first thread releases the RLock.


# NOTE: This test requires itkConfig.LazyLoading=True
#       Explicitly set to override potential environmental
#       variable settings.
import itkConfig

itkConfig.LazyLoading = True

from multiprocessing.pool import ThreadPool
from multiprocessing import cpu_count

from typing import List

import sys


test_image_fn: List[str] = sys.argv[1:]
# print(f"Reading: {test_image_fn}")


def test_itk_multi_load(num_workers: int):
    num_images_to_be_read: int = max(100, 4 * num_workers)
    all_filenames = [test_image_fn] * num_images_to_be_read

    def local_image_load(idx: int):
        # Purposely import inside of thread pool call
        # to ensure that all the lazy loading of modules
        # can be configured consistently when all threads
        # attempt to load itk at the same time
        import itk

        return itk.imread(all_filenames[idx])

    with ThreadPool(num_workers) as p:
        return list(p.map(local_image_load, range(len(all_filenames))))


simultaneous_loads: int = max(cpu_count(), 4)  # use at least 4 threads for testing
test_itk_multi_load(simultaneous_loads)

import sys

sys.exit(0)
