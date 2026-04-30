#!/usr/bin/env python
# Lazy module loading must be threadsafe
#
# The loading of modules in python *must* occur as a
# single atomic transaction in multiprocessing environments (i.e.
# the module should only be loaded by one thread).
#
# Historically ITK's lazy loader did not treat the loading of
# modules as an atomic transaction, and multiple threads
# would attempt to load the cascading dependencies out of
# order.
#
# The PEP 562 ``__getattr__`` hook that triggers a delayed
# load now blocks while the first thread completes the load.
# After the first thread completes module load as an atomic
# transaction, the other threads fall through (skip loading)
# and return the value requested.
#
# Need to use a recursive lock for thread ownership so that the
# first thread can can acquire a RLock as often as needed while
# recursively processing dependent module loads.  Other threads need
# to wait until this first thread releases the RLock.


from multiprocessing.pool import ThreadPool
from multiprocessing import cpu_count

import sys


test_image_fn: list[str] = sys.argv[1:]
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
