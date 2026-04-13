"""
Test that the Python Global Interpreter Lock (GIL) is released during ITK operations.

This test verifies that when ITK_PYTHON_RELEASE_GIL is enabled, multiple Python threads
can execute ITK operations concurrently.
"""

import sys
import threading
import time

# Threshold for determining if parallel execution is significantly faster than sequential
# With 4 outer threads and 1 ITK thread per filter, we expect at least 2x speedup
# A value of 0.5 means parallel execution should be at most 50% of sequential time
# This accounts for threading overhead and ensures GIL is being released
PARALLEL_SPEEDUP_THRESHOLD = 0.5


def test_gil_release():
    """Test that GIL is released during ITK operations."""
    try:
        import itk
    except ImportError:
        print("ITK not available, skipping GIL release test")
        sys.exit(0)

    # Create a simple test image
    image_type = itk.Image[itk.F, 2]
    size = [100, 100]

    # Shared counter to track concurrent execution
    execution_times = []
    lock = threading.Lock()

    def run_filter():
        """Run an ITK filter operation that should release the GIL."""
        # Create an image
        image = itk.Image[itk.F, 2].New()
        region = itk.ImageRegion[2]()
        region.SetSize(size)
        image.SetRegions(region)
        image.Allocate()
        image.FillBuffer(1.0)

        start_time = time.time()

        # Run a computationally intensive filter
        # MedianImageFilter is a good test as it performs actual computation
        median_filter = itk.MedianImageFilter[image_type, image_type].New()
        median_filter.SetInput(image)
        median_filter.SetRadius(5)
        # Limit ITK internal threads to 1 to make the test more reliable
        median_filter.SetNumberOfWorkUnits(1)
        median_filter.Update()

        end_time = time.time()

        with lock:
            execution_times.append((start_time, end_time))

    # Run multiple threads
    num_threads = 4
    threads = []

    overall_start = time.time()

    for _ in range(num_threads):
        thread = threading.Thread(target=run_filter)
        thread.start()
        threads.append(thread)

    for thread in threads:
        thread.join()

    overall_end = time.time()

    # If GIL is properly released, the threads should have overlapping execution times
    # and the total time should be less than the sum of individual execution times

    total_sequential_time = sum(end - start for start, end in execution_times)
    total_parallel_time = overall_end - overall_start

    print(f"Total sequential time if run serially: {total_sequential_time:.3f}s")
    print(f"Total parallel time: {total_parallel_time:.3f}s")

    # Check for overlap in execution times
    has_overlap = False
    if len(execution_times) >= 2:
        for i in range(len(execution_times)):
            for j in range(i + 1, len(execution_times)):
                start1, end1 = execution_times[i]
                start2, end2 = execution_times[j]
                # Check if there's any overlap
                if (start1 <= start2 < end1) or (start2 <= start1 < end2):
                    has_overlap = True
                    break
            if has_overlap:
                break

    if has_overlap:
        print("SUCCESS: Thread execution times overlap - GIL appears to be released")
        return 0
    else:
        # Even without overlap, if parallel time is significantly less than sequential,
        # it suggests concurrent execution
        if total_parallel_time < total_sequential_time * PARALLEL_SPEEDUP_THRESHOLD:
            print("SUCCESS: Parallel execution is faster - GIL appears to be released")
            return 0
        else:
            print("FAILURE: No clear evidence of concurrent execution")
            print("This indicates that GIL is not being released properly")
            print(
                f"Expected parallel time < {total_sequential_time * PARALLEL_SPEEDUP_THRESHOLD:.3f}s, got {total_parallel_time:.3f}s"
            )
            return 1


if __name__ == "__main__":
    sys.exit(test_gil_release())
