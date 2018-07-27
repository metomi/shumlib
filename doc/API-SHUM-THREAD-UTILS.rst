API Reference: shum_thread_utils
---------------------------------

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_thread_utils_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_thread_utils_version_mod``

    **Syntax**
        ``version = get_shum_thread_utils_version()``

    **Returns**
        ``version (INTEGER(KIND=C_INT64_T))``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

C Functions
%%%%%%%%%%%

``get_shum_thread_utils_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a function named in this format; it allows access
to the Shumlib version number used when compiling the library.

    **Required header/s**
        ``c_shum_thread_utils_version.h``

    **Syntax**
        ``version = get_shum_thread_utils_version()``

    **Returns**
        ``version (int)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_newLock``
''''''''''''''''''''''''''''

This creates a new lock and returns its lock id number.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``lock = f_shum_newLock()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            Lock id number of the newly created lock.

``f_shum_releaseLock``
''''''''''''''''''''''''''''

This will destroy the lock "l" if:

  - it exists,
  - there are no pending lockings
  - and it is currently unlocked

If these conditions are met it returns successCode, else it returns failCode

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_releaseLock(l)``

    **Arguments**
        ``l (int64_t)``
            Lock id number of the lock to destroy.

    **Return Value**
        ``(int64_t)``
            Code for the success of the operation. Is equal to successCode on
            successful destruction of the lock, else it is equal to failCode.

``f_shum_Lock``
''''''''''''''''''''''''''''

This will lock the lock "l" if:

 - it exists,
 - and is not already locked by the calling thread

If these conditions are met it returns successCode, else it returns failCode

If the lock is currently locked by another thread, execution will be blocked
until that thread relinquishes it, at which point the calling thread will
re-lock it and continue execution.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_Lock(l)``

    **Arguments**
        ``l (int64_t)``
            Lock id number of the lock to acquire.

    **Return Value**
        ``(int64_t)``
            Code for the success of the operation. Is equal to successCode on
            successful acquisition of the lock, else it is equal to failCode.

``f_shum_TestLock``
''''''''''''''''''''''''''''

This will attempt to lock the lock "l" if:

 - it exists,
 - and is not already locked by the current thread

If these conditions are met and:

 - the lock was previously unlocked - successCode is returned
 - the lock is already locked - alreadyLockedCode is returned

If these conditions are not met it returns failCode

Unlike Lock(), TestLock(l) will not block execution if the lock is currently
locked by another thread. Instead alreadyLockedCode is returned, and
this thread abandons trying to obtain ownership of the lock.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_TestLock(l)``

    **Arguments**
        ``l (int64_t)``
            Lock id number of the lock to acquire.

    **Return Value**
        ``(int64_t)``
            Code for the success of the operation. Is equal to successCode on
            successful acquisition of the lock. Is equal to alreadyLockedCode
            if the lock is already locked. Else it is equal to failCode.

``f_shum_Unlock``
''''''''''''''''''''''''''''

This will unlock the lock "l" if:

 - it exists,
 - and is locked by the current thread

If these conditions are met it returns successCode, else it returns failCode

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_Unlock(l)``

    **Arguments**
        ``l (int64_t)``
            Lock id number of the lock to release.

    **Return Value**
        ``(int64_t)``
            Code for the success of the operation. Is equal to successCode on
            successful release of the lock, else it is equal to failCode.

``f_shum_threadFlush``
''''''''''''''''''''''''''''

This flushes the OpenMP environment and always returns successCode

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_threadFlush()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            Is always equal to successCode

``f_shum_threadID``
''''''''''''''''''''''''''''

This returns the OpenMP thread number

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``tid = f_shum_threadID()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            the OpenMP thread number, as defined by the OpenMP
            specification.

``f_shum_inPar``
''''''''''''''''''''''''''''

This returns ``1`` when called within an OpenMP parallel region,
and ``0`` otherwise

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``result = f_shum_inPar()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            ``1`` when called within an OpenMP parallel region,
            and ``0`` otherwise

``f_shum_numThreads``
''''''''''''''''''''''''''''

This returns the current number of OpenMP threads in the parallel team.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``threads = f_shum_numThreads()``

    **Arguments**
        None

    **Return Value**
        ``(int64_t)``
            the current number of OpenMP threads

``f_shum_startOMPparallel``
'''''''''''''''''''''''''''

Starts an OpenMP parallel region, and executes ``par_ftn_ptr()`` within it.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``f_shum_startOMPparallel(struct_ptr,par_ftn_ptr)``

    **Arguments**
        ``struct_ptr (void **)``
            A pointer to a shared memory pointer. Typically this points to a
            struct pointer, which is used to access all the SHARED variables
            needed within the parallel region.

        ``par_ftn_ptr (void (*)(void **const))``
            A function pointer to the code to execute in the parallel region.
            It will be executed with ``struct_ptr`` passed as an argument.

    **Return Value**
        None

``f_shum_startOMPparallelfor``
''''''''''''''''''''''''''''''

Starts an OpenMP parallel region, and executes ``par_ftn_ptr()`` within it.
Similar to ``f_shum_startOMPparallel()``, except it also includes an iteration
range (``istart``; ``iend``) and increment (``incr``) as arguments. This
iteration range is divided as equally as possible between the threads, which
each receive a contiguous sub-range.

    **Required header/s**
        ``c_shum_thread_utils.h``

    **Syntax**
        ``f_shum_startOMPparallelfor(struct_ptr,par_ftn_ptr,istart,iend,incr)``

    **Arguments**
        ``struct_ptr (void **)``
            A pointer to a shared memory pointer. Typically this points to a
            struct pointer, which is used to access all the SHARED variables
            needed within the parallel region.

        ``par_ftn_ptr (<par_ftn_ptr_spec>)``
            A function pointer to the code to execute in the parallel region. 
            See additional notes below for more details on the 
            ``<par_ftn_ptr_spec>`` specification.

        ``istart (const int64_t *)``
            A pointer to the start value of the iteration range.

        ``iend (const int64_t *)``
            A pointer to the end value of the iteration range.

        ``incr (const int64_t *)``
            A pointer to the increment value for each iteration.

    **Return Value**
        None

    **Additional Notes**
        ``<par_ftn_ptr_spec>``
            The specification for ``par_ftn_ptr`` is further detailed here. It
            is a pointer to a function taking four arguments, derived from those
            passed to ``f_shum_startOMPparallelfor``

                ``par_ftn_ptr (void (*)())``
                    The function pointed to returns ``void`` - it has no return
                    value.

                ``struct_ptr (void **)``
                    The first argument is ``struct_ptr``, the pointer passed on 
                    from the parent (``f_shum_startOMPparallelfor``).

                ``istart (const int64_t *const restrict)``
                    The second argument is a pointer to a value derived from the
                    ``istart`` value passed to the parent. Rather than directly 
                    pass the value, it is modified such that the iteration
                    range passed to the parent is divided into a different
                    sub-range for each thread.

                ``iend (const int64_t *const restrict)``
                    The third argument is a pointer to a value derived from the
                    ``iend`` value passed to the parent. Rather than directly 
                    pass the value, it is modified such that the iteration
                    range passed to the parent is divided into a different
                    sub-range for each thread.

                ``incr (const int64_t *const restrict)``
                    The first argument is ``incr``, a pointer to a value passed
                    on from the parent.
