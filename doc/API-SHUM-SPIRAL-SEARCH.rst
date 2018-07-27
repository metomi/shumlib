API Reference: shum_spiral_search
---------------------------------

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_spiral_search_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_spiral_search_version_mod``

    **Syntax**
        ``version = get_shum_spiral_search_version()``

    **Returns**
        ``version (INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).


``f_shum_spiral_search_algorithm``
''''''''''''''''''''''''''''''''''

This function calculates values for unresolved points.
using a spiral search method, which finds the closest point by distance (in m)
Method: Uses the Haversine formula to calculate the distances.
Searches in steps of 3*minimum local distance for each unresolved point until 
it finds a resolved point. For land points in a field that is not "land only" 
a 200km constraint is applied - if there is no resolved land point within
this distance, it uses the closest resolved sea point value instead.
For Global (cyclic) domains, if hit an edge it calculates the distances of 
every point in the domain as it can't cope with looping over the edges.  
This will cause the scheme to take much longer to run, however this is unlikely
to happen often.

It is used in the Unified Model's reconfiguration for consistency of some
ancillary file fields with the land-sea mask.


    **Available via module**
        ``f_shum_spiral_search_mod``

    **Syntax**
        ``status = f_shum_spiral_search_algorithm(lsm, index_unres, no_point_unres, points_phi, points_lambda, lats, lons, is_land_field, constrained, constrained_max_dist, dist_step, cyclic_domain, unres_mask, indices, planet_radius, cmessage)``

    **Inputs**
        ``lsm (LOGICAL, KIND=C_BOOL, array, size=points_phi*points_lambda)``
            Land-sea mask (1D array of logicals).
        ``index_unres (INTEGER, 64- or 32-bit, size=no_point_unres)``
            Indices of unresolved points.
        ``no_point_unres (INTEGER, 64- or 32-bit scalar)``
            Number of unresolved points in index_unres.
        ``points_phi, points_lambda (INTEGER, 64- or 32-bit scalars)``
            Number of latitudes & longitudes defining the grid.
        ``lats, lons (REAL, 64- or 32-bit arrays, size=points_...)``
            Latitude & longitude values defining the grid.
        ``is_land_field (LOGICAL, KIND=C_BOOL, scalar)``
            True if considering a land field.
        ``constrained (LOGICAL, KIND=C_BOOL, scalar)``
            True if constraining spiral search.
        ``constrained_max_dist (REAL, 64- or 32-bit scalar)``
            If constrained, the maximum distance (in m) to constrain by.
        ``dist_step (REAL, 64- or 32-bit scalar)``
            Adjusts the distance step size of the iterations done whilst searching.
        ``cyclic_domain (LOGICAL, KIND=C_BOOL, scalar)``
            True if grid has cyclic (wraparound) edges.
        ``unres_mask (LOGICAL, KIND=C_BOOL, size=points_phi*points_lambda)``
            Mask of grid locations of points to be resolved by spiral search.
    
    **Outputs**
        ``indices (INTEGER, 64- or 32-bit, size=no_point_unres)``
            Grid locations returned as result of spiral search.

    **Input & Output**
        ``cmessage (CHARACTER(LEN=*))``
            Error message buffer.

    **Return Value**
        ``status (INTEGER)``
            Exit status; ``0`` means success, anything else means an error has
            occurred, in which case the ``message`` argument will contain 
            information about the problem.

    **Notes**
        The arguments here must be either *all* 32-bit or *all* 64-bit 
        (but *not* a mixture of the two) and logicals must be kind=C_BOOL.

C Functions
%%%%%%%%%%%

``get_shum_spiral_search_version``
''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a function named in this format; it allows access
to the Shumlib version number used when compiling the library.

    **Required header/s**
        ``c_shum_spiral_search_version.h``

    **Syntax**
        ``version = get_shum_spiral_search_version()``

    **Returns**
        ``version (int)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``c_shum_spiral_search_algorithm``
''''''''''''''''''''''''''''''''''

This is the C interface to the Fortran routine (see the description of it 
above under ``f_shum_spiral_search_algorithm``.

    **Required header/s**
        ``c_shum_spiral_search.h``

    **Syntax**
        ``c_shum_spiral_search_algorithm(lsm, index_unres, no_point_unres, no_point_unres, points_phi, points_lambda, lats, lons, is_land_field, constrained, constrained_max_dist, dist_step, cyclic_domain, unres_mask, indices, planet_radius, cmessage, message_len)``
   
    **Arguments**
        ``lsm (bool*)``
            Land-sea mask (1D array of length points_phi*points_lambda).
        ``index_unres (int64_t*)``
            Indices of unresolved points (1D array of length no_point_unres).
        ``no_point_unres (int64_t*)``
            Number of indices in index_unres.
        ``points_phi, points_lambda (int64_t*)``
            Number of latitudes & longitudes defining the grid.
        ``lats, lons (double*)``
            Latitude & longitude values defining the grid (arrays of lengths given
            by their respective points values).
        ``is_land_field (bool*)``
            True if considering a land field.
        ``constrained (bool*)``
            True if constraining spiral search.
        ``constrained_max_dist (double*)``
            If constrained, gives the maximum distance (in m) to constraint by.
        ``dist_step(double*)``
            Adjusts the distance step size of the iterations done whilst searching.            
        ``cyclic_domain (bool*)``
            True if grid has cyclic (wraparound) edges.
        ``unres_mask (bool*)``
            Mask of grid locations (1D array of length points_phi*points_lambda) 
            of points to be resolved by spiral search.
        ``message (char*)``
            Error message buffer.
        ``message_len (int64_t*)``
            Length of error message buffer.     

    **Return Value**
        ``(int64_t)``
            Exit status; ``0`` means success, anything else means an error has
            occurred and in that case the ``message`` argument will contain 
            information about the problem.

Unified Model Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This shumlib function replaces the spiral circle search subroutine 
``SPIRAL_CIRCLE_SEARCH`` called in subroutine ``rcf_spiral_circle_s``. 
In the Unified Model the shumlib function is invoked through a module 
E.g. 
``USE hum_spiral_search_mod, ONLY: f_shum_spiral_search_algorithm``
while the logicals require
``USE , INTRINSIC :: ISO_C_BINDING, ONLY: C_BOOL``.

