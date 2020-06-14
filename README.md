# Ice nucleation parameterization adjoint

These files have been built with **[TAPENADE version 3.5](http://www-tapenade.inria.fr:8080/tapenade/)** and require adBuffer.c and adStack.o to compile. 

*ice_aer_cloud_adjoint.f90* - Basic offline adjoint code of the Barhona and Nenes ice nucleation parameterization.

*ice_driver.f90* - Driver for the above

*model_b.f90* - Adjoint of the Barahona and Nenes ice nucleation parameterization configured for the GEOS-5 with McRAS-AC.

*aie_d_b.f90* - Adjoint of the droplet activation parameterization within the larger aerosol-cloud interaction scheme of the GEOS-5 with McRAS-AC.
