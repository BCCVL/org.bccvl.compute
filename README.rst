org.bccvl.compute
=================

Collection and wrappers of R compute scripts


Run Tests
=========

Run Unittests
-------------

.. code:: shell

          python setup.py test -s org.bccvl.compute.tests

Run full R script test suite
----------------------------

.. code:: shell

          export TEST_R_SCRIPTS=1
          python setup.py test

Run individual R test
---------------------

.. code:: shell

          export TEST_R_SCRIPTS=1
          python setup.py test -s test_rscripts.test_R.Test_VORONOIHULL
