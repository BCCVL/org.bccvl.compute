"""
  test_r
"""
from __future__ import print_function
# Run a single test:
#  TEST_R_SCRIPTS=1 python setup.py -q test -v -s test_rscripts.test_R.Test_BIOCLIM_Egernia # noqa
# Use nose as test runner?
#  TEST_R_SCRIPTS=1 python setup.py -q test -v -r nose -s test_rscripts.test_R.Test_BIOCLIM_Egernia # noqa
# How do use nose or anything else to run them? (generate XML unittest output)

import csv
from glob import iglob
from itertools import imap, repeat, chain
import json
import logging
import os
import os.path
from pkg_resources import resource_string
import shutil
import subprocess
import tarfile
import tempfile
import unittest
import urllib

import numpy as np
from osgeo import gdal
from skimage.io import imread
try:
    from skimage.measure import compare_ssim as ssim
    HAVE_MULTICHANNEL_SSIM = True
except ImportError:
    from skimage.measure import structural_similarity as ssim
    HAVE_MULTICHANNEL_SSIM = False

logging.basicConfig(level=logging.INFO)
LOG = logging.getLogger(__name__)

OUTPUTS_VERSION = "1.13"
OUTPUTS_BASE = "https://swift.rc.nectar.org.au:8888/v1/AUTH_0bc40c2c2ff94a0b9404e6f960ae5677/test_r_outputs"  # noqa

# A map for known csv files to parse the correct data types
# only str, int and float allowed, as  compare_csv is not handling
# anything else
DTYPE_MAP = {
    'Evaluation data.csv':
        [int, float, float, float, float, float, float, float,
         float, float, float, float, float, float, ],
    'Evaluation statistics.csv':
        [(str, 50), float],
    'Loss function intervals table.csv':
        [(str, 50), float, float, float],
    'variableImportance.RUN1.csv':
        [(str, 50), float],
    'biomod2_like_VariableImportance.csv':
        [(str, 50), float, float, float, float],
    'maxent_like_VariableImportance.csv':
        [(str, 50), float, float, float, float],
}


def get_outputs_url(version, species, algorithm):
    return ('{base}/{version}/{species}-{algorithm}-{version}.tar.bz2'.format(
        base=OUTPUTS_BASE, version=version,
        species=species, algorithm=algorithm))


def ensure_ref_outputs(version, species, algorithm):
    out_base = os.path.join(os.path.dirname(__file__), 'outputs')
    if not os.path.exists(out_base):
        os.makedirs(out_base)
    ref_base = os.path.join(out_base,
                            OUTPUTS_VERSION, species, algorithm)
    if os.path.exists(ref_base):
        # seems to be here.. so we are done
        return ref_base
    # data not there? ... let's fetch it
    ref_url = get_outputs_url(version, species, algorithm)
    LOG.info('Fetch reference outputs')
    temp_file = None
    try:
        temp_file, http_msg = urllib.urlretrieve(ref_url)
        # and extract it
        LOG.info('Extract reference outputs')
        tar_file = tarfile.open(temp_file, 'r:bz2')
        tar_file.extractall(out_base)
    finally:
        if temp_file and os.path.exists(temp_file):
            os.remove(temp_file)
    return ref_base


def compare_geotiff(img1, img2, eps=1e-03):
    """img1 and img2 are the filenames to the tiff files.
    """
    da1 = gdal.Open(img1)
    ba1 = da1.GetRasterBand(1)
    ar1 = np.nan_to_num(ba1.ReadAsArray())  # replace no data with 0
    da2 = gdal.Open(img2)
    ba2 = da2.GetRasterBand(1)
    ar2 = np.nan_to_num(ba2.ReadAsArray())  # replace no data with 0
    # return true if the two arrays are equal (with tolerance)
    return np.allclose(ar1, ar2, rtol=0, atol=eps)


def compare_png(img1, img2, eps=0.99):
    """check whether img1 and img2 are similar

       we use structural similarity (SSIM) to compare them.

       SSIM generates values between 0 and 1, where 1 represents
       identical images

       If SSIM result is greater eps, then this method returns True.

    """
    im1 = imread(img1)
    im2 = imread(img2)
    if len(im1.shape) == 2 or im1.shape[-1] == 1:
        # only one color channel
        mssim = ssim(im1, im2)
    elif HAVE_MULTICHANNEL_SSIM:
        # multi color channel
        mssim = ssim(im1, im2, multichannel=True)
    else:
        # We have to do multichannel ssim ourselves
        nch = im1.shape[-1]
        mssim = np.empty(nch)
        for chan in range(nch):
            # use copy to generate contiguous array and avoid warning
            ch_result = ssim(im1[..., chan].copy(), im2[..., chan].copy())
            mssim[..., chan] = ch_result
        mssim = mssim.mean()
    return mssim > eps


def read_csv(filename, column_headers, column_types):
    # use csv module to parse csv (understands quotes etc...) and use \t as
    # seperator to help numpy parsing the csv
    reader = csv.reader(open(filename))
    if column_headers:
        names = reader.next()
    return np.recfromtxt(("\t".join(row) for row in reader),
                         delimiter="\t", names=names, dtype=column_types)


def compare_csv(csv1, csv2, column_headers=True, eps=1e-3):
    """
    """
    column_types = DTYPE_MAP[os.path.basename(csv1)]
    da1 = read_csv(csv1, column_headers, column_types)
    da2 = read_csv(csv2, column_headers, column_types)
    # compare cloumn names
    ret = da1.dtype.names == da2.dtype.names
    # compare all string columns
    fields = [field for field in da1.dtype.fields
              if da1.dtype[field].kind == 'S']
    if fields:
        ret = ret and np.array_equal(da1[fields], da2[fields])
    # compare all integer fields
    fields = [field for field in da1.dtype.fields
              if da1.dtype[field].kind == 'i']
    if fields:
        ret = ret and np.array_equal(da1[fields], da2[fields])
    # compare all float type columns (with epsilon)
    fields = [field for field in da1.dtype.fields
              if da1.dtype[field].kind == 'f']
    # make copy of float view, so that we can safely replace nan's
    if fields:
        fa1 = np.nan_to_num(da1[fields].view((float, len(fields))))
        fa2 = np.nan_to_num(da2[fields].view((float, len(fields))))
        ret = ret and np.allclose(fa1, fa2, rtol=0, atol=eps)
    return ret


class BaseTestCase(object):
    """
    Class to generate algorithm test cases

    It is there to encapsulate base test case to avoid auto discovery
    """
    # TODO: maybe model base test case as mixin?

    class AlgorithmTestCase(unittest.TestCase):
        """
        Base class for Algorithm test cases
        """

        temp_dir = None
        out_dir = None
        ref_dir = None
        algo_params = {}
        algo_scripts = []
        md5_digests = {}
        species = None
        algo_name = None

        def setUp(self):
            if 'TEST_R_SCRIPTS' not in os.environ:
                self.skipTest(
                    'R Script tests not enabled - TEST_R_SCRIPTS not set')
            # make sure our reference outputs are in place
            self.ref_dir = ensure_ref_outputs(OUTPUTS_VERSION,
                                              self.species,
                                              self.algo_name)
            # Create a temp directory for testing
            temp_dir = None
            try:
                temp_dir = tempfile.mkdtemp(
                    prefix='test_' + self.__class__.__name__ + '_')
                out_dir = os.path.join(temp_dir, 'output')
                os.mkdir(out_dir)
                self.temp_dir = temp_dir
                self.out_dir = out_dir
                # setup test parameters
                self.params = self.setup_parameters()
                self.params['params'].update(self.algo_params)
                jfile = open(os.path.join(temp_dir, 'params.json'), mode='w')
                json.dump(self.params, jfile, indent=2)
                # setup algorithm script
                script = self.build_r_script()
                test_script = os.path.join(temp_dir, 'test.R')
                open(test_script, 'w').write(script)
                # place input data
                data_dir = os.path.join(os.path.dirname(__file__), 'data')
                input_dir = os.path.join(self.temp_dir, 'input')
                os.mkdir(input_dir)
                shutil.copy(os.path.join(data_dir,
                                         '{}_bg.csv'.format(self.species)),
                            input_dir)
                shutil.copy(os.path.join(data_dir,
                                         '{}_occ.csv'.format(self.species)),
                            input_dir)
                shutil.copy(os.path.join(data_dir, 'B01.tif'), input_dir)
                shutil.copy(os.path.join(data_dir, 'B04.tif'), input_dir)
                shutil.copy(os.path.join(data_dir, 'B12.tif'), input_dir)
                shutil.copy(os.path.join(data_dir, 'B15.tif'), input_dir)
            except Exception as ex:
                if temp_dir and os.path.exists(temp_dir):
                    # TODO: make this optional to produce new
                    #       reference dataset easier
                    shutil.rmtree(temp_dir)
                    # print "KEEPING:", temp_dir
                    pass
                self.temp_dir = None
                raise ex

        def tearDown(self):
            if self.temp_dir and os.path.exists(self.temp_dir):
                # TODO: make this optional to produce new
                #       reference dataset easier
                # print "KEEPING:", self.temp_dir
                shutil.rmtree(self.temp_dir)
                pass
            self.temp_dir = None

        def build_r_script(self):
            """
            build R script to run
            """
            return u'\n'.join([
                resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
                resource_string('org.bccvl.compute', 'rscripts/eval.R'),
                resource_string(
                    'org.bccvl.compute',
                    'content/toolkit/{0}/{0}.R'.format(self.algo_name)),
            ])

        def setup_parameters(self):
            """
            create params dictionary for algorithm
            """

            input_dir = os.path.join(self.temp_dir, 'input')
            params = json.load(
                open(os.path.join(os.path.dirname(__file__), 'params.json')))
            params["env"]["outputdir"] = self.out_dir
            params["env"]["workdir"] = self.temp_dir
            (params["params"]
                   ["species_absence_dataset"]
                   ["filename"]) = os.path.join(
                       input_dir, '{}_bg.csv'.format(self.species))
            (params["params"]
                   ["species_occurrence_dataset"]
                   ["filename"]) = os.path.join(
                       input_dir, '{}_occ.csv'.format(self.species))
            (params["params"]
                   ["species_occurrence_dataset"]
                   ["species"]) = self.species
            params["params"]["environmental_datasets"] = [
                {
                    "layer": "bioclim_01",
                    "type": "continuous",
                    "filename": os.path.join(input_dir, 'B01.tif')
                },
                {
                    "layer": "bioclim_04",
                    "type": "continuous",
                    "filename": os.path.join(input_dir, 'B04.tif')
                },
                {
                    "layer": "bioclim_12",
                    "type": "continuous",
                    "filename": os.path.join(input_dir, 'B12.tif')
                },
                {
                    "layer": "bioclim_15",
                    "type": "continuous",
                    "filename": os.path.join(input_dir, 'B15.tif')
                }
            ]
            return params

        def runTest(self):
            """
            Run the script
            """
            proc = subprocess.Popen(["Rscript", "test.R"], cwd=self.temp_dir,
                                    close_fds=True, stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT)
            # use proc.stdout.read() to retrieve R script output
            ret = proc.wait()
            if ret != 0:
                # keep Rout so that I know the algorihm ran fine
                stdout = proc.stdout.read()
                open(os.path.join(self.temp_dir, "test.Rout"),
                     'w').write(stdout)
                # also print it to show up in jenkins logs
                print(stdout)
            self.assertEqual(ret, 0)
            # Compare outputs
            success = self.compare_outputs()
            # Test pass?
            self.assertTrue(success)

        def _get_file_list(self, base):
            """
            returns a list of all files inside base directory.
            """
            # exclude subfolders starting with .
            # exclude files starting with .
            filelist = [
                imap(os.path.join, repeat(path),
                     (f for f in files if not f.startswith('.')))
                for (path, _, files) in os.walk(base)
                if not os.path.basename(path).startswith('.')]
            # flatten list
            return chain.from_iterable(filelist)

        def compare_outputs(self):
            """
            compare files in self.out_dir to self.ref_dir
            """
            # assume all is going well
            success = True
            # this method only checks for some files, ignores
            # directory structure and assumes that file names are
            # unique across all sub folders
            # 1. get a set of all files from reference output relative
            #    to this file
            reffiles = {os.path.relpath(i, self.ref_dir) for i in
                        chain(
                iglob(os.path.join(self.ref_dir, '*.csv')),
                iglob(os.path.join(self.ref_dir, '*.png')),
                iglob(os.path.join(self.ref_dir, '*.tif')),
            )
            }
            # 2. get set of all output files
            outfiles = {os.path.relpath(i, self.out_dir) for i in
                        chain(
                iglob(os.path.join(self.out_dir, '*.csv')),
                iglob(os.path.join(self.out_dir, '*.png')),
                iglob(os.path.join(self.out_dir, '*.tif')),
            )
            }
            # iterate over all reffiles and compare with same name in output
            # file list
            for fname in sorted(reffiles):
                if fname not in outfiles:
                    success = False
                    print("Missing file", fname, "in outputs")
                    continue
                # we have two files let's compare them
                outfiles.discard(fname)
                fcomp = True
                _, ext = os.path.splitext(fname)
                reffile = os.path.join(self.ref_dir, fname)
                outfile = os.path.join(self.out_dir, fname)
                if ext in ('.csv',):
                    fcomp = compare_csv(reffile, outfile)
                elif ext in ('.png',):
                    fcomp = compare_png(reffile, outfile)
                elif ext in ('.tif',):
                    fcomp = compare_geotiff(reffile, outfile)
                else:
                    # ignore file comparison
                    fcomp = True
                if not fcomp:
                    print("File", fname, 'does not match')
                    success = False
            # all good ... check if we have more files than expected:
            if outfiles:
                print("Algorithm produced more files than expected")
                success = False
            # return result
            return success


class Test_ANN_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'ann'
    algo_params = {
        'nbcv': 5,
        'rang': 0.1,
        'maxit': 200
    }

    def build_r_script(self):
        return u'\n'.join([
            resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
            resource_string('org.bccvl.compute', 'rscripts/eval.R'),
            open(os.path.join(os.path.dirname(__file__),
                              'SampleMat2.R')).read(),
            resource_string('org.bccvl.compute',
                            'content/toolkit/{0}/{0}.R'.format(
                                self.algo_name)),
        ])


class Test_ANN_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'ann'
    algo_params = {
        'nbcv': 5,
        'rang': 0.1,
        'maxit': 200
    }

    def build_r_script(self):
        return u'\n'.join([
            resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
            resource_string('org.bccvl.compute', 'rscripts/eval.R'),
            open(os.path.join(os.path.dirname(__file__),
                              'SampleMat2.R')).read(),
            resource_string('org.bccvl.compute',
                            'content/toolkit/{0}/{0}.R'.format(
                                self.algo_name)),
        ])


class Test_ANN_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'ann'
    algo_params = {
        'nbcv': 5,
        'rang': 0.1,
        'maxit': 200
    }

    def build_r_script(self):
        return u'\n'.join([
            resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
            resource_string('org.bccvl.compute', 'rscripts/eval.R'),
            open(os.path.join(os.path.dirname(__file__),
                              'SampleMat2.R')).read(),
            resource_string('org.bccvl.compute',
                            'content/toolkit/{0}/{0}.R'.format(
                                self.algo_name)),
        ])


class Test_CTA_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'cta'
    algo_params = {
        'method': 'class',
        'control_xval': 5,
        'control_minbucket': 5,
        'control_minsplit': 5,
        'control_cp': 0.001,
        'control_maxdepth': 25,
    }


class Test_CTA_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'cta'
    algo_params = {
        'method': 'class',
        'control_xval': 5,
        'control_minbucket': 5,
        'control_minsplit': 5,
        'control_cp': 0.001,
        'control_maxdepth': 25,
    }


class Test_CTA_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'cta'
    algo_params = {
        'method': 'class',
        'control_xval': 5,
        'control_minbucket': 5,
        'control_minsplit': 5,
        'control_cp': 0.001,
        'control_maxdepth': 25,
    }


class Test_FDA_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'fda'
    algo_params = {
        'method': 'mars',
    }


class Test_FDA_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'fda'
    algo_params = {
        'method': 'mars',
    }


class Test_FDA_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'fda'
    algo_params = {
        'method': 'mars',
    }


class Test_SRE_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'sre'
    algo_params = {
        'quant': 0.025,
    }


class Test_SRE_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'sre'
    algo_params = {
        'quant': 0.025,
    }


class Test_SRE_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'sre'
    algo_params = {
        'quant': 0.025,
    }


class Test_GAM_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'gam'
    algo_params = {
        'family': 'binomial',
        'irls_reg': 0,
        'epsilon': 1e-07,
        'maxit': 200,
        'mgcv_tol': 1e-07,
        'mgcv_half': 15,
    }


class Test_GAM_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'gam'
    algo_params = {
        'family': 'binomial',
        'irls_reg': 0,
        'epsilon': 1e-07,
        'maxit': 200,
        'mgcv_tol': 1e-07,
        'mgcv_half': 15,
    }


class Test_GAM_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'gam'
    algo_params = {
        'family': 'binomial',
        'irls_reg': 0,
        'epsilon': 1e-07,
        'maxit': 200,
        'mgcv_tol': 1e-07,
        'mgcv_half': 15,
    }


class Test_GBM_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'gbm'
    algo_params = {
        'distribution': 'bernoulli',
        'n_trees': 2500,
        'interaction_depth': 7,
        'n_minobsinnode': 5,
        'shrinkage': 0.001,
        'bag_fraction': 0.5,
        'train_fraction': 1,
        'cv_folds': 3,
    }


class Test_GBM_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'gbm'
    algo_params = {
        'distribution': 'bernoulli',
        'n_trees': 2500,
        'interaction_depth': 7,
        'n_minobsinnode': 5,
        'shrinkage': 0.001,
        'bag_fraction': 0.5,
        'train_fraction': 1,
        'cv_folds': 3,
    }


class Test_GBM_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'gbm'
    algo_params = {
        'distribution': 'bernoulli',
        'n_trees': 2500,
        'interaction_depth': 7,
        'n_minobsinnode': 5,
        'shrinkage': 0.001,
        'bag_fraction': 0.5,
        'train_fraction': 1,
        'cv_folds': 3,
    }


class Test_GLM_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'glm'
    algo_params = {
        'type': 'quadratic',
        'interaction_level': 0,
        'test': 'AIC',
        'family': 'binomial',
        'mustart': 0.5,
        'control_epsilon': 1e-08,
        'control_maxit': 50,
        'control_trace': False,
    }


class Test_GLM_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'glm'
    algo_params = {
        'type': 'quadratic',
        'interaction_level': 0,
        'test': 'AIC',
        'family': 'binomial',
        'mustart': 0.5,
        'control_epsilon': 1e-08,
        'control_maxit': 50,
        'control_trace': False,
    }


class Test_GLM_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'glm'
    algo_params = {
        'type': 'quadratic',
        'interaction_level': 0,
        'test': 'AIC',
        'family': 'binomial',
        'mustart': 0.5,
        'control_epsilon': 1e-08,
        'control_maxit': 50,
        'control_trace': False,
    }


class Test_MARS_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'mars'
    algo_params = {
        'degree': 2,
        'penalty': 2,
        'thresh': 0.001,
        'prune': True,
    }


class Test_MARS_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'mars'
    algo_params = {
        'degree': 2,
        'penalty': 2,
        'thresh': 0.001,
        'prune': True,
    }


class Test_MARS_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'mars'
    algo_params = {
        'degree': 2,
        'penalty': 2,
        'thresh': 0.001,
        'prune': True,
    }


class Test_RF_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'rf'
    algo_params = {
        'do.classif': True,
        'ntree': 500,
        'nodesize': 5,
        'mtry': 'default',
    }


class Test_RF_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'rf'
    algo_params = {
        'do.classif': True,
        'ntree': 500,
        'nodesize': 5,
        'mtry': 'default',
    }


class Test_RF_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'rf'
    algo_params = {
        'do.classif': True,
        'ntree': 500,
        'nodesize': 5,
        'mtry': 'default',
    }


class Test_MAXENT_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'maxent'
    algo_params = {
        'maximumiterations': 200,
        'linear': True,
        'quadratic': True,
        'product': True,
        'threshold': True,
        'hinge': True,
        'lq2lqptthreshold': 80,
        'l2lqthreshold': 10,
        'hingethreshold': 15,
        'beta_threshold': -1.0,
        'beta_categorical': -1.0,
        'beta_lqp': -1.0,
        'beta_hinge': -1.0,
        'defaultprevalence': 0.5,
    }


class Test_MAXENT_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'maxent'
    algo_params = {
        'maximumiterations': 200,
        'linear': True,
        'quadratic': True,
        'product': True,
        'threshold': True,
        'hinge': True,
        'lq2lqptthreshold': 80,
        'l2lqthreshold': 10,
        'hingethreshold': 15,
        'beta_threshold': -1.0,
        'beta_categorical': -1.0,
        'beta_lqp': -1.0,
        'beta_hinge': -1.0,
        'defaultprevalence': 0.5,
    }


class Test_MAXENT_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'maxent'
    algo_params = {
        'maximumiterations': 200,
        'linear': True,
        'quadratic': True,
        'product': True,
        'threshold': True,
        'hinge': True,
        'lq2lqptthreshold': 80,
        'l2lqthreshold': 10,
        'hingethreshold': 15,
        'beta_threshold': -1.0,
        'beta_categorical': -1.0,
        'beta_lqp': -1.0,
        'beta_hinge': -1.0,
        'defaultprevalence': 0.5,
    }


class Test_BRT_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'brt'
    algo_params = {
        'tree_complexity': 1,
        'learning_rate': 0.01,
        'bag_fraction': 0.75,
        'n_folds': 10,
        'prev_stratify': True,
        'family': 'bernoulli',
        'n_trees': 50,
        'max_trees': 10000,
        'tolerance_method': 'auto',
        'tolerance_value': 0.001,
    }


class Test_BRT_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'brt'
    algo_params = {
        'tree_complexity': 1,
        'learning_rate': 0.01,
        'bag_fraction': 0.75,
        'n_folds': 10,
        'prev_stratify': True,
        'family': 'bernoulli',
        'n_trees': 50,
        'max_trees': 10000,
        'tolerance_method': 'auto',
        'tolerance_value': 0.001,
    }


class Test_BRT_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'brt'
    algo_params = {
        'tree_complexity': 1,
        'learning_rate': 0.01,
        'bag_fraction': 0.75,
        'n_folds': 10,
        'prev_stratify': True,
        'family': 'bernoulli',
        'n_trees': 50,
        'max_trees': 10000,
        'tolerance_method': 'auto',
        'tolerance_value': 0.001,
    }


class Test_BIOCLIM_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'bioclim'


class Test_BIOCLIM_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'bioclim'


class Test_BIOCLIM_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'bioclim'


class Test_CIRCLES_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'circles'


class Test_CIRCLES_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'circles'


class Test_CIRCLES_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'circles'


class Test_CONVHULL_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'convhull'


class Test_CONVHULL_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'convhull'


class Test_CONVHULL_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'convhull'


class Test_VORONOIHULL_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'voronoiHull'


class Test_VORONOIHULL_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'voronoiHull'


class Test_VORONOIHULL_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'voronoiHull'


class Test_GEODIST_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'geoDist'


class Test_GEODIST_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'geoDist'


class Test_GEODIST_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'geoDist'


class Test_GEOIDW_Egernia(BaseTestCase.AlgorithmTestCase):

    species = 'Egernia'
    algo_name = 'geoIDW'


class Test_GEOIDW_Eucalyptus(BaseTestCase.AlgorithmTestCase):

    species = 'Eucalyptus'
    algo_name = 'geoIDW'


class Test_GEOIDW_Rhinella(BaseTestCase.AlgorithmTestCase):

    species = 'Rhinella'
    algo_name = 'geoIDW'
