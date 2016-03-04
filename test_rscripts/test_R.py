# Run a single test:
#   TEST_R_SCRIPTS=1 python setup.py test -v -s test_rscripts.test_R.Test_BIOCLIM
# Use nose as test runner?
#   TEST_R_SCRIPTS=1 python setup.py test -v -r nose -s test_rscripts.test_R.Test_BIOCLIM
# How do use nose or anything else to run them? (generate XML unittest output)

import csv
from glob import iglob
from itertools import imap, repeat, chain
import json
import os
import os.path
from pkg_resources import resource_string
import subprocess
import shutil
import tempfile
import unittest

import numpy as np
from osgeo import gdal
from skimage.io import imread
try:
    from skimage.measure import compare_ssim as ssim
    HAVE_MULTICHANNEL_SSIM = True
except ImportError:
    from skimage.measure import structural_similarity as ssim
    HAVE_MULTICHANNEL_SSIM = False


# A map for known csv files to parse the correct data types
#    only str, int and float allowed, as  compare_csv is not handling anything else
DTYPE_MAP = {
    'biomod2_like_VariableImportance.csv': [(str, 50), float, float, float, float],
    'biomod2.modelEvaluation.csv': [(str, 50), float, float, float, float],
    'combined.RUN1.modelEvaluation.csv': [(str, 50), float, float, float, float],
    'combined.modelEvaluation.csv': [(str, 50), float, float, float, float],
    'evaluation.performance.csv': [int, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, ],
    'evaluation.summary.csv': [(str, 50), float, float, (str, 50), float],
    'maxent_like_VariableImportance.csv': [(str, 50), float, float, float, float],
    'variableImportance.RUN1.csv': [(str, 50), float],
}


def compare_geotiff(img1, img2, eps=1e-03):
    """img1 and img2 are the filenames to the tiff files.
    """
    d1 = gdal.Open(img1)
    b1 = d1.GetRasterBand(1)
    a1 = np.nan_to_num(b1.ReadAsArray())  # replace no data with 0
    d2 = gdal.Open(img2)
    b2 = d2.GetRasterBand(1)
    a2 = np.nan_to_num(b2.ReadAsArray())  # replace no data with 0
    # return true if the two arrays are equal (with tolerance)
    return np.allclose(a1, a2, rtol=0, atol=eps)


def compare_png(img1, img2, eps=0.99):
    """check whether img1 and img2 are similar

       we use structural similarity (SSIM) to compare them.

       SSIM generates values between 0 and 1, where 1 represents identical images

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
        # We have to do our multichannel ssim ourselves
        nch = im1.shape[-1]
        mssim = np.empty(nch)
        for ch in range(nch):
            # use copy to generate contiguous array and avoid warning
            ch_result = ssim(im1[..., ch].copy(), im2[..., ch].copy())
            mssim[..., ch] = ch_result
        mssim = mssim.mean()
    return mssim > eps


def read_csv(filename, column_headers, column_types):
    # use csv module to parse csv (understands quotes etc...) and use \t as seperator to help numpy parsing the csv
    reader = csv.reader(open(filename))
    if column_headers:
        names = reader.next()
    return np.recfromtxt(("\t".join(row) for row in reader), delimiter="\t", names=names, dtype=column_types)


def compare_csv(csv1, csv2, column_headers=True, eps=1e-3):
    column_types = DTYPE_MAP[os.path.basename(csv1)]
    d1 = read_csv(csv1, column_headers, column_types)
    d2 = read_csv(csv2, column_headers, column_types)
    # compare cloumn names
    ret = d1.dtype.names == d2.dtype.names
    # compare all non float columns
    fields = [field for field in d1.dtype.fields if d1.dtype[field].kind != 'f']
    ret = ret and (d1[fields] == d2[fields]).all()
    # compare all float type columns (with epsilon)
    fields = [field for field in d1.dtype.fields if d1.dtype[field].kind == 'f']
    # make copy of float view, so that we can safely replace nan's
    f1 = np.nan_to_num(d1[fields].view((float, len(fields))))
    f2 = np.nan_to_num(d2[fields].view((float, len(fields))))
    ret = ret and np.allclose(f1, f2, rtol=0, atol=eps)
    return ret


class BaseTestCase(object):

    class AlgorithmTestCase(unittest.TestCase):

        temp_dir = None
        out_dir = None
        algo_params = {}
        algo_scripts = []
        md5_digests = {}

        def setUp(self):
            if 'TEST_R_SCRIPTS' not in os.environ:
                self.skipTest('R Script tests not enabled - TEST_R_SCRIPTS not set')
            # Create a temp directory for testing
            temp_dir = None
            try:
                temp_dir = tempfile.mkdtemp(prefix='test_' + self.__class__.__name__ + '_')
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
            except Exception as e:
                if temp_dir and os.path.exists(temp_dir):
                    shutil.rmtree(temp_dir)
                self.temp_dir = None
                raise e

        def tearDown(self):
            if self.temp_dir and os.path.exists(self.temp_dir):
                shutil.rmtree(self.temp_dir)
            self.temp_dir = None

        def build_r_script(self):
            return u'\n'.join([
                resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
                resource_string('org.bccvl.compute', 'rscripts/eval.R'),
                resource_string('org.bccvl.compute', 'content/toolkit/{0}/{0}.R'.format(self.algo_name)),
            ])

        def setup_parameters(self):

            data_dir = os.path.join(os.path.dirname(__file__), 'data', 'Rhinella')
            params = json.load(open(os.path.join(os.path.dirname(__file__), 'params.json')))
            params["env"]["outputdir"] = self.out_dir
            params["env"]["workdir"] = self.temp_dir
            params["params"]["species_absence_dataset"]["filename"] = os.path.join(data_dir, 'rhinella_absence.csv')
            params["params"]["species_occurrence_dataset"]["filename"] = os.path.join(data_dir, 'rhinella_occurrence.csv')
            params["params"]["species_occurrence_dataset"]["species"] = 'Rhinella'
            params["params"]["environmental_datasets"] = [
                {
                    "layer": "bioclim_01",
                    "type": "continuous",
                    "filename": os.path.join(data_dir, 'B01.tif')
                },
                {
                    "layer": "bioclim_04",
                    "type": "continuous",
                    "filename": os.path.join(data_dir, 'B04.tif')
                },
                {
                    "layer": "bioclim_12",
                    "type": "continuous",
                    "filename": os.path.join(data_dir, 'B12.tif')
                },
                {
                    "layer": "bioclim_15",
                    "type": "continuous",
                    "filename": os.path.join(data_dir, 'B15.tif')
                }
            ]
            return params

        def runTest(self):
            # Run the script
            proc = subprocess.Popen(["Rscript", "test.R"], cwd=self.temp_dir, close_fds=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            # use proc.stdout.read() to retrieve R script output
            ret = proc.wait()
            if ret != 0:
                print proc.stdout.read()
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
                imap(os.path.join, repeat(path), (f for f in files if not f.startswith('.')))
                for (path, dirs, files) in os.walk(base) if not os.path.basename(path).startswith('.')]
            # flatten list
            return chain.from_iterable(filelist)


        def compare_outputs(self):
            # assume all is going well
            success = True
            # this method only checks for some files, ignores
            # directory structure and assumes thath file names are
            # unique across all sub folders
            # 1. get a set of all files from reference output relative to this file
            refdir = os.path.join(os.path.dirname(__file__), 'outputs', self.algo_name)
            reffiles = {os.path.relpath(i, refdir) for i in
                chain(
                    iglob(os.path.join(refdir, '*.csv')),
                    iglob(os.path.join(refdir, '*.png')),
                    iglob(os.path.join(refdir, '*.tif')),
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
            # iterate over all reffiles and compare with same name in output file list
            for fname in sorted(reffiles):
                if fname not in outfiles:
                    success = False
                    print "Missing file", fname, "in outputs"
                    continue
                # we have two files let's compare them
                outfiles.discard(fname)
                fcomp = True
                _, ext = os.path.splitext(fname)
                reffile = os.path.join(refdir, fname)
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
                    print "File", fname, 'does not match'
                    success = False
            # all good ... check if we have more files than expected:
            if outfiles:
                print "Algorithm produced more files than expected"
                success = False
            # return result
            return success


class Test_ANN(BaseTestCase.AlgorithmTestCase):

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
            open(os.path.join(os.path.dirname(__file__), 'SampleMat2.R')).read(),
            resource_string('org.bccvl.compute', 'content/toolkit/{0}/{0}.R'.format(self.algo_name)),
        ])



class Test_CTA(BaseTestCase.AlgorithmTestCase):

    algo_name = 'cta'
    algo_params = {
        'method': 'class',
        'control_xval': 5,
        'control_minbucket': 5,
        'control_minsplit': 5,
        'control_cp': 0.001,
        'control_maxdepth': 25,
    }


class Test_FDA(BaseTestCase.AlgorithmTestCase):

    algo_name = 'fda'
    algo_params = {
        'method': 'mars',
    }


class Test_SRE(BaseTestCase.AlgorithmTestCase):

    algo_name = 'sre'
    algo_params = {
        'quant': 0.025,
    }


class Test_GAM(BaseTestCase.AlgorithmTestCase):

    algo_name = 'gam'
    algo_params = {
        'family': 'binomial',
        'irls_reg': 0,
        'epsilon': 1e-07,
        'maxit': 200,
        'mgcv_tol': 1e-07,
        'mgcv_half': 15,
    }


class Test_GBM(BaseTestCase.AlgorithmTestCase):

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


class Test_GLM(BaseTestCase.AlgorithmTestCase):

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


class Test_MARS(BaseTestCase.AlgorithmTestCase):

    algo_name = 'mars'
    algo_params = {
        'degree': 2,
        'penalty': 2,
        'thresh': 0.001,
        'prune': True,
    }


class Test_RF(BaseTestCase.AlgorithmTestCase):

    algo_name = 'rf'
    algo_params = {
        'do.classif': True,
        'ntree': 500,
        'nodesize': 5,
        'mtry': 'default',
    }


class Test_MAXENT(BaseTestCase.AlgorithmTestCase):

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


class Test_BRT(BaseTestCase.AlgorithmTestCase):

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


class Test_BIOCLIM(BaseTestCase.AlgorithmTestCase):

    algo_name = 'bioclim'


class Test_CIRCLES(BaseTestCase.AlgorithmTestCase):

    algo_name = 'circles'


class Test_CONVHULL(BaseTestCase.AlgorithmTestCase):

    algo_name = 'convHull'


class Test_VORONOIHULL(BaseTestCase.AlgorithmTestCase):

    algo_name = 'voronoiHull'


class Test_GEODIST(BaseTestCase.AlgorithmTestCase):

    algo_name = 'geoDist'


class Test_GEOIDW(BaseTestCase.AlgorithmTestCase):

    algo_name = 'geoIDW'
