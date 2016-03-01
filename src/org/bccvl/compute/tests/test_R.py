import hashlib
import json
import os
import os.path
from pkg_resources import resource_string
from pkg_resources import resource_filename
import subprocess
import shutil
import tempfile
import unittest

class BaseTestCase(object):

    class AlgorithmTestCase(unittest.TestCase):

        temp_dir = None
        algo_params = {}
        algo_scripts = []
        md5_digests = {}

        def setUp(self):
            if 'TEST_R_SCRIPTS' not in os.environ:
                self.skipTest('R Script tests not enabled - TEST_R_SCRIPTS not set')
            # Create a temp directory for testing
            try:
                temp_dir = tempfile.mkdtemp(prefix='test_')
                out_dir = os.path.join(temp_dir, 'output')
                os.mkdir(out_dir)
                os.mkdir(os.path.join(out_dir, 'output'))
                self.temp_dir = temp_dir
                # setup test parameters
                self.params = self.setup_parameters()
                self.params['params'].update(self.algo_params)
                jfile = open(os.path.join(temp_dir, 'params.json'), mode='w')
                json.dump(self.params, jfile, indent=2)
                # setup algorithm script
                script = u'\n'.join([
                    resource_string('org.bccvl.compute', 'rscripts/bccvl.R'),
                    resource_string('org.bccvl.compute', 'rscripts/eval.R')] +
                    [resource_string('org.bccvl.compute', script) for script in self.algo_scripts])
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

        def setup_parameters(self):
            params = json.load(open(resource_filename('org.bccvl.compute', 'tests/params.json')))
            params["params"]["species_absence_dataset"]["filename"] = resource_filename('org.bccvl.compute', 'tests/data/Rhinella/rhinella_absence.csv')
            params["params"]["species_occurrence_dataset"]["filename"] = resource_filename('org.bccvl.compute', 'tests/data/Rhinella/rhinella_occurrence.csv')
            params["params"]["species_occurrence_dataset"]["species"] = 'Rhinella'
            params["params"]["environmental_datasets"] = [
                {
                    "layer": "bioclim_01",
                    "type": "continuous",
                    "filename": resource_filename('org.bccvl.compute', 'tests/data/Rhinella/B01.tif')
                },
                {
                    "layer": "bioclim_04",
                    "type": "continuous",
                    "filename": resource_filename('org.bccvl.compute', 'tests/data/Rhinella/B04.tif')
                },
                {
                    "layer": "bioclim_12",
                    "type": "continuous",
                    "filename": resource_filename('org.bccvl.compute', 'tests/data/Rhinella/B12.tif')
                },
                {
                    "layer": "bioclim_15",
                    "type": "continuous",
                    "filename": resource_filename('org.bccvl.compute', 'tests/data/Rhinella/B15.tif')
                }
            ]
            return params

        def runTest(self):
            # Run the script
            proc = subprocess.Popen(["Rscript", "test.R"], cwd=self.temp_dir, close_fds=True)
            ret = proc.wait()
            self.assertEqual(ret, 0)
            # Check the md5 digest
            success = self.validate_md5(self.md5_digests, self.temp_dir)
            # Test pass?
            self.assertTrue(success)

        def validate_md5(self, md5_digests, work_dir):
            success = True
            for fname in md5_digests.keys():
                filepath = os.path.join(work_dir, 'output/output/' + fname)
                if os.path.exists(filepath):
                    # Check md5 only if it has md5 digest
                    if md5_digests[fname]:
                        md5 = hashlib.md5(open(filepath, 'rb').read()).hexdigest()
                        if md5 != md5_digests[fname]:
                            print "md5 digest mismacthed:", fname, md5
                            success = False
                else:
                    print "missing output file:", fname
                    success = False
            return success


class Test_ANN(BaseTestCase.AlgorithmTestCase):

    algo_params = {
        'nbcv': 5,
        'rang': 0.1,
        'maxit': 200
    }
    algo_scripts = [
        'tests/SampleMat2.R',
        'content/toolkit/ann/ann.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : '10173036b71a0f517133ae4e80aa2761',
        'combined.RUN1.modelEvaluation.csv' : '36d61d6b5d1f6d3edbd5b0f51bcf6903',
        'evaluation.performance.csv' : 'd6888b2887a8fd3ed6f7d580bcb26fdd',
        'evaluation.summary.csv' : '0d40c721d3553c8e0b81b499bc12b3a6',
        'model.object.RData' : None,
        'pROC.RUN1.png' : 'e8b33f97dd8e4fd2973147289409fbb5',
        'RUN1-error.png' : 'a99d9732994dc6351431b820cb2d52b4',
        'RUN1-intervals.png': '7f9c12b4fafb969daa537cee63930229',
        'RUN1-occurence_absence_pdf.png' : '95990e150b4eca77f98defa9fcc4a3a6',
        'RUN1-occurrence_absence_hist.png' : '80604a4c6eadf5e20eea856af672d370',
        'RUN1-ppp.png' : '8f0561fdc0938d510b208d306ec46569',
        'RUN1-roc.png' : '49585d0e3171f48e10d6def874c035ce',
        'RUN1-tradeoffs.png' : '7e65927333edc740e05c76b6bfa55f3d',
        'RUN1-true_and_false_posivite_rates.png' : 'e98485abacf4f693045e2dc703f616a5',
        'variableImportance.RUN1.csv' : '866af6e91c8f08220d2063c38d81d03f',
        'mean_response_curves_Rhinella_AllData_RUN1_ANN.png' : '62c6cf75ee92650e6ad5bbe333fe356c'
    }


class Test_CTA(BaseTestCase.AlgorithmTestCase):

    algo_params = {
        'method': 'class',
        'control_xval': 5,
        'control_minbucket': 5,
        'control_minsplit': 5,
        'control_cp': 0.001,
        'control_maxdepth': 25,
    }
    algo_scripts = [
        'content/toolkit/cta/cta.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : 'a4eb942b63418fc9792115e11952207a',
        'combined.RUN1.modelEvaluation.csv' : '9ffae2a43108afb08c60c074d0c14a1b',
        'evaluation.performance.csv' : '3b2c5c347fdb647d2773e2e77808d35d',
        'evaluation.summary.csv' : '7cf162396c8d50142ff20e32107249d9',
        'model.object.RData' : None,
        'pROC.RUN1.png' : '499062ebb98cb5cfcf43f3350d1c7d8b',
        'RUN1-error.png' : '3df52f6378a240fef0967296c560c390',
        'RUN1-intervals.png': '46001e1ba6e39c188859204cd9c4b570',
        'RUN1-occurence_absence_pdf.png' : '4ce4249e1ce8902ba2651dd45d8d16e0',
        'RUN1-occurrence_absence_hist.png' : 'db6a385d68d00ebe5bda57912bfc7b0e',
        'RUN1-ppp.png' : 'e6affcdf5844d5d7ea8dfefdb349c1d7',
        'RUN1-roc.png' : 'f2099ce0e0d9a5aaa367e692a17e44c9',
        'RUN1-tradeoffs.png' : 'd0759b8709e1048644d7079fdded3134',
        'RUN1-true_and_false_posivite_rates.png' : '90bdaaeffa5c3c2554876f2fa983664f',
        'variableImportance.RUN1.csv' : 'e36cbaef002a5a2ca2344dfe72376150',
        'mean_response_curves_Rhinella_AllData_RUN1_CTA.png' : '05d4071b71597295d10f35a6af5dc98b'
    }


class Test_FDA(BaseTestCase.AlgorithmTestCase):

    algo_params = {
        'method': 'mars',
    }
    algo_scripts = [
        'content/toolkit/mars/mars.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : 'ce40e19526e0d5ae75b397d55e9fc08b',
        'combined.RUN1.modelEvaluation.csv' : '6bcf52afbe21232b8e30c3d2f7e7d733',
        'evaluation.performance.csv' : '470dedeb576a4b5d5d79dc141e7e422e',
        'evaluation.summary.csv' : '7c4d2aefc7a4a597cef2c307d9f7a074',
        'model.object.RData' : None,
        'pROC.RUN1.png' : '672d8f3560e387af9559eab094db3e6b',
        'RUN1-error.png' : '966cf7b7291adb724a8d4a53bcbbdbce',
        'RUN1-intervals.png': '09ea2810a407fd3d222bf0fb734ea556',
        'RUN1-occurence_absence_pdf.png' : '29d07bd2584d34e12f7768f0a7a7f434',
        'RUN1-occurrence_absence_hist.png' : '087416f90207508160c14ca8b5a44cbf',
        'RUN1-ppp.png' : '58d1efd3e38d654d572da09ac2940bc7',
        'RUN1-roc.png' : 'b85e3136cf31419af9f2d8844c0ab148',
        'RUN1-tradeoffs.png' : 'e7b52c70c31d53f993b3a66c00590611',
        'RUN1-true_and_false_posivite_rates.png' : 'ee9a82459d847ad87aa39a745b5d5667',
        'variableImportance.RUN1.csv' : 'f24c71f56b25ef4d8d5ad7be80219de2',
        'mean_response_curves_Rhinella_AllData_RUN1_FDA.png' : 'cb73e0e1333f7adadc50fd405428d0ba'
    }


class Test_SRE(BaseTestCase.AlgorithmTestCase):

    algo_params = {
        'quant': 0.025,
    }
    algo_scripts = [
        'content/toolkit/sre/sre.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : 'f59f276dce68e9eec22895f529a71f98',
        'combined.RUN1.modelEvaluation.csv' : '1e1fc1aaaa8cdef04b70b2d92fa512a5',
        'evaluation.performance.csv' : '45bb503731028afb7e056e5f28ca87b3',
        'evaluation.summary.csv' : '979124db2c8f39d8f9dc3446b8e53707',
        'model.object.RData' : None,
        'pROC.RUN1.png' : 'b1c0d94376b1dd59c1e43c1cbe06f7a2',
        'RUN1-error.png' : '965ff00e8454ea25f246aa781715daa6',
        'RUN1-intervals.png': '0f6eb27970e1abd343966da7a04d2a88',
        'RUN1-occurence_absence_pdf.png' : '8fff38dc198bc2cb3c84588c71a45302',
        'RUN1-occurrence_absence_hist.png' : '1fb0b0081597c8fadafc91bccf107ad4',
        'RUN1-ppp.png' : '631acd42f64a019eb75296cad1dd54b1',
        'RUN1-roc.png' : 'ed6425db8e3955d8100cdd0d11840060',
        'RUN1-tradeoffs.png' : '632b4873205cf02083ce42c96db7d222',
        'RUN1-true_and_false_posivite_rates.png' : '793dd7168ebf6e9351ef729eb5b98583',
        'variableImportance.RUN1.csv' : '23bf508e3821b7850d8674884e6c49d4',
        'mean_response_curves_Rhinella_AllData_RUN1_SRE.png' : '807fa70d54dafcfc4eb94d17213959d9'
    }


class Test_GAM(BaseTestCase.AlgorithmTestCase):

    algo_params = {
        'family': 'binomial',
        'irls_reg': 0,
        'epsilon': 1e-07,
        'maxit': 200,
        'mgcv_tol': 1e-07,
        'mgcv_half': 15,
    }
    algo_scripts = [
        'content/toolkit/gam/gam.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : 'a58e56e4b3f2fc07cad4ea1d84debe35',
        'combined.RUN1.modelEvaluation.csv' : '0fe95be68beba8f64b1fd8ddc2b4f6fe',
        'evaluation.performance.csv' : 'b7b5030f7272db08043e199d61ce61eb',
        'evaluation.summary.csv' : '5f1bb8454c668e84311cfe53e1d560e0',
        'model.object.RData' : None,
        'pROC.RUN1.png' : '6dcf5e1273ff22532992aa305d4e4eb3',
        'RUN1-error.png' : '9158030006e649df66ac6e1b8b19bcee',
        'RUN1-intervals.png': '1affdd053da31b1afa867d72d257f41f',
        'RUN1-occurence_absence_pdf.png' : '0e6cb922b3d351b72a3bb80ff70afb4d',
        'RUN1-occurrence_absence_hist.png' : '87948dccf02ba81e77b41aed836677e1',
        'RUN1-ppp.png' : 'a36e5fd942c0c07112c062676efb0c28',
        'RUN1-roc.png' : '8e3ef888098f4de6de2890dbf52fcac5',
        'RUN1-tradeoffs.png' : 'ecf4089ab2835e1eeb95feea01fc7147',
        'RUN1-true_and_false_posivite_rates.png' : '1bd49e1d74f0ff490008660968cb9cba',
        'variableImportance.RUN1.csv' : '83db3de921511f53715bf9d4351df083',
        'mean_response_curves_Rhinella_AllData_RUN1_GAM.png' : '7f465e5e4b2c8d2afad8d9dcb6ecf9da'
    }


class Test_GBM(BaseTestCase.AlgorithmTestCase):

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
    algo_scripts = [
        'content/toolkit/gbm/gbm.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : '54e1d8732801b3694991256977b49078',
        'combined.RUN1.modelEvaluation.csv' : '3bb2f5727ba1213089c2feaa0de89d65',
        'evaluation.performance.csv' : 'c5ffebbc9efcaf57cf1776dfe2cdd1c9',
        'evaluation.summary.csv' : '2efc43546687702038bae2099d7c4d52',
        'model.object.RData' : None,
        'pROC.RUN1.png' : '596f405914bcf2e04246ad6c0cc85d41',
        'RUN1-error.png' : '8f2801f18d8e65851d728ada90aca3a9',
        'RUN1-intervals.png': 'd89ef896f5c1cc2ee1d20c1d372ae301',
        'RUN1-occurence_absence_pdf.png' : '2d7267d7a2957986d1599cd075fb13b6',
        'RUN1-occurrence_absence_hist.png' : 'a31cd873d599eea3b4579c5d4eea931f',
        'RUN1-ppp.png' : 'b589128c3c75f76a650dbe8e0a8dfb78',
        'RUN1-roc.png' : 'aca0e5609ebf0fb750403fa6ae583be0',
        'RUN1-tradeoffs.png' : '61d23c4361d453b483885a9ca1bad6c8',
        'RUN1-true_and_false_posivite_rates.png' : '475e86063ad427687ccf1d7e11a33353',
        'variableImportance.RUN1.csv' : '992afccf3a57d425c7f68330b6c4e715',
        'mean_response_curves_Rhinella_AllData_RUN1_GBM.png' : 'cc51756b48fb99327b218a603925b230'
    }


class Test_GLM(BaseTestCase.AlgorithmTestCase):

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
    algo_scripts = [
        'content/toolkit/glm/glm.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : 'e1bdd6e457cd40c7c3464eb95c97ca04',
        'combined.RUN1.modelEvaluation.csv' : 'c23734bba3131ecebe112d73755d8a8e',
        'evaluation.performance.csv' : 'a8bc5b8cefc6e4980a6fef6b499d3414',
        'evaluation.summary.csv' : '0470b4be161601c5de00cb083cb4547a',
        'model.object.RData' : None,
        'pROC.RUN1.png' : 'fce6af586955870131eb0e967cfc7b2f',
        'RUN1-error.png' : '94673a6807deb45ff369d3669f488f65',
        'RUN1-intervals.png': '029f875a6370edce5c5736b722e15c9f',
        'RUN1-occurence_absence_pdf.png' : 'a198e27274ad3acd01f3d431956f0124',
        'RUN1-occurrence_absence_hist.png' : '694e180d97d2b04c0a7cc485b51af624',
        'RUN1-ppp.png' : '08cebdb3469427c321dbbead0c725848',
        'RUN1-roc.png' : 'be8be4161fc4151464f2ab146f1e874d',
        'RUN1-tradeoffs.png' : '134666d196b42e3d7e0a44d28e62628f',
        'RUN1-true_and_false_posivite_rates.png' : '1356e1b4670c914543f721d6854cdf4d',
        'variableImportance.RUN1.csv' : 'b944a88f041740dbc04cc48aa4ab7a88',
        'mean_response_curves_Rhinella_AllData_RUN1_GLM.png' : '44254a75ce9f69ebe2f230dbad29dd78'
    }


class Test_MARS(BaseTestCase.AlgorithmTestCase):

    algo_params = {
        'degree': 2,
        'penalty': 2,
        'thresh': 0.001,
        'prune': True,
    }
    algo_scripts = [
        'content/toolkit/mars/mars.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : 'c54b18d8baf87ee62b3961f1b831f095',
        'combined.RUN1.modelEvaluation.csv' : 'cdd4b5922f84010e2481900da912059b',
        'evaluation.performance.csv' : 'b6817652f7fd5f8f8cb2531975019526',
        'evaluation.summary.csv' : 'ac45ae1e913bd95d07e26566235d563a',
        'model.object.RData' : None,
        'pROC.RUN1.png' : '8094e4b5c40f392fafd3360b7db1f63b',
        'RUN1-error.png' : '3d9b278ebab9774fbb3b9fb60ea58dd4',
        'RUN1-intervals.png': '66ca61767a28974a0451af8e739a4b90',
        'RUN1-occurence_absence_pdf.png' : '96884bf7041c5d48059e6255342ca2e6',
        'RUN1-occurrence_absence_hist.png' : '72f1e800f437e7720dddca9abffdf666',
        'RUN1-ppp.png' : 'c610f432465a39126a8edb5770a78133',
        'RUN1-roc.png' : 'e6e72e1dd6836d8841f197fc84bf4343',
        'RUN1-tradeoffs.png' : 'c4afee4089e332c74ee57129e075a94f',
        'RUN1-true_and_false_posivite_rates.png' : '67e3f407c985393a9bd4e4e274f03a4f',
        'variableImportance.RUN1.csv' : '6888dc725edba0d574cc081136902242',
        'mean_response_curves_Rhinella_AllData_RUN1_MARS.png' : '29faf42209ef195f0a31bf9f4a4608ca'
    }


class Test_RF(BaseTestCase.AlgorithmTestCase):

    algo_params = {
        'do.classif': True,
        'ntree': 500,
        'nodesize': 5,
        'mtry': 'default',
    }
    algo_scripts = [
        'content/toolkit/rf/rf.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : 'f495735d5fe7d3a9f68b03fadd870180',
        'combined.RUN1.modelEvaluation.csv' : '2b022a4cbbd3d2ca887158619634ed22',
        'evaluation.performance.csv' : '4b220ab20cd6721d5766a7b90897efad',
        'evaluation.summary.csv' : 'a202dacde77b3d9f773f9f252ec8785e',
        'model.object.RData' : None,
        'pROC.RUN1.png' : '6b8cb2bc0d422e56f3921f512ad5260e',
        'RUN1-error.png' : 'cd5e95b18eab548ebb0bc2a491973a48',
        'RUN1-intervals.png': 'ac890318762fb4a7b35bf354e17a6d68',
        'RUN1-occurence_absence_pdf.png' : '6c8a8c059b4183ea72a4999f294a5d1e',
        'RUN1-occurrence_absence_hist.png' : '6d8973daf61e9cd2f7585f0f27228cec',
        'RUN1-ppp.png' : '826393155fa2927556df0f7b7f71724f',
        'RUN1-roc.png' : '0d1e660f21578a074af779341942e7a2',
        'RUN1-tradeoffs.png' : 'b2d9e0114227a19fb7a8608e2b47457c',
        'RUN1-true_and_false_posivite_rates.png' : '11a13ba15b871d8f39a19f5517aba696',
        'variableImportance.RUN1.csv' : '15dff6d5c276e005c31dd4b9abd6ae2c',
        'mean_response_curves_Rhinella_AllData_RUN1_RF.png' : '0cde81858475fe4fc65cd4c0d27f3037'
    }


class Test_MAXENT(BaseTestCase.AlgorithmTestCase):

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
    algo_scripts = [
        'content/toolkit/maxent/maxent.R'
    ]
    md5_digests =  {
        'biomod2.modelEvaluation.csv' : '7d9c4e0ec5fcaf0ee44ddca8d762439b',
        'combined.RUN1.modelEvaluation.csv' : '5f7228a6c7214605aa75c7b94189cd5f',
        'evaluation.performance.csv' : 'feb4be53b886cd9e9efa8f20ff6c2884',
        'evaluation.summary.csv' : 'f3e1f829cbe3f27fe123b8a26edc6837',
        'model.object.RData' : None,
        'pROC.RUN1.png' : '665e15dd3389e2e81e45f24f4605c983',
        'RUN1-error.png' : '1ea3eb0e4ba0098c12261e5579a771e9',
        'RUN1-intervals.png': '6f1748d73b2b648da71400ec6906a52a',
        'RUN1-occurence_absence_pdf.png' : '55a46b40015b49276d2021938b3cfefa',
        'RUN1-occurrence_absence_hist.png' : 'c7ca006021df038b828fded9f884c8c4',
        'RUN1-ppp.png' : '98213d220208759841f5f249df38d7bd',
        'RUN1-roc.png' : 'cf9acbd98a34c36a955c787aee47b44f',
        'RUN1-tradeoffs.png' : 'fdfaa78f2d02bb269263e9296aaf2ec7',
        'RUN1-true_and_false_posivite_rates.png' : '6d33308147a7fc40c99aa2320c497f55',
        'variableImportance.RUN1.csv' : 'bf2347f629a219aa2c74fbbcb30ca201',
        'mean_response_curves_Rhinella_AllData_RUN1_MAXENT.png' : '11ad69d01c4bd1a8adac2d825aa45f6c'
    }

class Test_BRT(BaseTestCase.AlgorithmTestCase):

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
    algo_scripts = [
        'content/toolkit/brt/brt.R'
    ]
    md5_digests =  {
        'AUC.png' : 'daa94284788159870efe0a748df8a765',
        'B01_response.png' : '93a4ac952a5ac6537d90d1eda7cfcac7',
        'B04_response.png' : '664c1df080d003ef9bb6265c01124ab8',
        'B12_response.png' : '57fdf516c177ac2dde7ee56fc701dd2b',
        'B15_response.png' : '2e86c7c4e4cfe6007edb92c74bb07193',
        'brt-error.png' : 'a3ff199e1c4338da866fb64f5daace41',
        'brt-intervals.png' : 'b050883bc321b9dd649a43f49759e75b',
        'brt-ppp.png': '4a5ff2f099d0aed1789607216eace63d',
        'brt-roc.png' : '63e369ee3e80e807c0f8f947a92ea841',
        'brt-tradeoffs.png' : '1c72727272c32228c6654b1bb05849da',
        'biomod2_like_VariableImportance.csv' : '0b99181320f4cc8a8639baca7e487119',
        'combined.modelEvaluation.csv' : '353695164ced9bb2f373f82059bf2363',
        'dismo.eval.object.RData' : '60ae14d269c1e7c68350dee0d005f0f6',
        'evaluation.performance.csv' : 'fdd25be1c1af8a9add1d15ab9bb85596',
        'evaluation.summary.csv' : '573f326ecd906d0e07d9beb4b15f7850',
        'maxent_like_VariableImportance.csv' : '73448cfe46f4f3dfb25b62a10530b475',
        'proj_current_Rhinella.tif' : '2f856926ff5ed06881ea885105f7122d',
        'Rhinella.model.object.RData' : None,
        'results.html' : None
    }


class Test_BIOCLIM(BaseTestCase.AlgorithmTestCase):

    algo_scripts = [
        'content/toolkit/bioclim/bioclim.R'
    ]
    md5_digests = {
        'AUC.png' : 'aec420da80d33406eab0c0670c302603',
        'B01_response.png' : '21d5b80d0e89866bb336e11f7d29f3ce',
        'B04_response.png' : '1acae9eae3d053993046f1750639e847',
        'B12_response.png' : 'aae600f9cff3c4b50f3866b104a232a7',
        'B15_response.png' : '79d88753c058500392e2b52d43415c0c',
        'bioclim-error.png' : '47e9faf82eb38fc9bb142ced24323b0a',
        'bioclim-intervals.png' : '9d8d33a6da3f440b15179ceb0a9cfde2',
        'bioclim-ppp.png': '43a8bbe12687392d703e7e54d29ccb22',
        'bioclim-roc.png' : '20fc782511e8d99cced81e4592cfaac1',
        'bioclim-tradeoffs.png' : '729eb26df7e7122a075b86293a119f8d',
        'biomod2_like_VariableImportance.csv' : 'ec011effca1c5210f5c3b057d2675310',
        'combined.modelEvaluation.csv' : 'ba847ef449c51cf7e4edd685c943391c',
        'dismo.eval.object.RData' : 'a1475cfd04fe5c6d12f8fbecac140b51',
        'evaluation.performance.csv' : '150e7328de438a642c072e3852f4fc76',
        'evaluation.summary.csv' : '59c7eaace45a8262df89e39cc11546e6',
        'maxent_like_VariableImportance.csv' : '57f556e10554cdf86124e2ae48fbfcf3',
        'proj_current_Rhinella.tif' : '1fa9f8263d67b209e08f7bf9f2a17915',
        'Rhinella.model.object.RData' : None,
        'results.html' : None
    }


class Test_CIRCLES(BaseTestCase.AlgorithmTestCase):

    algo_scripts = [
        'content/toolkit/circles/circles.R'
    ]
    md5_digests =  {
        'proj_current_Rhinella.tif' : '2438901fd4b1414ebf0f23e86352a20f',
        'Rhinella.model.object.RData' : None
    }


class Test_CONVHULL(BaseTestCase.AlgorithmTestCase):

    algo_scripts = [
        'content/toolkit/convHull/convHull.R'
    ]
    md5_digests =  {
        'proj_current_Rhinella.tif' : '896efaaded8035a41ec8b67b23af81b0',
        'Rhinella.model.object.RData' : None
    }


class Test_VORONOIHULL(BaseTestCase.AlgorithmTestCase):

    algo_scripts = [
        'content/toolkit/voronoiHull/voronoiHull.R'
    ]
    md5_digests =  {
        'proj_current_Rhinella.tif' : '2438901fd4b1414ebf0f23e86352a20f',
        'Rhinella.model.object.RData' : None
    }


class Test_GEODIST(BaseTestCase.AlgorithmTestCase):

    algo_scripts = [
        'content/toolkit/geoDist/geoDist.R'
    ]
    md5_digests = {
        'proj_current_Rhinella.tif' : '896efaaded8035a41ec8b67b23af81b0',
        'Rhinella.model.object.RData' : None
    }


class Test_GEOIDW(BaseTestCase.AlgorithmTestCase):

    algo_scripts = [
        'content/toolkit/geoIDW/geoIDW.R'
    ]
    md5_digests =  {
        'proj_current_Rhinella.tif' : 'f817e6703142a870d5d0eef340c9cfe2',
        'Rhinella.model.object.RData' : None
    }
